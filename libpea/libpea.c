#include <fcntl.h>
#define UNW_LOCAL_ONLY
#include <libunwind.h>
#include <pthread.h>
#include <setjmp.h>
#include <signal.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/errno.h>
#include <sys/wait.h>
#include <unistd.h>

#include "libpea.h"

// pea_print_stack prints the stack trace to standard output.
static void pea_print_stack();

// pea_abort prints the stack trace and then aborts the program.
static void pea_abort();

// abort_errno prints the errno error string and calls pea_abort.
static void abort_errno(const char *msg, int err);

// panic_test_output writes the data to the test panic output file
// if a test is currently running, otherwise does nothing.
static void panic_test_output(const char* data, int n);

void* pea_malloc(uint64_t bytes) {
	return GC_MALLOC(bytes);
}

void* pea_malloc_no_scan(uint64_t bytes) {
	return GC_MALLOC_ATOMIC(bytes);
}

void pea_register_finalizer(void* obj, void(*fn)(void*, void*), void* data) {
	void (*old_fn)(void*, void*);
	void *old_data;
	GC_REGISTER_FINALIZER(obj, fn, data, &old_fn, &old_data);
}

// pea_print prints a string to standard output.
void pea_print(pea_string* pstr) {
	for (int i = 0;  i< pstr->length; i++) {
		putchar(pstr->data[i]);
	}
	fflush(stdout);
}

// pea_print_int prints an int to standand output.
// This is a temporary debugging function.
void pea_print_int(int64_t i) {
	printf("%ld", (long) i);
}

// pea_panic prints a sting and stack trace to standard output
// and aborts the program.
void pea_panic(pea_string* pstr, const char* file, int32_t line) {
	panic_test_output(&pstr->data[0], pstr->length);
	printf("Panic: ");
	pea_print(pstr);
	putchar('\n');
	if (file != NULL) {
		printf("%s:%d\n", file, line);
	}
	pea_abort();
}

void pea_panic_cstring(const char* str, const char* file, int32_t line) {
	panic_test_output(str, strlen(str));
	printf("Panic: %s\n", str);
	if (file != NULL) {
		printf("%s:%d\n", file, line);
	}
	pea_abort();
}

// pea_index_oob_string returns an index-out-of-bounds panic string.
pea_string* pea_index_oob_string(intptr_t index, intptr_t length) {
	const int buf_size = 500;
	char *buf = pea_malloc(buf_size);
	int n = snprintf(buf, buf_size, "index out of bounds: index=%ld, length=%ld",
		(long int) index, (long int) length);
	pea_string *pstr = pea_malloc(sizeof(pea_string));
	pstr->data = buf;
	pstr->length = n >= buf_size ? buf_size - 1 : n;
	return pstr;
}

// pea_slice_oob_string returns a slice-out-of-bounds panic string.
pea_string* pea_slice_oob_string(intptr_t start, intptr_t end, intptr_t length) {
	const int buf_size = 500;
	char *buf = pea_malloc(buf_size);
	int n = snprintf(buf, buf_size, "slice out of bounds: start=%ld, end=%ld, length=%ld",
		(long int) start, (long int) end, (long int) length);
	pea_string *pstr = pea_malloc(sizeof(pea_string));
	pstr->data = buf;
	pstr->length = n >= buf_size ? buf_size - 1 : n;
	return pstr;
}

struct frame {
	jmp_buf jmp;
	struct frame* next;
};

static pthread_key_t frame_stack_key;
static pthread_once_t frame_stack_once = PTHREAD_ONCE_INIT;

static void create_frame_stack() {
	int err = pthread_key_create(&frame_stack_key, NULL);
	if (err == 0) {
		return;
	}
	abort_errno("failed to create frame stack key", err);
}

// pea_new_frame returns a new frame used for long returning.
// The caller must use setjmp to set the jmp field,
// guaranteed to be the first field of the returned struct.
void* pea_new_frame() {
	pthread_once(&frame_stack_once, create_frame_stack);

	struct frame* frame = pea_malloc(sizeof(struct frame));
	frame->next = pthread_getspecific(frame_stack_key);
	pthread_setspecific(frame_stack_key, frame);
	return frame;
}

// pea_finish_frame marks the frame pointed to by frame_handle
// as finished; no further long returns can return to this frame_handle
// without causing the program to abort.
void pea_finish_frame(void* frame_handle) {
	struct frame* frame = frame_handle;
	pthread_setspecific(frame_stack_key, frame->next);
}

// pea_check_frame aborts if the frame pointed to by frame_handle
// is not a live frame on the current thread's stack.
void pea_check_frame(void *frame_handle) {
	struct frame* frame = frame_handle;
	struct frame* p = NULL;
	for (p = pthread_getspecific(frame_stack_key);
		p != NULL && p != frame;
		p = p->next)
		;
	if (p == NULL) {
		puts("Panic: long return across threads or to returned frame");
		pea_abort();
	}
}

// pea_long_return marks the frame pointed to by frame_handle
// as finished and returns from the function corresponding to the frame.
void pea_long_return(void* frame_handle) {
	struct frame* frame = frame_handle;
	pea_check_frame(frame);
	pea_finish_frame(frame);
	longjmp(frame->jmp, 1);
}

// test_panic_fd, if non-negative, as a file descriptor
// to copy panic messages to in implementation of test failure output.
// This is non-negative if executing a test.
static int test_panic_fd = -1;

// test_file_mutex guards test_file_name and test_file_line.
static pthread_mutex_t test_file_mutex = PTHREAD_MUTEX_INITIALIZER;

// test_file_name is the file name of the current test definition.
// This is non-NULL if locations were enabled at compilation time
// and if currently executing a function called from a test body.
static const char *test_file_name = NULL;

// test_file_line is just like test_file_name, but it contains the line number.
static int32_t test_file_line = 0;

// pea_set_test_call_loc sets the location of a function call made by a test.
// This is used to implement test failure messages.
// The failure should print the line of the test definition that eventually panicked.
// Since we currently don't have support for finding file/line location
// during stack unwinding, we implement it by annotating each call
// made from a test definition with its file name and line.
// This function sets the annotation for the call made immediately after it.
// When a panic happens in a test, the file/line set by this function
// can be used to generate the error message.
//
// TODO: remove the pea_set_test_call_loc mechanism
// once unwinding is source-location aware.
void pea_set_test_call_loc(const char* file, int32_t line) {
	pthread_mutex_lock(&test_file_mutex);
	test_file_name = file;
	test_file_line = line;
	pthread_mutex_unlock(&test_file_mutex);
}

static void panic_test_output(const char* data, int n) {
	if (test_panic_fd < 0) {
		return;
	}
	FILE* test_panic_file = fdopen(test_panic_fd, "w");
	if (test_panic_file == NULL) {
		abort_errno("failed to open the test panic filedescr", errno);
	}
	pthread_mutex_lock(&test_file_mutex);
	if (test_file_name != NULL) {
		fprintf(test_panic_file, "%s:%d\n", test_file_name, test_file_line);
	}
	pthread_mutex_unlock(&test_file_mutex);
	fwrite(data, 1, n, test_panic_file);
}

static void print_test_output(int fd) {
	putchar('\t');
	char buf[512];
	int nl = 0;
	for (; ;) {
		ssize_t n = read(fd, buf, 512);
		if (n < 0 && errno != EAGAIN) {
			abort_errno("failed to read", errno);
		}
		if (n == 0) {
			break;
		}
		for (int i = 0; i < n; i++) {
			if (nl) {
				putchar('\t');
			}
			putchar(buf[i]);
			nl = buf[i] == '\n';
		}
	}
}

static void test_timeout_handler(int signal) {
	pea_panic_cstring("test timed out", __FILE__, __LINE__);
}

// pea_run_test runs a test and returns 0 on pass and 1 on failure.
int32_t pea_run_test(void(*test)(), const char* name) {
	printf("Test %s", name);

	long timeout_sec = -1;
	const char* timeout_str = getenv("PEA_TEST_TIMEOUT_SECONDS");
	if (timeout_str != NULL) {
		char* end = NULL;
		errno = 0;
		timeout_sec = strtoll(timeout_str, &end, 10);
		if (errno != 0) {
			printf("bad test timeout: %s\n", timeout_str);
			return 1;
		}
		if (timeout_sec > 3600) {
			printf("ignoring too-long timeout: %ld > 3600 seconds\n", timeout_sec);
			return 1;
		}
		printf(" (timeout: %ld seconds)", timeout_sec);
	}

	printf("… ");
	// fflush stdout so the buffer is empty for the child process.
	fflush(stdout);

	char out_file[] = "/tmp/pea.out.XXXXXX";
	int out = mkstemp(out_file);
	if (out < 0) {
		abort_errno("failed to create the test output file", errno);
	}
	char panic_file[] = "/tmp/pea.panic.XXXXXX";
	int panic_out = mkstemp(panic_file);
	if (panic_out < 0) {
		abort_errno("failed to create the test panic output file", errno);
	}

	pid_t kid = fork();
	if (kid < 0) {
		abort_errno("fork failed", errno);
	}
	if (kid == 0) {
		int in = open("/dev/null", O_RDONLY);
		if (in < 0) {
			abort_errno("failed to open /dev/null for reading", errno);
		}
		dup2(in, STDIN_FILENO);
		dup2(out, STDOUT_FILENO);
		dup2(out, STDERR_FILENO);
		test_panic_fd = panic_out;
		if (timeout_sec > 0) {
			struct sigaction sa;
			sa.sa_handler = test_timeout_handler;
			sigemptyset(&sa.sa_mask);
			sa.sa_flags = SA_RESTART;
			if (sigaction(SIGALRM, &sa, NULL) < 0) {
				abort_errno("sigaction failed", errno);
			}
			alarm(timeout_sec);
		}
		test();
		exit(0);
	}

	// Close the temporary files; they will be written by the child.
	// We re-open them after the child has exited.
	close(out);
	close(panic_out);

	int status = 0;
	if (waitpid(kid, &status, 0) < 0) {
		abort_errno("wait failed", errno);
	}
	if (WIFEXITED(status) && WEXITSTATUS(status) == 0) {
		puts("ok");
		unlink(panic_file);
		unlink(out_file);
		return 0;
	}

	puts("FAILED");
	panic_out = open(panic_file, O_RDONLY);
	if (panic_out < 0) {
		abort_errno("failed to open the test output file", errno);
	}
	print_test_output(panic_out);
	close(panic_out);
	unlink(panic_file);

	printf("\n\t---- output ----\n");
	close(out);
	out = open(out_file, O_RDONLY);
	if (out < 0) {
		abort_errno("failed to open the test output file", errno);
	}
	print_test_output(out);
	close(out);
	unlink(out_file);
	return 1;
}

static void pea_abort() {
	pea_print_stack();
	fflush(stdout);
	abort();
}

static void abort_errno(const char *msg, int err) {
	puts("Panic: ");
	char buf[256];
	int n = strerror_r(err, buf, 256);
	if (n != 0) {
		puts(msg);
	} else {
		printf("%s: %s\n", msg, buf);
	}
	pea_abort();
}

static void pea_print_stack() {
	unw_context_t uc;
	unw_getcontext(&uc);
	unw_cursor_t cursor;
	unw_init_local(&cursor, &uc);
	int i = 0;
	puts("Stack:");
	while (unw_step(&cursor) > 0) {
		char buf[512];
		unw_word_t offs;
		if (unw_get_proc_name(&cursor, buf, 512, &offs) < 0) {
			break;
		}
		printf("%3d: %s+0x%lx\n", i, buf, (unsigned long) offs);
		i++;
	}
}