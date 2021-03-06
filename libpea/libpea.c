#define UNW_LOCAL_ONLY
#include <fcntl.h>
#include <gc.h>
#include <libunwind.h>
#include <pthread.h>
#include <setjmp.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/errno.h>
#include <sys/wait.h>
#include <unistd.h>

// pea_malloc returns a pointer to a new object of the given number of bytes.
void* pea_malloc(int bytes) {
	return GC_MALLOC(bytes);
}

// pea_print_stack prints the stack trace to standard output.
void pea_print_stack() {
	unw_context_t uc;
	unw_getcontext(&uc);
	unw_cursor_t cursor;
	unw_init_local(&cursor, &uc);
	int i = 0;
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

static void die(const char *msg, int err) {
	char buf[256];
	int n = strerror_r(err, buf, 256);
	if (n != 0) {
		puts(msg);
	} else {
		printf("%s: %s\n", msg, buf);
	}
	pea_print_stack();
	abort();
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
	die("failed to create frame stack key", err);
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
		puts("long return across threads or to already-returned frame");
		pea_print_stack();
		abort();
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

struct pea_string {
	uintptr_t length;
	const char* data;
};

// pea_print prints a string to standard output.
void pea_print(struct pea_string* pstr) {
	for (int i = 0;  i< pstr->length; i++) {
		putchar(pstr->data[i]);
	}
}

// test_panic_fd, if non-negative, as a file descriptor
// to copy panic messages to in implementation of test failure output.
// This is non-negative if executing a test.
static int test_panic_fd = -1;

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
	test_file_name = file;
	test_file_line = line;
}

// pea_panic prints a sting and stack trace to standard output
// and aborts the program.
void pea_panic(struct pea_string* pstr, const char* file, int32_t line) {
	if (test_panic_fd >= 0) {
		FILE* test_panic_file = fdopen(test_panic_fd, "w");
		if (test_panic_file == NULL) {
			die("failed to open the test panic filedescr", errno);
		}
		if (test_file_name != NULL) {
			fprintf(test_panic_file, "%s:%d\n", test_file_name, test_file_line);
		}
		fwrite(&pstr->data[0], 1, pstr->length, test_panic_file);
	}
	printf("Panic: ");
	pea_print(pstr);
	putchar('\n');
	if (file != NULL) {
		printf("%s:%d\n", file, line);
	}
	puts("Stack:");
	pea_print_stack();
	abort();
}

// pea_print_int prints an int to standand output.
// This is a temporary debugging function.
void pea_print_int(int64_t i) {
	printf("%ld", (long) i);
}

static void print_test_output(int fd) {
	putchar('\t');
	char buf[512];
	int nl = 0;
	for (; ;) {
		ssize_t n = read(fd, buf, 512);
		if (n < 0 && errno != EAGAIN) {
			die("failed to read", errno);
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

// pea_run_test runs a test and returns 0 on pass and 1 on failure.
int32_t pea_run_test(void(*test)(), const char* name) {
	printf("Test %s... ", name);
	// fflush stdout so the buffer is empty for the child process.
	fflush(stdout);

	char out_file[] = "/tmp/pea.out.XXXXXX";
	int out = mkstemp(out_file);
	if (out < 0) {
		die("failed to create the test output file", errno);
	}
	char panic_file[] = "/tmp/pea.panic.XXXXXX";
	int panic_out = mkstemp(panic_file);
	if (panic_out < 0) {
		die("failed to create the test panic output file", errno);
	}

	pid_t kid = fork();
	if (kid < 0) {
		die("fork failed", errno);
	}
	if (kid == 0) {
		int in = open("/dev/null", O_RDONLY);
		if (in < 0) {
			die("failed to open /dev/null for reading", errno);
		}
		dup2(in, STDIN_FILENO);
		dup2(out, STDOUT_FILENO);
		dup2(out, STDERR_FILENO);
		test_panic_fd = panic_out;
		test();
		exit(0);
	}

	// Close the temporary files; they will be written by the child.
	// We re-open them after the child has exited.
	close(out);
	close(panic_out);

	int status = 0;
	if (waitpid(kid, &status, 0) < 0) {
		puts("wait failed");
		abort();
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
		die("failed to open the test output file", errno);
	}
	print_test_output(panic_out);
	close(panic_out);
	unlink(panic_file);

	printf("\n\t---- output ----\n");
	close(out);
	out = open(out_file, O_RDONLY);
	if (out < 0) {
		die("failed to open the test output file", errno);
	}
	print_test_output(out);
	close(out);
	unlink(out_file);
	return 1;
}