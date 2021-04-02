#define UNW_LOCAL_ONLY
#include <gc.h>
#include <libunwind.h>
#include <pthread.h>
#include <setjmp.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

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
		printf("\t%3d: %s+0x%lx\n", i, buf, (unsigned long) offs);
		i++;
	}
}

struct frame {
	jmp_buf jmp;
	struct frame* next;
};

static pthread_key_t frame_stack_key;
static pthread_once_t frame_stack_once = PTHREAD_ONCE_INIT;

static void create_frame_stack() {
	int errno = pthread_key_create(&frame_stack_key, NULL);
	if (errno == 0) {
		return;
	}
	char buf[256];
	int n = strerror_r(errno, buf, 256);
	if (n != 0) {
		puts("failed to create frame stack key");
	} else {
		printf("failed to create frame stack key: %s\n", buf);
	}
	pea_print_stack();
	abort();
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

// pea_panic prints a sting and stack trace to standard output
// and aborts the program.
void pea_panic(struct pea_string* pstr) {
	printf("panic: ");
	pea_print(pstr);
	pea_print_stack();
	abort();
}
