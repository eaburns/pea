#include <pthread.h>
#include <stdio.h>
#include <string.h>

// libpea.h should be included after pthread.h
// because it redefines some pthread functions
// to make them compatible with the garbage collector.
#include "libpea.h"

struct pea_func {
	void (*func)(void*);
	void *capture;
};

static void panic_errno(const char* fun, int en, const char* file, int line) {
	char buf[512];
	strerror_r(en, buf, 512);
	pea_panic_cstring(buf, file, line);
}

static void* thread_start(void* pea_func) {
	struct pea_func *f = pea_func;
	f->func(f->capture);
	pthread_exit(NULL);
	return NULL;
}

// The argument is passed by-value.
// We need to copy it to the heap before passing it to the thread.
void sys__thread__new(struct pea_func* f_by_value) {
	pthread_attr_t attr;
	int en = pthread_attr_init(&attr);
	if (en != 0) {
		panic_errno("pthread_attr_init", en, __FILE__, __LINE__);
	}
	en = pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED);
	if (en != 0) {
		pthread_attr_destroy(&attr);
		panic_errno("pthread_attr_setdetachstate", en, __FILE__, __LINE__);
	}
	pthread_t thread_id;
	// Copy the argument value to the heap before passing it to the thread.
	struct pea_func *f = pea_malloc(sizeof(struct pea_func));
	*f = *f_by_value;
	en = pthread_create(&thread_id, &attr, thread_start, f);
	if (en != 0) {
		pthread_attr_destroy(&attr);
		panic_errno("pthread_create", en, __FILE__, __LINE__);
	}
}

static void destroy_mutex(void* mu, void* unused) {
	pthread_mutex_destroy(mu);
}

void sys__thread__mutex(void** ret) {
	pthread_mutexattr_t mutex_attr;
	pthread_mutexattr_init(&mutex_attr);
	pthread_mutexattr_settype(&mutex_attr, PTHREAD_MUTEX_ERRORCHECK);
	pthread_mutex_t* mu = pea_malloc(sizeof(pthread_mutex_t));
	int en = pthread_mutex_init(mu, &mutex_attr);
	pthread_mutexattr_destroy(&mutex_attr);
	if (en != 0) {
		panic_errno("pthread_mutex_init", en, __FILE__, __LINE__);
	}
	pea_register_finalizer(mu, destroy_mutex, NULL);
	*ret = mu;
}

void sys__thread__lock(void* mu) {
	int en = pthread_mutex_lock(mu);
	if (en != 0) {
		panic_errno("pthread_mutex_lock", en, __FILE__, __LINE__);
	}
}

void sys__thread__unlock(void* mu) {
	int en = pthread_mutex_unlock(mu);
	if (en != 0) {
		panic_errno("pthread_mutex_unlock", en, __FILE__, __LINE__);
	}
}
