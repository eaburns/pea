#include <pthread.h>
#include <stdio.h>
#include <string.h>

static void destroy_mutex(void* mu, void* unused) {
	pthread_mutex_destroy(mu);
}

static void panic_errno(const char* fun, int en, const char* file, int line) {
	extern void pea_panic_cstring(const char*, const char*, int32_t);

	char buf[512];
	strerror_r(en, buf, 512);
	pea_panic_cstring(buf, file, line);
}

void sys__thread__mutex(void** ret) {
	extern void* pea_malloc(int);
	extern void pea_register_finalizer(void*, void(*)(void*, void*), void*);

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
