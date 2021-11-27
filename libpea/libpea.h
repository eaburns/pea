/*
libpea.h contains declarations of Pea runtime functions useful in C implementations of Pea functions.

This header file may re-define some pthread functions to intercept their calls in order to play well with the garbage collector. So this .h file should be included after pthread.h to make sure those defines are re-directed properly.
*/

#ifndef _LIBPEA_H_
#define _LIBPEA_H_

#define GC_THREADS
#include <gc.h>
#include <stdint.h>

// pea_string is the Pea representation of a string.
struct pea_string {
	uintptr_t length;
	char* data;
};

// pea_malloc returns a pointer to a new object of the given number of bytes.
// The returned pointer is owned by the garbage collector, so it should not be freed, but instead will be released once it is determined to be unreachable.
// Pointer-sized spans of the returned object will be scanned by the garbage collector for liveness checking.
void* pea_malloc(int bytes);

// pea_malloc_no_scan returns a pointer to a new object of the given number of bytes.
// The returned pointer is owned by the garbage collector, so it should not be freed, but instead will be released once it is determined to be unreachable.
// The object will not be scanned by the garbage collector, so it must not contain pointers to objects owned by the garbage collector.
void* pea_malloc_no_scan(int bytes);

// pea_register_finalizer registers fn to run when obj is collected.
// The fn function is called with obj as the first argument and data as the second.
//
// All the various caveats of when to not use finalizers apply:
// this is provided for compatibility with libraries that have explicitly managed memory.
// For example: pthread_mutex_destroy must be called before the mutex is freed or memory will leak.
void pea_register_finalizer(void* obj, void(*fn)(void*, void*), void* data);

// pea_panic_cstring prints a c-style sting and stack trace to standard output
// and aborts the program.
void pea_panic_cstring(const char* str, const char* file, int32_t line);

#endif // _LIBPEA_H_