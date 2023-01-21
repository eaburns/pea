#include <errno.h>
#include <stdint.h>
#include <string.h>

#include "libpea.h"

#define DEFINE_CONSTANT(name) \
	PEA_FUNC0(int32_t, sys__internal__posix__##name) { PEA_RETURN(name); }

DEFINE_CONSTANT(errno)

PEA_VOID_FUNC2(sys__internal__posix__strerror_r, int32_t e, PEA_ARRAY(char)* buf) {
	strerror_r(e, buf->data, buf->length);
}

DEFINE_CONSTANT(CLOCK_REALTIME)
DEFINE_CONSTANT(CLOCK_MONOTONIC)

PEA_FUNC3(int32_t, sys__internal__posix__clock_gettime, int32_t clock_id, int64_t *sec, int64_t *nsec) {
	struct timespec timespec;
	if (clock_gettime(clock_id,  &timespec) < 0) {
		PEA_RETURN(-1);
	}
	*sec = timespec.tv_sec;
	*nsec = timespec.tv_nsec;
	PEA_RETURN(0);
}
