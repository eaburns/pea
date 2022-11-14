#include <errno.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

static uint64_t must_clock_gettime_ns(clockid_t clock_type) {
	struct timespec timespec;
	if (clock_gettime(clock_type,  &timespec) < 0) {
		char buf[512];
		strerror_r(errno, buf, 512);
		printf("clock_gettime() failed: %s\n", buf);
		abort();
	}
	int64_t sec = timespec.tv_sec;
	int64_t nsec = timespec.tv_nsec;
	int64_t ret = sec*1000000000 + nsec;
	if (ret < 0) {
		printf("clock_gettime() time is too big");
		abort();
	}
	return ret;
}

PEA_FUNC0(pea_int64, sys__time__wall_ns) {
	PEA_RETURN(must_clock_gettime_ns(CLOCK_REALTIME));
}

PEA_FUNC0(pea_int64, sys__time__monotonic_ns) {
	PEA_RETURN(must_clock_gettime_ns(CLOCK_MONOTONIC));
}
