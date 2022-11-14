#include <sys/errno.h>
#include <stdint.h>
#include <string.h>
#include <unistd.h>

#include "libpea.h"

PEA_FUNC0(pea_int32, sys__internal__eacces) { PEA_RETURN(EACCES); }
PEA_FUNC0(pea_int32, sys__internal__eexist) { PEA_RETURN(EEXIST); }
PEA_FUNC0(pea_int32, sys__internal__enoent) { PEA_RETURN(ENOENT); }
PEA_FUNC0(pea_int32, sys__internal__enotdir) { PEA_RETURN(ENOTDIR); }
PEA_FUNC0(pea_int32, sys__internal__eisdir) { PEA_RETURN(EISDIR); }

PEA_VOID_FUNC2(sys__internal__strerror_r, pea_int32 n, pea_string* buf) {
	strerror_r(n, buf->data, buf->length);
}

PEA_FUNC1(pea_int32, sys__internal__close, pea_int32 fd) {
	for ( ; ; ) {
		if (close(fd) == 0) {
			PEA_RETURN(0);
		}
		if (errno != EINTR) {
			PEA_RETURN(-errno);
		}
	}
}

PEA_FUNC2(pea_int64, sys__internal__read, pea_int32 fd, pea_string* buf) {
	for ( ; ; ) {
		ssize_t n = read(fd, buf->data, buf->length);
		if (n >= 0) {
			PEA_RETURN(n);
		}
		if (errno != EINTR) {
			PEA_RETURN(-errno);
		}
	}
}

PEA_FUNC2(pea_int64, sys__internal__write, pea_int32 fd, pea_string* buf) {
	for ( ; ; ) {
		ssize_t n = write(fd, buf->data, buf->length);
		if (n >= 0) {
			PEA_RETURN(n);
		}
		if (errno != EINTR) {
			PEA_RETURN(-errno);
		}
	}
}