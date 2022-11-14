#include <stdint.h>
#include <sys/errno.h>
#include <unistd.h>

#include "libpea.h"

PEA_FUNC2(pea_int32, sys__io__create_pipe, pea_int32* read, pea_int32* write) {
	int filedes[2];
	for ( ; ; ) {
		if (pipe(filedes) == 0) {
			*read = filedes[0];
			*write = filedes[1];
			PEA_RETURN(0);
		}
		if (errno != EINTR) {
			PEA_RETURN(-errno);
		}
	}
}
