#include <stdint.h>
#include <sys/errno.h>
#include <unistd.h>

void sys__io__create_pipe(int32_t* read, int32_t* write, int32_t* ret) {
	int filedes[2];
	for ( ; ; ) {
		if (pipe(filedes) == 0) {
			*read = filedes[0];
			*write = filedes[1];
			*ret = 0;
			return;
		}
		if (errno != EINTR) {
			*ret = -errno;
			return;
		}
	}
}
