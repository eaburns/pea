#include <sys/errno.h>
#include <stdint.h>
#include <string.h>
#include <unistd.h>

struct pea_string {
	uintptr_t length;
	char* data;
};

void sys__internal__eacces(int32_t *ret) { *ret = EACCES; }
void sys__internal__eexist(int32_t *ret) { *ret = EEXIST; }
void sys__internal__enoent(int32_t *ret) { *ret = ENOENT; }
void sys__internal__enotdir(int32_t *ret) { *ret = ENOTDIR; }

void sys__internal__strerror_r(int32_t n, struct pea_string* buf) {
	strerror_r(n, buf->data, buf->length);
}

void sys__internal__close(int32_t fd, int32_t *ret) {
	for ( ; ; ) {
		if (close(fd) == 0) {
			*ret = 0;
			return;
		}
		if (errno != EINTR) {
			*ret = -errno;
			return;
		}
	}
}

void sys__internal__read(int32_t fd, struct pea_string* buf, int64_t *ret) {
	for ( ; ; ) {
		ssize_t n = read(fd, buf->data, buf->length);
		if (n >= 0) {
			*ret = n;
			return;
		}
		if (errno != EINTR) {
			*ret = -errno;
			return;
		}
	}
}

void sys__internal__write(int32_t fd, struct pea_string* buf, int64_t *ret) {
	for ( ; ; ) {
		ssize_t n = write(fd, buf->data, buf->length);
		if (n >= 0) {
			*ret = n;
			return;
		}
		if (errno != EINTR) {
			*ret = -errno;
			return;
		}
	}
}