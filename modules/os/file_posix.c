#include <dirent.h>
#include <fcntl.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <sys/errno.h>
#include <unistd.h>

struct pea_string {
	uintptr_t length;
	char* data;
};

static char* cstring(struct pea_string* str) {
	char* cstr = calloc(str->length + 1, 1);
	memcpy(cstr, str->data, str->length);
	return cstr;
}

void os__posix_eacces(int32_t *ret) { *ret = EACCES; }
void os__posix_eexist(int32_t *ret) { *ret = EEXIST; }
void os__posix_enoent(int32_t *ret) { *ret = ENOENT; }
void os__posix_enotdir(int32_t *ret) { *ret = ENOTDIR; }

void os__posix_strerror_r(int32_t n, struct pea_string* buf) {
	strerror_r(n, buf->data, buf->length);
}

void os__posix_o_rdonly(int32_t *ret) { *ret = O_RDONLY; }
void os__posix_o_wronly(int32_t *ret) { *ret = O_WRONLY; }
void os__posix_o_rdwr(int32_t *ret) { *ret = O_RDWR; }
void os__posix_o_append(int32_t *ret) { *ret = O_APPEND; }
void os__posix_o_create(int32_t *ret) { *ret = O_CREAT; }
void os__posix_o_trunc(int32_t *ret) { *ret = O_TRUNC; }
void os__posix_o_excl(int32_t *ret) { *ret = O_EXCL; }

void os__posix_open(struct pea_string* str, int32_t flags, int32_t perm, int32_t *ret) {
	char* cstr = cstring(str);
	for ( ; ; ) {
		int fd = open(cstr, flags, perm);
		if (fd >= 0) {
			free(cstr);
			*ret = fd;
			return;
		}
		if (errno != EINTR) {
			free(cstr);
			*ret = -errno;
			return;
		}
	}
}

void os__posix_close(int32_t fd, int32_t *ret) {
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

void os__posix_read(int32_t fd, struct pea_string* buf, int64_t *ret) {
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

void os__posix_write(int32_t fd, struct pea_string* buf, int64_t *ret) {
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

void os__posix_unlink(struct pea_string* str, int32_t *ret) {
	char* cstr = cstring(str);
	for ( ; ; ) {
		int res = unlink(cstr);
		if (res >= 0) {
			free(cstr);
			*ret = 0;
			return;
		}
		if (errno != EINTR) {
			free(cstr);
			*ret = -errno;
			return;
		}
	}
}

void os__posix_fdopendir(int32_t fd, DIR** dir_ret, int32_t *ret) {
	for ( ; ; ) {
		DIR* dir = fdopendir(fd);
		if (dir != NULL) {
			*dir_ret = dir;
			*ret = 0;
			return;
		}
		if (errno != EINTR) {
			*ret = -errno;
			return;
		}
	}
}

void os__posix_closedir(void* dir, int32_t *ret) {
	for ( ; ; ) {
		if (closedir(dir) == 0) {
			*ret = 0;
			return;
		}
		if (errno != EINTR) {
			*ret = -errno;
			return;
		}
	}
}

void os__posix_readdir(DIR* dir, struct pea_string* str, int32_t *ret) {
	extern void* pea_malloc(int);

	struct dirent* ent = NULL;
	for ( ; ; ) {
		errno = 0;
		ent = readdir(dir);
		if (ent != NULL) {
			str->length = strlen(ent->d_name);
			str->data = pea_malloc(str->length);
			memcpy(str->data, ent->d_name, str->length);
			*ret = 0;
			return;
		}
		if (errno == 0) {
			str->length = 0;
			str->data = NULL;
			*ret = 0;
			return;
		}
		if (errno != EINTR) {
			*ret = -errno;
			return;
		}
	}
}

void os__posix_mkdir(struct pea_string* str, int32_t perm, int32_t *ret) {
	char* cstr = cstring(str);
	for ( ; ; ) {
		if (mkdir(cstr, perm) == 0) {
			free(cstr);
			*ret = 0;
			return;
		}
		if (errno != EINTR) {
			free(cstr);
			*ret = -errno;
			return;
		}
	}
}

void os__posix_rmdir(struct pea_string* str, int32_t *ret) {
	char* cstr = cstring(str);
	for ( ; ; ) {
		if (rmdir(cstr) == 0) {
			free(cstr);
			*ret = 0;
			return;
		}
		if (errno != EINTR) {
			free(cstr);
			*ret = -errno;
			return;
		}
	}
}

