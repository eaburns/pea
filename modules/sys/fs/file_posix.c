#include <dirent.h>
#include <fcntl.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <sys/errno.h>
#include <sys/stat.h>
#include <unistd.h>

#include "libpea.h"

struct pea_string {
	uintptr_t length;
	char* data;
};

static char* cstring(struct pea_string* str) {
	char* cstr = calloc(str->length + 1, 1);
	memcpy(cstr, str->data, str->length);
	return cstr;
}

void sys__fs__o_rdonly(int32_t *ret) { *ret = O_RDONLY; }
void sys__fs__o_wronly(int32_t *ret) { *ret = O_WRONLY; }
void sys__fs__o_rdwr(int32_t *ret) { *ret = O_RDWR; }
void sys__fs__o_append(int32_t *ret) { *ret = O_APPEND; }
void sys__fs__o_create(int32_t *ret) { *ret = O_CREAT; }
void sys__fs__o_trunc(int32_t *ret) { *ret = O_TRUNC; }
void sys__fs__o_excl(int32_t *ret) { *ret = O_EXCL; }
void sys__fs__o_directory(int32_t *ret) { *ret = O_DIRECTORY; }

void sys__fs__fstat(int32_t fd, int32_t *is_dir, int32_t *ret) {
	struct stat buf;
	for ( ; ; ) {
		int res = fstat(fd, &buf);
		if (res >= 0) {
			*is_dir = S_ISDIR(buf.st_mode);
			*ret = res;
			return;
		}
		if (errno != EINTR) {
			*is_dir = 0;
			*ret = -errno;
			return;
		}
	}
}

void sys__fs__open(struct pea_string* str, int32_t flags, int32_t perm, int32_t *ret) {
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

void sys__fs__unlink(struct pea_string* str, int32_t *ret) {
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

void sys__fs__fdopendir(int32_t fd, DIR** dir_ret, int32_t *ret) {
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

void sys__fs__closedir(void* dir, int32_t *ret) {
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

void sys__fs__readdir(DIR* dir, struct pea_string* str, int32_t *ret) {
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

void sys__fs__mkdir(struct pea_string* str, int32_t perm, int32_t *ret) {
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

void sys__fs__rmdir(struct pea_string* str, int32_t *ret) {
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

