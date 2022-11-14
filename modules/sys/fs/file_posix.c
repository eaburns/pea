#include <dirent.h>
#include <fcntl.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <sys/errno.h>
#include <sys/stat.h>
#include <unistd.h>

#include "libpea.h"

static char* cstring(pea_string* str) {
	char* cstr = calloc(str->length + 1, 1);
	memcpy(cstr, str->data, str->length);
	return cstr;
}

PEA_FUNC0(pea_int32, sys__fs__o_rdonly) { PEA_RETURN(O_RDONLY); }
PEA_FUNC0(pea_int32, sys__fs__o_wronly) { PEA_RETURN(O_WRONLY); }
PEA_FUNC0(pea_int32, sys__fs__o_rdwr) { PEA_RETURN(O_RDWR); }
PEA_FUNC0(pea_int32, sys__fs__o_append) { PEA_RETURN(O_APPEND); }
PEA_FUNC0(pea_int32, sys__fs__o_create) { PEA_RETURN(O_CREAT); }
PEA_FUNC0(pea_int32, sys__fs__o_trunc) { PEA_RETURN(O_TRUNC); }
PEA_FUNC0(pea_int32, sys__fs__o_excl) { PEA_RETURN(O_EXCL); }
PEA_FUNC0(pea_int32, sys__fs__o_directory) { PEA_RETURN(O_DIRECTORY); }

PEA_FUNC2(pea_int32, sys__fs__fstat, pea_int32 fd, pea_int32 *is_dir) {
	struct stat buf;
	for ( ; ; ) {
		int res = fstat(fd, &buf);
		if (res >= 0) {
			*is_dir = S_ISDIR(buf.st_mode);
			PEA_RETURN(res);
		}
		if (errno != EINTR) {
			*is_dir = 0;
			PEA_RETURN(-errno);
		}
	}
}

PEA_FUNC3(pea_int32, sys__fs__open, pea_string* str, pea_int32 flags, pea_int32 perm) {
	char* cstr = cstring(str);
	for ( ; ; ) {
		int fd = open(cstr, flags, perm);
		if (fd >= 0) {
			free(cstr);
			PEA_RETURN(fd);
		}
		if (errno != EINTR) {
			free(cstr);
			PEA_RETURN(-errno);
		}
	}
}

PEA_FUNC1(pea_int32, sys__fs__unlink, pea_string* str) {
	char* cstr = cstring(str);
	for ( ; ; ) {
		int res = unlink(cstr);
		if (res >= 0) {
			free(cstr);
			PEA_RETURN(0);
		}
		if (errno != EINTR) {
			free(cstr);
			PEA_RETURN(-errno);
		}
	}
}

PEA_FUNC2(pea_int32, sys__fs__fdopendir, pea_int32 fd, DIR** dir_ret) {
	for ( ; ; ) {
		DIR* dir = fdopendir(fd);
		if (dir != NULL) {
			*dir_ret = dir;
			PEA_RETURN(0);
		}
		if (errno != EINTR) {
			PEA_RETURN(-errno);
		}
	}
}

PEA_FUNC1(pea_int32, sys__fs__closedir, void* dir) {
	for ( ; ; ) {
		if (closedir(dir) == 0) {
			PEA_RETURN(0);
		}
		if (errno != EINTR) {
			PEA_RETURN(-errno);
		}
	}
}

PEA_FUNC2(pea_int32, sys__fs__readdir, DIR* dir, pea_string* str) {
	struct dirent* ent = NULL;
	for ( ; ; ) {
		errno = 0;
		ent = readdir(dir);
		if (ent != NULL) {
			str->length = strlen(ent->d_name);
			str->data = pea_malloc(str->length);
			memcpy(str->data, ent->d_name, str->length);
			PEA_RETURN(0);
		}
		if (errno == 0) {
			str->length = 0;
			str->data = NULL;
			PEA_RETURN(0);
		}
		if (errno != EINTR) {
			PEA_RETURN(-errno);
		}
	}
}

PEA_FUNC2(pea_int32, sys__fs__mkdir, pea_string* str, pea_int32 perm) {
	char* cstr = cstring(str);
	for ( ; ; ) {
		if (mkdir(cstr, perm) == 0) {
			free(cstr);
			PEA_RETURN(0);
		}
		if (errno != EINTR) {
			free(cstr);
			PEA_RETURN(-errno);
		}
	}
}

PEA_FUNC1(pea_int32, sys__fs__rmdir, pea_string* str) {
	char* cstr = cstring(str);
	for ( ; ; ) {
		if (rmdir(cstr) == 0) {
			free(cstr);
			PEA_RETURN(0);
		}
		if (errno != EINTR) {
			free(cstr);
			PEA_RETURN(-errno);
		}
	}
}
