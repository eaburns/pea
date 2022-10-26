#include <errno.h>
#include <netdb.h>
#include <netinet/in.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <unistd.h>

#include "libpea.h"

DEFINE_PEA_ARRAY_STRUCT(pea_uint8_array, uint8_t);

static void get_pea_addr(const struct sockaddr* sock, struct pea_uint8_array* ip, uint16_t* port) {
	if (sock->sa_family == AF_INET) {
		const struct sockaddr_in *sock_ip = (const struct sockaddr_in*) sock;
		in_addr_t addr = sock_ip->sin_addr.s_addr;
		ip->length = 4;
		ip->data = pea_malloc(4);
		ip->data[3] = (addr >> 24) & 0xFF;
		ip->data[2] = (addr >> 16) & 0xFF;
		ip->data[1] = (addr >> 8) & 0xFF;
		ip->data[0] = (addr >> 0) & 0xFF;
		*port = ntohs(sock_ip->sin_port);
		return;
	}
	if (sock->sa_family == AF_INET6) {
		const struct sockaddr_in6 *sock_ip = (const struct sockaddr_in6*) sock;
		ip->length = 16;
		ip->data = pea_malloc(16);
		memcpy(ip->data, sock_ip->sin6_addr.s6_addr, 16);
		*port = ntohs(sock_ip->sin6_port);
		return;
	}
	pea_panic_cstring("impossible non-IP address", __FILE__, __LINE__);
}

// The return value is a gai_errno
// The value by ai must be freed with freeaddrinfo if non-NULL.
static int get_pea_addrinfo(const struct pea_string* network, const struct pea_string* service, _Bool passive, struct addrinfo** ai) {
	char *c_network = NULL;
	if (network->length > 0) {
		c_network = calloc(network->length+1, sizeof(char));
		memcpy(c_network, network->data, network->length);
	}
	char *c_service = NULL;
	if (service->length > 0) {
		c_service = calloc(service->length+1, sizeof(char));
		memcpy(c_service, service->data, service->length);
	}
	struct addrinfo hint = {0};
	hint.ai_family = AF_UNSPEC;
	hint.ai_socktype = SOCK_STREAM;
	hint.ai_protocol = IPPROTO_TCP;
	if (passive) {
		hint.ai_flags = AI_PASSIVE;
	}
	int res = 0;
	if (passive && c_network == NULL) {
		// No network means use all networks.
		// We first try IPv6, which should handle
		// IPv4 also by way of IPv4-to-IPv6 mapping.
		// It's unclear whether IPv4-to-IPv6 mapping works
		// on systems without IPv6 configured.
		// To hedge against that case, if IPv6 fails,
		// fallback on trying the IPv4 any networkess
		res = getaddrinfo("::", c_service, &hint, ai);
		if (res != 0) {
			res = getaddrinfo("0.0.0.0", c_service, &hint, ai);
		}
	} else {
		res = getaddrinfo(c_network, c_service, &hint, ai);
	}
	free(c_service);
	free(c_network);
	return res;
}

void sys__net__tcp__listener_fd(const struct pea_string* network, const struct pea_string* service, struct pea_string* err, int32_t* ret) {
	struct addrinfo* ai = NULL;
	int res = get_pea_addrinfo(network, service, true, &ai);
	if (res != 0 || ai == NULL) {
		const char* str = gai_strerror(res);
		err->length = strlen(str);
		err->data = pea_malloc(err->length);
		memcpy(err->data, str, err->length);
		*ret = -1;
		return;
	}
	int fd = socket(ai->ai_family, ai->ai_socktype, ai->ai_protocol);
	if (fd < 0) {
		goto errno_exit;
	}
	if (bind(fd, ai->ai_addr, ai->ai_addrlen) < 0) {
		goto errno_exit;
	}
	if (listen(fd, 0) < 0) {
		goto errno_exit;
	}
	*ret = fd;
	goto exit;
errno_exit:
	err->data = pea_malloc(100);
	strerror_r(errno, err->data, 99);
	err->length = strlen(err->data);
	// Make sure to call close(fd) only after reading errno,
	// so that the call to close doesn't overwrite the errno value
	// from an error above.
	if (fd >= 0) {
		close(fd);
	}
	*ret = -1;
exit:
	freeaddrinfo(ai);
}

void sys__net__tcp__accept(int32_t fd, int32_t *ret) {
	*ret = accept(fd, NULL, NULL);
	if (*ret < 0) {
		*ret = errno;
	}
}

void sys__net__tcp__connect_fd(const struct pea_string* network, const struct pea_string* service, struct pea_string* err, int32_t* ret) {
	struct addrinfo* ai = NULL;
	int res = get_pea_addrinfo(network, service, false, &ai);
	if (res != 0 || ai == NULL) {
		const char* str = gai_strerror(res);
		err->length = strlen(str);
		err->data = pea_malloc(err->length);
		memcpy(err->data, str, err->length);
		*ret = -1;
		return;
	}
	int fd = socket(ai->ai_family, ai->ai_socktype, ai->ai_protocol);
	if (fd < 0) {
		goto errno_exit;
	}
	for (struct addrinfo* p = ai; p != NULL; p = p->ai_next) {
		if (connect(fd, p->ai_addr, p->ai_addrlen) >= 0) {
			*ret = fd;
			goto exit;
		}
	}
	// Fall through to report error from last connect attempt.
errno_exit:
	err->data = pea_malloc(100);
	strerror_r(errno, err->data, 99);
	err->length = strlen(err->data);
	// Make sure to call close(fd) only after reading errno,
	// so that the call to close doesn't overwrite the errno value
	// from an error above.
	if (fd >= 0) {
		close(fd);
	}
	*ret = -1;
exit:
	freeaddrinfo(ai);
}

void sys__net__tcp__getsockname(int32_t fd, struct pea_uint8_array* ip, uint16_t* port, int32_t* ret) {
	struct sockaddr_storage sock = {0};
	socklen_t socklen = sizeof(sock);
	int res = getsockname(fd, (struct sockaddr*) &sock, &socklen);
	if (res < 0) {
		*ret = -errno;
		return;
	};
	get_pea_addr((struct sockaddr*) &sock, ip, port);
	*ret = 0;
	return;
}

void sys__net__tcp__getpeername(int32_t fd, struct pea_uint8_array* ip, uint16_t* port, int32_t* ret) {
	struct sockaddr_storage sock = {0};
	socklen_t socklen = sizeof(sock);
	int res = getpeername(fd, (struct sockaddr*) &sock, &socklen);
	if (res < 0) {
		*ret = -errno;
		return;
	};
	get_pea_addr((struct sockaddr*) &sock, ip, port);
	*ret = 0;
	return;
}
