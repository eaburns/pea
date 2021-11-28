#include <netdb.h>
#include <netinet/in.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>

#include <stdio.h>

#include "libpea.h"

DEFINE_PEA_ARRAY_STRUCT(pea_uint8_array, uint8_t);
DEFINE_PEA_ARRAY_STRUCT(pea_addr_array, struct pea_uint8_array);

void sys__net__ip__lookup_host(struct pea_string* host, struct pea_addr_array* addrs, int32_t* ret) {
	char *c_host = malloc(host->length+1);
	memcpy(c_host, host->data, host->length);
	c_host[host->length] = 0;

	struct addrinfo hint = {0};
	hint.ai_family = AF_UNSPEC;
	struct addrinfo *addrinfos = NULL;
	*ret = getaddrinfo(c_host, NULL, &hint, &addrinfos);
	if (*ret != 0) {
		goto exit;
	}

	addrs->length = 0;
	for (struct addrinfo *p = addrinfos; p != NULL; p = p->ai_next) {
		if (p->ai_family == AF_INET || p->ai_family == AF_INET6) {
			addrs->length++;
		}
	}
	int i = 0;
	addrs->data = pea_malloc(sizeof(struct pea_uint8_array)*addrs->length);
	for (struct addrinfo *p = addrinfos; p != NULL; p = p->ai_next) {
		if (p->ai_family == AF_INET) {
			struct pea_uint8_array* pea_addr = &addrs->data[i];
			struct sockaddr_in *sock = (struct sockaddr_in*) p->ai_addr;
			in_addr_t addr = sock->sin_addr.s_addr;
			pea_addr->length = 4;
			pea_addr->data = pea_malloc(4);
			pea_addr->data[3] = (addr >> 24) & 0xFF;
			pea_addr->data[2] = (addr >> 16) & 0xFF;
			pea_addr->data[1] = (addr >> 8) & 0xFF;
			pea_addr->data[0] = (addr >> 0) & 0xFF;
			i++;
			continue;
		}
		if (p->ai_family == AF_INET6) {
			struct pea_uint8_array* pea_addr = &addrs->data[i];
			struct sockaddr_in6 *sock = (struct sockaddr_in6*) p->ai_addr;
			addrs->data[i].length = 16;
			addrs->data[i].data = pea_malloc(16);
			memcpy(pea_addr->data, sock->sin6_addr.s6_addr, 16);
			i++;
			continue;
		}
	}
exit:
	free(c_host);
	if (addrinfos != NULL) {
		freeaddrinfo(addrinfos);
	}
}
