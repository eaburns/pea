#include <netdb.h>
#include <netinet/in.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>

#include "libpea.h"

PEA_FUNC2(pea_int32, sys__net__ip__lookup_host, pea_string* host, PEA_ARRAY(PEA_ARRAY(pea_uint8))* addrs) {
	char *c_host = malloc(host->length+1);
	memcpy(c_host, host->data, host->length);
	c_host[host->length] = 0;

	struct addrinfo hint = {0};
	hint.ai_family = AF_UNSPEC;
	struct addrinfo *addrinfos = NULL;
	int res = getaddrinfo(c_host, NULL, &hint, &addrinfos);
	if (res != 0) {
		goto exit;
	}

	addrs->length = 0;
	for (struct addrinfo *p = addrinfos; p != NULL; p = p->ai_next) {
		if (p->ai_family == AF_INET || p->ai_family == AF_INET6) {
			addrs->length++;
		}
	}
	int i = 0;
	addrs->data = pea_malloc(sizeof(PEA_ARRAY(pea_uint8))*addrs->length);
	for (struct addrinfo *p = addrinfos; p != NULL; p = p->ai_next) {
		if (p->ai_family == AF_INET) {
			PEA_ARRAY(pea_uint8)* pea_addr = &addrs->data[i];
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
			PEA_ARRAY(pea_uint8)* pea_addr = &addrs->data[i];
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
	PEA_RETURN(res);
}
