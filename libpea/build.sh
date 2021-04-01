#!/bin/sh
clang=/usr/bin/clang
ar=/usr/bin/ar

echo "Building gc"
(cd libpea/vendor/gc-8.0.4 && make -f Makefile.direct 2&>1 /dev/null)
echo "Building libpea.c"
$clang -pthread \
	-g \
	-I libpea/vendor/gc-8.0.4/include \
	-c libpea/libpea.c \
	-o libpea/libpea.o
