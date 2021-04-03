#!/bin/sh
clang=/usr/bin/clang
ar=/usr/bin/ar

echo "Building gc"
(cd libpea/vendor/gc-8.0.4 && make -f Makefile.direct)
echo "Building libunwind"
(cd libpea/vendor/libunwind-11.0.0.src && cmake -Wno-dev . && make)
echo "Building libpea.c"
$clang -pthread \
	-g \
	-I libpea/vendor/libunwind-11.0.0.src/include \
	-I libpea/vendor/gc-8.0.4/include \
	-c libpea/libpea.c \
	-o libpea/libpea.o
