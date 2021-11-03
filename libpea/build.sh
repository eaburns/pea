#!/bin/sh
clang=clang
ar=llvm-ar

echo "Building gc"
gc_path=vendor/gc-8.2.0
(cd libpea/$gc_path && \
	make -f Makefile.direct > /dev/null)

echo "Building libunwind"
libunwind_path=vendor/libunwind-11.0.0.src
(cd libpea/$libunwind_path && \
	cmake -Wno-dev . > /dev/null && \
	make > /dev/null)

echo "Building libpea.a"
rm -f libpea/libpea.a
$clang -pthread \
	-g \
	-I libpea/$gc_path/include \
	-I libpea/$libunwind_path/include \
	-c libpea/libpea.c \
	-o libpea/libpea.o
$ar qcL libpea/libpea.a \
	libpea/libpea.o \
	libpea/$gc_path/gc.a \
	libpea/$libunwind_path/lib/libunwind.a