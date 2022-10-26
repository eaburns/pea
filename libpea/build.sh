#!/bin/sh
echo "Building gc"
gc_path=vendor/gc-8.2.0
(cd libpea/$gc_path && \
	make CC=clang CPP=clang++ -f Makefile.direct > /dev/null)

echo "Building libunwind"
libunwind_path=vendor/libunwind-11.0.0.src
(cd libpea/$libunwind_path && \
	cmake -Wno-dev . > /dev/null && \
	make > /dev/null)

echo "Building libpea"
rm -f libpea/libpea.a
clang -pthread \
	-g \
	-I libpea/$gc_path/include \
	-I libpea/$libunwind_path/include \
	-c libpea/libpea.c \
	-o libpea/libpea.o

echo "Bundling libpea.a"
mkdir archive
cd archive
ar x ../libpea/$gc_path/gc.a
ar x ../libpea/$libunwind_path/lib/libunwind.a
ar cr ../libpea/libpea.a ../libpea/libpea.o *.o
cd ..
rm -fr archive
