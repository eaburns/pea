#!/bin/sh

root=$1
mods=$(
	(for file in `find $root -name \*.pea`; do
		egrep -q '^test' $file && dirname $file | sed s/^$root[/]//
	done) | sort | uniq
)

fail=0
for mod in $mods; do
	echo Testing $mod
	if ! go run peac/main.go -libpea ./libpea -test -root $root $mod; then
		fail=1
		continue
	fi
	o=$(mktemp pea.XXXXXXXXXX)
	if ! $root/$mod/$mod.test > $o; then
		fail=1
		cat $o
	fi
	rm $o

	# TODO: currently the .a file contains test main.
	# If we do not remove the .a files then there will be dup test mains.
	find $root -name \*.a | xargs rm
done

exit $fail