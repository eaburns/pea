#!/bin/sh

root=$1
mods=$(
	(for file in `find $root -name \*.pea`; do
		egrep -q '^test' $file && dirname $file | sed s/^$root[/]//
	done) | sort | uniq
)

go build -o peafmt/peafmt ./peafmt || exit 1
o=$(mktemp pea_tmp.XXXXXXXXXX)
for file in `find $root -name \*.pea`; do
	cp $file $o
	./peafmt/peafmt $o
	if ! diff $file $o 2>&1 > /dev/null; then
		echo $file is not formatted
		diff -up $file $o
		rm -f $o
		exit 1
	fi
done
rm -f $o

go build -o peac/peac ./peac || exit 1

fail=0
for mod in $mods; do
	/bin/echo -n "Testing $mod … "
	o=$(mktemp pea_tmp.XXXXXXXXXX)
	trap "rm -f $o" EXIT
	if ! ./peac/peac -libpea ./libpea -test -root $root $mod > $o; then
		fail=1
		rm -f $root/$mod/*.pea_cache
		echo failed to build
		cat $o
		rm $o
		continue
	fi
	rm $o

	bin=$root/$mod/$(basename $mod).test
	# Use the first 6 characters of the sha1 only.
	# This should be unique enough,
	# and makes for file names that don't overflow
	# columnated directory output.
	#
	# TODO: revisit sha1 truncation decision
	# when we stop outputting compiler intermediate
	# files into the source directory.
	sha1=$(sha1sum $bin | awk '{ print substr($1,1,6) }')
	pass_cache=$bin-$sha1-pass.pea_cache
	fail_cache=$bin-$sha1-fail.pea_cache
	if test -e $pass_cache; then
		echo "pass (cached)"
		continue
	fi
	if test -e $fail_cache; then
		fail=1
		echo "FAIL (cached)"
		cat $fail_cache
		continue
	fi

	rm -f $root/$mod/*.pea_cache
	if ! $bin > $o; then
		fail=1
		echo FAIL
		cat $o
		cat $o > $fail_cache
		rm $o
		continue
	fi
	touch $pass_cache
	echo pass
	rm $o
done

exit $fail