package printer

import (
	"strings"
	"testing"

	"github.com/eaburns/pea/loc"
	"github.com/eaburns/pea/parser"
	"github.com/google/go-cmp/cmp"
)

func TestCommentsLists(t *testing.T) {
	tests := []string{
		`// Package comment.
// Comment 2.

// Leading comment
type (X, Y) pair [
	.x X, // detatched a
	.y Y, // detatched b

	.z Z, // detatched c
]

// detatched 0
// detatched 1

// Attached
type (X, Y) pair [
	.x X, /* the x value. */
	.y Y, /* the y value. */
	// This is an attached comment.
	.z Z, // The z value

	// Where did this come from?
	.a A, // Oops A?
]

// detatched I
// detatched J

func foo() {}
`,
		`//
func f() {
	5, // five
	6 // six
}
`,
	}
	for _, src := range tests {
		runIdentTest(src, t)
	}
}

func TestFuncDef(t *testing.T) {
	tests := []string{
		`//
func f()
`,
		`//
func f() {}
`,
		`//
func f() { print(5) }
`,
		`//
func f() {
	print(5)
}
`,
		`//
func f() bool {
	return: false
}
`,
		`//
func f(b bool) {
	return: false
}
`,
		`//
func f(b bool) bool {
	return: false
}
`,
		`//
func f(a, b int) {}
`,
		`//
func f(a,
	b int) {}
`,
		`//
func f(a string,
	b int,
	c [.x int, .y string]) {}
`,
		`//
func f(
	a string,
	b int,
	c [.x int, .y string]) {}
`,
		`//
func f(
	x [
		.x int,
		.y int,
		.z int,
	],
) {}
`,
		`//
func f(
	x [
		.x int,
		.y int,
		.z int,
	],
	i int,
	s string,
) {}
`,
		`//
func f(
	x [
		.x int,
		.y [
			.a int,
			.b string,
			.c [
				.d d,
				.e e,
			],
		],
		.z int,
	],
	i int,
	s string,
) {}
`,
		`//
func f() { a }
`,
		`//
func f() {
	a
}
`,
		`//
func f() {
	a,
	b,
	c
}
`,
	}
	for _, src := range tests {
		runIdentTest(src, t)
	}
}

func TestCall(t *testing.T) {
	tests := []string{
		`//
func f() {
	bar(i, j, k)
}
`,
		`//
func f() {
	name_with_underscores(i, j, k)
}
`,
		`//
func f() {
	bar(i,
		j, k)
}
`,
		`//
func f() {
	bar(
		i,
		j,
		k)
}
`,
		`//
func f() {
	bar(baz(qux(
		i,
		j,
		k)))
}
`,
		`//
func f() {
	bar(baz(
		qux(
			i,
			j,
			k)))
}
`,
	}
	for _, src := range tests {
		runIdentTest(src, t)
	}
}

func TestSelectorCall(t *testing.T) {
	tests := []string{
		`//
func f() {
	x.foo
}
`,
		`//
func f() {
	x
		.foo
}
`,
		`//
func f() {
	x.foo.bar.baz
}
`,
		`//
func f() {
	x
		.foo
		.bar
		.baz
}
`,
	}
	for _, src := range tests {
		runIdentTest(src, t)
	}
}

func TestIndexCall(t *testing.T) {
	tests := []string{
		`//
func f() {
	x[5]
}
`,
		`//
func f() {
	x[5, 6]
}
`,
		`//
func f() {
	x[5,
		6]
}
`,
	}
	for _, src := range tests {
		runIdentTest(src, t)
	}
}

func TestKqCall(t *testing.T) {
	tests := []string{
		`//
func f() {
	return: x
}
`,
		`//
func f() {
	x push_back: y
}
`,
		`//
func f() {
	for: 0 to: 10 do: y
}
`,
		`//
func f() {
	x
		push_back: y
}
`,
		`//
func f() {
	x
		push_back: y
		and: z
}
`,
		`//
func f() {
	for: 0
	to: 10 do: y
}
`,
		`//
func f() {
	for: 0
	to: 10
	do: y
}
`,
		`//
func f() {
	if:
		foo
	then:
		baz
	else:
		qux
}
`,
		`//
func f() {
	if:
		foo |
			bar
	then:
		baz
	else:
		qux
}
`,
		`//
func f() {
	if: foo |
		bar
	then: {
		baz
	} else: {
		qux
	}
}
`,
		`//
func f() {
	if:
		//
		foo |
			bar
	then: {
		baz
	} else: {
		qux
	}
}
`,
	}
	for _, src := range tests {
		runIdentTest(src, t)
	}
}

func TestUnOpCall(t *testing.T) {
	tests := []string{
		`//
func f() {
	-x
}
`,
		`//
func f() {
	- -2
}
`,
		`//
func f() {
	- +2
}
`,
		`//
func f() {
	- -3.14
}
`,
		`//
func f() {
	- +3.14
}
`,
		`//
func f() {
	- -x
}
`,
		`//
func f() {
	(mod#-)(x)
}
`,
		`//
func f() {
	(mod#-)((mod#-)(x))
}
`,
	}
	for _, src := range tests {
		runIdentTest(src, t)
	}
}

func TestBinOpCall(t *testing.T) {
	tests := []string{
		`//
func f() {
	1 + 2
}
`,
		`//
func f() {
	1* -2
}
`,
		`//
func f() {
	1* +2
}
`,
		`//
func f() {
	1* -3.14
}
`,
		`//
func f() {
	1* +3.14
}
`,
		`//
func f() {
	1* -x
}
`,
		`//
func f() {
	1 +
		2 +
		3 +
		4
}
`,
		`//
func f() {
	1 +
		2*
			3*
			4 +
		5
}
`,
		`//
func f() {
	{
		1 +
			2*
				3*
				4 +
			5
	}
}
`,
		`//
func f() {
	(mod#+)(1, 2)
}
`,
	}
	for _, src := range tests {
		runIdentTest(src, t)
	}
}

func TestNamedTypes(t *testing.T) {
	tests := []string{
		`//
type _ x
`,
		`//
type _ (X, Y) hello
`,
		`//
type _ X hello
`,
		`//
type _ &X hello
`,
		`//
type _ (&X) hello
`,
		`//
type _ (X,
	Y) hello
`,
		`//
type _ (X,
	Y,
	Z) hello
`,
		`//
type _ (X, Y) hello
`,
		`//
type _ mod#hello
`,
		`//
type _ mod#more_mod#hello
`,
		`//
type _ (X, Y) mod#more_mod#hello
`,
		`//
type _ (int,
	string) mod#more_mod#hello
`,
		`//
type _ ([.x int, .y string], [some? float, none?]) pair
`,
		`//
type _ (
	[
		.x int,
		.y string,
	],
	[
		some? float,
		none?,
	],
) pair
`,
	}
	for _, src := range tests {
		runIdentTest(src, t)
	}
}

func TestStructTypes(t *testing.T) {
	tests := []string{
		`//
type s [.x int, .y int]
`,
		`//
type s [
	.x int,
	.y int,
]
`,
	}
	for _, src := range tests {
		runIdentTest(src, t)
	}
}

func TestFuncTypes(t *testing.T) {
	tests := []string{
		`//
type s (){}
`,
		`//
type s (){int}
`,
		`//
type s (int){}
`,
		`//
type s (int){int}
`,
		`//
type s (int, string){int}
`,
		`//
type s (
	int,
	string){int}
`,
		`//
type s (int,
	string){int}
`,
		`//
type s (
	int,
	[
		.x int,
		.y string,
	],
){int}
`,
		`//
type s (int){[
	.x int,
]}
`,
		`//
type s (int){[
	.z [
		.y string,
		.x int,
	],
]}
`,
	}
	for _, src := range tests {
		runIdentTest(src, t)
	}
}

func TestConvert(t *testing.T) {
	tests := []string{
		`//
func f() { int :: 5 }
`,
		`//
func f() { (X, Y) hash_table :: new() }
`,
		`//
func f() { [T] :: new() }
`,
	}
	for _, src := range tests {
		runIdentTest(src, t)
	}
}

func TestSubExpr(t *testing.T) {
	tests := []string{
		`//
func foo() { (x) }
`,
	}
	for _, src := range tests {
		runIdentTest(src, t)
	}
}

func TestModSel(t *testing.T) {
	tests := []string{
		`//
func foo() { a#b }
`,
		`//
func foo() { a#b#c }
`,
	}
	for _, src := range tests {
		runIdentTest(src, t)
	}
}

func TestArrayLit(t *testing.T) {
	tests := []string{
		`//
func f() { [] }
`,
		`//
func f() {
	[]
}
`,
		`//
func f() { [1, 2, 3] }
`,
		`//
func f() {
	[1, 2, 3]
}
`,
		`//
func f() {
	[1,
		2,
		3]
}
`,
		`//
func f() {
	[1, 2, 3,
		4, 5, 6]
}
`,
		`//
func f() {
	[
		1,
		2,
		3,
	]
}
`,
		`//
func f() {
	[
		1,
		2,
		3,
	]
}
`,
		`//
func f() {
	[[[[[
		1,
		2,
		3,
	]]]]]
}
`,
		`//
func f() {
	[
		[
			1,
			2,
			3,
		],
	]
}
`,
		`//
func f() {
	[
		[
			1,
			2,
			3,
		],
		[
			4,
			5,
			6,
		],
	]
}
`,
		`//
func f() {
	[[
		1,
		2,
		3,
	], [
		4,
		5,
		6,
	]]
}
`,
		`//
func f() {
	[[[[[
		1,
		2,
		3,
	]]]], [[[[
		4,
		5,
		6,
	]]]]]
}
`,
		`//
func f() {
	[[
		[
			1,
			2,
			3,
		],
	], [[
		4,
		5,
		6,
	]]]
}
`,
		`//
func f() {
	[[[
		[[
			1,
			2,
			3,
		]],
	]], [[
		[[
			4,
			5,
			6,
		]],
	]]]
}
`,
		`//
func f() {
	[[[
		[[
			1,
			2,
			3,
		]],
	]], [[[[
		4,
		5,
		6,
	]]]]]
}
`,
		`//
func f() {
	[[[
		[[
			1,
			2,
			3,
		]],
	]], [[[
		[
			4,
			5,
			6,
		],
	]]]]
}
`,
	}
	for _, src := range tests {
		runIdentTest(src, t)
	}
}

func TestStructLit(t *testing.T) {
	tests := []string{
		`//
func foo() { [.] }
`,
		`//
func foo() { [.x 5] }
`,
		`//
func foo() { [.x 5, .y 3.14] }
`,
		`//
func foo() {
	[.x 5,
		.y 3.14]
}
`,
		`//
func foo() {
	[
		.x 5,
		.y 3.14]
}
`,
		// TODO: insert a trailing comma.
		`//
func foo() {
	[
		.x 5,
		.y 3.14,
	]
}
`,
		`//
func foo() {
	[
		.x 5,
		.y 3.14]
}
`,
		`//
func foo() {
	[
		.x
			foo: 5
			bar: 6,
		.y
			baz: 3.14
			qux: 7,
	]
}
`,
		`//
func foo() {
	[[
		.x 5,
		.y 3.14,
	]]
}
`,
	}
	for _, src := range tests {
		runIdentTest(src, t)
	}
}

func TestUnionLit(t *testing.T) {
	tests := []string{
		`//
func foo() { [x?] }
`,
		`//
func foo() { [x? 5] }
`,
		`//
func foo() {
	[x?
		5]
}
`,
		`//
func foo() {
	[
		x?
			5
	]
}
`,
		`//
func foo() {
	[x?
		foo: 5
		bar: 6
	]
}
`,
		`//
func foo() {
	[
		x?
			foo: 5
			bar: 6
	]
}
`,
	}
	for _, src := range tests {
		runIdentTest(src, t)
	}
}

func TestBlockLit(t *testing.T) {
	tests := []string{
		`//
func foo() { {} }
`,
		`//
func foo() { (i){} }
`,
		`//
func foo() { (i){ i } }
`,
		`//
func foo() { (i){ i, j } }
`,
		`//
func foo() { (i, j){ i, j } }
`,
		`//
func foo() { (i int){ i } }
`,
		`//
func foo() { (i, j int){ i } }
`,
		`//
func foo() { (i int, j int){ i } }
`,
		`//
func foo() {
	(i){
		i,
		j
	}
}
`,
		`//
func for:each_entry:(b (K, V) bucket, f ((K, V) entry){}) {
	for: b each: (e){
		if: e some: f
	}
}
`,
	}
	for _, src := range tests {
		runIdentTest(src, t)
	}
}

func TestStrLit(t *testing.T) {
	tests := []string{
		`//
func foo() { "hello" }
`,
		`//
func foo() { "he\\nllo" }
`,
		`//
func foo() { "\n" }
`,
		"func f() { `hello` }\n",
		"func f() {\n\t`he\nllo\n`\n}\n",
		"func f() {\n\t`\x01`\n}\n",
	}
	for _, src := range tests {
		runIdentTest(src, t)
	}
}

func TestCharLit(t *testing.T) {
	tests := []string{
		`func foo() { 'a' }
`,
		`func foo() { '\n' }
`,
		`func foo() { '\x00' }
`,
	}
	for _, src := range tests {
		runIdentTest(src, t)
	}
}

func TestIntLit(t *testing.T) {
	tests := []string{
		"func foo() { 1 }\n",
		"func foo() { -1 }\n",
		"func foo() { +1 }\n",
		"func foo() { 0 }\n",
		"func foo() { 123456 }\n",
		"func foo() { 0xabcdef }\n",
		"func foo() { 0xABCDEF }\n",
	}
	for _, src := range tests {
		runIdentTest(src, t)
	}
}

func TestFloatLit(t *testing.T) {
	tests := []string{
		"func foo() { 1.0 }\n",
		"func foo() { 1.123 }\n",
		"func foo() { -1.0 }\n",
		"func foo() { -1.123 }\n",
		"func foo() { +1.0 }\n",
		"func foo() { +1.123 }\n",
		"func foo() { 0.0 }\n",
		"func foo() { 0.123 }\n",
		"func foo() { 123456.12345 }\n",
		"func foo() { 1.0e-10 }\n",
		"func foo() { 1.0e+10 }\n",
		"func foo() { 1.0e10 }\n",
		"func foo() { 123.123e-10 }\n",
		"func foo() { 123.123e+10 }\n",
		"func foo() { 123.123e10 }\n",
		"func foo() { 1.0E-10 }\n",
		"func foo() { 1.0E+10 }\n",
		"func foo() { 1.0E10 }\n",
		"func foo() { 123.123E-10 }\n",
		"func foo() { 123.123E+10 }\n",
		"func foo() { 123.123E10 }\n",
	}
	for _, src := range tests {
		runIdentTest(src, t)
	}
}

// TestReproduceEmptyStructTypeIssue reproduces a bug
// where empty struct types printed [].
func TestReproduceEmptyStructTypeIssue(t *testing.T) {
	tests := []string{`//
func foo(x int, y [.]) {}
`}
	for _, src := range tests {
		runIdentTest(src, t)
	}
}

// TestReproduceBlankLineAfterOpenCurlyBrace
// reproduces a bug where there was a blank line after the open {.
func TestReproduceBlankLineAfterOpenCurlyBrace(t *testing.T) {
	tests := []string{`//
Func assert:some:(o T option, b T) : {
	++(print#printer, T)print#printer,
	=(T, T)bool,
} {
	if: o some: (a T){
		if: !(a = b) then: {
			panic(print#string(o ++ " != some(" ++ b ++ ")"))
		}
	} none: {
		panic(print#string(o ++ " != some(" ++ b ++ ")"))
	}
}
`}
	for _, src := range tests {
		runIdentTest(src, t)
	}
}

func TestReproduceCollapsingArrayToOneLine(t *testing.T) {
	tests := []string{`//
test foo {
	assert: m unordered_elements: [
		[.key 8, .val 72],
		[.key 16, .val 38],
		[.key 26, .val 83],
		[.key 32, .val 71],
		[.key 33, .val 93],
		[.key 43, .val 95],
		[.key 49, .val 13],
		[.key 50, .val 74],
		[.key 62, .val 93],
	]
}
`}
	for _, src := range tests {
		runIdentTest(src, t)
	}
}

func TestReproduceKeepUnaryOpsSeparate(t *testing.T) {
	tests := []string{`//
test foo {
	assert: - -second equals: second
}
`}
	for _, src := range tests {
		runIdentTest(src, t)
	}
}

func TestReproduceBadStructIndentation(t *testing.T) {
	tests := []string{`//
test foo {
	assert: (nanosecond + nanosecond)*(int64 :: int#max_uint32)
	equals: (duration :: [
		.s (int64 :: int#max_uint32)*(int64 :: 2)/1.0e9,
		.ns uint32 :: (int64 :: int#max_uint32)*(int64 :: 2)%2.0e9,
	])
}
`}
	for _, src := range tests {
		runIdentTest(src, t)
	}
}

func TestReproduceForceSingleLineLogicalExpression(t *testing.T) {
	tests := []string{`//
test foo {
	return: (uint64 :: data[0]) |
		(uint64 :: data[1]) << 8 |
		(uint64 :: data[2]) << 16 |
		(uint64 :: data[3]) << 24
}
`}
	for _, src := range tests {
		runIdentTest(src, t)
	}
}

func TestReproduceCommentMovedOutsideOfBlock(t *testing.T) {
	tests := []string{`//
test foo {
	{
		// foo
	}
}
`}
	for _, src := range tests {
		runIdentTest(src, t)
	}
}

// Previously this was reproducing that we _do_ elide the trailing comma,
// but iI changed my mind on whether that comma should be required.
func TestNoElideCommaAtEndOfInterface(t *testing.T) {
	tests := []string{`//
func max_heap_down(ts C, less (&T, &T){bool}, i int) : {
	.length(C)int,
	[](C, int)&T,
} {}
`}
	for _, src := range tests {
		runIdentTest(src, t)
	}
}

func TestReproduceWeirdModSelCallIssue(t *testing.T) {
	tests := []string{`//
test foo {
	utf8#for: s runes: (_ rune, w int){
		fields[n] := s[i, i + w],
		i += w,
		++n
	},
	sort#sort: ents
}
`}
	for _, src := range tests {
		runIdentTest(src, t)
	}
}

func TestReproduceBadOpIndent(t *testing.T) {
	tests := []string{`//
test foo {
	a := z |
		y ||
		x
}
`}
	for _, src := range tests {
		runIdentTest(src, t)
	}
}

func TestReproduceBadOpIndentOrig(t *testing.T) {
	tests := []string{`//
test foo {
	a := (uint64 :: data[0]) << 16 |
		(uint64 :: data[data.length >> 1]) << 8 |
		(uint64 :: data[data.length - 1])
}
`}
	for _, src := range tests {
		runIdentTest(src, t)
	}
}

func TestReproduceAnotherBadOpIndent(t *testing.T) {
	tests := []string{`//
test foo {
	h ||
		string#x ||
		string#y ||
		string#z ||
		string#aa
}
`}
	for _, src := range tests {
		runIdentTest(src, t)
	}
}

func TestReproduceAnotherBadOpIndentOrig(t *testing.T) {
	tests := []string{`//
test foo {
	{ string#contains(regexp, "[a-b-c]") } ||
		// flags
		string#contains(regexp, "(?i") ||
		string#contains(regexp, "(?m") ||
		string#contains(regexp, "(?s") ||
		string#contains(regexp, "(?U")
}
`}
	for _, src := range tests {
		runIdentTest(src, t)
	}
}

func TestReproduceArrayLineCollapsing(t *testing.T) {
	tests := []string{`//
test foo {
	addr(l).network ip? (ip){
		assert: ip one_of: [
			sys#net#ip#addr(0, 0, 0, 0),
			sys#net#ip#addr(0, 0, 0, 0, 0, 0, 0, 0),
		]
	}
}
`}
	for _, src := range tests {
		runIdentTest(src, t)
	}
}

func TestReproduceBadQCallIdent(t *testing.T) {
	tests := []string{`//
test foo {
	x y? {
		// foo
	} z? {
		bar()
	}
}
`}
	for _, src := range tests {
		runIdentTest(src, t)
	}
}

func TestReproduceBadQCallIdentOrig(t *testing.T) {
	tests := []string{`//
test foo {
	upsert(m, m.root, k, v) ok? {
		// nothing to do
	} prev? (v){
		return: some(v)
	} split? (split){
		new_root := (K, V) node :: [
			.n 1,
			.keys new(m.max_keys + 1, split.med),
			.kids new(m.max_keys + 2, m.empty),
		],
		new_root.kids[0] := m.root,
		new_root.kids[1] := split.right,
		m.root := new_root
	}
}
`}
	for _, src := range tests {
		runIdentTest(src, t)
	}
}

// This splitting is correct, because multi-line function splitting.
func TestReproduceBadExpressionSplitting(t *testing.T) {
	tests := []string{`//
Func =(a civil, b civil) bool {
	return: a.y = b.y && {
		a.m = b.m && {
			a.d = b.d &&
				{ a.hh = b.hh && { a.mm = b.mm && { a.ss = b.ss && { a.nn = b.nn } } } }
		}
	}
}
`}
	for _, src := range tests {
		runIdentTest(src, t)
	}
}

func TestBetterWayToWriteChainOfAnds(t *testing.T) {
	tests := []string{`//
Func =(a civil, b civil) bool {
	return: a.y = b.y &&
		{ a.m = b.m } &&
		{ a.d = b.d } &&
		{ a.hh = b.hh } &&
		{ a.mm = b.mm } &&
		{ a.ss = b.ss } &&
		{ a.nn = b.nn }
}
`}
	for _, src := range tests {
		runIdentTest(src, t)
	}
}

func TestReproduceRemovingBlankLines(t *testing.T) {
	tests := []string{`//
test pointer {
	x := 1,
	xp := int pointer :: &x,
	assert: *xp eq: 1,
	assert: x eq: 1,

	*xp := 2,
	assert: *xp eq: 2,
	assert: x eq: 2,

	y := 3,
	xp := &y,
	assert: *xp eq: 3,
	assert: y eq: 3,
	assert: x eq: 2,

	*xp := 4,
	assert: *xp eq: 4,
	assert: y eq: 4,
	assert: x eq: 2
}
`}
	for _, src := range tests {
		runIdentTest(src, t)
	}
}

func runIdentTest(src string, t *testing.T) {
	t.Helper()
	p := parser.New()
	if err := p.Parse("", strings.NewReader(src)); err != nil {
		t.Fatalf("failed to parse: %s", err)
	}
	var locs loc.Files
	for _, f := range p.Files {
		locs = append(locs, f)
	}
	var w strings.Builder
	if err := Print(&w, p.Files[0], locs); err != nil {
		t.Fatalf("failed to print: %s", err)
	}
	got := w.String()
	if diff := cmp.Diff(src, got); diff != "" {
		t.Errorf("\n==> GOT:\n%s==> WANTED:\n%s\n==> DIFF: %s\n",
			w.String(), src, diff)
	}
}
