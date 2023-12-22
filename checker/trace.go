package checker

import (
	"flag"
	"fmt"
	"strings"

	"github.com/eaburns/pea/loc"
)

var (
	traceDepth = flag.Int("trace.depth", 0, "max depth for trace (0 = no trace; -1 = infinite)")
)

const traceIndent = "\t"

type traceItem struct {
	top    *topScope
	indent string
	bullet int
}

func trItem(x scope, f string, vs ...interface{}) *traceItem {
	t := top(x)
	tr := &traceItem{top: t, indent: t.trIndent, bullet: t.nextBullet}
	t.trIndent += traceIndent
	t.nextBullet++
	tr.trace(f, vs...)
	return tr
}

func (tr *traceItem) done() {
	tr.top.trIndent = strings.TrimSuffix(tr.top.trIndent, traceIndent)
	tr.top.nextBullet--
}

func (tr *traceItem) trace(f string, vs ...interface{}) {
	if *traceDepth == 0 {
		return
	}
	depth := strings.Count(tr.indent, traceIndent) + 1
	if *traceDepth > 0 && depth > *traceDepth {
		return
	}
	for i := range vs {
		l, ok := vs[i].(loc.Loc)
		if !ok {
			continue
		}
		lo := tr.top.locFiles.Location(l)
		lo.Path = strings.TrimPrefix(lo.Path, tr.top.trimErrorPathPrefix)
		vs[i] = lo
	}
	s := fmt.Sprintf(f, vs...)
	s = strings.TrimSuffix(s, "\n")
	s = strings.ReplaceAll(s, "\n", "\n"+tr.indent+"  ")
	if tr.bullet >= 0 {
		s = bullets[tr.bullet%len(bullets)] + " " + s
		tr.bullet = -1
	} else {
		s = "  " + s
	}
	fmt.Println(s)
}
