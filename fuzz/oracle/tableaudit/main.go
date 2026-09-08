// SPDX-License-Identifier: GPL-3.0-or-later

// Command tableaudit compares fzf-native's generated normalization table with
// the map in a pinned junegunn/fzf source tree.  Pass -write to regenerate the
// C include before performing the comparison.
package main

import (
	"bytes"
	"flag"
	"fmt"
	"go/ast"
	"go/parser"
	"go/token"
	"os"
	"regexp"
	"sort"
	"strconv"
	"unicode/utf8"
)

type pair struct {
	source rune
	target rune
}

var cEntry = regexp.MustCompile(`\{0x([0-9A-Fa-f]+), ('(?:\\.|[^'])+')\}`)
var cPageEntry = regexp.MustCompile(`FZF_NORMALIZED_SOURCE_PAGE\(0x([0-9A-Fa-f]+)\)`)

func sourcePages(pairs []pair) []rune {
	var result []rune
	for _, entry := range pairs {
		page := entry.source >> 8
		if len(result) == 0 || result[len(result)-1] != page {
			result = append(result, page)
		}
	}
	return result
}

func runeLiteral(text string) (rune, error) {
	value, err := strconv.Unquote(text)
	if err != nil {
		return 0, err
	}
	r, size := utf8.DecodeRuneInString(value)
	if r == utf8.RuneError || size != len(value) {
		return 0, fmt.Errorf("not one rune: %q", text)
	}
	return r, nil
}

func upstream(path string) ([]pair, error) {
	file, err := parser.ParseFile(token.NewFileSet(), path, nil, 0)
	if err != nil {
		return nil, err
	}
	var result []pair
	ast.Inspect(file, func(node ast.Node) bool {
		spec, ok := node.(*ast.ValueSpec)
		if !ok || len(spec.Names) != 1 || spec.Names[0].Name != "normalized" || len(spec.Values) != 1 {
			return true
		}
		literal, ok := spec.Values[0].(*ast.CompositeLit)
		if !ok {
			return false
		}
		for _, element := range literal.Elts {
			entry := element.(*ast.KeyValueExpr)
			key := entry.Key.(*ast.BasicLit)
			var source rune
			var parseErr error
			if key.Kind == token.CHAR {
				source, parseErr = runeLiteral(key.Value)
			} else {
				value, err := strconv.ParseInt(key.Value, 0, 32)
				parseErr = err
				source = rune(value)
			}
			target, targetErr := runeLiteral(entry.Value.(*ast.BasicLit).Value)
			if parseErr != nil || targetErr != nil {
				panic(fmt.Sprintf("bad upstream pair: %v %v", parseErr, targetErr))
			}
			result = append(result, pair{source, target})
		}
		return false
	})
	if len(result) == 0 {
		return nil, fmt.Errorf("normalized map not found in %s", path)
	}
	sort.Slice(result, func(i, j int) bool { return result[i].source < result[j].source })
	return result, nil
}

func native(path string) ([]pair, []rune, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, nil, err
	}
	if !bytes.Contains(data, []byte("fzf_normalized_runes")) {
		return nil, nil, fmt.Errorf("normalization table not found in %s", path)
	}
	var result []pair
	for _, match := range cEntry.FindAllSubmatch(data, -1) {
		source, err := strconv.ParseInt(string(match[1]), 16, 32)
		if err != nil {
			return nil, nil, err
		}
		target, err := runeLiteral(string(match[2]))
		if err != nil {
			return nil, nil, err
		}
		if len(result) > 0 && rune(source) <= result[len(result)-1].source {
			return nil, nil, fmt.Errorf("C table not strictly sorted at U+%04X", source)
		}
		result = append(result, pair{rune(source), target})
	}
	var pages []rune
	for _, match := range cPageEntry.FindAllSubmatch(data, -1) {
		page, err := strconv.ParseInt(string(match[1]), 16, 32)
		if err != nil {
			return nil, nil, err
		}
		if len(pages) > 0 && rune(page) <= pages[len(pages)-1] {
			return nil, nil, fmt.Errorf("C source pages not strictly sorted at 0x%02X", page)
		}
		pages = append(pages, rune(page))
	}
	if len(pages) == 0 {
		return nil, nil, fmt.Errorf("normalization source pages not found in %s", path)
	}
	return result, pages, nil
}

func writeTable(path string, pairs []pair) error {
	var output bytes.Buffer
	fmt.Fprint(&output, `/* SPDX-License-Identifier: MIT
 * Pinned fzf Latin-script normalization table.
 *
 * Generated from junegunn/fzf src/algo/normalize.go at
 * 1372d04f79bde0daa3bab4b96a068baafa808e67.  Keep this sorted by source
 * codepoint so fzf_normalize_codepoint can use binary search.  The source-page
 * inventory is generated and audited from these same entries.
 */

typedef struct {
  utf8proc_int32_t source;
  utf8proc_int32_t target;
} fzf_normalized_rune_t;

`)
	pages := sourcePages(pairs)
	fmt.Fprintln(&output, "#define FZF_NORMALIZED_SOURCE_PAGES \\")
	for index, page := range pages {
		fmt.Fprintf(&output, "  FZF_NORMALIZED_SOURCE_PAGE(0x%02X)", page)
		if index != len(pages)-1 {
			fmt.Fprint(&output, " \\")
		}
		fmt.Fprintln(&output)
	}
	fmt.Fprint(&output, `
static const fzf_normalized_rune_t fzf_normalized_runes[] = {
`)
	for index, entry := range pairs {
		if index%4 == 0 {
			fmt.Fprint(&output, "  ")
		}
		fmt.Fprintf(&output, "{0x%04X, %s},", entry.source, strconv.QuoteRune(entry.target))
		if index%4 == 3 || index == len(pairs)-1 {
			fmt.Fprintln(&output)
		} else {
			fmt.Fprint(&output, " ")
		}
	}
	fmt.Fprint(&output, "};\n")
	return os.WriteFile(path, output.Bytes(), 0o644)
}

func main() {
	write := flag.Bool("write", false, "regenerate the C include before checking it")
	flag.Parse()
	if flag.NArg() != 2 {
		fmt.Fprintln(os.Stderr, "usage: tableaudit [-write] GO_NORMALIZE C_INCLUDE")
		os.Exit(2)
	}
	want, err := upstream(flag.Arg(0))
	if err != nil {
		panic(err)
	}
	if *write {
		if err := writeTable(flag.Arg(1), want); err != nil {
			panic(err)
		}
	}
	got, gotPages, err := native(flag.Arg(1))
	if err != nil {
		panic(err)
	}
	wantPages := sourcePages(want)
	differences := 0
	limit := len(want)
	if len(got) < limit {
		limit = len(got)
	}
	for index := 0; index < limit; index++ {
		if got[index] != want[index] {
			fmt.Printf("entry %d got U+%04X->U+%04X want U+%04X->U+%04X\n",
				index, got[index].source, got[index].target, want[index].source, want[index].target)
			differences++
		}
	}
	if len(got) != len(want) {
		fmt.Printf("entry count got %d want %d\n", len(got), len(want))
		differences++
	}
	pageLimit := len(wantPages)
	if len(gotPages) < pageLimit {
		pageLimit = len(gotPages)
	}
	for index := 0; index < pageLimit; index++ {
		if gotPages[index] != wantPages[index] {
			fmt.Printf("source page %d got 0x%02X want 0x%02X\n",
				index, gotPages[index], wantPages[index])
			differences++
		}
	}
	if len(gotPages) != len(wantPages) {
		fmt.Printf("source page count got %d want %d\n", len(gotPages), len(wantPages))
		differences++
	}
	fmt.Printf("upstream=%d native=%d pages=%d differences=%d\n",
		len(want), len(got), len(gotPages), differences)
	if differences != 0 {
		os.Exit(1)
	}
}
