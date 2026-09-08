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

func native(path string) ([]pair, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	if !bytes.Contains(data, []byte("fzf_normalized_runes")) {
		return nil, fmt.Errorf("normalization table not found in %s", path)
	}
	var result []pair
	for _, match := range cEntry.FindAllSubmatch(data, -1) {
		source, err := strconv.ParseInt(string(match[1]), 16, 32)
		if err != nil {
			return nil, err
		}
		target, err := runeLiteral(string(match[2]))
		if err != nil {
			return nil, err
		}
		if len(result) > 0 && rune(source) <= result[len(result)-1].source {
			return nil, fmt.Errorf("C table not strictly sorted at U+%04X", source)
		}
		result = append(result, pair{rune(source), target})
	}
	return result, nil
}

func writeTable(path string, pairs []pair) error {
	var output bytes.Buffer
	fmt.Fprint(&output, `/* SPDX-License-Identifier: MIT
 * Pinned fzf Latin-script normalization table.
 *
 * Generated from junegunn/fzf src/algo/normalize.go at
 * 1372d04f79bde0daa3bab4b96a068baafa808e67.  Keep this sorted by source
 * codepoint so fzf_normalize_codepoint can use binary search.
 */

typedef struct {
  utf8proc_int32_t source;
  utf8proc_int32_t target;
} fzf_normalized_rune_t;

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
	got, err := native(flag.Arg(1))
	if err != nil {
		panic(err)
	}
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
	fmt.Printf("upstream=%d native=%d differences=%d\n", len(want), len(got), differences)
	if differences != 0 {
		os.Exit(1)
	}
}
