// SPDX-License-Identifier: GPL-3.0-or-later

package main

import (
	"flag"
	"fmt"
	"os"
)

func main() {
	schemeName := flag.String("scheme", "default", "fixed scoring scheme: default, path, or history")
	flag.Parse()
	if flag.NArg() != 0 {
		fmt.Fprintln(os.Stderr, "fzf-raw-oracle does not accept positional arguments")
		os.Exit(2)
	}

	scheme, err := parseScheme(*schemeName)
	if err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(2)
	}
	oracle, err := newRawOracle(scheme)
	if err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(2)
	}
	if err := (&server{oracle: oracle}).serve(os.Stdin, os.Stdout); err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}
}
