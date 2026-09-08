// SPDX-License-Identifier: GPL-3.0-or-later

package main

import (
	"fmt"
	"sort"
	"strings"

	"github.com/junegunn/fzf/src/algo"
	"github.com/junegunn/fzf/src/util"
)

const pinnedUpstreamCommit = "1372d04f79bde0daa3bab4b96a068baafa808e67"

type algorithmID byte

const (
	algorithmV1            algorithmID = 0
	algorithmV2            algorithmID = 1
	algorithmExact         algorithmID = 2
	algorithmExactBoundary algorithmID = 3
	algorithmPrefix        algorithmID = 4
	algorithmSuffix        algorithmID = 5
	algorithmEqual         algorithmID = 6
)

type schemeID byte

const (
	schemeDefault schemeID = 0
	schemePath    schemeID = 1
	schemeHistory schemeID = 2
)

const (
	flagCaseSensitive byte = 1 << 0
	flagNormalize     byte = 1 << 1
	flagForward       byte = 1 << 2
	validMatchFlags        = flagCaseSensitive | flagNormalize | flagForward
)

type matchRequest struct {
	algorithm algorithmID
	scheme    schemeID
	flags     byte
	pattern   []byte
	candidate []byte
}

type matchResponse struct {
	matched          bool
	positionsPresent bool
	start            int64
	end              int64
	score            int64
	positions        []int64
}

type rawOracle struct {
	scheme schemeID
	slab   *util.Slab
}

func parseScheme(value string) (schemeID, error) {
	switch value {
	case "default":
		return schemeDefault, nil
	case "path":
		return schemePath, nil
	case "history":
		return schemeHistory, nil
	default:
		return 0, fmt.Errorf("unknown scheme %q", value)
	}
}

func (s schemeID) String() string {
	switch s {
	case schemeDefault:
		return "default"
	case schemePath:
		return "path"
	case schemeHistory:
		return "history"
	default:
		return "unknown"
	}
}

func newRawOracle(scheme schemeID) (*rawOracle, error) {
	if !algo.Init(scheme.String()) {
		return nil, fmt.Errorf("cannot initialize scheme %d", scheme)
	}
	return &rawOracle{
		scheme: scheme,
		slab:   util.MakeSlab(100*1024, 2048),
	}, nil
}

func selectAlgorithm(id algorithmID) (algo.Algo, error) {
	switch id {
	case algorithmV1:
		return algo.FuzzyMatchV1, nil
	case algorithmV2:
		return algo.FuzzyMatchV2, nil
	case algorithmExact:
		return algo.ExactMatchNaive, nil
	case algorithmExactBoundary:
		return algo.ExactMatchBoundary, nil
	case algorithmPrefix:
		return algo.PrefixMatch, nil
	case algorithmSuffix:
		return algo.SuffixMatch, nil
	case algorithmEqual:
		return algo.EqualMatch, nil
	default:
		return nil, fmt.Errorf("unknown algorithm %d", id)
	}
}

func (o *rawOracle) match(request matchRequest) (matchResponse, error) {
	if request.scheme != o.scheme {
		return matchResponse{}, fmt.Errorf(
			"request scheme %s does not match process scheme %s",
			request.scheme.String(), o.scheme.String())
	}
	if request.flags&^validMatchFlags != 0 {
		return matchResponse{}, fmt.Errorf("unknown match flags 0x%02x", request.flags&^validMatchFlags)
	}

	matchAlgorithm, err := selectAlgorithm(request.algorithm)
	if err != nil {
		return matchResponse{}, err
	}

	caseSensitive := request.flags&flagCaseSensitive != 0
	normalize := request.flags&flagNormalize != 0
	forward := request.flags&flagForward != 0
	patternText := string(request.pattern)
	if !caseSensitive {
		patternText = strings.ToLower(patternText)
	}
	pattern := []rune(patternText)
	if normalize {
		pattern = algo.NormalizeRunes(pattern)
	}

	candidate := util.ToChars(request.candidate)
	result, rawPositions := matchAlgorithm(
		caseSensitive, normalize, forward, &candidate, pattern, true, o.slab)

	response := matchResponse{
		matched: result.Start >= 0,
		start:   int64(result.Start),
		end:     int64(result.End),
		score:   int64(result.Score),
	}
	if rawPositions != nil {
		response.positionsPresent = true
		positions := append([]int(nil), (*rawPositions)...)
		sort.Ints(positions)
		response.positions = make([]int64, len(positions))
		for index, position := range positions {
			response.positions[index] = int64(position)
		}
	}
	return response, nil
}
