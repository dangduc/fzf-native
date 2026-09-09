// SPDX-License-Identifier: MIT

// This file is injected into the pinned upstream fzf/src package with the Go
// overlay mechanism.  Package membership gives the oracle access to the exact
// Pattern and Result rank data without maintaining a second Go matcher.
package fzf

import (
	"encoding/json"
	"math"
	"os"
	"sort"
	"testing"

	"github.com/junegunn/fzf/src/algo"
	"github.com/junegunn/fzf/src/util"
)

type semanticOracleInput struct {
	Cases []semanticOracleCase `json:"cases"`
}

type semanticOracleCase struct {
	ID         string   `json:"id"`
	Query      string   `json:"query"`
	Normalize  bool     `json:"normalize"`
	Candidates []string `json:"candidates"`
}

type semanticOracleRecord struct {
	Index       int32     `json:"index"`
	RawScore    int64     `json:"raw_score"`
	MinBegin    int       `json:"min_begin"`
	MinEnd      int       `json:"min_end"`
	MaxEnd      int       `json:"max_end"`
	BoundsValid bool      `json:"bounds_valid"`
	RankScore   uint16    `json:"rank_score"`
	Points      [4]uint16 `json:"points"`
	result      Result
}

type semanticOracleOutput struct {
	Cases []semanticOracleCaseOutput `json:"cases"`
}

type semanticOracleCaseOutput struct {
	ID      string                 `json:"id"`
	Records []semanticOracleRecord `json:"records"`
}

func semanticOracleBounds(offsets []Offset) (int, int, int, bool) {
	minBegin := math.MaxInt
	minEnd := math.MaxInt
	maxEnd := 0
	valid := false
	for _, offset := range offsets {
		begin, end := int(offset[0]), int(offset[1])
		if begin < end {
			minBegin = min(minBegin, begin)
			minEnd = min(minEnd, end)
			maxEnd = max(maxEnd, end)
			valid = true
		}
	}
	if !valid {
		return 0, 0, 0, false
	}
	return minBegin, minEnd, maxEnd, true
}

func semanticOracleMatch(pattern *Pattern, item *Item, slab *util.Slab) (semanticOracleRecord, bool) {
	var rawScore int
	var minBegin, minEnd, maxEnd int
	var boundsValid bool
	var result Result

	if pattern.directAlgo != nil && len(pattern.denylist) == 0 {
		term := pattern.directTerm
		matched, _ := pattern.directAlgo(
			term.caseSensitive, term.normalize, pattern.forward,
			&item.text, term.text, false, slab)
		if matched.Start < 0 {
			return semanticOracleRecord{}, false
		}
		rawScore = matched.Score
		minBegin, minEnd, maxEnd, boundsValid =
			matched.Start, matched.End, matched.End, true
		result = buildResultFromBounds(
			item, rawScore, minBegin, minEnd, maxEnd, boundsValid)
	} else {
		offsets, score, _ := pattern.extendedMatch(item, false, slab)
		if len(offsets) != len(pattern.termSets) {
			return semanticOracleRecord{}, false
		}
		rawScore = score
		minBegin, minEnd, maxEnd, boundsValid = semanticOracleBounds(offsets)
		result = buildResult(item, offsets, rawScore)
	}

	rankScore := util.AsUint16(rawScore)
	return semanticOracleRecord{
		Index:       item.Index(),
		RawScore:    int64(rawScore),
		MinBegin:    minBegin,
		MinEnd:      minEnd,
		MaxEnd:      maxEnd,
		BoundsValid: boundsValid,
		RankScore:   rankScore,
		Points:      result.points,
		result:      result,
	}, true
}

func TestFzfNativeSemanticOracle(t *testing.T) {
	inputPath := os.Getenv("FZF_SEMANTIC_ORACLE_INPUT")
	outputPath := os.Getenv("FZF_SEMANTIC_ORACLE_OUTPUT")
	if inputPath == "" || outputPath == "" {
		t.Fatal("FZF_SEMANTIC_ORACLE_INPUT and FZF_SEMANTIC_ORACLE_OUTPUT are required")
	}

	inputBytes, err := os.ReadFile(inputPath)
	if err != nil {
		t.Fatal(err)
	}
	var input semanticOracleInput
	if err := json.Unmarshal(inputBytes, &input); err != nil {
		t.Fatal(err)
	}

	// The benchmark command uses --tiebreak=index. parseTiebreak("index")
	// leaves score as the sole rank point; input index is the final comparator.
	previousCriteria := sortCriteria
	sortCriteria = []criterion{byScore}
	defer func() { sortCriteria = previousCriteria }()
	if !algo.Init("default") {
		t.Fatal("could not initialize the default fzf scoring scheme")
	}

	output := semanticOracleOutput{Cases: make([]semanticOracleCaseOutput, 0, len(input.Cases))}
	for _, oracleCase := range input.Cases {
		pattern := BuildPattern(
			NewChunkCache(), map[string]*Pattern{}, true, algo.FuzzyMatchV2,
			true, CaseSmart, oracleCase.Normalize, true, false, false,
			nil, Delimiter{}, revision{}, []rune(oracleCase.Query), nil, 0)
		slab := util.MakeSlab(slab16Size, slab32Size)
		records := make([]semanticOracleRecord, 0, len(oracleCase.Candidates))
		for index, candidate := range oracleCase.Candidates {
			chars := util.ToChars([]byte(candidate))
			chars.Index = int32(index)
			item := Item{text: chars}
			if record, matched := semanticOracleMatch(pattern, &item, slab); matched {
				records = append(records, record)
			}
		}
		if pattern.sortable {
			sort.SliceStable(records, func(i, j int) bool {
				return compareRanks(records[i].result, records[j].result, false)
			})
		}
		for index := range records {
			records[index].result = Result{}
		}
		output.Cases = append(output.Cases, semanticOracleCaseOutput{
			ID: oracleCase.ID, Records: records,
		})
	}

	encoded, err := json.MarshalIndent(output, "", "  ")
	if err != nil {
		t.Fatal(err)
	}
	encoded = append(encoded, '\n')
	if err := os.WriteFile(outputPath, encoded, 0o644); err != nil {
		t.Fatal(err)
	}
}
