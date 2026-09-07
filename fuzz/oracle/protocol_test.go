// SPDX-License-Identifier: GPL-3.0-or-later

package main

import (
	"bytes"
	"encoding/binary"
	"encoding/hex"
	"io"
	"runtime"
	"strings"
	"testing"
)

func requestFrame(payload []byte) []byte {
	var buffer bytes.Buffer
	if err := writeFrame(&buffer, payload); err != nil {
		panic(err)
	}
	return buffer.Bytes()
}

func matchRequestPayload(algorithm algorithmID, scheme schemeID, flags byte, pattern, candidate []byte) []byte {
	payload := []byte{protocolVersion, opcodeMatch, byte(algorithm), byte(scheme), flags}
	payload = appendUint32(payload, uint32(len(pattern)))
	payload = appendUint32(payload, uint32(len(candidate)))
	payload = append(payload, pattern...)
	return append(payload, candidate...)
}

func responseFrames(t *testing.T, data []byte) [][]byte {
	t.Helper()
	reader := bytes.NewReader(data)
	var frames [][]byte
	for reader.Len() > 0 {
		payload, err := readFrame(reader)
		if err != nil {
			t.Fatal(err)
		}
		frames = append(frames, payload)
	}
	return frames
}

func testServer(t *testing.T, scheme schemeID) *server {
	t.Helper()
	oracle, err := newRawOracle(scheme)
	if err != nil {
		t.Fatal(err)
	}
	return &server{oracle: oracle}
}

func TestPersistentInfoAndMatch(t *testing.T) {
	server := testServer(t, schemeDefault)
	input := append(requestFrame([]byte{protocolVersion, opcodeInfo}), requestFrame(
		matchRequestPayload(algorithmV2, schemeDefault, flagCaseSensitive|flagForward, []byte("fzf"), []byte("src/fzf")))...)
	var output bytes.Buffer
	if err := server.serve(bytes.NewReader(input), &output); err != nil {
		t.Fatal(err)
	}

	frames := responseFrames(t, output.Bytes())
	if len(frames) != 2 {
		t.Fatalf("got %d response frames; want 2", len(frames))
	}
	if !bytes.Contains(frames[0], []byte(pinnedUpstreamCommit)) || !bytes.Contains(frames[0], []byte(runtime.Version())) {
		t.Fatalf("INFO response lacks build identity: %x", frames[0])
	}

	match := frames[1]
	if got := hex.EncodeToString(match); got != "010100010100000000000000040000000000000007000000000000005400000003000000000000000400000000000000050000000000000006" {
		t.Fatalf("unexpected deterministic MATCH response: %s", got)
	}
}

func TestV2Alignment(t *testing.T) {
	oracle := testServer(t, schemeDefault).oracle
	response, err := oracle.match(matchRequest{
		algorithm: algorithmV2,
		scheme:    schemeDefault,
		flags:     flagForward,
		pattern:   []byte("/a"),
		candidate: []byte("a//a"),
	})
	if err != nil {
		t.Fatal(err)
	}
	if !response.matched || response.start != 2 || response.end != 4 || response.score != 59 {
		t.Fatalf("unexpected result: %+v", response)
	}
	if len(response.positions) != 2 || response.positions[0] != 2 || response.positions[1] != 3 {
		t.Fatalf("unexpected positions: %v", response.positions)
	}
}

func TestMalformedUTF8UsesGoRuneError(t *testing.T) {
	oracle := testServer(t, schemeDefault).oracle
	response, err := oracle.match(matchRequest{
		algorithm: algorithmV2,
		scheme:    schemeDefault,
		flags:     flagCaseSensitive | flagForward,
		pattern:   []byte{0xff},
		candidate: []byte{0xfe},
	})
	if err != nil {
		t.Fatal(err)
	}
	if !response.matched || response.start != 0 || response.end != 1 {
		t.Fatalf("Go replacement-rune behavior changed: %+v", response)
	}
}

func TestContiguousMatcherReportsNilPositions(t *testing.T) {
	oracle := testServer(t, schemeDefault).oracle
	response, err := oracle.match(matchRequest{
		algorithm: algorithmExact,
		scheme:    schemeDefault,
		flags:     flagCaseSensitive | flagForward,
		pattern:   []byte("foo"),
		candidate: []byte("x foo y"),
	})
	if err != nil {
		t.Fatal(err)
	}
	if !response.matched || response.start != 2 || response.end != 5 || response.positionsPresent {
		t.Fatalf("unexpected exact result: %+v", response)
	}
}

func TestBadRequestDoesNotStopServer(t *testing.T) {
	server := testServer(t, schemeDefault)
	bad := requestFrame([]byte{protocolVersion, opcodeMatch})
	good := requestFrame([]byte{protocolVersion, opcodeInfo})
	var output bytes.Buffer
	if err := server.serve(bytes.NewReader(append(bad, good...)), &output); err != nil {
		t.Fatal(err)
	}
	frames := responseFrames(t, output.Bytes())
	if len(frames) != 2 || frames[0][2] != statusBadRequest || frames[1][2] != statusOK {
		t.Fatalf("unexpected statuses: %v", frames)
	}
}

func TestSchemeIsFixedPerProcess(t *testing.T) {
	server := testServer(t, schemeDefault)
	payload := matchRequestPayload(algorithmV1, schemePath, flagForward, []byte("a"), []byte("a"))
	response := server.handle(payload)
	if response[2] != statusBadRequest || !strings.Contains(string(response[7:]), "does not match process scheme") {
		t.Fatalf("unexpected response: %q", response)
	}
}

func TestFrameLimitAndTruncation(t *testing.T) {
	var oversized [4]byte
	binary.BigEndian.PutUint32(oversized[:], maxFrameSize+1)
	if _, err := readFrame(bytes.NewReader(oversized[:])); err == nil {
		t.Fatal("oversized frame succeeded")
	}

	truncated := []byte{0, 0, 0, 3, 1, 2}
	if _, err := readFrame(bytes.NewReader(truncated)); err != io.ErrUnexpectedEOF {
		t.Fatalf("got %v; want unexpected EOF", err)
	}
}
