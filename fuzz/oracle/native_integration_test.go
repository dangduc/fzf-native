// SPDX-License-Identifier: GPL-3.0-or-later

package main

import (
	"bytes"
	"encoding/binary"
	"fmt"
	"io"
	"os"
	"os/exec"
	"reflect"
	"runtime"
	"strconv"
	"strings"
	"sync"
	"testing"
	"time"
)

const (
	peerDeadline           = 5 * time.Second
	fullResultExampleLimit = 8
)

type nativePeer struct {
	command *exec.Cmd
	input   io.WriteCloser
	output  io.Reader
	stderr  synchronizedCapture
}

type synchronizedCapture struct {
	mu       sync.Mutex
	contents bytes.Buffer
}

func (c *synchronizedCapture) Write(data []byte) (int, error) {
	c.mu.Lock()
	defer c.mu.Unlock()
	return c.contents.Write(data)
}

func (c *synchronizedCapture) String() string {
	c.mu.Lock()
	defer c.mu.Unlock()
	return c.contents.String()
}

func TestSynchronizedCaptureConcurrentWriteAndString(t *testing.T) {
	const (
		writers    = 4
		readers    = 4
		iterations = 512
		chunk      = "stderr\n"
	)

	var capture synchronizedCapture
	start := make(chan struct{})
	writersDone := make(chan struct{})
	failures := make(chan error, writers+readers)

	var writerGroup sync.WaitGroup
	writerGroup.Add(writers)
	for writer := 0; writer < writers; writer++ {
		go func() {
			defer writerGroup.Done()
			<-start
			for iteration := 0; iteration < iterations; iteration++ {
				written, err := capture.Write([]byte(chunk))
				if err != nil || written != len(chunk) {
					failures <- fmt.Errorf("capture write = %d, %v", written, err)
					return
				}
				runtime.Gosched()
			}
		}()
	}
	go func() {
		writerGroup.Wait()
		close(writersDone)
	}()

	var readerGroup sync.WaitGroup
	readerGroup.Add(readers)
	for reader := 0; reader < readers; reader++ {
		go func() {
			defer readerGroup.Done()
			<-start
			for {
				snapshot := capture.String()
				if len(snapshot)%len(chunk) != 0 {
					failures <- fmt.Errorf("partial capture snapshot length %d", len(snapshot))
					return
				}
				for offset := 0; offset < len(snapshot); offset += len(chunk) {
					if snapshot[offset:offset+len(chunk)] != chunk {
						failures <- fmt.Errorf("corrupt capture snapshot at byte %d", offset)
						return
					}
				}
				select {
				case <-writersDone:
					return
				default:
					runtime.Gosched()
				}
			}
		}()
	}

	close(start)
	<-writersDone
	readerGroup.Wait()
	close(failures)
	for err := range failures {
		t.Error(err)
	}

	wantLength := writers * iterations * len(chunk)
	if got := len(capture.String()); got != wantLength {
		t.Fatalf("final capture length = %d, want %d", got, wantLength)
	}
}

type infoResponse struct {
	revision string
	runtime  string
}

func startNativePeer(t *testing.T, path string) *nativePeer {
	return startNativePeerWithArgs(t, path)
}

func startNativePeerWithArgs(t *testing.T, path string, args ...string) *nativePeer {
	t.Helper()
	peer := &nativePeer{command: exec.Command(path, args...)}
	var err error
	peer.input, err = peer.command.StdinPipe()
	if err != nil {
		t.Fatal(err)
	}
	peer.output, err = peer.command.StdoutPipe()
	if err != nil {
		t.Fatal(err)
	}
	peer.command.Stderr = &peer.stderr
	if err := peer.command.Start(); err != nil {
		t.Fatal(err)
	}
	return peer
}

func (p *nativePeer) exchange(t *testing.T, payload []byte) []byte {
	t.Helper()
	type exchangeResult struct {
		payload []byte
		err     error
	}
	done := make(chan exchangeResult, 1)
	go func() {
		if err := writeFrame(p.input, payload); err != nil {
			done <- exchangeResult{err: fmt.Errorf("write request: %w", err)}
			return
		}
		response, err := readFrame(p.output)
		done <- exchangeResult{payload: response, err: err}
	}()
	timer := time.NewTimer(peerDeadline)
	defer stopTimer(timer)
	select {
	case result := <-done:
		if result.err != nil {
			t.Fatalf("native exchange: %v\nstderr: %s", result.err, p.stderr.String())
		}
		return result.payload
	case <-timer.C:
		if p.command.Process != nil {
			_ = p.command.Process.Kill()
		}
		t.Fatal("native exchange exceeded the five-second deadline")
		return nil
	}
}

func (p *nativePeer) close(t *testing.T) {
	t.Helper()
	if err := p.closeWithin(peerDeadline); err != nil {
		t.Errorf("%v\nstderr: %s", err, p.stderr.String())
	}
}

func stopTimer(timer *time.Timer) {
	if !timer.Stop() {
		select {
		case <-timer.C:
		default:
		}
	}
}

func (p *nativePeer) closeWithin(deadline time.Duration) error {
	closeErr := p.input.Close()
	waitDone := make(chan error, 1)
	go func() {
		waitDone <- p.command.Wait()
	}()

	timer := time.NewTimer(deadline)
	defer stopTimer(timer)
	select {
	case waitErr := <-waitDone:
		if closeErr != nil && waitErr != nil {
			return fmt.Errorf("close native input: %v; native peer failed: %w", closeErr, waitErr)
		}
		if closeErr != nil {
			return fmt.Errorf("close native input: %w", closeErr)
		}
		if waitErr != nil {
			return fmt.Errorf("native peer failed: %w", waitErr)
		}
		return nil
	case <-timer.C:
		if p.command.Process != nil {
			_ = p.command.Process.Kill()
		}
		return fmt.Errorf("native peer did not exit within %s", deadline)
	}
}

func TestNativePeerCloseHasDeadline(t *testing.T) {
	const helperArgument = "fzf-oracle-close-hang-helper"
	if len(os.Args) > 1 && os.Args[len(os.Args)-1] == helperArgument {
		payload, err := readFrame(os.Stdin)
		if err != nil || !bytes.Equal(payload, []byte{protocolVersion, opcodeInfo}) {
			os.Exit(2)
		}
		if err := writeFrame(os.Stdout, infoPayload()); err != nil {
			os.Exit(2)
		}
		for {
			time.Sleep(time.Hour)
		}
	}

	peer := startNativePeerWithArgs(t, os.Args[0],
		"-test.run=^TestNativePeerCloseHasDeadline$", "--", helperArgument)
	if _, err := decodeInfoResponse(peer.exchange(t, []byte{protocolVersion, opcodeInfo})); err != nil {
		t.Fatal(err)
	}
	started := time.Now()
	err := peer.closeWithin(100 * time.Millisecond)
	if err == nil || !strings.Contains(err.Error(), "did not exit within") {
		t.Fatalf("close got %v; want a deadline error", err)
	}
	if elapsed := time.Since(started); elapsed > time.Second {
		t.Fatalf("bounded close took %s", elapsed)
	}
}

func TestStopTimerDrainsExpiredTimer(t *testing.T) {
	timer := time.NewTimer(0)
	<-timer.C
	stopTimer(timer)
	select {
	case <-timer.C:
		t.Fatal("stopped timer still had a value")
	default:
	}
}

func decodeMatchResponse(payload []byte) (matchResponse, byte, error) {
	if len(payload) < 3 {
		return matchResponse{}, 0, fmt.Errorf("response has %d bytes; need at least 3", len(payload))
	}
	if payload[0] != protocolVersion || payload[1] != opcodeMatch {
		return matchResponse{}, payload[2], fmt.Errorf("unexpected response prefix %x", payload[:3])
	}
	status := payload[2]
	if status != statusOK {
		if len(payload) < 7 {
			return matchResponse{}, status, fmt.Errorf("error response has %d bytes; need at least 7", len(payload))
		}
		messageLength := binary.BigEndian.Uint32(payload[3:7])
		if uint64(messageLength)+7 != uint64(len(payload)) {
			return matchResponse{}, status, fmt.Errorf("error response length is inconsistent")
		}
		return matchResponse{}, status, fmt.Errorf("native status %d: %s", status, payload[7:])
	}
	if len(payload) < 33 {
		return matchResponse{}, status, fmt.Errorf("success response has %d bytes; need at least 33", len(payload))
	}
	if payload[3] > 1 || payload[4] > 1 {
		return matchResponse{}, status, fmt.Errorf("invalid Boolean fields %d and %d", payload[3], payload[4])
	}
	count := binary.BigEndian.Uint32(payload[29:33])
	want := uint64(33) + uint64(count)*8
	if want != uint64(len(payload)) {
		return matchResponse{}, status, fmt.Errorf("success response has %d bytes; positions require %d", len(payload), want)
	}
	var positions []int64
	if payload[4] != 0 {
		positions = make([]int64, int(count))
	} else if count != 0 {
		return matchResponse{}, status, fmt.Errorf("absent positions have count %d", count)
	}
	response := matchResponse{
		matched:          payload[3] != 0,
		positionsPresent: payload[4] != 0,
		start:            int64(binary.BigEndian.Uint64(payload[5:13])),
		end:              int64(binary.BigEndian.Uint64(payload[13:21])),
		score:            int64(binary.BigEndian.Uint64(payload[21:29])),
		positions:        positions,
	}
	for index := range response.positions {
		offset := 33 + index*8
		response.positions[index] = int64(binary.BigEndian.Uint64(payload[offset : offset+8]))
	}
	return response, status, nil
}

func decodeInfoResponse(payload []byte) (infoResponse, error) {
	if len(payload) < 7 || payload[0] != protocolVersion || payload[1] != opcodeInfo || payload[2] != statusOK {
		return infoResponse{}, fmt.Errorf("invalid INFO response prefix: %x", payload)
	}
	revisionLength := uint64(binary.BigEndian.Uint32(payload[3:7]))
	if revisionLength > uint64(len(payload)-7) {
		return infoResponse{}, fmt.Errorf("INFO revision length exceeds payload")
	}
	runtimeOffset := uint64(7) + revisionLength
	if runtimeOffset+4 > uint64(len(payload)) {
		return infoResponse{}, fmt.Errorf("INFO response lacks runtime length")
	}
	runtimeLength := uint64(binary.BigEndian.Uint32(payload[runtimeOffset : runtimeOffset+4]))
	if runtimeOffset+4+runtimeLength != uint64(len(payload)) {
		return infoResponse{}, fmt.Errorf("INFO runtime length is inconsistent")
	}
	return infoResponse{
		revision: string(payload[7:runtimeOffset]),
		runtime:  string(payload[runtimeOffset+4:]),
	}, nil
}

func TestNativePeerMatchesRawOracle(t *testing.T) {
	driver := os.Getenv("FZF_NATIVE_ALGO_DRIVER")
	if driver == "" {
		t.Skip("set FZF_NATIVE_ALGO_DRIVER to check the native peer")
	}

	peer := startNativePeer(t, driver)
	defer peer.close(t)
	info, err := decodeInfoResponse(peer.exchange(t, []byte{protocolVersion, opcodeInfo}))
	if err != nil {
		t.Fatal(err)
	}
	wantRevision := os.Getenv("FZF_NATIVE_EXPECTED_REVISION")
	if wantRevision == "" {
		t.Fatal("FZF_NATIVE_EXPECTED_REVISION is required with FZF_NATIVE_ALGO_DRIVER")
	}
	if info.revision != wantRevision || info.runtime == "" {
		t.Fatalf("native INFO got revision=%q runtime=%q; want revision=%q", info.revision, info.runtime, wantRevision)
	}

	oracle, err := newRawOracle(schemeDefault)
	if err != nil {
		t.Fatal(err)
	}
	cases := []struct {
		name      string
		algorithm algorithmID
		flags     byte
		pattern   []byte
		candidate []byte
	}{
		{"cjk-v2", algorithmV2, flagCaseSensitive | flagForward, []byte("中文"), []byte("测试中文")},
		{"supplementary-v1", algorithmV1, flagCaseSensitive | flagForward, []byte("😀"), []byte("a😀b")},
		{"case-fold-expands-v2", algorithmV2, flagForward, []byte("Ⱥ"), []byte("xⱥ")},
		{"kelvin-fold-v2", algorithmV2, flagForward, []byte("K"), []byte("xk")},
		{"sigma-fold-v2", algorithmV2, flagForward, []byte("Σ"), []byte("xσ")},
		{"embedded-nul-v2", algorithmV2, flagCaseSensitive | flagForward, []byte{'a', 0}, []byte{'x', 'a', 0}},
	}
	for _, testCase := range cases {
		t.Run(testCase.name, func(t *testing.T) {
			request := matchRequest{
				algorithm: testCase.algorithm,
				scheme:    schemeDefault,
				flags:     testCase.flags,
				pattern:   testCase.pattern,
				candidate: testCase.candidate,
			}
			want, err := oracle.match(request)
			if err != nil {
				t.Fatal(err)
			}
			payload := matchRequestPayload(request.algorithm, request.scheme, request.flags, request.pattern, request.candidate)
			got, _, err := decodeMatchResponse(peer.exchange(t, payload))
			if err != nil {
				t.Fatal(err)
			}
			if !reflect.DeepEqual(got, want) {
				t.Fatalf("native result %+v does not match upstream result %+v", got, want)
			}
		})
	}
}

func TestBuiltOracleProcessIsPersistent(t *testing.T) {
	binary := os.Getenv("FZF_RAW_ORACLE_BINARY")
	if binary == "" {
		t.Skip("set FZF_RAW_ORACLE_BINARY to check the built oracle process")
	}

	// fzf scoring schemes mutate package-global tables that are not fully
	// reset by a later Init call.  Keep each scheme in its own built process
	// and compare with pinned results instead of mixing schemes in this process.
	for _, testCase := range []struct {
		name       string
		scheme     schemeID
		matchScore int64
	}{
		{"default", schemeDefault, 84},
		{"path", schemePath, 84},
		{"history", schemeHistory, 80},
	} {
		t.Run(testCase.name, func(t *testing.T) {
			peer := startNativePeerWithArgs(t, binary, "--scheme="+testCase.name)
			defer peer.close(t)

			info, err := decodeInfoResponse(peer.exchange(t, []byte{protocolVersion, opcodeInfo}))
			if err != nil {
				t.Fatal(err)
			}
			if info.revision != pinnedUpstreamCommit || info.runtime == "" {
				t.Fatalf("oracle INFO got revision=%q runtime=%q", info.revision, info.runtime)
			}
			for _, candidate := range []string{"src/fzf", "测试中文"} {
				request := matchRequest{
					algorithm: algorithmV2,
					scheme:    testCase.scheme,
					flags:     flagCaseSensitive | flagForward,
					pattern:   []byte("fzf"),
					candidate: []byte(candidate),
				}
				want := matchResponse{start: -1, end: -1}
				if candidate == "src/fzf" {
					want = matchResponse{
						matched: true, positionsPresent: true,
						start: 4, end: 7, score: testCase.matchScore,
						positions: []int64{4, 5, 6},
					}
				}
				got, _, err := decodeMatchResponse(peer.exchange(t,
					matchRequestPayload(request.algorithm, request.scheme, request.flags, request.pattern, request.candidate)))
				if err != nil {
					t.Fatal(err)
				}
				if !reflect.DeepEqual(got, want) {
					t.Fatalf("built process result %+v does not match in-process result %+v", got, want)
				}
			}
		})
	}
}

func TestNativePeerKnownAlignmentAndNormalizationGaps(t *testing.T) {
	driver := os.Getenv("FZF_NATIVE_ALGO_DRIVER")
	if driver == "" {
		t.Skip("set FZF_NATIVE_ALGO_DRIVER to check the native peer")
	}
	peer := startNativePeer(t, driver)
	defer peer.close(t)
	oracle, err := newRawOracle(schemeDefault)
	if err != nil {
		t.Fatal(err)
	}

	cases := []struct {
		name           string
		flags          byte
		pattern        []byte
		candidate      []byte
		wantUpstream   matchResponse
		wantNativePeer matchResponse
	}{
		{
			name:      "v2-alignment",
			flags:     flagForward,
			pattern:   []byte("/a"),
			candidate: []byte("a//a"),
			wantUpstream: matchResponse{
				matched: true, positionsPresent: true, start: 2, end: 4, score: 59, positions: []int64{2, 3},
			},
			wantNativePeer: matchResponse{
				matched: true, positionsPresent: true, start: 1, end: 4, score: 56, positions: []int64{1, 3},
			},
		},
		{
			name:      "latin-normalization",
			flags:     flagCaseSensitive | flagNormalize | flagForward,
			pattern:   []byte("cafe"),
			candidate: []byte("café"),
			wantUpstream: matchResponse{
				matched: true, positionsPresent: true, start: 0, end: 4, score: 114, positions: []int64{0, 1, 2, 3},
			},
			wantNativePeer: matchResponse{
				matched: false, positionsPresent: true, start: -1, end: -1, score: 0, positions: []int64{},
			},
		},
	}
	for _, testCase := range cases {
		t.Run(testCase.name, func(t *testing.T) {
			request := matchRequest{
				algorithm: algorithmV2,
				scheme:    schemeDefault,
				flags:     testCase.flags,
				pattern:   testCase.pattern,
				candidate: testCase.candidate,
			}
			upstream, err := oracle.match(request)
			if err != nil {
				t.Fatal(err)
			}
			native, _, err := decodeMatchResponse(peer.exchange(t,
				matchRequestPayload(request.algorithm, request.scheme, request.flags, request.pattern, request.candidate)))
			if err != nil {
				t.Fatal(err)
			}
			if !reflect.DeepEqual(upstream, testCase.wantUpstream) {
				t.Fatalf("upstream gap shape changed: got %+v; want %+v", upstream, testCase.wantUpstream)
			}
			if !reflect.DeepEqual(native, testCase.wantNativePeer) {
				t.Fatalf("native gap shape changed: got %+v; want %+v", native, testCase.wantNativePeer)
			}
		})
	}
}

func TestNativePeerKnownScoreGaps(t *testing.T) {
	driver := os.Getenv("FZF_NATIVE_ALGO_DRIVER")
	if driver == "" {
		t.Skip("set FZF_NATIVE_ALGO_DRIVER to check the native peer")
	}

	peer := startNativePeer(t, driver)
	defer peer.close(t)
	oracle, err := newRawOracle(schemeDefault)
	if err != nil {
		t.Fatal(err)
	}
	cases := []struct {
		name          string
		flags         byte
		pattern       []byte
		candidate     []byte
		upstreamScore int64
		nativeScore   int64
	}{
		{"ascii-boundary-bonus", flagCaseSensitive | flagForward, []byte("fzf"), []byte("src/fzf"), 84, 80},
		{"unicode-fold-boundary-bonus", flagForward, []byte("Ⱥ"), []byte("ⱥ"), 36, 32},
	}
	for _, testCase := range cases {
		t.Run(testCase.name, func(t *testing.T) {
			request := matchRequest{
				algorithm: algorithmV2,
				scheme:    schemeDefault,
				flags:     testCase.flags,
				pattern:   testCase.pattern,
				candidate: testCase.candidate,
			}
			upstream, err := oracle.match(request)
			if err != nil {
				t.Fatal(err)
			}
			native, _, err := decodeMatchResponse(peer.exchange(t,
				matchRequestPayload(request.algorithm, request.scheme, request.flags, request.pattern, request.candidate)))
			if err != nil {
				t.Fatal(err)
			}
			if upstream.score != testCase.upstreamScore || native.score != testCase.nativeScore {
				t.Fatalf("score gap changed: upstream=%d native=%d; want upstream=%d native=%d",
					upstream.score, native.score, testCase.upstreamScore, testCase.nativeScore)
			}
			upstream.score = 0
			native.score = 0
			if !reflect.DeepEqual(native, upstream) {
				t.Fatalf("non-score result changed: native=%+v upstream=%+v", native, upstream)
			}
		})
	}
}

func TestNativePeerKnownContiguousResultGaps(t *testing.T) {
	driver := os.Getenv("FZF_NATIVE_ALGO_DRIVER")
	if driver == "" {
		t.Skip("set FZF_NATIVE_ALGO_DRIVER to check the native peer")
	}
	peer := startNativePeer(t, driver)
	defer peer.close(t)
	oracle, err := newRawOracle(schemeDefault)
	if err != nil {
		t.Fatal(err)
	}

	cases := []struct {
		name           string
		algorithm      algorithmID
		pattern        []byte
		candidate      []byte
		wantUpstream   matchResponse
		wantNativePeer matchResponse
	}{
		{
			name: "exact-position-representation", algorithm: algorithmExact,
			pattern: []byte("ab"), candidate: []byte("xabx"),
			wantUpstream:   matchResponse{matched: true, start: 1, end: 3, score: 36},
			wantNativePeer: matchResponse{matched: true, positionsPresent: true, start: 1, end: 3, score: 36, positions: []int64{1, 2}},
		},
		{
			name: "suffix-position-representation", algorithm: algorithmSuffix,
			pattern: []byte("ab"), candidate: []byte("xab"),
			wantUpstream:   matchResponse{matched: true, start: 1, end: 3, score: 36},
			wantNativePeer: matchResponse{matched: true, positionsPresent: true, start: 1, end: 3, score: 36, positions: []int64{1, 2}},
		},
		{
			name: "prefix-score-and-position-representation", algorithm: algorithmPrefix,
			pattern: []byte("ab"), candidate: []byte("abx"),
			wantUpstream:   matchResponse{matched: true, start: 0, end: 2, score: 62},
			wantNativePeer: matchResponse{matched: true, positionsPresent: true, start: 0, end: 2, score: 56, positions: []int64{0, 1}},
		},
		{
			name: "equal-score-and-position-representation", algorithm: algorithmEqual,
			pattern: []byte("ab"), candidate: []byte("ab"),
			wantUpstream:   matchResponse{matched: true, start: 0, end: 2, score: 62},
			wantNativePeer: matchResponse{matched: true, positionsPresent: true, start: 0, end: 2, score: 56, positions: []int64{0, 1}},
		},
		{
			name: "v1-empty-pattern-position-representation", algorithm: algorithmV1,
			pattern: []byte{}, candidate: []byte("abc"),
			wantUpstream:   matchResponse{matched: true, start: 0, end: 0, score: 0},
			wantNativePeer: matchResponse{matched: true, positionsPresent: true, start: 0, end: 0, score: 0, positions: []int64{}},
		},
	}
	for _, testCase := range cases {
		t.Run(testCase.name, func(t *testing.T) {
			request := matchRequest{
				algorithm: testCase.algorithm,
				scheme:    schemeDefault,
				flags:     flagCaseSensitive | flagForward,
				pattern:   testCase.pattern,
				candidate: testCase.candidate,
			}
			upstream, err := oracle.match(request)
			if err != nil {
				t.Fatal(err)
			}
			native, _, err := decodeMatchResponse(peer.exchange(t,
				matchRequestPayload(request.algorithm, request.scheme, request.flags, request.pattern, request.candidate)))
			if err != nil {
				t.Fatal(err)
			}
			if !reflect.DeepEqual(upstream, testCase.wantUpstream) {
				t.Fatalf("upstream gap shape changed: got %+v; want %+v", upstream, testCase.wantUpstream)
			}
			if !reflect.DeepEqual(native, testCase.wantNativePeer) {
				t.Fatalf("native gap shape changed: got %+v; want %+v", native, testCase.wantNativePeer)
			}
		})
	}
}

func TestNativePeerMembershipMatrix(t *testing.T) {
	driver := os.Getenv("FZF_NATIVE_ALGO_DRIVER")
	if driver == "" {
		t.Skip("set FZF_NATIVE_ALGO_DRIVER to check the native peer")
	}
	peer := startNativePeer(t, driver)
	defer peer.close(t)
	oracle, err := newRawOracle(schemeDefault)
	if err != nil {
		t.Fatal(err)
	}

	cases := []struct {
		name      string
		algorithm algorithmID
		pattern   string
		candidate string
	}{
		{"v1-match", algorithmV1, "ab", "a_b"},
		{"v1-miss", algorithmV1, "ab", "ba"},
		{"v2-match", algorithmV2, "ab", "a_b"},
		{"v2-miss", algorithmV2, "ab", "ba"},
		{"exact-match", algorithmExact, "ab", "xabx"},
		{"exact-miss", algorithmExact, "ab", "axb"},
		{"prefix-match", algorithmPrefix, "ab", "abx"},
		{"prefix-miss", algorithmPrefix, "ab", "xab"},
		{"suffix-match", algorithmSuffix, "ab", "xab"},
		{"suffix-miss", algorithmSuffix, "ab", "abx"},
		{"equal-match", algorithmEqual, "ab", "ab"},
		{"equal-miss", algorithmEqual, "ab", "xab"},
	}
	for _, testCase := range cases {
		t.Run(testCase.name, func(t *testing.T) {
			request := matchRequest{
				algorithm: testCase.algorithm,
				scheme:    schemeDefault,
				flags:     flagCaseSensitive | flagForward,
				pattern:   []byte(testCase.pattern),
				candidate: []byte(testCase.candidate),
			}
			upstream, err := oracle.match(request)
			if err != nil {
				t.Fatal(err)
			}
			native, _, err := decodeMatchResponse(peer.exchange(t,
				matchRequestPayload(request.algorithm, request.scheme, request.flags, request.pattern, request.candidate)))
			if err != nil {
				t.Fatal(err)
			}
			if native.matched != upstream.matched {
				t.Fatalf("membership differs: native=%t upstream=%t", native.matched, upstream.matched)
			}
		})
	}
}

func TestNativePeerReportsCurrentCapabilityBoundary(t *testing.T) {
	driver := os.Getenv("FZF_NATIVE_ALGO_DRIVER")
	if driver == "" {
		t.Skip("set FZF_NATIVE_ALGO_DRIVER to check the native peer")
	}
	peer := startNativePeer(t, driver)
	defer peer.close(t)

	unsupported := []struct {
		name      string
		algorithm algorithmID
		scheme    schemeID
		flags     byte
	}{
		{"exact-boundary", algorithmExactBoundary, schemeDefault, flagForward},
		{"path-scheme", algorithmV2, schemePath, flagForward},
		{"backward-search", algorithmV2, schemeDefault, 0},
	}
	for _, testCase := range unsupported {
		t.Run(testCase.name, func(t *testing.T) {
			request := matchRequestPayload(testCase.algorithm, testCase.scheme, testCase.flags, []byte("a"), []byte("a"))
			_, status, err := decodeMatchResponse(peer.exchange(t, request))
			if status != statusUnsupported || err == nil {
				t.Fatalf("got status %d and error %v; want unsupported status", status, err)
			}
		})
	}
}

func TestMalformedUTF8DifferenceIsExplicit(t *testing.T) {
	driver := os.Getenv("FZF_NATIVE_ALGO_DRIVER")
	if driver == "" {
		t.Skip("set FZF_NATIVE_ALGO_DRIVER to check the native peer")
	}
	peer := startNativePeer(t, driver)
	defer peer.close(t)

	request := matchRequest{
		algorithm: algorithmV2,
		scheme:    schemeDefault,
		flags:     flagCaseSensitive | flagForward,
		pattern:   []byte{0xff},
		candidate: []byte{0xfe},
	}
	oracle, err := newRawOracle(schemeDefault)
	if err != nil {
		t.Fatal(err)
	}
	upstream, err := oracle.match(request)
	if err != nil {
		t.Fatal(err)
	}
	native, _, err := decodeMatchResponse(peer.exchange(t,
		matchRequestPayload(request.algorithm, request.scheme, request.flags, request.pattern, request.candidate)))
	if err != nil {
		t.Fatal(err)
	}
	if !upstream.matched || native.matched {
		t.Fatalf("malformed UTF-8 exception changed: upstream=%+v native=%+v", upstream, native)
	}
}

type rawMatrixRNG uint64

func (r *rawMatrixRNG) next() uint64 {
	x := uint64(*r)
	x ^= x << 13
	x ^= x >> 7
	x ^= x << 17
	*r = rawMatrixRNG(x)
	return x
}

func rawMatrixEnv(t *testing.T, name string, fallback uint64) uint64 {
	t.Helper()
	value := os.Getenv(name)
	if value == "" {
		return fallback
	}
	parsed, err := strconv.ParseUint(value, 10, 64)
	if err != nil {
		t.Fatalf("%s must be an unsigned decimal integer: %v", name, err)
	}
	return parsed
}

func rawMatrixRequest(seed, serial uint64) matchRequest {
	if serial%20000 == 0 {
		return matchRequest{
			algorithm: algorithmEqual,
			scheme:    schemeDefault,
			flags:     flagForward,
			pattern:   []byte("- "),
			candidate: []byte("- "),
		}
	}

	state := rawMatrixRNG(seed ^ ((serial + 1) * 0x9e3779b97f4a7c15))
	if state == 0 {
		state = 0x6a09e667f3bcc909
	}
	algorithms := [...]algorithmID{
		algorithmV1, algorithmV2, algorithmExact,
		algorithmPrefix, algorithmSuffix, algorithmEqual,
	}
	alphabet := []byte("abAB/_- .:")
	request := matchRequest{
		algorithm: algorithms[state.next()%uint64(len(algorithms))],
		scheme:    schemeDefault,
		flags:     flagForward,
	}
	if state.next()%2 != 0 {
		request.flags |= flagCaseSensitive
	}
	if serial%5000 == 1 {
		length := [...]int{999, 1000, 1001}[(serial/5000)%3]
		request.algorithm = algorithmV2
		request.flags = flagCaseSensitive | flagForward
		request.pattern = bytes.Repeat([]byte("a"), length)
		request.candidate = append([]byte("/"), request.pattern...)
		return request
	}
	if serial%1024 == 4 {
		pairs := [...][2]string{
			{"k", "K"}, {"ⱥ", "Ⱥ"}, {"σ", "Σ"},
			{"你", "你"}, {"😀", "😀"}, {"𐐷", "𐐏"},
		}
		pair := pairs[(serial/1024)%uint64(len(pairs))]
		request.flags = flagForward
		request.pattern = []byte(pair[0])
		request.candidate = []byte(pair[1])
		return request
	}
	if serial%16 == 3 {
		atoms := [...]string{
			"a", "B", "/", "_", " ", "é", "σ", "你", "😀", "K", "Ⱥ", "ⱥ", "𐐷",
		}
		patternLength := int(state.next() % 7)
		candidateLength := int(state.next() % 14)
		for index := 0; index < patternLength; index++ {
			request.pattern = append(request.pattern,
				[]byte(atoms[state.next()%uint64(len(atoms))])...)
		}
		for index := 0; index < candidateLength; index++ {
			request.candidate = append(request.candidate,
				[]byte(atoms[state.next()%uint64(len(atoms))])...)
		}
		return request
	}
	patternLength := int(state.next() % 7)
	candidateLength := int(state.next() % 14)
	request.pattern = make([]byte, patternLength)
	request.candidate = make([]byte, candidateLength)
	for index := range request.pattern {
		request.pattern[index] = alphabet[state.next()%uint64(len(alphabet))]
	}
	for index := range request.candidate {
		request.candidate[index] = alphabet[state.next()%uint64(len(alphabet))]
	}
	return request
}

type fullResultAudit struct {
	differenceCount uint64
	examples        []string
}

func compactBytes(value []byte) string {
	const prefixLimit = 12
	prefix := value
	suffix := ""
	if len(prefix) > prefixLimit {
		prefix = prefix[:prefixLimit]
		suffix = "..."
	}
	return fmt.Sprintf("len=%d hex=%x%s", len(value), prefix, suffix)
}

func compactResponse(response matchResponse) string {
	positionSummary := "absent"
	if response.positionsPresent {
		positionSummary = fmt.Sprintf("count=%d", len(response.positions))
		if len(response.positions) != 0 {
			positionSummary += fmt.Sprintf(" first=%d last=%d",
				response.positions[0], response.positions[len(response.positions)-1])
		}
	}
	return fmt.Sprintf("matched=%t start=%d end=%d score=%d positions=(%s)",
		response.matched, response.start, response.end, response.score, positionSummary)
}

func compactMatrixDifference(seed, serial uint64, request matchRequest,
	upstream, native matchResponse) string {
	return fmt.Sprintf(
		"seed=%d serial=%d algorithm=%d scheme=%d flags=0x%02x pattern=(%s) candidate=(%s) upstream=(%s) native=(%s)",
		seed, serial, request.algorithm, request.scheme, request.flags,
		compactBytes(request.pattern), compactBytes(request.candidate),
		compactResponse(upstream), compactResponse(native))
}

func (audit *fullResultAudit) add(seed, serial uint64, request matchRequest,
	upstream, native matchResponse) {
	audit.differenceCount++
	if len(audit.examples) < fullResultExampleLimit {
		audit.examples = append(audit.examples,
			compactMatrixDifference(seed, serial, request, upstream, native))
	}
}

func TestFullResultAuditUsesCompactBoundedExamples(t *testing.T) {
	request := matchRequest{
		algorithm: algorithmV2,
		flags:     flagCaseSensitive | flagForward,
		pattern:   bytes.Repeat([]byte("a"), 1001),
		candidate: append([]byte("/"), bytes.Repeat([]byte("a"), 1001)...),
	}
	positions := make([]int64, 1001)
	for index := range positions {
		positions[index] = int64(index + 1)
	}
	upstream := matchResponse{matched: true, positionsPresent: true,
		start: 1, end: 1002, score: 10, positions: positions}
	native := upstream
	native.score = 9

	var audit fullResultAudit
	for serial := uint64(0); serial < fullResultExampleLimit+3; serial++ {
		audit.add(7, serial, request, upstream, native)
	}
	if audit.differenceCount != fullResultExampleLimit+3 {
		t.Fatalf("difference count is %d", audit.differenceCount)
	}
	if len(audit.examples) != fullResultExampleLimit {
		t.Fatalf("example count is %d", len(audit.examples))
	}
	for _, example := range audit.examples {
		if len(example) > 500 || strings.Contains(example, "[1 2 3") {
			t.Fatalf("diagnostic is not compact: %q", example)
		}
	}
}

func TestNativePeerDeterministicMembershipMatrix(t *testing.T) {
	driver := os.Getenv("FZF_NATIVE_ALGO_DRIVER")
	if driver == "" {
		t.Skip("set FZF_NATIVE_ALGO_DRIVER to check the native peer")
	}
	oracleBinary := os.Getenv("FZF_RAW_ORACLE_BINARY")
	if oracleBinary == "" {
		t.Fatal("FZF_RAW_ORACLE_BINARY is required with FZF_NATIVE_ALGO_DRIVER")
	}
	seed := rawMatrixEnv(t, "FZF_RAW_MATRIX_SEED", 20260906)
	start := rawMatrixEnv(t, "FZF_RAW_MATRIX_START", 0)
	caseCount := rawMatrixEnv(t, "FZF_RAW_MATRIX_CASES", 20000)
	fullResults := os.Getenv("FZF_RAW_MATRIX_FULL_RESULTS") == "1"

	nativePeer := startNativePeer(t, driver)
	defer nativePeer.close(t)
	oraclePeer := startNativePeerWithArgs(t, oracleBinary, "--scheme=default")
	defer oraclePeer.close(t)
	oracleInfo, err := decodeInfoResponse(
		oraclePeer.exchange(t, []byte{protocolVersion, opcodeInfo}))
	if err != nil {
		t.Fatal(err)
	}
	if oracleInfo.revision != pinnedUpstreamCommit || oracleInfo.runtime != runtime.Version() {
		t.Fatalf("oracle INFO got revision=%q runtime=%q", oracleInfo.revision, oracleInfo.runtime)
	}
	var audit fullResultAudit
	for iteration := uint64(0); iteration < caseCount; iteration++ {
		serial := start + iteration
		if serial < start {
			t.Fatal("raw matrix serial overflow")
		}
		request := rawMatrixRequest(seed, serial)
		payload := matchRequestPayload(request.algorithm, request.scheme, request.flags,
			request.pattern, request.candidate)
		upstream, _, err := decodeMatchResponse(oraclePeer.exchange(t, payload))
		if err != nil {
			t.Fatalf("seed=%d serial=%d upstream error: %v", seed, serial, err)
		}
		native, _, err := decodeMatchResponse(nativePeer.exchange(t, payload))
		if err != nil {
			t.Fatalf("seed=%d serial=%d native error: %v", seed, serial, err)
		}
		if native.matched != upstream.matched {
			t.Fatalf("membership differs: %s",
				compactMatrixDifference(seed, serial, request, upstream, native))
		}
		if fullResults && !reflect.DeepEqual(native, upstream) {
			audit.add(seed, serial, request, upstream, native)
		}
	}
	if audit.differenceCount != 0 {
		t.Fatalf("full-result debt in %d of %d cases; first %d differences:\n%s\nreplay one case with FZF_RAW_MATRIX_SEED=%d FZF_RAW_MATRIX_START=SERIAL FZF_RAW_MATRIX_CASES=1 FZF_RAW_MATRIX_FULL_RESULTS=1",
			audit.differenceCount, caseCount, len(audit.examples),
			strings.Join(audit.examples, "\n"), seed)
	}
	t.Logf("raw matrix seed=%d start=%d cases=%d full-results=%t",
		seed, start, caseCount, fullResults)
}
