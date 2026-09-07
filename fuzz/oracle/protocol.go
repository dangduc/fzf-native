// SPDX-License-Identifier: GPL-3.0-or-later

package main

import (
	"bufio"
	"encoding/binary"
	"errors"
	"fmt"
	"io"
	"runtime"
)

const (
	protocolVersion byte = 1

	opcodeInfo  byte = 0
	opcodeMatch byte = 1

	statusOK          byte = 0
	statusBadRequest  byte = 1
	statusUnsupported byte = 2
	statusInternal    byte = 3

	maxFrameSize    = 64 * 1024 * 1024
	matchHeaderSize = 13
)

type server struct {
	oracle *rawOracle
}

func readFrame(reader io.Reader) ([]byte, error) {
	var header [4]byte
	_, err := io.ReadFull(reader, header[:])
	if err != nil {
		return nil, err
	}
	length := binary.BigEndian.Uint32(header[:])
	if length > maxFrameSize {
		return nil, fmt.Errorf("frame length %d exceeds limit %d", length, maxFrameSize)
	}
	payload := make([]byte, int(length))
	if _, err := io.ReadFull(reader, payload); err != nil {
		return nil, err
	}
	return payload, nil
}

func writeFrame(writer io.Writer, payload []byte) error {
	if len(payload) > maxFrameSize {
		return fmt.Errorf("response length %d exceeds limit %d", len(payload), maxFrameSize)
	}
	var header [4]byte
	binary.BigEndian.PutUint32(header[:], uint32(len(payload)))
	if _, err := writer.Write(header[:]); err != nil {
		return err
	}
	_, err := writer.Write(payload)
	return err
}

func appendUint32(buffer []byte, value uint32) []byte {
	var encoded [4]byte
	binary.BigEndian.PutUint32(encoded[:], value)
	return append(buffer, encoded[:]...)
}

func appendInt64(buffer []byte, value int64) []byte {
	var encoded [8]byte
	binary.BigEndian.PutUint64(encoded[:], uint64(value))
	return append(buffer, encoded[:]...)
}

func appendBytes(buffer []byte, value []byte) []byte {
	buffer = appendUint32(buffer, uint32(len(value)))
	return append(buffer, value...)
}

func errorPayload(opcode, status byte, err error) []byte {
	payload := []byte{protocolVersion, opcode, status}
	return appendBytes(payload, []byte(err.Error()))
}

func infoPayload() []byte {
	payload := []byte{protocolVersion, opcodeInfo, statusOK}
	payload = appendBytes(payload, []byte(pinnedUpstreamCommit))
	return appendBytes(payload, []byte(runtime.Version()))
}

func matchPayload(response matchResponse) []byte {
	matched := byte(0)
	if response.matched {
		matched = 1
	}
	positionsPresent := byte(0)
	if response.positionsPresent {
		positionsPresent = 1
	}
	payload := []byte{protocolVersion, opcodeMatch, statusOK, matched, positionsPresent}
	payload = appendInt64(payload, response.start)
	payload = appendInt64(payload, response.end)
	payload = appendInt64(payload, response.score)
	payload = appendUint32(payload, uint32(len(response.positions)))
	for _, position := range response.positions {
		payload = appendInt64(payload, position)
	}
	return payload
}

func decodeMatchRequest(payload []byte) (matchRequest, error) {
	if len(payload) < matchHeaderSize {
		return matchRequest{}, fmt.Errorf("match payload has %d bytes; need at least %d", len(payload), matchHeaderSize)
	}
	patternLength := binary.BigEndian.Uint32(payload[5:9])
	candidateLength := binary.BigEndian.Uint32(payload[9:13])
	want := uint64(matchHeaderSize) + uint64(patternLength) + uint64(candidateLength)
	if want != uint64(len(payload)) {
		return matchRequest{}, fmt.Errorf("match payload has %d bytes; lengths require %d", len(payload), want)
	}
	patternEnd := matchHeaderSize + int(patternLength)
	return matchRequest{
		algorithm: algorithmID(payload[2]),
		scheme:    schemeID(payload[3]),
		flags:     payload[4],
		pattern:   payload[matchHeaderSize:patternEnd],
		candidate: payload[patternEnd:],
	}, nil
}

func (s *server) handle(payload []byte) []byte {
	opcode := byte(0xff)
	if len(payload) >= 2 {
		opcode = payload[1]
	}
	if len(payload) < 2 {
		return errorPayload(opcode, statusBadRequest, errors.New("request needs a version and opcode"))
	}
	if payload[0] != protocolVersion {
		return errorPayload(opcode, statusBadRequest, fmt.Errorf("unsupported protocol version %d", payload[0]))
	}

	switch opcode {
	case opcodeInfo:
		if len(payload) != 2 {
			return errorPayload(opcode, statusBadRequest, errors.New("INFO request must contain two bytes"))
		}
		return infoPayload()
	case opcodeMatch:
		request, err := decodeMatchRequest(payload)
		if err != nil {
			return errorPayload(opcode, statusBadRequest, err)
		}
		response, err := s.oracle.match(request)
		if err != nil {
			return errorPayload(opcode, statusBadRequest, err)
		}
		return matchPayload(response)
	default:
		return errorPayload(opcode, statusBadRequest, fmt.Errorf("unknown opcode %d", opcode))
	}
}

func (s *server) serve(input io.Reader, output io.Writer) error {
	reader := bufio.NewReader(input)
	writer := bufio.NewWriter(output)
	for {
		payload, err := readFrame(reader)
		if errors.Is(err, io.EOF) {
			return writer.Flush()
		}
		if err != nil {
			return err
		}
		if err := writeFrame(writer, s.handle(payload)); err != nil {
			return err
		}
		if err := writer.Flush(); err != nil {
			return err
		}
	}
}
