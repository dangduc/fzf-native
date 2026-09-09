# SPDX-License-Identifier: GPL-3.0-or-later

export EMACS ?= emacs

BUILD_DIR ?= build
UNAME_S := $(shell uname -s)

ifeq ($(UNAME_S),Darwin)
ASAN_PRELOAD_VAR := DYLD_INSERT_LIBRARIES
ASAN_RUNTIME := $(shell $(CC) -print-file-name=libclang_rt.asan_osx_dynamic.dylib)
else
ASAN_PRELOAD_VAR := LD_PRELOAD
ASAN_RUNTIME := $(shell $(CC) -print-file-name=libasan.so)
endif

# Vendored utf8proc, linked into the C tests because fzf.c's UTF-8 matching
# variants (via utf8_char_index.h -> utf8proc.h) depend on it.
UTF8PROC_DIR ?= utf8proc-2.10.0
UTF8PROC_SRC := $(UTF8PROC_DIR)/utf8proc.c
PYTHON ?= python3

PACKAGE := fzf-native
AUTOLOADS := $(PACKAGE)-autoloads.el

# Sandbox for `make lint` — package-lint is fetched from MELPA into this
# directory the first time it runs, so linting does not touch the user's
# global ~/.emacs.d/elpa.
LINT_SANDBOX := .lint-sandbox

.PHONY: autoloads
autoloads:
	$(EMACS) -Q --batch \
	  --eval "(loaddefs-generate default-directory \"$(AUTOLOADS)\" nil \"(add-to-list 'load-path (or (and load-file-name (file-name-directory load-file-name)) (car load-path)))\n\")"

.PHONY: compile
compile: autoloads
	$(EMACS) -Q --batch -L . -f batch-byte-compile \
	  fzf-native.el fzf-native-module-init-probe.el

.PHONY: test
test:
	$(EMACS) -Q --batch -L . \
	  -l ert -l fzf-native-test.el -l fzf-native-utf8-test.el \
	  -f ert-run-tests-batch-and-exit

.PHONY: lint
lint:
	$(EMACS) -Q --batch \
	  --eval "(setq package-user-dir (expand-file-name \"$(LINT_SANDBOX)/elpa\"))" \
	  --eval "(setq network-security-level 'low)" \
	  --eval "(require 'package)" \
	  --eval "(add-to-list 'package-archives '(\"melpa\" . \"https://melpa.org/packages/\") t)" \
	  --eval "(package-initialize)" \
	  --eval "(unless (package-installed-p 'package-lint) (package-refresh-contents) (package-install 'package-lint))" \
	  --eval "(require 'package-lint)" \
	  -f package-lint-batch-and-exit fzf-native.el

.PHONY: format
format:
	cmake-format --in-place CMakeLists.txt

# --- Native module build targets ---

# Standard release build (RelWithDebInfo, same as the cmake default).
# Logging is compiled out unless FZF_NATIVE_DEBUG=1 is set in the env.
.PHONY: build
build:
	cmake -B $(BUILD_DIR) -DCMAKE_BUILD_TYPE=RelWithDebInfo
	cmake --build $(BUILD_DIR)

# Release build with file logging compiled in. Logs to ~/.emacs.d/fzf-native.log,
# truncated on each module load. Cleans first so CMake re-reads the env var.
.PHONY: build-log
build-log:
	rm -rf $(BUILD_DIR)
	FZF_NATIVE_DEBUG=1 cmake -B $(BUILD_DIR) -DCMAKE_BUILD_TYPE=RelWithDebInfo
	cmake --build $(BUILD_DIR)

# Full debug build: symbols, no optimization, accurate stack frames
.PHONY: build-debug
build-debug:
	cmake -B $(BUILD_DIR) -DCMAKE_BUILD_TYPE=Debug
	cmake --build $(BUILD_DIR)

# Debug + AddressSanitizer: catches segfaults, heap/stack overflows,
# use-after-free. Run emacs via: make emacs-asan
.PHONY: build-asan
build-asan:
	cmake -B $(BUILD_DIR) -DCMAKE_BUILD_TYPE=Debug -DENABLE_ASAN=ON
	cmake --build $(BUILD_DIR)

# Debug + UndefinedBehaviorSanitizer: catches null derefs, signed overflow, etc.
.PHONY: build-ubsan
build-ubsan:
	cmake -B $(BUILD_DIR) -DCMAKE_BUILD_TYPE=Debug -DENABLE_UBSAN=ON
	cmake --build $(BUILD_DIR)

# Debug + both sanitizers at once
.PHONY: build-san
build-san:
	cmake -B $(BUILD_DIR) -DCMAKE_BUILD_TYPE=Debug -DENABLE_ASAN=ON -DENABLE_UBSAN=ON
	cmake --build $(BUILD_DIR)

# Launch Emacs with the ASan runtime preloaded (required when the .so was built
# with -fsanitize=address). Requires build-asan to have been run first.
.PHONY: emacs-asan
emacs-asan:
	$(ASAN_PRELOAD_VAR)=$(ASAN_RUNTIME) $(EMACS)

# C-level unit tests for module internals (counting_sort_candidates, etc.).
# Includes fzf-native-module.c directly so static functions are visible.
# No Emacs runtime needed; runs as a plain executable.
.PHONY: ctest
ctest: ctest-module ctest-additions ctest-parser-oom ctest-scorer-oom \
	ctest-session-growth-benchmark ctest-session-trace-benchmark \
	ctest-core-hotpath-oracle ctest-simd-prefilter ctest-fzf-bench-driver

# Verify the standalone fzf --bench-equivalent driver with multiple workers.
# A small chunk size gives the fixture independently scheduled chunks; the
# production benchmark keeps fzf's 1024-item chunk size.
.PHONY: ctest-fzf-bench-driver
ctest-fzf-bench-driver:
	mkdir -p $(BUILD_DIR)
	$(CC) -std=gnu11 -Wall -Wextra -O2 -DFZF_BENCH_CHUNK_SIZE=1 \
		-I. -I$(UTF8PROC_DIR) -pthread \
		-o $(BUILD_DIR)/fzf-bench-driver-unit-test \
		benchmarks/fzf-bench-driver-test.c fzf.c $(UTF8PROC_SRC)
	$(BUILD_DIR)/fzf-bench-driver-unit-test
	$(CC) -std=gnu11 -Wall -Wextra -O2 -DFZF_BENCH_CHUNK_SIZE=3 \
		-I. -I$(UTF8PROC_DIR) -pthread \
		-o $(BUILD_DIR)/fzf-bench-driver-ctest \
		benchmarks/fzf-bench-driver.c fzf.c $(UTF8PROC_SRC)
	$(BUILD_DIR)/fzf-bench-driver-ctest --filter=abc --literal \
		--algo=v2 --tiebreak=index --threads=1 --check \
		< benchmarks/fzf-bench-fixture.txt \
		> $(BUILD_DIR)/fzf-bench-driver-w1.txt
	$(BUILD_DIR)/fzf-bench-driver-ctest --filter=abc --literal \
		--algo=v2 --tiebreak=index --threads=3 --check \
		< benchmarks/fzf-bench-fixture.txt \
		> $(BUILD_DIR)/fzf-bench-driver-w3.txt
	cmp $(BUILD_DIR)/fzf-bench-driver-w1.txt \
		$(BUILD_DIR)/fzf-bench-driver-w3.txt
	grep -Fx 'semantic items=12 matches=9 input_checksum=18c3f63929c5b7fa result_checksum=59c7cb863237a38b' \
		$(BUILD_DIR)/fzf-bench-driver-w1.txt
	$(BUILD_DIR)/fzf-bench-driver-ctest --filter=abc --literal \
		--algo=v2 --tiebreak=index --threads=1 --no-sort --check \
		< benchmarks/fzf-bench-fixture.txt \
		> $(BUILD_DIR)/fzf-bench-driver-nosort-w1.txt
	$(BUILD_DIR)/fzf-bench-driver-ctest --filter=abc --literal \
		--algo=v2 --tiebreak=index --threads=3 --no-sort --check \
		< benchmarks/fzf-bench-fixture.txt \
		> $(BUILD_DIR)/fzf-bench-driver-nosort-w3.txt
	cmp $(BUILD_DIR)/fzf-bench-driver-nosort-w1.txt \
		$(BUILD_DIR)/fzf-bench-driver-nosort-w3.txt
	grep -Fx 'semantic items=12 matches=9 input_checksum=18c3f63929c5b7fa result_checksum=57088aed5d299e8b' \
		$(BUILD_DIR)/fzf-bench-driver-nosort-w1.txt
	$(BUILD_DIR)/fzf-bench-driver-ctest --filter=abc --literal \
		--algo=v2 --tiebreak=index --threads=1 --dump-results \
		< benchmarks/fzf-bench-fixture.txt \
		> $(BUILD_DIR)/fzf-bench-driver-dump-w1.txt
	$(BUILD_DIR)/fzf-bench-driver-ctest --filter=abc --literal \
		--algo=v2 --tiebreak=index --threads=3 --dump-results \
		< benchmarks/fzf-bench-fixture.txt \
		> $(BUILD_DIR)/fzf-bench-driver-dump-w3.txt
	cmp $(BUILD_DIR)/fzf-bench-driver-dump-w1.txt \
		$(BUILD_DIR)/fzf-bench-driver-dump-w3.txt
	test "$$(wc -l < $(BUILD_DIR)/fzf-bench-driver-dump-w1.txt)" -eq 9
	! $(BUILD_DIR)/fzf-bench-driver-ctest --filter=abc --literal \
		--algo=v2 --tiebreak=index --threads=1 \
		--bench=18446744073709551616ns < /dev/null

# Compare the adapter's ordered identities and rank inputs with the exact
# upstream implementation.  The caller must supply a local, pinned fzf tree;
# the checker disables module and toolchain downloads.
.PHONY: ctest-fzf-upstream-semantic
ctest-fzf-upstream-semantic:
	test -n "$(FZF_SOURCE)"
	mkdir -p $(BUILD_DIR)
	$(CC) -std=gnu11 -Wall -Wextra -O2 \
		-I. -I$(UTF8PROC_DIR) -pthread \
		-o $(BUILD_DIR)/fzf-bench-driver-semantic \
		benchmarks/fzf-bench-driver.c fzf.c $(UTF8PROC_SRC)
	$(PYTHON) benchmarks/check-fzf-semantic-parity.py \
		--fzf-source "$(FZF_SOURCE)" \
		--native-driver $(BUILD_DIR)/fzf-bench-driver-semantic

# Prove that the core-hotpath benchmark validates exact per-item scores before
# it starts timing.  The injected scorer fault must be rejected by the pinned
# fingerprints rather than producing a plausible-looking timing result.
.PHONY: ctest-core-hotpath-oracle
ctest-core-hotpath-oracle:
	mkdir -p $(BUILD_DIR)
	$(CC) -std=gnu11 -Wall -Wextra -O2 \
		-I. -I$(UTF8PROC_DIR) \
		-o $(BUILD_DIR)/core-hotpath-oracle \
		benchmarks/core-hotpath-probe.c fzf.c $(UTF8PROC_SRC)
	$(BUILD_DIR)/core-hotpath-oracle
	$(CC) -std=gnu11 -Wall -Wextra -O2 \
		-DFZF_CORE_HOTPATH_FAULT_SCORE \
		-I. -I$(UTF8PROC_DIR) \
		-o $(BUILD_DIR)/core-hotpath-oracle-fault \
		benchmarks/core-hotpath-probe.c fzf.c $(UTF8PROC_SRC)
	@set +e; \
		$(BUILD_DIR)/core-hotpath-oracle-fault \
			>$(BUILD_DIR)/core-hotpath-oracle-fault.log 2>&1; \
		rc=$$?; \
		if [ $$rc -ne 1 ] || \
		   ! grep -q "core-hotpath fingerprint mismatch" \
			$(BUILD_DIR)/core-hotpath-oracle-fault.log; then \
			cat $(BUILD_DIR)/core-hotpath-oracle-fault.log; \
			echo "core-hotpath benchmark oracle did not reject score fault"; \
			exit 1; \
		fi; \
		cat $(BUILD_DIR)/core-hotpath-oracle-fault.log; \
		echo "core-hotpath benchmark oracle rejected score fault"

# Module-internal tests (counting sort, cache, async_reader, etc.).
# Links fzf-additions.c because fzf-native-module.c now references
# fzf_has_match in the scoring thread's filter-only path.
.PHONY: ctest-module
ctest-module:
	mkdir -p $(BUILD_DIR)
	$(CC) -std=gnu11 -Wall -Wextra -O2 -I. -I$(UTF8PROC_DIR) -pthread \
		-o $(BUILD_DIR)/fzf-native-ctest fzf-native-ctest.c fzf.c fzf-additions.c $(UTF8PROC_SRC)
	$(BUILD_DIR)/fzf-native-ctest

# fzf-additions tests (fzf_has_match agreement with fzf_get_score).
# Linked against fzf.c + fzf-additions.c + utf8proc — pure-C, no module deps.
.PHONY: ctest-additions
ctest-additions:
	mkdir -p $(BUILD_DIR)
	$(CC) -std=gnu11 -Wall -Wextra -O2 -I. -I$(UTF8PROC_DIR) \
		-o $(BUILD_DIR)/fzf-additions-test fzf-additions-test.c fzf.c fzf-additions.c $(UTF8PROC_SRC)
	$(BUILD_DIR)/fzf-additions-test
	$(CC) -std=gnu11 -Wall -Wextra -O2 -DFZF_TEST_WINDOWS_PATH_SCORING -I. -I$(UTF8PROC_DIR) \
		-o $(BUILD_DIR)/fzf-additions-test-windows-paths fzf-additions-test.c fzf.c fzf-additions.c $(UTF8PROC_SRC)
	$(BUILD_DIR)/fzf-additions-test-windows-paths

# Direct scalar/SIMD parity and exact-sized-tail coverage for the private
# prefilters, plus parser fallback at the query-plan size limit.
.PHONY: ctest-simd-prefilter
ctest-simd-prefilter:
	mkdir -p $(BUILD_DIR)
	$(CC) -std=gnu11 -Wall -Wextra -O2 -I. -I$(UTF8PROC_DIR) \
		-o $(BUILD_DIR)/fzf-simd-prefilter-test \
		fzf-simd-prefilter-test.c fzf.c fzf-additions.c $(UTF8PROC_SRC)
	$(BUILD_DIR)/fzf-simd-prefilter-test

# Allocation-failure injection for every parser allocation.  fzf.c is
# included by the test so malloc/calloc/realloc can be replaced locally.
.PHONY: ctest-parser-oom
ctest-parser-oom:
	mkdir -p $(BUILD_DIR)
	$(CC) -std=gnu11 -Wall -Wextra -O2 -I. -I$(UTF8PROC_DIR) \
		-o $(BUILD_DIR)/fzf-parser-oom-ctest fzf-parser-oom-ctest.c fzf-additions.c $(UTF8PROC_SRC)
	$(BUILD_DIR)/fzf-parser-oom-ctest

# Allocation-failure injection for scoring scratch and position arrays.
.PHONY: ctest-scorer-oom
ctest-scorer-oom:
	mkdir -p $(BUILD_DIR)
	$(CC) -std=gnu11 -Wall -Wextra -O2 -I. -I$(UTF8PROC_DIR) \
		-o $(BUILD_DIR)/fzf-scorer-oom-ctest fzf-scorer-oom-ctest.c $(UTF8PROC_SRC)
	$(BUILD_DIR)/fzf-scorer-oom-ctest

# Keep the benchmark's full-scan oracle honest with candidates whose producer
# order differs from their final fzf rank order.
.PHONY: ctest-session-growth-benchmark
ctest-session-growth-benchmark:
	mkdir -p $(BUILD_DIR)
	$(CC) -std=gnu11 -Wall -Wextra -O2 -I. -I$(UTF8PROC_DIR) -pthread \
		-o $(BUILD_DIR)/session-growth-benchmark-ctest \
		etc/session-growth-benchmark-test.c fzf.c fzf-additions.c $(UTF8PROC_SRC)
	$(BUILD_DIR)/session-growth-benchmark-ctest

# Exercise the benchmark's complete edit trace with its full-scan/qsort oracle
# enabled.  Three timed samples keep this check short while still executing the
# median-reporting path used by the full benchmark.
.PHONY: ctest-session-trace-benchmark
ctest-session-trace-benchmark:
	mkdir -p $(BUILD_DIR)
	$(CC) -std=gnu11 -Wall -Wextra -O2 -I. -I$(UTF8PROC_DIR) -pthread \
		-o $(BUILD_DIR)/session-trace-probe-ctest \
		benchmarks/session-trace-probe.c fzf.c fzf-additions.c $(UTF8PROC_SRC)
	$(BUILD_DIR)/session-trace-probe-ctest 4096 2 64 3

# AddressSanitizer + UndefinedBehaviorSanitizer run of the C unit tests.
# Builds both suites with the sanitizers enabled into distinctly-named
# binaries (-asan suffix) so they never clobber the plain `ctest` ones,
# then runs them. A clean run prints the normal test output and exits 0.
# ASan aborts on error by default; UBSAN_OPTIONS=halt_on_error=1 makes a
# UBSan diagnostic abort too, so any finding fails the target (and CI).
.PHONY: ctest-asan
ctest-asan: export UBSAN_OPTIONS = halt_on_error=1:print_stacktrace=1
ctest-asan:
	mkdir -p $(BUILD_DIR)
	$(CC) -std=gnu11 -Wall -Wextra -fsanitize=address,undefined -fno-sanitize-recover=undefined -fno-omit-frame-pointer -g \
		-DFZF_BENCH_CHUNK_SIZE=1 -I. -I$(UTF8PROC_DIR) -pthread \
		-o $(BUILD_DIR)/fzf-bench-driver-unit-test-asan \
		benchmarks/fzf-bench-driver-test.c fzf.c $(UTF8PROC_SRC)
	$(BUILD_DIR)/fzf-bench-driver-unit-test-asan
	$(CC) -std=gnu11 -Wall -Wextra -fsanitize=address,undefined -fno-sanitize-recover=undefined -fno-omit-frame-pointer -g \
		-I. -I$(UTF8PROC_DIR) -pthread \
		-o $(BUILD_DIR)/fzf-native-ctest-asan fzf-native-ctest.c fzf.c fzf-additions.c $(UTF8PROC_SRC)
	$(BUILD_DIR)/fzf-native-ctest-asan
	$(CC) -std=gnu11 -Wall -Wextra -fsanitize=address,undefined -fno-sanitize-recover=undefined -fno-omit-frame-pointer -g \
		-I. -I$(UTF8PROC_DIR) -pthread \
		-o $(BUILD_DIR)/fzf-additions-test-asan fzf-additions-test.c fzf.c fzf-additions.c $(UTF8PROC_SRC)
	$(BUILD_DIR)/fzf-additions-test-asan
	$(CC) -std=gnu11 -Wall -Wextra -fsanitize=address,undefined -fno-sanitize-recover=undefined -fno-omit-frame-pointer -g \
		-I. -I$(UTF8PROC_DIR) \
		-o $(BUILD_DIR)/fzf-simd-prefilter-test-asan \
		fzf-simd-prefilter-test.c fzf.c fzf-additions.c $(UTF8PROC_SRC)
	$(BUILD_DIR)/fzf-simd-prefilter-test-asan
	$(CC) -std=gnu11 -Wall -Wextra -fsanitize=address,undefined -fno-sanitize-recover=undefined -fno-omit-frame-pointer -g \
		-DFZF_TEST_WINDOWS_PATH_SCORING -I. -I$(UTF8PROC_DIR) -pthread \
		-o $(BUILD_DIR)/fzf-additions-test-windows-paths-asan fzf-additions-test.c fzf.c fzf-additions.c $(UTF8PROC_SRC)
	$(BUILD_DIR)/fzf-additions-test-windows-paths-asan
	$(CC) -std=gnu11 -Wall -Wextra -fsanitize=address,undefined -fno-sanitize-recover=undefined -fno-omit-frame-pointer -g \
		-I. -I$(UTF8PROC_DIR) \
		-o $(BUILD_DIR)/fzf-parser-oom-ctest-asan fzf-parser-oom-ctest.c fzf-additions.c $(UTF8PROC_SRC)
	$(BUILD_DIR)/fzf-parser-oom-ctest-asan
	$(CC) -std=gnu11 -Wall -Wextra -fsanitize=address,undefined -fno-sanitize-recover=undefined -fno-omit-frame-pointer -g \
		-I. -I$(UTF8PROC_DIR) \
		-o $(BUILD_DIR)/fzf-scorer-oom-ctest-asan fzf-scorer-oom-ctest.c $(UTF8PROC_SRC)
	$(BUILD_DIR)/fzf-scorer-oom-ctest-asan
	$(CC) -std=gnu11 -Wall -Wextra -fsanitize=address,undefined -fno-sanitize-recover=undefined -fno-omit-frame-pointer -g \
		-I. -I$(UTF8PROC_DIR) -pthread \
		-o $(BUILD_DIR)/session-growth-benchmark-ctest-asan \
		etc/session-growth-benchmark-test.c fzf.c fzf-additions.c $(UTF8PROC_SRC)
	$(BUILD_DIR)/session-growth-benchmark-ctest-asan

.PHONY: clean
clean:
	rm -rf $(BUILD_DIR)

# Long-session scale probe for the stable-batch query index.  This separate
# output directory does not replace a bundled release artifact.
.PHONY: benchmark-batch-cache-history
benchmark-batch-cache-history:
	cmake -B $(BUILD_DIR)/bench-cmake -DCMAKE_BUILD_TYPE=RelWithDebInfo \
		-DFZF_NATIVE_MODULE_OUTPUT_DIR=$(abspath $(BUILD_DIR)/bench-module)
	cmake --build $(BUILD_DIR)/bench-cmake
	FZF_NATIVE_SOURCE_DIR=$(CURDIR) \
		FZF_NATIVE_TEST_MODULE=$(abspath $(BUILD_DIR)/bench-module/fzf-native-module.so) \
		$(EMACS) -Q --batch -l etc/batch-cache-query-history-benchmark.el

# Short, deterministic A/B probe for the legacy score+positions pair versus
# the combined one-pass API.  Increase the two arguments for steadier local
# measurements; defaults intentionally stay suitable for developer loops.
BENCH_SCORE_POSITIONS_ROUNDS ?= 4000
BENCH_SCORE_POSITIONS_SAMPLES ?= 9
.PHONY: benchmark-score-positions
benchmark-score-positions:
	mkdir -p $(BUILD_DIR)
	$(CC) -std=gnu11 -O3 -DNDEBUG -I. -I$(UTF8PROC_DIR) \
		-o $(BUILD_DIR)/score-positions-benchmark \
		etc/score-positions-benchmark.c fzf.c $(UTF8PROC_SRC)
	$(BUILD_DIR)/score-positions-benchmark \
		$(BENCH_SCORE_POSITIONS_ROUNDS) $(BENCH_SCORE_POSITIONS_SAMPLES)

# Short synthetic probe for the core matcher paths used by the Chromium,
# Arabic, and Korean holdouts, plus first-byte ASCII hits and UTF-8 inputs that
# exceed the v2 slab.  This isolates scoring and prints provisional timings;
# it is not a replacement for the real-data benchmark.
.PHONY: benchmark-core-hotpath-probe
benchmark-core-hotpath-probe:
	mkdir -p $(BUILD_DIR)
	$(CC) -std=gnu11 -O3 -DNDEBUG -I. -I$(UTF8PROC_DIR) \
		-o $(BUILD_DIR)/core-hotpath-probe \
		benchmarks/core-hotpath-probe.c fzf.c $(UTF8PROC_SRC)
	$(BUILD_DIR)/core-hotpath-probe

# Synthetic scorer-entry probe.  It keeps the candidate bytes and expected
# scores fixed, then separates C-string length discovery, bounded ASCII
# classification, and the already-classified matcher path.  Use it to keep
# adapter/setup work distinct from scorer changes.
.PHONY: benchmark-api-overhead-probe
benchmark-api-overhead-probe:
	mkdir -p $(BUILD_DIR)
	$(CC) -std=gnu11 -O3 -DNDEBUG -I. -I$(UTF8PROC_DIR) \
		-o $(BUILD_DIR)/api-overhead-probe \
		benchmarks/api-overhead-probe.c fzf.c $(UTF8PROC_SRC)
	$(BUILD_DIR)/api-overhead-probe

# Rejection-focused ASCII scorer probe. It reports preclassified, bounded,
# and public C-string API lanes separately and includes dense-match, short,
# case-sensitive, and repeated-byte counterweights.
.PHONY: benchmark-rejection-hotpath-probe
benchmark-rejection-hotpath-probe:
	mkdir -p $(BUILD_DIR)
	$(CC) -std=gnu11 -O3 -DNDEBUG -I. -I$(UTF8PROC_DIR) \
		-o $(BUILD_DIR)/rejection-hotpath-probe \
		benchmarks/rejection-hotpath-probe.c fzf.c $(UTF8PROC_SRC)
	$(BUILD_DIR)/rejection-hotpath-probe

# Native counterpart of junegunn/fzf's filter-mode --bench loop.  Ingestion,
# pattern parsing, and the final semantic checksum are outside the timer.  Each
# timed scan includes full scoring, worker-local result collection and sorting,
# and only match-count materialization, matching fzf's lazy-merger boundary.
# FZF_BENCH_SORT=--no-sort selects a native-only diagnostic.  fzf filter mode
# forces sorting for sortable patterns, even with its --no-sort option.
FZF_BENCH_INPUT ?= /dev/stdin
FZF_BENCH_QUERY ?= linux
FZF_BENCH_DURATION ?= 1s
FZF_BENCH_THREADS ?= 1
FZF_BENCH_SORT ?= --sort
FZF_BENCH_DRIVER := $(BUILD_DIR)/fzf-bench-driver

.PHONY: benchmark-fzf-bench-build benchmark-fzf-bench
benchmark-fzf-bench-build:
	mkdir -p $(BUILD_DIR)
	$(CC) -std=gnu11 -Wall -Wextra -O3 -DNDEBUG \
		-I. -I$(UTF8PROC_DIR) -pthread \
		-o $(FZF_BENCH_DRIVER) benchmarks/fzf-bench-driver.c \
		fzf.c $(UTF8PROC_SRC)

benchmark-fzf-bench: benchmark-fzf-bench-build
	$(FZF_BENCH_DRIVER) --filter='$(FZF_BENCH_QUERY)' \
		--tiebreak=index --bench='$(FZF_BENCH_DURATION)' \
		--threads='$(FZF_BENCH_THREADS)' $(FZF_BENCH_SORT) \
		--algo=v2 --literal \
		< '$(FZF_BENCH_INPUT)'

# Real persistent-session growth probe.  Timings include producer appends,
# growth notification, coordinator work, shared workers, cache update, and
# result publication.  Full-scan validation runs after all timed rounds.
SESSION_GROWTH_INITIAL ?= 1000000
SESSION_GROWTH_DELTA ?= 1000
# Exercise enough growth epochs to include bounded-chain flatten tail latency.
SESSION_GROWTH_ROUNDS ?= 40
SESSION_GROWTH_WORKERS ?= 8
SESSION_GROWTH_LIMIT ?= 10000
SESSION_GROWTH_BENCH := $(BUILD_DIR)/session-growth-benchmark

.PHONY: benchmark-session-growth-build benchmark-session-growth
benchmark-session-growth-build:
	mkdir -p $(BUILD_DIR)
	$(CC) -std=gnu11 -Wall -Wextra -O3 -DNDEBUG \
		-I. -I$(UTF8PROC_DIR) -pthread \
		-o $(SESSION_GROWTH_BENCH) etc/session-growth-benchmark.c \
		fzf.c fzf-additions.c $(UTF8PROC_SRC)

benchmark-session-growth: benchmark-session-growth-build
	$(SESSION_GROWTH_BENCH) \
		$(SESSION_GROWTH_INITIAL) $(SESSION_GROWTH_DELTA) \
		$(SESSION_GROWTH_ROUNDS) $(SESSION_GROWTH_WORKERS) \
		$(SESSION_GROWTH_LIMIT)

# Persistent interactive edit trace.  The executable validates every result
# against an independent full scan before it starts timing, then checks the
# timed results against those validated fingerprints.
SESSION_TRACE_CANDIDATES ?= 100000
SESSION_TRACE_WORKERS ?= 8
SESSION_TRACE_LIMIT ?= 256
SESSION_TRACE_SAMPLES ?= 9
SESSION_TRACE_BENCH := $(BUILD_DIR)/session-trace-probe

.PHONY: benchmark-session-trace-build benchmark-session-trace
benchmark-session-trace-build:
	mkdir -p $(BUILD_DIR)
	$(CC) -std=gnu11 -Wall -Wextra -O3 -DNDEBUG \
		-I. -I$(UTF8PROC_DIR) -pthread \
		-o $(SESSION_TRACE_BENCH) benchmarks/session-trace-probe.c \
		fzf.c fzf-additions.c $(UTF8PROC_SRC)

benchmark-session-trace: benchmark-session-trace-build
	$(SESSION_TRACE_BENCH) \
		$(SESSION_TRACE_CANDIDATES) $(SESSION_TRACE_WORKERS) \
		$(SESSION_TRACE_LIMIT) $(SESSION_TRACE_SAMPLES)

# Cold first-query probe.  Invoke the executable once per sample so every run
# starts with a fresh process-wide worker pool.  Corpus construction and an
# independent full-scan/qsort oracle are outside the measured interval.
FIRST_QUERY_CANDIDATES ?= 300000
FIRST_QUERY_LIMIT ?= 256
FIRST_QUERY_QUERY ?= omega
FIRST_QUERY_BENCH := $(BUILD_DIR)/first-query-probe

.PHONY: benchmark-first-query-build benchmark-first-query
benchmark-first-query-build:
	mkdir -p $(BUILD_DIR)
	$(CC) -std=gnu11 -Wall -Wextra -O3 -DNDEBUG \
		-I. -I$(UTF8PROC_DIR) -pthread \
		-o $(FIRST_QUERY_BENCH) benchmarks/first-query-probe.c \
		fzf.c fzf-additions.c $(UTF8PROC_SRC)

benchmark-first-query: benchmark-first-query-build
	$(FIRST_QUERY_BENCH) \
		$(FIRST_QUERY_CANDIDATES) $(FIRST_QUERY_LIMIT) \
		$(FIRST_QUERY_QUERY)

# Coverage-guided and differential test targets live in a separate include so
# they do not alter the release build or the public module ABI.
include fuzz/fuzz.mk
