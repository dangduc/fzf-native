# SPDX-License-Identifier: GPL-3.0-or-later

# Test-only fuzzing targets.  Nothing in this file is linked into the release
# module unless an explicit fuzz target is requested.

FUZZ_CC ?= $(or $(firstword $(wildcard /opt/homebrew/opt/llvm/bin/clang /usr/local/opt/llvm/bin/clang)),clang)
FUZZ_EMACS ?= $(or $(strip $(EMACS)),\
	$(shell command -v emacs 2>/dev/null),\
	$(firstword $(wildcard $(HOME)/emacs/nextstep/Emacs.app/Contents/MacOS/Emacs \
		/Applications/Emacs.app/Contents/MacOS/Emacs)))
FUZZ_SECONDS ?= 30
FZF_NATIVE_FUZZ_SEED ?= 12648430
FZF_NATIVE_FUZZ_ABI_CASES ?= 200
FZF_NATIVE_FUZZ_SESSION_CASES ?= 100
FUZZ_MAX_LEN ?= 4096
FUZZ_VERBOSITY ?= 0
FUZZ_RSS_LIMIT_MB ?= 2048
FUZZ_ASAN_OPTIONS ?= quarantine_size_mb=64:malloc_context_size=5
FUZZ_SEED_DIR ?= fuzz/corpus
FUZZ_DICTIONARY ?= fuzz/fzf-native.dict
FUZZ_CORPUS_DIR ?= $(BUILD_DIR)/fuzz-corpus
FUZZ_MERGED_CORPUS_DIR ?= $(BUILD_DIR)/fuzz-corpus-merged
FUZZ_ARTIFACT_DIR ?= $(BUILD_DIR)/fuzz-artifacts
FUZZ_BINARY := $(BUILD_DIR)/fzf-native-fuzz
FUZZ_REPLAY_BINARY := $(BUILD_DIR)/fzf-native-fuzz-replay
FUZZ_SESSION_SECONDS ?= $(FUZZ_SECONDS)
FUZZ_SESSION_READER_SECONDS ?= $(FUZZ_SESSION_SECONDS)
FUZZ_SESSION_READER_EPOCH_SECONDS ?= 1800
FUZZ_SESSION_READER_EPOCHS ?= 0
FUZZ_SESSION_MAX_LEN ?= 8192
FUZZ_SESSION_SEED_DIR ?= fuzz/session-corpus
FUZZ_SESSION_READER_SEED_DIR ?= $(FUZZ_SESSION_SEED_DIR)
FUZZ_SESSION_CORPUS_DIR ?= $(BUILD_DIR)/fuzz-session-corpus
FUZZ_SESSION_READER_CORPUS_DIR ?= $(BUILD_DIR)/fuzz-session-reader-corpus
FUZZ_SESSION_MERGED_CORPUS_DIR ?= $(BUILD_DIR)/fuzz-session-corpus-merged
FUZZ_SESSION_READER_MERGED_CORPUS_DIR ?= $(BUILD_DIR)/fuzz-session-reader-corpus-merged
FUZZ_SESSION_ARTIFACT_DIR ?= $(BUILD_DIR)/fuzz-session-artifacts
FUZZ_SESSION_READER_ARTIFACT_DIR ?= $(FUZZ_SESSION_ARTIFACT_DIR)/reader
FUZZ_SESSION_READER_LOCK_DIR ?= $(BUILD_DIR)/fuzz-session-reader-continuous.lock
FUZZ_SESSION_BINARY := $(BUILD_DIR)/fzf-native-session-fuzz
FUZZ_SESSION_READER_BINARY := $(BUILD_DIR)/fzf-native-session-reader-fuzz
FUZZ_SESSION_REPLAY_BINARY := $(BUILD_DIR)/fzf-native-session-fuzz-replay
FUZZ_SESSION_READER_REPLAY_BINARY := $(BUILD_DIR)/fzf-native-session-reader-fuzz-replay
FUZZ_SESSION_TSAN_BINARY := $(BUILD_DIR)/fzf-native-session-fuzz-tsan
FUZZ_SESSION_READER_TSAN_BINARY := $(BUILD_DIR)/fzf-native-session-reader-fuzz-tsan
FUZZ_MODULE_DIR := $(BUILD_DIR)/fuzz-module
FUZZ_MODULE := $(abspath $(FUZZ_MODULE_DIR)/fzf-native-module.so)
FZF_REFERENCE ?= fzf
FZF_REFERENCE_VERSION ?=
FZF_SOURCE ?=
FZF_NATIVE_UPSTREAM_CASES ?= 200
FZF_NATIVE_UPSTREAM_START ?= 0
FZF_NATIVE_UPSTREAM_PROFILE ?= common
FZF_NATIVE_REVISION ?= $(shell git rev-parse --verify HEAD 2>/dev/null)$(shell \
	test -z "$$(git status --porcelain --untracked-files=all 2>/dev/null)" || \
	printf '%s' '-dirty')
FUZZ_ALGO_DRIVER := $(BUILD_DIR)/fzf-native-algo-driver
FUZZ_ALGO_DRIVER_SAN := $(BUILD_DIR)/fzf-native-algo-driver-san
FUZZ_RAW_ORACLE := $(BUILD_DIR)/fzf-raw-oracle
FUZZ_GO_CACHE := $(abspath $(BUILD_DIR)/go-cache)

# The baseline matcher has no external runtime.  The stacked UTF-8 matcher
# vendors utf8proc, so discover and link that source when it is present.  This
# keeps the fuzz-infrastructure commit independently buildable while making
# the exact PR40+PR41 composition build without branch-specific Makefile edits.
FUZZ_UTF8PROC_SOURCE := $(firstword $(wildcard $(UTF8PROC_SRC)))
FUZZ_UTF8PROC_FLAGS := $(if $(FUZZ_UTF8PROC_SOURCE),-DUTF8PROC_STATIC,)

# The raw differential lane uses two persistent peers with one framed binary
# protocol.  FZF_SOURCE must point at the pinned source revision documented in
# fuzz/oracle/README.org.  The oracle build script rejects any other revision.
.PHONY: fuzz-algo-driver-build fuzz-algo-driver-san-build
fuzz-algo-driver-build:
	mkdir -p $(BUILD_DIR)
	$(FUZZ_CC) -std=gnu11 -Wall -Wextra -O2 -g -I. \
		$(FUZZ_UTF8PROC_FLAGS) \
		-DFZF_NATIVE_REVISION=\"$(FZF_NATIVE_REVISION)\" \
		-o $(FUZZ_ALGO_DRIVER) fuzz/fzf-native-algo-driver.c fzf.c \
		$(FUZZ_UTF8PROC_SOURCE)

fuzz-algo-driver-san-build:
	mkdir -p $(BUILD_DIR)
	$(FUZZ_CC) -std=gnu11 -Wall -Wextra -O1 -g -I. \
		-fsanitize=address,undefined -fno-sanitize-recover=undefined \
		-fno-omit-frame-pointer \
		$(FUZZ_UTF8PROC_FLAGS) \
		-DFZF_NATIVE_REVISION=\"$(FZF_NATIVE_REVISION)\" \
		-o $(FUZZ_ALGO_DRIVER_SAN) fuzz/fzf-native-algo-driver.c fzf.c \
		$(FUZZ_UTF8PROC_SOURCE)

.PHONY: fuzz-oracle-build fuzz-oracle-test fuzz-oracle-test-san
fuzz-oracle-build:
	test -n "$(FZF_SOURCE)"
	mkdir -p $(FUZZ_GO_CACHE)
	GOCACHE=$(FUZZ_GO_CACHE) ./fuzz/oracle/build.sh \
		"$(FZF_SOURCE)" "$(abspath $(FUZZ_RAW_ORACLE))"

fuzz-oracle-test: fuzz-algo-driver-build fuzz-oracle-build
	GOCACHE=$(FUZZ_GO_CACHE) \
		FZF_NATIVE_ALGO_DRIVER=$(abspath $(FUZZ_ALGO_DRIVER)) \
		FZF_RAW_ORACLE_BINARY=$(abspath $(FUZZ_RAW_ORACLE)) \
		FZF_NATIVE_EXPECTED_REVISION="$(FZF_NATIVE_REVISION)" \
		./fuzz/oracle/test.sh "$(FZF_SOURCE)"

.PHONY: fuzz-normalization-table-audit
fuzz-normalization-table-audit:
	test -n "$(FZF_SOURCE)"
	test "$$(git -C "$(FZF_SOURCE)" rev-parse HEAD)" = \
		"$(FUZZ_PINNED_FZF_REVISION)"
	git -C "$(FZF_SOURCE)" diff --quiet \
		"$(FUZZ_PINNED_FZF_REVISION)" -- src/algo/normalize.go
	mkdir -p $(FUZZ_GO_CACHE)
	cd fuzz/oracle && GOCACHE=$(FUZZ_GO_CACHE) go run ./tableaudit -- \
		"$(abspath $(FZF_SOURCE))/src/algo/normalize.go" \
		"$(abspath fzf-normalize.inc)"

fuzz-oracle-test-san: fuzz-algo-driver-san-build fuzz-oracle-build
	GOCACHE=$(FUZZ_GO_CACHE) \
		FZF_NATIVE_ALGO_DRIVER=$(abspath $(FUZZ_ALGO_DRIVER_SAN)) \
		FZF_RAW_ORACLE_BINARY=$(abspath $(FUZZ_RAW_ORACLE)) \
		FZF_NATIVE_EXPECTED_REVISION="$(FZF_NATIVE_REVISION)" \
		./fuzz/oracle/test.sh "$(FZF_SOURCE)"

.PHONY: fuzz-build
fuzz-build:
	mkdir -p $(BUILD_DIR)
	$(FUZZ_CC) -std=gnu11 -Wall -Wextra -O1 -g \
		-fsanitize=fuzzer,address,undefined -fno-sanitize-recover=undefined \
		-fno-omit-frame-pointer \
		-I. $(FUZZ_UTF8PROC_FLAGS) -o $(FUZZ_BINARY) \
		fuzz/fzf-native-fuzz.c fzf.c fzf-additions.c \
		$(FUZZ_UTF8PROC_SOURCE)

.PHONY: fuzz fuzz-matcher
fuzz: fuzz-matcher fuzz-session

fuzz-matcher: fuzz-build
	mkdir -p $(FUZZ_CORPUS_DIR) $(FUZZ_ARTIFACT_DIR)
	cp $(FUZZ_SEED_DIR)/* $(FUZZ_CORPUS_DIR)/
	$(FUZZ_BINARY) $(FUZZ_CORPUS_DIR) -max_len=$(FUZZ_MAX_LEN) \
		-dict=$(FUZZ_DICTIONARY) -verbosity=$(FUZZ_VERBOSITY) \
		-artifact_prefix=$(FUZZ_ARTIFACT_DIR)/ \
		-rss_limit_mb=$(FUZZ_RSS_LIMIT_MB) \
		-max_total_time=$(FUZZ_SECONDS) -print_final_stats=1

.PHONY: fuzz-replay-build
fuzz-replay-build:
	mkdir -p $(BUILD_DIR)
	$(FUZZ_CC) -std=gnu11 -Wall -Wextra -O1 -g \
		-DFZF_FUZZ_STANDALONE -fsanitize=address,undefined \
		-fno-sanitize-recover=undefined \
		-fno-omit-frame-pointer -I. $(FUZZ_UTF8PROC_FLAGS) \
		-o $(FUZZ_REPLAY_BINARY) fuzz/fzf-native-fuzz.c fzf.c \
		fzf-additions.c $(FUZZ_UTF8PROC_SOURCE)

.PHONY: fuzz-replay fuzz-matcher-replay
fuzz-replay: fuzz-matcher-replay fuzz-session-replay

fuzz-matcher-replay: fuzz-replay-build
	$(FUZZ_REPLAY_BINARY) $(FUZZ_SEED_DIR)/*

# Both targets include the real AsyncSession core.  The state bytecode reaches
# the scorer, worker pool, caches, request publication, and teardown at high
# throughput.  The separate reader target covers blocking producer I/O without
# making every state-machine input wait for a pipe reader.
.PHONY: fuzz-session-build fuzz-session-state-build fuzz-session-reader-build
fuzz-session-build: fuzz-session-state-build fuzz-session-reader-build

fuzz-session-state-build:
	mkdir -p $(BUILD_DIR)
	$(FUZZ_CC) -std=gnu11 -Wall -Wextra -O1 -g \
		-fsanitize=fuzzer,address,undefined -fno-sanitize-recover=undefined \
		-fno-omit-frame-pointer \
		-DFZF_SESSION_FUZZ_MODE=FZF_SESSION_FUZZ_MODE_STATE \
		-I. $(FUZZ_UTF8PROC_FLAGS) -pthread \
		-o $(FUZZ_SESSION_BINARY) fuzz/fzf-native-session-fuzz.c fzf.c \
		fzf-additions.c $(FUZZ_UTF8PROC_SOURCE)

fuzz-session-reader-build:
	mkdir -p $(BUILD_DIR)
	$(FUZZ_CC) -std=gnu11 -Wall -Wextra -O1 -g \
		-fsanitize=fuzzer,address,undefined -fno-sanitize-recover=undefined \
		-fno-omit-frame-pointer \
		-DFZF_SESSION_FUZZ_MODE=FZF_SESSION_FUZZ_MODE_READER \
		-I. $(FUZZ_UTF8PROC_FLAGS) -pthread \
		-o $(FUZZ_SESSION_READER_BINARY) fuzz/fzf-native-session-fuzz.c \
		fzf.c fzf-additions.c $(FUZZ_UTF8PROC_SOURCE)

.PHONY: fuzz-session fuzz-session-state fuzz-session-reader
fuzz-session: fuzz-session-state fuzz-session-reader

fuzz-session-state: fuzz-session-state-build
	mkdir -p $(FUZZ_SESSION_CORPUS_DIR) $(FUZZ_SESSION_ARTIFACT_DIR)
	cp $(FUZZ_SESSION_SEED_DIR)/* $(FUZZ_SESSION_CORPUS_DIR)/
	$(FUZZ_SESSION_BINARY) $(FUZZ_SESSION_CORPUS_DIR) \
		-max_len=$(FUZZ_SESSION_MAX_LEN) -dict=$(FUZZ_DICTIONARY) \
		-verbosity=$(FUZZ_VERBOSITY) \
		-artifact_prefix=$(FUZZ_SESSION_ARTIFACT_DIR)/ \
		-rss_limit_mb=$(FUZZ_RSS_LIMIT_MB) \
		-max_total_time=$(FUZZ_SESSION_SECONDS) -print_final_stats=1

fuzz-session-reader: fuzz-session-reader-build
	mkdir -p $(FUZZ_SESSION_READER_CORPUS_DIR) \
		$(FUZZ_SESSION_READER_ARTIFACT_DIR)
	cp $(FUZZ_SESSION_READER_SEED_DIR)/* $(FUZZ_SESSION_READER_CORPUS_DIR)/
	ASAN_OPTIONS=$(FUZZ_ASAN_OPTIONS) \
	$(FUZZ_SESSION_READER_BINARY) $(FUZZ_SESSION_READER_CORPUS_DIR) \
		-max_len=$(FUZZ_SESSION_MAX_LEN) -dict=$(FUZZ_DICTIONARY) \
		-verbosity=$(FUZZ_VERBOSITY) \
		-artifact_prefix=$(FUZZ_SESSION_READER_ARTIFACT_DIR)/ \
		-rss_limit_mb=$(FUZZ_RSS_LIMIT_MB) \
		-max_total_time=$(FUZZ_SESSION_READER_SECONDS) -print_final_stats=1

# Keep only inputs that add coverage for each independently instrumented
# target.  The learned corpus directories remain stable for CI caching.
.PHONY: fuzz-merge fuzz-matcher-merge fuzz-session-merge \
	fuzz-session-state-merge fuzz-session-reader-merge
fuzz-merge: fuzz-matcher-merge fuzz-session-merge

fuzz-matcher-merge: fuzz-build
	rm -rf $(FUZZ_MERGED_CORPUS_DIR)
	mkdir -p $(FUZZ_MERGED_CORPUS_DIR) $(FUZZ_CORPUS_DIR)
	$(FUZZ_BINARY) -merge=1 $(FUZZ_MERGED_CORPUS_DIR) \
		$(FUZZ_SEED_DIR) $(FUZZ_CORPUS_DIR) \
		-rss_limit_mb=$(FUZZ_RSS_LIMIT_MB) -verbosity=$(FUZZ_VERBOSITY)
	rm -rf $(FUZZ_CORPUS_DIR)
	mv $(FUZZ_MERGED_CORPUS_DIR) $(FUZZ_CORPUS_DIR)

fuzz-session-merge: fuzz-session-state-merge fuzz-session-reader-merge

fuzz-session-state-merge: fuzz-session-state-build
	rm -rf $(FUZZ_SESSION_MERGED_CORPUS_DIR)
	mkdir -p $(FUZZ_SESSION_MERGED_CORPUS_DIR) $(FUZZ_SESSION_CORPUS_DIR)
	$(FUZZ_SESSION_BINARY) -merge=1 $(FUZZ_SESSION_MERGED_CORPUS_DIR) \
		$(FUZZ_SESSION_SEED_DIR) $(FUZZ_SESSION_CORPUS_DIR) \
		-rss_limit_mb=$(FUZZ_RSS_LIMIT_MB) -verbosity=$(FUZZ_VERBOSITY)
	rm -rf $(FUZZ_SESSION_CORPUS_DIR)
	mv $(FUZZ_SESSION_MERGED_CORPUS_DIR) $(FUZZ_SESSION_CORPUS_DIR)

fuzz-session-reader-merge: fuzz-session-reader-build
	rm -rf $(FUZZ_SESSION_READER_MERGED_CORPUS_DIR)
	mkdir -p $(FUZZ_SESSION_READER_MERGED_CORPUS_DIR) \
		$(FUZZ_SESSION_READER_CORPUS_DIR)
	ASAN_OPTIONS=$(FUZZ_ASAN_OPTIONS) \
	$(FUZZ_SESSION_READER_BINARY) -merge=1 \
		$(FUZZ_SESSION_READER_MERGED_CORPUS_DIR) \
		$(FUZZ_SESSION_READER_SEED_DIR) $(FUZZ_SESSION_READER_CORPUS_DIR) \
		-max_len=$(FUZZ_SESSION_MAX_LEN) \
		-rss_limit_mb=$(FUZZ_RSS_LIMIT_MB) -verbosity=$(FUZZ_VERBOSITY)
	rm -rf $(FUZZ_SESSION_READER_CORPUS_DIR)
	mv $(FUZZ_SESSION_READER_MERGED_CORPUS_DIR) \
		$(FUZZ_SESSION_READER_CORPUS_DIR)

# A fresh process starts each reader epoch.  This releases libFuzzer and ASan
# allocator state while the coverage corpus stays available for the next epoch.
# Zero FUZZ_SESSION_READER_EPOCHS runs until a signal or a failure stops it.
.PHONY: fuzz-session-reader-continuous
fuzz-session-reader-continuous: fuzz-session-reader-build
	mkdir -p $(FUZZ_SESSION_READER_CORPUS_DIR) \
		$(FUZZ_SESSION_READER_ARTIFACT_DIR)
	@if ! mkdir $(FUZZ_SESSION_READER_LOCK_DIR) 2>/dev/null; then \
		echo "fzf-native: another reader campaign holds" \
			"$(FUZZ_SESSION_READER_LOCK_DIR)" >&2; \
		exit 2; \
	fi; \
	trap 'status=$$?; rmdir $(FUZZ_SESSION_READER_LOCK_DIR); exit $$status' EXIT; \
	trap 'exit 130' HUP INT TERM; \
	cp $(FUZZ_SESSION_READER_SEED_DIR)/* $(FUZZ_SESSION_READER_CORPUS_DIR)/; \
	epoch=0; set -e; \
	while test $(FUZZ_SESSION_READER_EPOCHS) -eq 0 || \
		test $$epoch -lt $(FUZZ_SESSION_READER_EPOCHS); do \
		ASAN_OPTIONS=$(FUZZ_ASAN_OPTIONS) \
		$(FUZZ_SESSION_READER_BINARY) $(FUZZ_SESSION_READER_CORPUS_DIR) \
			-max_len=$(FUZZ_SESSION_MAX_LEN) -dict=$(FUZZ_DICTIONARY) \
			-verbosity=$(FUZZ_VERBOSITY) \
			-artifact_prefix=$(FUZZ_SESSION_READER_ARTIFACT_DIR)/ \
			-rss_limit_mb=$(FUZZ_RSS_LIMIT_MB) \
			-max_total_time=$(FUZZ_SESSION_READER_EPOCH_SECONDS) \
			-print_final_stats=1; \
		rm -rf $(FUZZ_SESSION_READER_MERGED_CORPUS_DIR); \
		mkdir -p $(FUZZ_SESSION_READER_MERGED_CORPUS_DIR) \
			$(FUZZ_SESSION_READER_CORPUS_DIR); \
		ASAN_OPTIONS=$(FUZZ_ASAN_OPTIONS) \
		$(FUZZ_SESSION_READER_BINARY) -merge=1 \
			$(FUZZ_SESSION_READER_MERGED_CORPUS_DIR) \
			$(FUZZ_SESSION_READER_SEED_DIR) \
			$(FUZZ_SESSION_READER_CORPUS_DIR) \
			-max_len=$(FUZZ_SESSION_MAX_LEN) \
			-rss_limit_mb=$(FUZZ_RSS_LIMIT_MB) \
			-verbosity=$(FUZZ_VERBOSITY); \
		rm -rf $(FUZZ_SESSION_READER_CORPUS_DIR); \
		mv $(FUZZ_SESSION_READER_MERGED_CORPUS_DIR) \
			$(FUZZ_SESSION_READER_CORPUS_DIR); \
		epoch=$$((epoch + 1)); \
	done

.PHONY: fuzz-session-replay-build fuzz-session-state-replay-build \
	fuzz-session-reader-replay-build
fuzz-session-replay-build: fuzz-session-state-replay-build \
	fuzz-session-reader-replay-build

fuzz-session-state-replay-build:
	mkdir -p $(BUILD_DIR)
	$(FUZZ_CC) -std=gnu11 -Wall -Wextra -O1 -g \
		-DFZF_SESSION_FUZZ_STANDALONE=1 \
		-DFZF_SESSION_FUZZ_MODE=FZF_SESSION_FUZZ_MODE_STATE \
		-fsanitize=address,undefined -fno-sanitize-recover=undefined \
		-fno-omit-frame-pointer -I. $(FUZZ_UTF8PROC_FLAGS) -pthread \
		-o $(FUZZ_SESSION_REPLAY_BINARY) fuzz/fzf-native-session-fuzz.c \
		fzf.c fzf-additions.c $(FUZZ_UTF8PROC_SOURCE)

fuzz-session-reader-replay-build:
	mkdir -p $(BUILD_DIR)
	$(FUZZ_CC) -std=gnu11 -Wall -Wextra -O1 -g \
		-DFZF_SESSION_FUZZ_STANDALONE=1 \
		-DFZF_SESSION_FUZZ_MODE=FZF_SESSION_FUZZ_MODE_READER \
		-fsanitize=address,undefined -fno-sanitize-recover=undefined \
		-fno-omit-frame-pointer -I. $(FUZZ_UTF8PROC_FLAGS) -pthread \
		-o $(FUZZ_SESSION_READER_REPLAY_BINARY) \
		fuzz/fzf-native-session-fuzz.c fzf.c fzf-additions.c \
		$(FUZZ_UTF8PROC_SOURCE)

.PHONY: fuzz-session-replay fuzz-session-state-replay \
	fuzz-session-reader-replay
fuzz-session-replay: fuzz-session-state-replay fuzz-session-reader-replay

fuzz-session-state-replay: fuzz-session-state-replay-build
	$(FUZZ_SESSION_REPLAY_BINARY) $(FUZZ_SESSION_SEED_DIR)/*

fuzz-session-reader-replay: fuzz-session-reader-replay-build
	$(FUZZ_SESSION_READER_REPLAY_BINARY) $(FUZZ_SESSION_READER_SEED_DIR)/*

.PHONY: fuzz-session-tsan-build fuzz-session-state-tsan-build \
	fuzz-session-reader-tsan-build
fuzz-session-tsan-build: fuzz-session-state-tsan-build \
	fuzz-session-reader-tsan-build

fuzz-session-state-tsan-build:
	mkdir -p $(BUILD_DIR)
	$(FUZZ_CC) -std=gnu11 -Wall -Wextra -O1 -g \
		-DFZF_SESSION_FUZZ_STANDALONE=1 -DFZF_NATIVE_DEBUG=1 \
		-DFZF_SESSION_FUZZ_MODE=FZF_SESSION_FUZZ_MODE_STATE \
		-fsanitize=thread \
		-fno-omit-frame-pointer -I. $(FUZZ_UTF8PROC_FLAGS) -pthread \
		-o $(FUZZ_SESSION_TSAN_BINARY) fuzz/fzf-native-session-fuzz.c \
		fzf.c fzf-additions.c $(FUZZ_UTF8PROC_SOURCE)

fuzz-session-reader-tsan-build:
	mkdir -p $(BUILD_DIR)
	$(FUZZ_CC) -std=gnu11 -Wall -Wextra -O1 -g \
		-DFZF_SESSION_FUZZ_STANDALONE=1 -DFZF_NATIVE_DEBUG=1 \
		-DFZF_SESSION_FUZZ_MODE=FZF_SESSION_FUZZ_MODE_READER \
		-fsanitize=thread \
		-fno-omit-frame-pointer -I. $(FUZZ_UTF8PROC_FLAGS) -pthread \
		-o $(FUZZ_SESSION_READER_TSAN_BINARY) \
		fuzz/fzf-native-session-fuzz.c fzf.c fzf-additions.c \
		$(FUZZ_UTF8PROC_SOURCE)

.PHONY: fuzz-session-tsan fuzz-session-state-tsan fuzz-session-reader-tsan
fuzz-session-tsan: fuzz-session-state-tsan fuzz-session-reader-tsan

fuzz-session-state-tsan: fuzz-session-state-tsan-build
	TSAN_OPTIONS=halt_on_error=1:history_size=7 \
		$(FUZZ_SESSION_TSAN_BINARY) $(FUZZ_SESSION_SEED_DIR)/*

fuzz-session-reader-tsan: fuzz-session-reader-tsan-build
	TSAN_OPTIONS=halt_on_error=1:history_size=7 \
		$(FUZZ_SESSION_READER_TSAN_BINARY) $(FUZZ_SESSION_READER_SEED_DIR)/*

.PHONY: fuzz-module
fuzz-module:
	cmake -B $(BUILD_DIR)/fuzz-cmake -DCMAKE_BUILD_TYPE=RelWithDebInfo \
		-DFZF_NATIVE_MODULE_OUTPUT_DIR=$(FUZZ_MODULE_DIR)
	cmake --build $(BUILD_DIR)/fuzz-cmake

.PHONY: fuzz-elisp
fuzz-elisp: fuzz-module
	FZF_NATIVE_TEST_MODULE=$(FUZZ_MODULE) \
		FZF_NATIVE_FUZZ_SEED=$(FZF_NATIVE_FUZZ_SEED) \
		FZF_NATIVE_FUZZ_ABI_CASES=$(FZF_NATIVE_FUZZ_ABI_CASES) \
		FZF_NATIVE_FUZZ_SESSION_CASES=$(FZF_NATIVE_FUZZ_SESSION_CASES) \
		$(FUZZ_EMACS) -Q --batch -L . \
		--eval '(setq load-prefer-newer t)' \
		-l ./fuzz/fzf-native-fuzz-test.el \
		--eval '(ert-run-tests-batch-and-exit "^fzf-native-fuzz-")'

.PHONY: fuzz-upstream
fuzz-upstream: fuzz-module
	command -v $(FZF_REFERENCE)
	FZF_NATIVE_TEST_MODULE=$(FUZZ_MODULE) FZF_REFERENCE=$(FZF_REFERENCE) \
		FZF_REFERENCE_VERSION=$(FZF_REFERENCE_VERSION) \
		FZF_REFERENCE_REVISION=$(FZF_REFERENCE_REVISION) \
		FZF_NATIVE_FUZZ_SEED=$(FZF_NATIVE_FUZZ_SEED) \
		FZF_NATIVE_UPSTREAM_CASES=$(FZF_NATIVE_UPSTREAM_CASES) \
		FZF_NATIVE_UPSTREAM_START=$(FZF_NATIVE_UPSTREAM_START) \
		FZF_NATIVE_UPSTREAM_PROFILE=$(FZF_NATIVE_UPSTREAM_PROFILE) \
		$(FUZZ_EMACS) -Q --batch -L . \
		--eval '(setq load-prefer-newer t)' \
		-l ./fuzz/fzf-native-upstream-test.el \
		--eval '(ert-run-tests-batch-and-exit "^fzf-native-fuzz-upstream-")'

# Replay one persistent native session from any seed/serial boundary with:
# make fuzz-upstream-session FZF_NATIVE_FUZZ_SEED=N \
#   FZF_NATIVE_UPSTREAM_SESSION_START=N FZF_NATIVE_UPSTREAM_SESSION_CASES=1
FZF_NATIVE_UPSTREAM_SESSION_CASES ?= 4
FZF_NATIVE_UPSTREAM_SESSION_START ?= 0
FZF_REFERENCE_REVISION ?=
FUZZ_PINNED_FZF_REVISION := 1372d04f79bde0daa3bab4b96a068baafa808e67
FUZZ_PINNED_FZF := $(BUILD_DIR)/fzf-pinned

.PHONY: fuzz-pinned-fzf-build
fuzz-pinned-fzf-build:
	test -n "$(FZF_SOURCE)"
	mkdir -p $(BUILD_DIR) $(FUZZ_GO_CACHE)
	GOCACHE=$(FUZZ_GO_CACHE) ./fuzz/differential/build-pinned-fzf.sh \
		"$(FZF_SOURCE)" "$(abspath $(FUZZ_PINNED_FZF))"

.PHONY: fuzz-upstream-pinned
fuzz-upstream-pinned: fuzz-module fuzz-pinned-fzf-build
	FZF_NATIVE_TEST_MODULE=$(FUZZ_MODULE) \
		FZF_REFERENCE=$(abspath $(FUZZ_PINNED_FZF)) \
		FZF_REFERENCE_REVISION=$(FUZZ_PINNED_FZF_REVISION) \
		FZF_NATIVE_FUZZ_SEED=$(FZF_NATIVE_FUZZ_SEED) \
		FZF_NATIVE_UPSTREAM_CASES=$(FZF_NATIVE_UPSTREAM_CASES) \
		FZF_NATIVE_UPSTREAM_START=$(FZF_NATIVE_UPSTREAM_START) \
		FZF_NATIVE_UPSTREAM_PROFILE=$(FZF_NATIVE_UPSTREAM_PROFILE) \
		FZF_NATIVE_UPSTREAM_SESSION_CASES=$(FZF_NATIVE_UPSTREAM_SESSION_CASES) \
		FZF_NATIVE_UPSTREAM_SESSION_START=$(FZF_NATIVE_UPSTREAM_SESSION_START) \
		$(FUZZ_EMACS) -Q --batch -L . \
		--eval '(setq load-prefer-newer t)' \
		-l ./fuzz/fzf-native-upstream-test.el \
		--eval '(ert-run-tests-batch-and-exit "^fzf-native-fuzz-upstream-")'

.PHONY: fuzz-upstream-session
fuzz-upstream-session: fuzz-module
	command -v $(FZF_REFERENCE)
	FZF_NATIVE_TEST_MODULE=$(FUZZ_MODULE) FZF_REFERENCE=$(FZF_REFERENCE) \
		FZF_REFERENCE_VERSION=$(FZF_REFERENCE_VERSION) \
		FZF_REFERENCE_REVISION=$(FZF_REFERENCE_REVISION) \
		FZF_NATIVE_FUZZ_SEED=$(FZF_NATIVE_FUZZ_SEED) \
		FZF_NATIVE_UPSTREAM_SESSION_CASES=$(FZF_NATIVE_UPSTREAM_SESSION_CASES) \
		FZF_NATIVE_UPSTREAM_SESSION_START=$(FZF_NATIVE_UPSTREAM_SESSION_START) \
		$(FUZZ_EMACS) -Q --batch -L . \
		--eval '(setq load-prefer-newer t)' \
		-l ./fuzz/fzf-native-upstream-test.el \
		--eval '(ert-run-tests-batch-and-exit "fzf-native-fuzz-upstream-session-rounds")'

.PHONY: fuzz-upstream-session-pinned
fuzz-upstream-session-pinned: fuzz-module fuzz-pinned-fzf-build
	FZF_NATIVE_TEST_MODULE=$(FUZZ_MODULE) \
		FZF_REFERENCE=$(abspath $(FUZZ_PINNED_FZF)) \
		FZF_REFERENCE_REVISION=$(FUZZ_PINNED_FZF_REVISION) \
		FZF_NATIVE_FUZZ_SEED=$(FZF_NATIVE_FUZZ_SEED) \
		FZF_NATIVE_UPSTREAM_SESSION_CASES=$(FZF_NATIVE_UPSTREAM_SESSION_CASES) \
		FZF_NATIVE_UPSTREAM_SESSION_START=$(FZF_NATIVE_UPSTREAM_SESSION_START) \
		$(FUZZ_EMACS) -Q --batch -L . \
		--eval '(setq load-prefer-newer t)' \
		-l ./fuzz/fzf-native-upstream-test.el \
		--eval '(ert-run-tests-batch-and-exit "fzf-native-fuzz-upstream-session-rounds")'
