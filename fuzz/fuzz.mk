# SPDX-License-Identifier: GPL-3.0-or-later

# Test-only fuzzing targets.  Nothing in this file is linked into the release
# module unless an explicit fuzz target is requested.

FUZZ_CC ?= $(or $(firstword $(wildcard /opt/homebrew/opt/llvm/bin/clang /usr/local/opt/llvm/bin/clang)),clang)
FUZZ_EMACS ?= $(or $(strip $(EMACS)),\
	$(shell command -v emacs 2>/dev/null),\
	$(firstword $(wildcard $(HOME)/emacs/nextstep/Emacs.app/Contents/MacOS/Emacs \
		/Applications/Emacs.app/Contents/MacOS/Emacs)))
FUZZ_SECONDS ?= 30
FUZZ_MAX_LEN ?= 4096
FUZZ_VERBOSITY ?= 0
FUZZ_RSS_LIMIT_MB ?= 2048
FUZZ_SEED_DIR ?= fuzz/corpus
FUZZ_DICTIONARY ?= fuzz/fzf-native.dict
FUZZ_CORPUS_DIR ?= $(BUILD_DIR)/fuzz-corpus
FUZZ_ARTIFACT_DIR ?= $(BUILD_DIR)/fuzz-artifacts
FUZZ_BINARY := $(BUILD_DIR)/fzf-native-fuzz
FUZZ_REPLAY_BINARY := $(BUILD_DIR)/fzf-native-fuzz-replay
FUZZ_SESSION_SECONDS ?= $(FUZZ_SECONDS)
FUZZ_SESSION_MAX_LEN ?= 8192
FUZZ_SESSION_SEED_DIR ?= fuzz/session-corpus
FUZZ_SESSION_CORPUS_DIR ?= $(BUILD_DIR)/fuzz-session-corpus
FUZZ_SESSION_ARTIFACT_DIR ?= $(BUILD_DIR)/fuzz-session-artifacts
FUZZ_SESSION_BINARY := $(BUILD_DIR)/fzf-native-session-fuzz
FUZZ_SESSION_REPLAY_BINARY := $(BUILD_DIR)/fzf-native-session-fuzz-replay
FUZZ_SESSION_TSAN_BINARY := $(BUILD_DIR)/fzf-native-session-fuzz-tsan
FUZZ_MODULE_DIR := $(BUILD_DIR)/fuzz-module
FUZZ_MODULE := $(abspath $(FUZZ_MODULE_DIR)/fzf-native-module.so)
FZF_REFERENCE ?= fzf
FZF_REFERENCE_VERSION ?=

# The baseline matcher has no external runtime.  The stacked UTF-8 matcher
# vendors utf8proc, so discover and link that source when it is present.  This
# keeps the fuzz-infrastructure commit independently buildable while making
# the exact PR40+PR41 composition build without branch-specific Makefile edits.
FUZZ_UTF8PROC_SOURCE := $(firstword $(wildcard utf8proc-*/utf8proc.c))
FUZZ_UTF8PROC_FLAGS := $(if $(FUZZ_UTF8PROC_SOURCE),-DUTF8PROC_STATIC,)

.PHONY: fuzz-build
fuzz-build:
	mkdir -p $(BUILD_DIR)
	$(FUZZ_CC) -std=gnu11 -Wall -Wextra -O1 -g \
		-fsanitize=fuzzer,address,undefined -fno-omit-frame-pointer \
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
		-fno-omit-frame-pointer -I. $(FUZZ_UTF8PROC_FLAGS) \
		-o $(FUZZ_REPLAY_BINARY) fuzz/fzf-native-fuzz.c fzf.c \
		fzf-additions.c $(FUZZ_UTF8PROC_SOURCE)

.PHONY: fuzz-replay fuzz-matcher-replay
fuzz-replay: fuzz-matcher-replay fuzz-session-replay

fuzz-matcher-replay: fuzz-replay-build
	$(FUZZ_REPLAY_BINARY) $(FUZZ_SEED_DIR)/*

# This target includes the real AsyncSession core.  Its bytecode reaches the
# reader, scorer, worker pool, caches, request publication, and teardown.
.PHONY: fuzz-session-build
fuzz-session-build:
	mkdir -p $(BUILD_DIR)
	$(FUZZ_CC) -std=gnu11 -Wall -Wextra -O1 -g \
		-fsanitize=fuzzer,address,undefined -fno-omit-frame-pointer \
		-I. $(FUZZ_UTF8PROC_FLAGS) -pthread \
		-o $(FUZZ_SESSION_BINARY) fuzz/fzf-native-session-fuzz.c fzf.c \
		fzf-additions.c $(FUZZ_UTF8PROC_SOURCE)

.PHONY: fuzz-session
fuzz-session: fuzz-session-build
	mkdir -p $(FUZZ_SESSION_CORPUS_DIR) $(FUZZ_SESSION_ARTIFACT_DIR)
	cp $(FUZZ_SESSION_SEED_DIR)/* $(FUZZ_SESSION_CORPUS_DIR)/
	$(FUZZ_SESSION_BINARY) $(FUZZ_SESSION_CORPUS_DIR) \
		-max_len=$(FUZZ_SESSION_MAX_LEN) -dict=$(FUZZ_DICTIONARY) \
		-verbosity=$(FUZZ_VERBOSITY) \
		-artifact_prefix=$(FUZZ_SESSION_ARTIFACT_DIR)/ \
		-rss_limit_mb=$(FUZZ_RSS_LIMIT_MB) \
		-max_total_time=$(FUZZ_SESSION_SECONDS) -print_final_stats=1

.PHONY: fuzz-session-replay-build
fuzz-session-replay-build:
	mkdir -p $(BUILD_DIR)
	$(FUZZ_CC) -std=gnu11 -Wall -Wextra -O1 -g \
		-DFZF_SESSION_FUZZ_STANDALONE=1 -fsanitize=address,undefined \
		-fno-omit-frame-pointer -I. $(FUZZ_UTF8PROC_FLAGS) -pthread \
		-o $(FUZZ_SESSION_REPLAY_BINARY) fuzz/fzf-native-session-fuzz.c \
		fzf.c fzf-additions.c $(FUZZ_UTF8PROC_SOURCE)

.PHONY: fuzz-session-replay
fuzz-session-replay: fuzz-session-replay-build
	$(FUZZ_SESSION_REPLAY_BINARY) $(FUZZ_SESSION_SEED_DIR)/*

.PHONY: fuzz-session-tsan-build
fuzz-session-tsan-build:
	mkdir -p $(BUILD_DIR)
	$(FUZZ_CC) -std=gnu11 -Wall -Wextra -O1 -g \
		-DFZF_SESSION_FUZZ_STANDALONE=1 -DFZF_NATIVE_DEBUG=1 \
		-fsanitize=thread \
		-fno-omit-frame-pointer -I. $(FUZZ_UTF8PROC_FLAGS) -pthread \
		-o $(FUZZ_SESSION_TSAN_BINARY) fuzz/fzf-native-session-fuzz.c \
		fzf.c fzf-additions.c $(FUZZ_UTF8PROC_SOURCE)

.PHONY: fuzz-session-tsan
fuzz-session-tsan: fuzz-session-tsan-build
	TSAN_OPTIONS=halt_on_error=1:history_size=7 \
		$(FUZZ_SESSION_TSAN_BINARY) $(FUZZ_SESSION_SEED_DIR)/*

.PHONY: fuzz-module
fuzz-module:
	cmake -B $(BUILD_DIR)/fuzz-cmake -DCMAKE_BUILD_TYPE=RelWithDebInfo \
		-DFZF_NATIVE_MODULE_OUTPUT_DIR=$(FUZZ_MODULE_DIR)
	cmake --build $(BUILD_DIR)/fuzz-cmake

.PHONY: fuzz-elisp
fuzz-elisp: fuzz-module
	FZF_NATIVE_TEST_MODULE=$(FUZZ_MODULE) \
		$(FUZZ_EMACS) -Q --batch -L . -l ./fuzz/fzf-native-fuzz-test.el \
		--eval '(ert-run-tests-batch-and-exit "^fzf-native-fuzz-")'

.PHONY: fuzz-upstream
fuzz-upstream: fuzz-module
	command -v $(FZF_REFERENCE)
	FZF_NATIVE_TEST_MODULE=$(FUZZ_MODULE) FZF_REFERENCE=$(FZF_REFERENCE) \
		FZF_REFERENCE_VERSION=$(FZF_REFERENCE_VERSION) \
		$(FUZZ_EMACS) -Q --batch -L . -l ./fuzz/fzf-native-upstream-test.el \
		--eval '(ert-run-tests-batch-and-exit "^fzf-native-fuzz-upstream-")'
