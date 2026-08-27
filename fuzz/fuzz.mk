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
FUZZ_MODULE_DIR := $(BUILD_DIR)/fuzz-module
FUZZ_MODULE := $(abspath $(FUZZ_MODULE_DIR)/fzf-native-module.so)
FZF_REFERENCE ?= fzf
FZF_REFERENCE_VERSION ?=

.PHONY: fuzz-build
fuzz-build:
	mkdir -p $(BUILD_DIR)
	$(FUZZ_CC) -std=gnu11 -Wall -Wextra -O1 -g \
		-fsanitize=fuzzer,address,undefined -fno-omit-frame-pointer \
		-I. -o $(FUZZ_BINARY) fuzz/fzf-native-fuzz.c fzf.c fzf-additions.c

.PHONY: fuzz
fuzz: fuzz-build
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
		-fno-omit-frame-pointer -I. -o $(FUZZ_REPLAY_BINARY) \
		fuzz/fzf-native-fuzz.c fzf.c fzf-additions.c

.PHONY: fuzz-replay
fuzz-replay: fuzz-replay-build
	$(FUZZ_REPLAY_BINARY) $(FUZZ_SEED_DIR)/*

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
