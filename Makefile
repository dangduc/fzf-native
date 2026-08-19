export EMACS ?= $(shell which emacs)

BUILD_DIR ?= build

# Coverage-guided matcher fuzzing.  FUZZ_CC must support libFuzzer (Clang on
# macOS and Linux does).  Override FUZZ_SECONDS for longer local/nightly runs.
# Apple system Clang does not ship the libFuzzer runtime.  Prefer Homebrew
# LLVM when present, while retaining ordinary Clang on Linux.
FUZZ_CC ?= $(or $(firstword $(wildcard /opt/homebrew/opt/llvm/bin/clang /usr/local/opt/llvm/bin/clang)),clang)
FUZZ_SECONDS ?= 30
FUZZ_MAX_LEN ?= 4096
FUZZ_VERBOSITY ?= 0
FUZZ_SEED_DIR ?= fuzz/corpus
FUZZ_DICTIONARY ?= fuzz/fzf-native.dict
FUZZ_CORPUS_DIR ?= $(BUILD_DIR)/fuzz-corpus
FUZZ_ARTIFACT_DIR ?= $(BUILD_DIR)/fuzz-artifacts
FUZZ_BINARY := $(BUILD_DIR)/fzf-native-fuzz
FUZZ_REPLAY_BINARY := $(BUILD_DIR)/fzf-native-fuzz-replay
FZF_REFERENCE ?= fzf
FZF_REFERENCE_VERSION ?=

# Vendored utf8proc, linked into the C tests because fzf.c's UTF-8 matching
# variants (via utf8_char_index.h -> utf8proc.h) depend on it.
UTF8PROC_DIR ?= utf8proc-2.10.0
UTF8PROC_LIB := $(UTF8PROC_DIR)/libutf8proc.a

PACKAGE := fzf-native
AUTOLOADS := $(PACKAGE)-autoloads.el

.PHONY: install
install:
	eask package
	eask install

.PHONY: autoloads
autoloads:
	$(EMACS) -Q --batch \
	  --eval "(loaddefs-generate default-directory \"$(AUTOLOADS)\" nil \"(add-to-list 'load-path (or (and load-file-name (file-name-directory load-file-name)) (car load-path)))\n\")"

.PHONY: compile
compile: autoloads
	eask compile

.PHONY: test
test:
	eask install-deps --dev
	eask test ert ./fzf-native-test.el ./fzf-native-utf8-test.el

.PHONY: lint
lint:
	eask lint package

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
	LD_PRELOAD=$$($(CC) -print-file-name=libasan.so) $(EMACS)

# C-level unit tests for module internals (counting_sort_candidates, etc.).
# Includes fzf-native-module.c directly so static functions are visible.
# No Emacs runtime needed; runs as a plain executable.
.PHONY: ctest
ctest: ctest-module ctest-additions

# Build the vendored utf8proc static lib (fzf.c links against it).
$(UTF8PROC_LIB):
	$(MAKE) -C $(UTF8PROC_DIR) all

# Module-internal tests (counting sort, cache, async_reader, etc.).
# Links fzf-additions.c because fzf-native-module.c now references
# fzf_has_match in the scoring thread's filter-only path.
.PHONY: ctest-module
ctest-module: $(UTF8PROC_LIB)
	mkdir -p $(BUILD_DIR)
	$(CC) -std=gnu11 -Wall -Wextra -O2 -I. -I$(UTF8PROC_DIR) -pthread \
		-o $(BUILD_DIR)/fzf-native-ctest fzf-native-ctest.c fzf.c fzf-additions.c $(UTF8PROC_LIB)
	$(BUILD_DIR)/fzf-native-ctest

# fzf-additions tests (fzf_has_match agreement with fzf_get_score).
# Linked against fzf.c + fzf-additions.c + utf8proc — pure-C, no module deps.
.PHONY: ctest-additions
ctest-additions: $(UTF8PROC_LIB)
	mkdir -p $(BUILD_DIR)
	$(CC) -std=gnu11 -Wall -Wextra -O2 -I. -I$(UTF8PROC_DIR) \
		-o $(BUILD_DIR)/fzf-additions-test fzf-additions-test.c fzf.c fzf-additions.c $(UTF8PROC_LIB)
	$(BUILD_DIR)/fzf-additions-test

# AddressSanitizer + UndefinedBehaviorSanitizer run of the C unit tests.
# Builds both suites with the sanitizers enabled into distinctly-named
# binaries (-asan suffix) so they never clobber the plain `ctest` ones,
# then runs them. A clean run prints the normal test output and exits 0.
# ASan aborts on error by default; UBSAN_OPTIONS=halt_on_error=1 makes a
# UBSan diagnostic abort too, so any finding fails the target (and CI).
.PHONY: ctest-asan
ctest-asan: export UBSAN_OPTIONS = halt_on_error=1:print_stacktrace=1
ctest-asan: $(UTF8PROC_LIB)
	mkdir -p $(BUILD_DIR)
	$(CC) -std=gnu11 -Wall -Wextra -fsanitize=address,undefined -fno-omit-frame-pointer -g \
		-I. -I$(UTF8PROC_DIR) -pthread \
		-o $(BUILD_DIR)/fzf-native-ctest-asan fzf-native-ctest.c fzf.c fzf-additions.c $(UTF8PROC_LIB)
	$(BUILD_DIR)/fzf-native-ctest-asan
	$(CC) -std=gnu11 -Wall -Wextra -fsanitize=address,undefined -fno-omit-frame-pointer -g \
		-I. -I$(UTF8PROC_DIR) -pthread \
		-o $(BUILD_DIR)/fzf-additions-test-asan fzf-additions-test.c fzf.c fzf-additions.c $(UTF8PROC_LIB)
	$(BUILD_DIR)/fzf-additions-test-asan

# The core matcher fuzzer uses differential properties rather than fixed
# expected scores.  libFuzzer supplies coverage-guided inputs; ASan/UBSan turn
# memory-safety and undefined-behavior findings into reproducible crashes.
.PHONY: fuzz-build
fuzz-build: $(UTF8PROC_LIB)
	mkdir -p $(BUILD_DIR)
	$(FUZZ_CC) -std=gnu11 -Wall -Wextra -O1 -g \
		-fsanitize=fuzzer,address,undefined -fno-omit-frame-pointer \
		-I. -I$(UTF8PROC_DIR) -o $(FUZZ_BINARY) \
		fuzz/fzf-native-fuzz.c fzf.c fzf-additions.c $(UTF8PROC_LIB)

.PHONY: fuzz
fuzz: fuzz-build
	mkdir -p $(FUZZ_CORPUS_DIR) $(FUZZ_ARTIFACT_DIR)
	cp $(FUZZ_SEED_DIR)/* $(FUZZ_CORPUS_DIR)/
	$(FUZZ_BINARY) $(FUZZ_CORPUS_DIR) -max_len=$(FUZZ_MAX_LEN) \
		-dict=$(FUZZ_DICTIONARY) -verbosity=$(FUZZ_VERBOSITY) \
		-artifact_prefix=$(FUZZ_ARTIFACT_DIR)/ \
		-max_total_time=$(FUZZ_SECONDS) -print_final_stats=1

# Deterministic replay is useful in pre-commit checks and works without the
# libFuzzer runtime.  Every minimized finding should be added to fuzz/corpus.
.PHONY: fuzz-replay-build
fuzz-replay-build: $(UTF8PROC_LIB)
	mkdir -p $(BUILD_DIR)
	$(FUZZ_CC) -std=gnu11 -Wall -Wextra -O1 -g \
		-DFZF_FUZZ_STANDALONE -fsanitize=address,undefined \
		-fno-omit-frame-pointer -I. -I$(UTF8PROC_DIR) \
		-o $(FUZZ_REPLAY_BINARY) fuzz/fzf-native-fuzz.c fzf.c \
		fzf-additions.c $(UTF8PROC_LIB)

.PHONY: fuzz-replay
fuzz-replay: fuzz-replay-build
	$(FUZZ_REPLAY_BINARY) $(FUZZ_SEED_DIR)/*

# Randomized properties at the Emacs module boundary.  Override the seed and
# case counts in the environment to reproduce or deepen a run.
.PHONY: fuzz-elisp
fuzz-elisp:
	$(EMACS) -Q --batch -L . -l ./fuzz/fzf-native-fuzz-test.el \
		--eval '(ert-run-tests-batch-and-exit "^fzf-native-fuzz-")'

# Optional semantic differential against the upstream CLI.  CI pins the exact
# reference release; local runs may set FZF_REFERENCE and
# FZF_REFERENCE_VERSION to exercise the same binary.
.PHONY: fuzz-upstream
fuzz-upstream:
	command -v $(FZF_REFERENCE)
	FZF_REFERENCE=$(FZF_REFERENCE) FZF_REFERENCE_VERSION=$(FZF_REFERENCE_VERSION) \
		$(EMACS) -Q --batch -L . -l ./fuzz/fzf-native-upstream-test.el \
		--eval '(ert-run-tests-batch-and-exit "^fzf-native-fuzz-upstream-")'

# ThreadSanitizer cannot be combined with AddressSanitizer.  This target runs
# the module-internal async/cache/reader tests under TSan as a separate lane.
.PHONY: ctest-tsan
ctest-tsan: $(UTF8PROC_LIB)
	mkdir -p $(BUILD_DIR)
	$(FUZZ_CC) -std=gnu11 -Wall -Wextra -O1 -g -fsanitize=thread \
		-fno-omit-frame-pointer -I. -I$(UTF8PROC_DIR) -pthread \
		-o $(BUILD_DIR)/fzf-native-ctest-tsan fzf-native-ctest.c fzf.c \
		fzf-additions.c $(UTF8PROC_LIB)
	$(BUILD_DIR)/fzf-native-ctest-tsan

.PHONY: clean
clean:
	rm -rf $(BUILD_DIR)
