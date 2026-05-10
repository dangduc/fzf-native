export EMACS ?= $(shell which emacs)

BUILD_DIR ?= build

.PHONY: install
install:
	eask package
	eask install

.PHONY: compile
compile:
	eask compile

.PHONY: test
test:
	eask install-deps --dev
	eask test ert ./fzf-native-test.el

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
ctest:
	mkdir -p $(BUILD_DIR)
	$(CC) -std=c11 -D_POSIX_C_SOURCE=200809L -Wall -Wextra -O2 -I. -pthread \
		-o $(BUILD_DIR)/fzf-native-ctest fzf-native-ctest.c fzf.c
	$(BUILD_DIR)/fzf-native-ctest

.PHONY: clean
clean:
	rm -rf $(BUILD_DIR)
