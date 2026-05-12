;;; fzf-native.el --- Fuzzy completion style  -*- lexical-binding: t; -*-

;; Copyright 2021 Duc Dang
;; Author: Duc Dang <me@dangduc.com>
;; Assisted-by: Claude:claude-opus-4-7
;; Version: 0.3
;; Package-Requires: ((emacs "29.1"))
;; Keywords: matching
;; Homepage: https://github.com/dangduc/fzf-native
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; This is a package that provides fuzzy match scoring
;; based on the fzf algorithm by junegunn.

(require 'cl-lib)

;;; Code:

(defgroup fzf-native nil
  "Fuzzy completion style."
  :group 'minibuffer
  :link '(url-link :tag "GitHub" "https://github.com/dangduc/fzf-native"))

(declare-function fzf-native-score-all "fzf-native-module" (collection query &optional slab))
(declare-function fzf-native-highlight-all "fzf-native-module" (collection query))
(declare-function fzf-native-score "fzf-native-module" (str query &optional slab))
(declare-function fzf-native-make-default-slab "fzf-native-module" ())
(declare-function fzf-native-make-slab "fzf-native-module" (size16 size32))
(declare-function fzf-native-async-start "fzf-native-module" (command &optional dir))
(declare-function fzf-native-async-stop "fzf-native-module" (handle))
(declare-function fzf-native-async-generation "fzf-native-module" (handle))
(declare-function fzf-native-async-candidates "fzf-native-module" (handle filter &optional limit))
(declare-function fzf-native-async-stats "fzf-native-module" (handle))

(defconst fzf-native--dyn-name "fzf-native-module"
  "Dynamic module name.")

(defconst fzf-native--bin-dir
  (concat (file-name-directory load-file-name) "bin/")
  "Pre-built binaries directory path.")

(defvar fzf-native-module-install-buffer-name " *Install fzf-native-module* "
  "Name of the buffer used for compiling fzf-native-module.")

(defcustom fzf-native-module-cmake-args
  "-DCMAKE_C_FLAGS='-O3'"
  "Arguments given to CMake to compile fzf-native-module."
  :type 'string
  :group 'fzf-native)

(defcustom fzf-native-always-compile-module nil
  "If not nil, if `fzf-native-module' is not found, compile it without asking.

When `fzf-native-always-compile-module' is nil, fzf-native will ask for
confirmation before compiling."
  :type  'boolean
  :group 'fzf-native)

;; Canonical knobs the C module reads via `symbol-value' at call time.
;; Higher-level packages (fzf-async, fussy) keep their own user-facing
;; defcustoms and bridge their values onto these names — fussy via
;; `setq-local' (synchronous, same-buffer call pattern), fzf-async via
;; `:around' advice on the C entry points (timer-driven, cross-buffer).

(defcustom fzf-native-case-mode 'smart
  "How fzf-native treats letter case when matching queries.
smart    Case-insensitive when the query is all lowercase; case-sensitive
         once it contains any uppercase character (fzf's default).
ignore   Always case-insensitive.
respect  Always case-sensitive.

Read on every scoring call; changes take effect immediately."
  :type '(choice (const :tag "Smart case (default)" smart)
                 (const :tag "Ignore case"          ignore)
                 (const :tag "Respect case"         respect))
  :group 'fzf-native)

(defcustom fzf-native-batch-highlight 25
  "Highlight cap for the synchronous (batch) scoring path.
Read by `fzf-native-score' / `fzf-native-score-all' on every call.
nil disables highlighting; a positive integer caps the number of
top-scoring candidates that get `completions-common-part' face
applied via `fzf_get_positions' inside the C module.

Bridged by fussy from `fussy-fzf-native-highlight' via `setq-local'."
  :type '(choice (const   :tag "Disabled" nil)
                 (integer :tag "Top N candidates"))
  :group 'fzf-native)

(defcustom fzf-native-async-highlight 200
  "Highlight cap for the streaming (async) candidate path.
Read by `fzf-native-async-candidates' on every call.  Same semantics
as `fzf-native-batch-highlight' (nil / positive integer).

Bridged by fzf-async from `fzf-async-highlight' via `:around' advice."
  :type '(choice (const   :tag "Disabled" nil)
                 (const   :tag "All candidates" t)
                 (integer :tag "Top N candidates"))
  :group 'fzf-native)

(defcustom fzf-native-max-line-length 256
  "Per-line character cap applied by the async reader thread.
nil        — no limit.
positive N — exclude lines longer than N characters.
negative -N — include but truncate lines to N characters.

Read once at session start by `fzf-native-async-start'.

Bridged by fzf-async from `fzf-async-max-line-length' via `:around'
advice; the read happens inside `fzf-native-async-start' so the
advice is in scope for the `symbol-value' lookup."
  :type '(choice (const   :tag "No limit" nil)
                 (integer :tag "N (positive = exclude, negative = truncate)"))
  :group 'fzf-native)

(defcustom fzf-native-async-cache-size 40
  "Per-session LRU result cache capacity for the async path.
Each entry stores top-K results and the full matched-candidate index
for one query — enables exact-fresh hits (skip scoring) and prefix-
refinement hits (rescore only previously-matched candidates plus
deltas) without re-scanning the full pool.

Read once at session start by `fzf-native-async-start'.

Bridged by fzf-async from `fzf-async-cache-size' via `:around' advice."
  :type 'integer
  :group 'fzf-native)

(defcustom fzf-native-async-result-cache-ttl 300
  "Time-to-live in seconds for the cross-session result cache.

When non-nil and positive, the candidate arena from a session whose
child closed its stdout (reader hit EOF) is kept alive after
`fzf-native-async-stop'.  A subsequent `fzf-native-async-start' with
the same \(COMMAND, DIRECTORY\) within TTL seconds skips fork+exec
entirely and adopts the cached arena.

Sessions where the child was still alive at stop time are skipped —
the arena could be partial.  Empty arenas are also skipped.

nil or <= 0 disables the cache (no lookup, no insert).

Read on every call to `fzf-native-async-start' (gates lookup) and on
every call to `fzf-native-async-stop' (gates insert).

Eviction emits a `message' so the Emacs side has visibility.

Bridged by fzf-async from `fzf-async-result-cache-ttl' via `:around'
advice on start, candidates, and stop."
  :type '(choice (const   :tag "Disabled" nil)
                 (integer :tag "Seconds"))
  :group 'fzf-native)

(defcustom fzf-native-async-result-cache-entries 3
  "Maximum number of entries in the cross-session result cache.
Each entry is one completed `(COMMAND, DIRECTORY)' candidate pool.
LRU eviction once the count would exceed this bound.

Each entry retains its full candidate arena plus a chunked pointer
table.  Keep this small — the cache is meant for the few command
shapes a user runs repeatedly, not as a durable archive.

Active only when `fzf-native-async-result-cache-ttl' is set; <= 0
disables the cache regardless of TTL.

Read on every call to `fzf-native-async-start' and
`fzf-native-async-stop'.

Bridged by fzf-async from `fzf-async-result-cache-entries' via
`:around' advice."
  :type 'integer
  :group 'fzf-native)

(defcustom fzf-native-filter-only-min-pool 10000000
  "Pool size at which scoring switches to filter-only mode.
When the candidate pool reaches at least this size, scoring replaces
full fzf evaluation with `fzf_has_match' (boolean match-only check
from fzf-additions) and skips top-K sorting.  The match-set is
still built exhaustively so the per-session query cache (m_idx) can
be used to refine subsequent keystrokes — and so the rest of the
pipeline (cache hits, refinement, downstream Elisp processing)
behaves identically across the threshold.

Pool size is sampled per scoring run, so a streaming session that
crosses the threshold mid-typing switches modes for the keystrokes
after the crossing.  Below the threshold the full scorer ranks
results; above it the result order is the pool's natural order
(typically directory traversal / find order), capped at the
candidate limit.

Threshold is checked as `pool-size >= N', so values shape behaviour
as follows:
  0 (or nil) — feature disabled; full scoring always.
  1          — filter-only as soon as the pool is non-empty
               (effectively \"always filter\"; handy for testing).
  10000000   — filter-only only once the pool reaches 10M (default).

Default 10000000 is empirical: below it the full scorer is fast
enough; above it the per-keystroke latency benefits noticeably from
the cheap path.

Read once at session start by `fzf-native-async-start'.

Bridged by fzf-async from `fzf-async-filter-only-min-pool' via
`:around' advice."
  :type '(choice (const :tag "Disabled" nil)
                 (integer :tag "Minimum pool size"))
  :group 'fzf-native)

(defun fzf-native-module--cmake-is-available ()
  "Return t if cmake is available.
CMake is needed to build fzf-native, here we check that we can find
the executable."
  (unless (executable-find "cmake")
    (error "Fzf-Native needs CMake to be compiled.  Please, install CMake"))
  t)

;;;###autoload
(defun fzf-native-module-compile ()
  "Compile fzf-native-module."
  (interactive)
  (when (fzf-native-module--cmake-is-available)
    (let* ((fzf-native-directory
            (shell-quote-argument
             ;; NOTE: This is a workaround to fix an issue with how the Emacs
             ;; feature/native-comp branch changes the result of
             ;; `(locate-library "fzf-native")'. See emacs-devel thread
             ;; https://lists.gnu.org/archive/html/emacs-devel/2020-07/msg00306.html
             ;; for a discussion.
             (file-name-directory (locate-library "fzf-native.el" t))))
           (make-commands
            (concat
             "cd " fzf-native-directory " ; "
             "cmake -B build/ " fzf-native-module-cmake-args " && "
             "cmake --build build/"))
           (buffer (get-buffer-create fzf-native-module-install-buffer-name)))
      (pop-to-buffer buffer)
      (compilation-mode)
      (if (zerop (let ((inhibit-read-only t))
                   (call-process "sh" nil buffer t "-c" make-commands)))
          (message "Compilation of `fzf-native' module succeeded")
        (error "Compilation of `fzf-native' module failed!")))))

;;;###autoload
(defun fzf-native-module-compile-with-logging ()
  "Compile fzf-native-module with file logging enabled.
Sets FZF_NATIVE_DEBUG=1 so CMake compiles in the log-to-file path.
Logs are written to user-emacs-directory/fzf-native.log and truncated
on each module load."
  (interactive)
  (when (fzf-native-module--cmake-is-available)
    (let* ((fzf-native-directory
            (shell-quote-argument
             (file-name-directory (locate-library "fzf-native.el" t))))
           (make-commands
            (concat
             "cd " fzf-native-directory " ; "
             "FZF_NATIVE_DEBUG=1 cmake -B build/ " fzf-native-module-cmake-args " && "
             "cmake --build build/"))
           (buffer (get-buffer-create fzf-native-module-install-buffer-name)))
      (pop-to-buffer buffer)
      (compilation-mode)
      (if (zerop (let ((inhibit-read-only t))
                   (call-process "sh" nil buffer t "-c" make-commands)))
          (message "Compilation of `fzf-native' module with logging succeeded")
        (error "Compilation of `fzf-native' module with logging failed!")))))

;;;###autoload
(defun fzf-native-load-dyn ()
  "Load dynamic module."
  (interactive)
  (let* ((dyn-name (cl-case system-type
                     ((windows-nt ms-dos cygwin) (concat "Windows/Release/" fzf-native--dyn-name ".dll"))
                     (darwin (if (string-prefix-p "x86_64" system-configuration)
                                 ;; Intel
                                 (concat "Darwin/" fzf-native--dyn-name ".so")
                               ;; Apple Silicon
                               (concat "Darwin/arm64/" fzf-native--dyn-name ".so")))
                     (berkeley-unix (concat  "FreeBSD/" fzf-native--dyn-name ".so"))
                     (t (concat "Linux/" fzf-native--dyn-name ".so"))))
         (dyn-path (concat fzf-native--bin-dir dyn-name)))
    (module-load dyn-path)
    (message "[INFO] Successfully load dynamic module, `%s`" dyn-name)))

;;;###autoload
(defun fzf-native-load-own-build-dyn ()
  "Loads user-compiled version of module, building it if necessary."
  (unless (require 'fzf-native-module nil t)
    (if (or fzf-native-always-compile-module
            (y-or-n-p "Fzf-Native needs `fzf-native-module' to work.  Compile it now? "))
        (progn
          (let ((fzf-native-module-cmake-args (concat "-DFZF_NATIVE_MODULE_OUTPUT_DIR=''"
                                                      " "
                                                      fzf-native-module-cmake-args)))
            (fzf-native-module-compile))
          (require 'fzf-native-module))
      (error "Fzf-Native will not work until `fzf-native-module' is compiled!"))))

(provide 'fzf-native)
;;; fzf-native.el ends here
