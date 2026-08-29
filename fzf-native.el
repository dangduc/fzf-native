;;; fzf-native.el --- Fuzzy completion style  -*- lexical-binding: t; -*-

;; Copyright 2021 Duc Dang
;; Author: Duc Dang <me@dangduc.com>
;; Assisted-by: Claude:claude-opus-4-7
;; Version: 2.6
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
(declare-function fzf-native-highlight-one "fzf-native-module" (cand query))
(declare-function fzf-native-score "fzf-native-module" (str query &optional slab))
(declare-function fzf-native-make-default-slab "fzf-native-module" ())
(declare-function fzf-native-make-slab "fzf-native-module" (size16 size32))
(declare-function fzf-native-async-start "fzf-native-module" (command &optional dir))
(declare-function fzf-native-async-stop "fzf-native-module" (handle))
(declare-function fzf-native-async-generation "fzf-native-module" (handle))
(declare-function fzf-native-async-submit "fzf-native-module" (handle query &optional limit))
(declare-function fzf-native-async-snapshot "fzf-native-module" (handle &optional request-id))
(declare-function fzf-native-async-status "fzf-native-module" (handle &optional request-id))
(declare-function fzf-native-async-candidates "fzf-native-module" (handle filter &optional limit))
(declare-function fzf-native-async-stats "fzf-native-module" (handle))
(declare-function fzf-native-async-result-fresh-p "fzf-native-module" (handle query))
(declare-function fzf-native-filter-only-p "fzf-native-module" (query-length pool-size))
(declare-function fzf-native-session-abi-version "fzf-native-module" ())

(defconst fzf-native--dyn-name "fzf-native-module"
  "Dynamic module name.")

(defvar fzf-native-loaded nil
  "Non-nil after the fzf-native dynamic module has been loaded.
Set by `fzf-native-load-dyn', `fzf-native-load-own-build-dyn', and
`fzf-native-ensure-loaded'.  Libraries that depend on the C entry
points can call `fzf-native-ensure-loaded' to guarantee the module
is available without tracking load state themselves.")

(defconst fzf-native-session-abi-required 1
  "Interactive-session ABI required by this version of fzf-native.el.")

(defun fzf-native--freebsd-target-p ()
  "Return non-nil when Emacs targets FreeBSD specifically.

Emacs reports FreeBSD, DragonFly, NetBSD, and OpenBSD as
`berkeley-unix'.  The configuration triplet is required to distinguish the
FreeBSD module's libc and kernel ABI from the other BSD targets."
  (and (eq system-type 'berkeley-unix)
       (string-match-p
        "-freebsd\\(?:[0-9.]*\\)?\\(?:-\\|\\'\\)"
        (downcase system-configuration))))

(defun fzf-native--session-platform-p ()
  "Return non-nil when this target implements the POSIX session ABI."
  (or (memq system-type '(darwin gnu/linux))
      (fzf-native--freebsd-target-p)))

(defun fzf-native--verify-session-abi ()
  "Fail if the loaded POSIX module has a stale session ABI.
Windows modules retain batch-only support and do not implement the session
entry points, so this handshake applies only where the native session API is
available."
  (when (fzf-native--session-platform-p)
    (unless (fboundp 'fzf-native-session-abi-version)
      (error (concat "Stale fzf-native module: interactive-session ABI "
                     "entry point is missing (need ABI %d); rebuild or "
                     "replace the bundled native module")
             fzf-native-session-abi-required))
    (let ((actual (fzf-native-session-abi-version)))
      (unless (equal actual fzf-native-session-abi-required)
        (error (concat "Incompatible fzf-native interactive-session ABI: "
                       "module has %S, Elisp requires %d; rebuild or "
                       "replace the bundled native module")
               actual fzf-native-session-abi-required))))
  t)

(defun fzf-native--verify-initialized-module ()
  "Verify the initialized module, or report the required safe recovery.

Emacs does not unload a dynamic module when its feature is removed.  Loading a
second ABI over live user pointers and worker callbacks is unsafe.  Therefore,
after an incompatible initializer has run, recovery requires replacing or
rebuilding the artifact and restarting Emacs."
  (condition-case err
      (fzf-native--verify-session-abi)
    (error
     (setq fzf-native-loaded nil)
     (error (concat "%s.  The incompatible native module is already loaded "
                    "and cannot be replaced safely in this Emacs process; "
                    "rebuild or replace it, then restart Emacs")
            (error-message-string err)))))

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
;; Higher-level packages (fzfa, fussy) keep their own user-facing
;; defcustoms and bridge their values onto these names — fussy via
;; `setq-local' (synchronous, same-buffer call pattern), fzfa via
;; explicit dynamic bindings at fzfa-owned call sites (timer-driven,
;; cross-buffer).

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

(defcustom fzf-native-fuzzy t
  "Whether to fuzzy match with `fzf-native'.

If t, use fuzzy matching, if nil, use exact/substring matching.

If t, prefixing a term with ' switches that term to exact matching.

If nil, prefixing a term with ' switches that term to fuzzy matching.

Read at the start of every scoring call."
  :type 'boolean
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

Bridged by fzfa from `fzfa-highlight' at fzfa-owned native call sites."
  :type '(choice (const   :tag "Disabled" nil)
                 (const   :tag "All candidates" t)
                 (integer :tag "Top N candidates"))
  :group 'fzf-native)

(defun fzf-native-default-highlight-fn (cand positions)
  "Default `fzf-native-highlight-fn'.  Preserves caller-attached faces.

Surgically removes any leftover `completions-common-part' face on CAND
\(so reused candidate strings don't accumulate stale highlights across
keystrokes), then layers a fresh `completions-common-part' on top of
any other faces present at the match positions.

POSITIONS is a vector of alternating character-offset start/end pairs:
  [s0 e0 s1 e1 …]"
  (let ((len (length cand))
        (i 0))
    ;; Surgical strip: walk face intervals, remove only
    ;; `completions-common-part' from the value, leaving other faces.
    ;; Always store the residual as a list — never unwrap a one-element
    ;; list back to its bare element.  Unwrapping is theoretically
    ;; equivalent for display but propagates any non-symbol garbage that
    ;; an upstream package may have left in the face list (numbers,
    ;; opaque values) into a top-level position where the display
    ;; engine signals `Invalid face reference'.
    (while (< i len)
      (let* ((face (get-text-property i 'face cand))
             (next (or (next-single-property-change i 'face cand) len)))
        (cond
         ((eq face 'completions-common-part)
          (remove-text-properties i next '(face nil) cand))
         ((and (consp face) (memq 'completions-common-part face))
          (let ((survivors (remq 'completions-common-part face)))
            (if survivors
                (put-text-property i next 'face survivors cand)
              (remove-text-properties i next '(face nil) cand)))))
        (setq i next))))
  ;; Additive apply at match positions; stacks on top of caller faces.
  (let ((n (length positions)))
    (dotimes (k (/ n 2))
      (add-face-text-property (aref positions (* 2 k))
                              (aref positions (1+ (* 2 k)))
                              'completions-common-part nil cand))))

(defvar fzf-native-highlight-fn #'fzf-native-default-highlight-fn
  "Function invoked by the C scorer to apply match highlights.

Called once per top-N highlighted candidate when highlighting is
enabled (capped by `fzf-native-batch-highlight' /
`fzf-native-async-highlight').

Signature: (CAND POSITIONS) → ignored.

  CAND       The fresh top-N copy made by fzf-native.  Mutate in
             place to attach faces / text properties.
  POSITIONS  Vector of alternating character-offset start/end pairs
             describing contiguous fzf match runs:
               [s0 e0 s1 e1 …]

Set to nil to suppress highlight application entirely (scoring still
happens).  Let-binding around a call swaps policy for that call.

`fzf-native-default-highlight-fn' is the standard implementation:
surgical strip of leftover `completions-common-part' followed by an
additive apply that preserves any caller-attached faces.")

(defcustom fzf-native-max-line-length 256
  "Per-line character cap applied by the async reader thread.
nil        — no limit.
positive N — exclude lines longer than N characters.
negative -N — include but truncate lines to N characters.

Read once at session start by `fzf-native-async-start'.

When this option is non-nil, the reader normalizes each record incrementally.
It retains at most N decoded candidate characters before the newline arrives.
ANSI escape payloads and discarded suffix bytes do not increase the partial
record buffer.  The reader still scans the complete record.  It rejects an
embedded NUL, including one after a retained prefix.  A nil value permits an
unbounded record.  A nil value is safe only with trusted producers.

Bridged by fzfa from `fzfa-max-line-length' while fzfa calls
`fzf-native-async-start'."
  :type '(choice (const   :tag "No limit" nil)
                 (integer :tag "N (positive = exclude, negative = truncate)"))
  :group 'fzf-native)

(defcustom fzf-native-async-cache-size 40
  "Per-session LRU result cache capacity for the async path.
Each entry stores top-K results and, when the byte budget permits, a
complete matched-candidate index for one query.  Complete membership
enables exact-growth and prefix-refinement scans; otherwise the scorer
uses stable-batch evidence or safely scans the selected pool boundary.

Read once at session start by `fzf-native-async-start'.

Bridged by fzfa from `fzfa-cache-size' while fzfa starts a session."
  :type 'integer
  :group 'fzf-native)

(defcustom fzf-native-async-cache-bytes (* 64 1024 1024)
  "Maximum bytes retained by the whole-result cache for one session.
The budget includes query keys, parsed patterns, top-K entries, and complete
matched-candidate index sets.  Set this to 0 to disable the whole-result
cache.  The entry-count limit in `fzf-native-async-cache-size' also applies.

Read once at session start by `fzf-native-async-start'."
  :type 'integer
  :group 'fzf-native)

(defcustom fzf-native-async-batch-cache-bytes (* 64 1024 1024)
  "Maximum bytes for stable-batch membership data in one async session.

The native scorer stores completed full-batch match sets before it checks
request cancellation.  A later exact or narrower query can reuse those sets.

Sparse match sets use local candidate indexes.  Denser sets use bitmaps.
The scorer does not cache a batch when more than half its candidates match.

The cache retains at most 4,096 query records.  An ancestor lookup scans at
most 256 recent records.  An exact lookup uses a hash table and does not scan
this list.  An eviction only removes optional cache evidence.

Set this value to zero to disable stable-batch membership reuse.  The module
reads the value once in `fzf-native-async-start'."
  :type 'natnum
  :group 'fzf-native)

(defcustom fzf-native-filter-only-min-pool 10000000
  "Pool size at which scoring switches to filter-only mode.
When the candidate pool reaches at least this size, scoring replaces
full fzf evaluation with `fzf_has_match' (boolean match-only check
from fzf-additions).  The async scorer retains the first result-limit
matches, then scores and ranks only that bounded visible window.
Complete match membership is optional cache evidence: it is retained
only when it fits half of `fzf-native-async-cache-bytes'.  If it does
not fit, later refinement safely uses stable-batch evidence or scans
the selected pool boundary again.

Pool size is sampled per scoring run, so a streaming session that
crosses the threshold mid-typing switches modes for the keystrokes
after the crossing.  Below the threshold the full scorer ranks
results; above it the result order is the pool's natural order
capped at the candidate limit.

Threshold is checked as `pool-size >= N', so values shape behaviour
as follows:
  0 (or nil) — feature disabled; full scoring always.
  1          — filter-only as soon as the pool is non-empty
               (effectively \"always filter\"; handy for testing).
  10000000   — filter-only only once the pool reaches 10M (default).

The default 10000000 is a conservative large-pool trigger.  Actual
latency depends on candidate length, query shape, match density,
hardware, and the requested result limit; measure with your workload.

Composes with `fzf-native-filter-only-length' under the rule
selected by `fzf-native-filter-only-logic' (OR by default).  Async
reads this once at session start; sync (`fzf-native-score-all')
reads it on every call.

Bridged by fzfa from `fzfa-filter-only-min-pool' at fzfa-owned native
call sites."
  :type '(choice (const :tag "Disabled" nil)
                 (integer :tag "Minimum pool size"))
  :group 'fzf-native)

(defcustom fzf-native-filter-only-length nil
  "Query length below which scoring switches to filter-only mode.
When non-nil and the current query is at most this many characters
long, scoring replaces full fzf evaluation with `fzf_has_match'
and skips counting-sort over the matched candidates.

For short queries the score signal is dominated by length / position
heuristics that don't carry much ranking information; the user is
typically still narrowing, not picking.  Filter-only here makes the
per-keystroke cost cheap and lets the caller (e.g. fussy) keep its
own subsuming candidate pool for the eventual full-score pass once
the query is long enough to rank.

Threshold is checked as `query-length <= N', so values shape behaviour
as follows:
  nil (or 0) — feature disabled; full scoring always (for this arm).
  1          — only single-character queries filter-only.
  3          — queries of length 1, 2, or 3 filter-only (matches
               fussy's default `fussy-company-prefix-length').

Read on every scoring call (sync) or every scoring run (async).

Composes with `fzf-native-filter-only-min-pool' under the rule
selected by `fzf-native-filter-only-logic' (OR by default — either
trigger is sufficient).

Bridged by higher-level packages at their owned call sites (fzfa) or
through buffer-local bindings (fussy)."
  :type '(choice (const :tag "Disabled" nil)
                 (integer :tag "Maximum query length"))
  :group 'fzf-native)

(defcustom fzf-native-filter-only-logic 'or
  "`fzf-native-filter-only-min-pool' or/and `fzf-native-filter-only-length'.
Both defcustoms gate the switch from full fzf scoring to the cheap
`fzf_has_match' path.  Each is an independent trigger; this knob
controls how the two triggers are combined when both are enabled.

`or' (default)
  Filter-only fires when *either* trigger fires.  Natural reading:
  each defcustom names an independent sufficient reason to skip full
  scoring (pool too large / query too short).

`and'
  Filter-only fires only when *every enabled* trigger fires.  A
  trigger that is disabled (its defcustom is nil/0) is treated as
  trivially satisfied and ignored.  If both are disabled the feature
  is off regardless of logic.

Read on every scoring call (sync) or every scoring run (async)."
  :type '(choice (const :tag "OR (either trigger fires)" or)
                 (const :tag "AND (all enabled triggers fire)" and))
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
(defun fzf-native--bundled-module-relative-path ()
  "Return the bundled module path for this target, or signal an error.

The release currently bundles both macOS architectures and x86-64
artifacts for Linux, FreeBSD, and Windows.  Refuse unsupported targets
before `module-load' reports a misleading bad-CPU-type or loader error."
  (let ((x86-64-p
         (string-match-p "\\`\\(?:x86_64\\|amd64\\)-" system-configuration))
        (arm64-p
         (string-match-p "\\`\\(?:arm64\\|aarch64\\)-" system-configuration)))
    (or (cl-case system-type
          ((windows-nt ms-dos cygwin)
           (and x86-64-p
                (concat "Windows/Release/" fzf-native--dyn-name ".dll")))
          (darwin
           (cond
            (x86-64-p (concat "Darwin/" fzf-native--dyn-name ".so"))
            (arm64-p (concat "Darwin/arm64/" fzf-native--dyn-name ".so"))))
          (berkeley-unix
           (and x86-64-p
                (fzf-native--freebsd-target-p)
                (concat "FreeBSD/" fzf-native--dyn-name ".so")))
          (gnu/linux
           (and x86-64-p
                (concat "Linux/" fzf-native--dyn-name ".so"))))
        (user-error
         (concat "No bundled fzf-native module for %s; "
                 "run M-x fzf-native-load-own-build-dyn")
         system-configuration))))

;;;###autoload
(defun fzf-native-load-dyn ()
  "Load the bundled dynamic module for the current target."
  (interactive)
  (let* ((dyn-name (fzf-native--bundled-module-relative-path))
         (dyn-path (concat fzf-native--bin-dir dyn-name)))
    ;; A module initializer may already have run even when the Lisp-side
    ;; convenience flag is nil.  Verify that live image first: calling a
    ;; second initializer cannot safely replace its functions or user pointers.
    (unless (featurep 'fzf-native-module)
      (module-load dyn-path))
    (fzf-native--verify-initialized-module)
    (setq fzf-native-loaded t)
    (let ((inhibit-message t))
      (message "[INFO] Successfully load dynamic module, `%s`" dyn-name))))

;;;###autoload
(defun fzf-native-load-own-build-dyn ()
  "Load a user-compiled module, building it if necessary.

If an incompatible module already initialized in this Emacs process, signal a
restart-required error.  Replacing a loaded dynamic module in place is unsafe."
  (if (featurep 'fzf-native-module)
      (fzf-native--verify-initialized-module)
    (unless (require 'fzf-native-module nil t)
      (if (or fzf-native-always-compile-module
              (y-or-n-p
               "Fzf-Native needs `fzf-native-module' to work.  Compile it now? "))
          (progn
            (let ((fzf-native-module-cmake-args
                   (concat "-DFZF_NATIVE_MODULE_OUTPUT_DIR='' "
                           fzf-native-module-cmake-args)))
              (fzf-native-module-compile))
            (require 'fzf-native-module))
        (error
         "Fzf-Native will not work until `fzf-native-module' is compiled!")))
    (fzf-native--verify-initialized-module))
  (setq fzf-native-loaded t))

;;;###autoload
(defun fzf-native-ensure-loaded ()
  "Load the fzf-native dynamic module if it isn't loaded yet.
Calls `fzf-native-load-dyn' on first use and is a no-op on
subsequent calls.  Intended for library code that needs the C
entry points available before invoking them."
  (unless fzf-native-loaded
    (fzf-native-load-dyn)))

(provide 'fzf-native)
;;; fzf-native.el ends here
