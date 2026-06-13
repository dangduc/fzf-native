;;; fzf-native.el --- Fuzzy completion style  -*- lexical-binding: t; -*-

;; Copyright 2021 Duc Dang
;; Author: Duc Dang <me@dangduc.com>
;; Assisted-by: Claude:claude-opus-4-7
;; Version: 1.9
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
(declare-function fzf-native-async-candidates "fzf-native-module" (handle filter &optional limit))
(declare-function fzf-native-async-stats "fzf-native-module" (handle))
(declare-function fzf-native-async-result-fresh-p "fzf-native-module" (handle query))
(declare-function fzf-native-filter-only-p "fzf-native-module" (query-length pool-size))

(defconst fzf-native--dyn-name "fzf-native-module"
  "Dynamic module name.")

(defvar fzf-native-loaded nil
  "Non-nil after the fzf-native dynamic module has been loaded.
Set by `fzf-native-load-dyn', `fzf-native-load-own-build-dyn', and
`fzf-native-ensure-loaded'.  Libraries that depend on the C entry
points can call `fzf-native-ensure-loaded' to guarantee the module
is available without tracking load state themselves.")

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

Bridged by fzfa from `fzfa-highlight' via `:around' advice."
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

Bridged by fzfa from `fzfa-max-line-length' via `:around'
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

Bridged by fzfa from `fzfa-cache-size' via `:around' advice."
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
capped at the candidate limit.

Threshold is checked as `pool-size >= N', so values shape behaviour
as follows:
  0 (or nil) — feature disabled; full scoring always.
  1          — filter-only as soon as the pool is non-empty
               (effectively \"always filter\"; handy for testing).
  10000000   — filter-only only once the pool reaches 10M (default).

Default 10000000 is empirical: below it the full scorer is fast
enough; above it the per-keystroke latency benefits noticeably from
the cheap path.

Composes with `fzf-native-filter-only-length' under the rule
selected by `fzf-native-filter-only-logic' (OR by default).  Async
reads this once at session start; sync (`fzf-native-score-all')
reads it on every call.

Bridged by fzfa from `fzfa-filter-only-min-pool' via
`:around' advice."
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

Bridged by higher-level packages (fzfa, fussy) via the usual
`:around' advice / `setq-local' patterns."
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
    (setq fzf-native-loaded t)
    (let ((inhibit-message t))
      (message "[INFO] Successfully load dynamic module, `%s`" dyn-name))))

;;;###autoload
(defun fzf-native-load-own-build-dyn ()
  "Load user-compiled version of module, building it if necessary."
  (unless (require 'fzf-native-module nil t)
    (if (or fzf-native-always-compile-module
            (y-or-n-p "Fzf-Native needs `fzf-native-module' to work.  Compile it now? "))
        (progn
          (let ((fzf-native-module-cmake-args (concat "-DFZF_NATIVE_MODULE_OUTPUT_DIR=''"
                                                      " "
                                                      fzf-native-module-cmake-args)))
            (fzf-native-module-compile))
          (require 'fzf-native-module))
      (error "Fzf-Native will not work until `fzf-native-module' is compiled!")))
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
