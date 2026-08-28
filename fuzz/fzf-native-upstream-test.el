;;; fzf-native-upstream-test.el --- Differential checks against fzf -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

(require 'cl-lib)
(require 'ert)
(require 'fzf-native)

(declare-function fzf-native-score-all "fzf-native-module"
                  (collection query &optional slab))

(let ((module (getenv "FZF_NATIVE_TEST_MODULE")))
  (if (and module (not (string-empty-p module)))
      (progn
        (module-load module)
        (setq fzf-native-loaded t))
    (fzf-native-load-dyn)))

(defvar fzf-native-upstream--state 1)

(defun fzf-native-upstream--env-integer (name default)
  "Read non-negative integer NAME, or return DEFAULT."
  (let ((value (getenv name)))
    (if (and value (string-match-p "\\`[0-9]+\\'" value))
        (string-to-number value)
      default)))

(defun fzf-native-upstream--seed (seed)
  "Initialize the deterministic generator with SEED."
  (setq fzf-native-upstream--state (logand (max seed 1) #xffffffff)))

(defun fzf-native-upstream--random (limit)
  "Return a deterministic integer in [0, LIMIT)."
  (let ((x fzf-native-upstream--state))
    (setq x (logxor x (ash x 13)))
    (setq x (logxor x (ash x -17)))
    (setq x (logxor x (ash x 5)))
    (setq fzf-native-upstream--state (logand x #xffffffff))
    (if (<= limit 0) 0 (% fzf-native-upstream--state limit))))

(defconst fzf-native-upstream--pieces
  ["a" "b" "c" "F" "K" "-" "_" "/" "." " " "\t"])

(defconst fzf-native-upstream--literals
  ["a" "b" "f" "F" "k" "K" "foo" "bar" "src" "main"])

(defun fzf-native-upstream--candidate ()
  "Generate one candidate accepted by fzf's NUL-delimited input."
  (let ((count (fzf-native-upstream--random 12)) pieces)
    (dotimes (_ count)
      (push (aref fzf-native-upstream--pieces
                  (fzf-native-upstream--random
                   (length fzf-native-upstream--pieces)))
            pieces))
    (apply #'concat (nreverse pieces))))

(defun fzf-native-upstream--term ()
  "Generate one extended-search term."
  (concat (aref ["" "" "'" "^"]
                (fzf-native-upstream--random 4))
          (aref fzf-native-upstream--literals
                (fzf-native-upstream--random
                 (length fzf-native-upstream--literals)))
          ""))

(defun fzf-native-upstream--query ()
  "Generate a small extended-search query."
  (let ((count (1+ (fzf-native-upstream--random 4))) terms)
    (dotimes (i count)
      (push (fzf-native-upstream--term) terms)
      (when (and (< i (1- count))
                 (zerop (fzf-native-upstream--random 4)))
        (push "|" terms)))
    (mapconcat #'identity (nreverse terms) " ")))

(defun fzf-native-upstream--keys (strings)
  "Return a sorted, property-free multiset for STRINGS."
  (sort (mapcar #'substring-no-properties (append strings nil)) #'string<))

(defun fzf-native-upstream--fzf (fzf collection query case-mode fuzzy)
  "Return matches produced by FZF for COLLECTION and QUERY."
  (let ((args (append
               (list "--read0" "--print0" "--no-sort" "--no-color"
                     "--no-multi-line" "--literal"
                     (concat "--filter=" query))
               (pcase case-mode
                 ('ignore '("--ignore-case"))
                 ('respect '("--no-ignore-case"))
                 (_ '("--smart-case")))
               (unless fuzzy '("--exact"))))
        (output (generate-new-buffer " *fzf-native-upstream*")))
    (unwind-protect
        (with-temp-buffer
          (insert (mapconcat #'identity collection "\0") "\0")
          (let ((coding-system-for-write 'no-conversion)
                (coding-system-for-read 'no-conversion)
                (status (apply #'call-process-region
                               (point-min) (point-max) fzf nil output nil
                               args)))
            (unless (memq status '(0 1))
              (error "%s exited with status %S" fzf status)))
          (with-current-buffer output
            (butlast (split-string (buffer-string) "\0" nil))))
      (kill-buffer output))))

(ert-deftest fzf-native-fuzz-upstream-ascii-match-set ()
  "Compare randomized ASCII match multisets with a pinned fzf CLI."
  (let* ((fzf (or (getenv "FZF_REFERENCE") (executable-find "fzf")))
         (expected-version (getenv "FZF_REFERENCE_VERSION"))
         (seed (fzf-native-upstream--env-integer
                "FZF_NATIVE_FUZZ_SEED" 12648430))
         (cases (fzf-native-upstream--env-integer
                 "FZF_NATIVE_UPSTREAM_CASES" 200))
         (fixed '("" "a" "A" "alpha" " foo" "foo " "foo.bar"
                  "src/main.c" "src-test")))
    (skip-unless fzf)
    (when (and expected-version (not (string-empty-p expected-version)))
      (let ((actual (car (process-lines fzf "--version"))))
        (should (string-prefix-p expected-version actual))))
    (fzf-native-upstream--seed (logxor seed #xa5a5a5a5))
    (dotimes (iteration cases)
      (let* ((fzf-native-case-mode
              (aref [smart ignore respect]
                    (fzf-native-upstream--random 3)))
             (fzf-native-fuzzy
              (not (zerop (fzf-native-upstream--random 2))))
             (collection
              (append fixed
                      (cl-loop repeat (fzf-native-upstream--random 24)
                               collect (fzf-native-upstream--candidate))))
             (query (fzf-native-upstream--query))
             (fzf-native-batch-highlight nil)
             (fzf-native-filter-only-min-pool nil)
             (fzf-native-filter-only-length nil)
             (native
              (fzf-native-upstream--keys
               (fzf-native-score-all
                (mapcar #'copy-sequence collection) query)))
             (upstream
              (fzf-native-upstream--keys
               (fzf-native-upstream--fzf
                fzf collection query fzf-native-case-mode
                fzf-native-fuzzy))))
        (ert-info ((format "seed=%d iteration=%d mode=%S fuzzy=%S query=%S collection=%S"
                           seed iteration fzf-native-case-mode
                           fzf-native-fuzzy query collection))
          (should (equal native upstream)))))))

(provide 'fzf-native-upstream-test)
;;; fzf-native-upstream-test.el ends here
