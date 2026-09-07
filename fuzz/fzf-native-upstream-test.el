;;; fzf-native-upstream-test.el --- Differential checks against fzf -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Duc Dang
;; Author: Duc Dang <me@dangduc.com>
;; Assisted-by: Codex:gpt-5
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is part of fzf-native.

;; fzf-native is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; fzf-native is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with fzf-native.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Compare structured native-module results with the upstream fzf executable.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'fzf-native)
(require 'subr-x)

(let ((directory
       (expand-file-name
        "differential"
        (file-name-directory (or load-file-name buffer-file-name)))))
  (add-to-list 'load-path directory))

(require 'fzf-native-differential-generator)
(require 'fzf-native-differential-exceptions)

(declare-function fzf-native-score-all "fzf-native-module"
                  (collection query &optional slab))
(declare-function fzf-native-async-start "fzf-native-module"
                  (command &optional directory))
(declare-function fzf-native-async-stop "fzf-native-module" (handle))
(declare-function fzf-native-async-submit "fzf-native-module"
                  (handle query &optional limit))
(declare-function fzf-native-async-snapshot "fzf-native-module"
                  (handle &optional request-id))
(declare-function fzf-native-async-status "fzf-native-module"
                  (handle &optional request-id))
(declare-function fzf-native--session-platform-p "fzf-native" ())
(declare-function fzf-native--verify-initialized-module "fzf-native" ())
(declare-function fzf-native--verify-session-abi "fzf-native" ())

(let ((module (getenv "FZF_NATIVE_TEST_MODULE")))
  (if (and module (not (string-empty-p module)))
      (progn
        (module-load module)
        (fzf-native--verify-initialized-module)
        (setq fzf-native-loaded t))
    (fzf-native-load-dyn)))

(when (fzf-native--session-platform-p)
  (dolist (function '(fzf-native-session-abi-version
                      fzf-native-async-start
                      fzf-native-async-stop
                      fzf-native-async-submit
                      fzf-native-async-snapshot
                      fzf-native-async-status))
    (unless (fboundp function)
      (error "Required fzf-native session ABI function is missing: %S"
             function))))

(defun fzf-native-upstream--env-integer (name default)
  "Read non-negative integer NAME, or return DEFAULT."
  (let ((value (getenv name)))
    (if (and value (string-match-p "\\`[0-9]+\\'" value))
        (string-to-number value)
      default)))

(defun fzf-native-upstream--profile ()
  "Return the requested differential profile."
  (let ((name (or (getenv "FZF_NATIVE_UPSTREAM_PROFILE") "common")))
    (pcase name
      ("common" 'common)
      ("parity" 'parity)
      ("long" 'long)
      (_ (error "Unknown FZF_NATIVE_UPSTREAM_PROFILE: %s" name)))))

(defun fzf-native-upstream--verify-reference (fzf)
  "Verify the configured version and revision of FZF.

`FZF_REFERENCE_VERSION' is an optional output prefix.
`FZF_REFERENCE_REVISION' is an optional literal substring."
  (let ((expected-version (getenv "FZF_REFERENCE_VERSION"))
        (expected-revision (getenv "FZF_REFERENCE_REVISION"))
        (actual (car (process-lines fzf "--version"))))
    (when (and expected-version (not (string-empty-p expected-version)))
      (should (string-prefix-p expected-version actual)))
    (when (and expected-revision (not (string-empty-p expected-revision)))
      (should (string-search expected-revision actual)))
    actual))

(defun fzf-native-upstream--case-rng (seed serial)
  "Return an independent deterministic generator for SEED and SERIAL."
  (fzf-native-differential-rng-create
   (logand (logxor seed #xa5a5a5a5
                   (* (1+ serial) #x9e3779b9))
           #xffffffff)))

(defun fzf-native-upstream--generated-case (seed serial profile)
  "Generate PROFILE case SERIAL through the public replay mapping for SEED."
  (fzf-native-differential-generate-case
   (fzf-native-upstream--case-rng seed serial) seed serial profile))

(defun fzf-native-upstream--candidate-texts (case)
  "Return candidate text from CASE in producer order."
  (mapcar #'fzf-native-differential-candidate-text
          (fzf-native-differential-case-candidates case)))

(defun fzf-native-upstream--fzf (fzf case)
  "Return raw matches from FZF for CASE."
  (let* ((query (fzf-native-differential-case-query case))
         (collection (fzf-native-upstream--candidate-texts case))
         (valid-utf8
          (plist-get (fzf-native-differential-case-dimensions case)
                     :valid-utf8))
         (args
          (append
           (list "--read0" "--print0" "--no-color" "--no-multi-line"
                 (concat "--filter="
                         (fzf-native-differential-case-rendered-query case)))
           (when (eq (fzf-native-differential-case-comparison case)
                     'membership)
             '("--no-sort"))
           (unless (fzf-native-differential-query-normalize query)
             '("--literal"))
           (pcase (fzf-native-differential-query-case-mode query)
             ('ignore '("--ignore-case"))
             ('respect '("--no-ignore-case"))
             (_ '("--smart-case")))
           (unless (fzf-native-differential-query-fuzzy query)
             '("--exact"))))
         (output (generate-new-buffer " *fzf-native-upstream*")))
    (unwind-protect
        (with-temp-buffer
          (unless valid-utf8
            (set-buffer-multibyte nil))
          (insert (mapconcat #'identity collection "\0") "\0")
          (with-current-buffer output
            (unless valid-utf8
              (set-buffer-multibyte nil)))
          (let ((coding-system-for-write
                 (if valid-utf8 'utf-8-unix 'binary))
                (coding-system-for-read
                 (if valid-utf8 'utf-8-unix 'binary))
                (status (apply #'call-process-region
                               (point-min) (point-max) fzf nil output nil
                               args)))
            (unless (memq status '(0 1))
              (error "%s exited with status %S" fzf status)))
          (with-current-buffer output
            (butlast (split-string (buffer-string) "\0" nil))))
      (kill-buffer output))))

(defun fzf-native-upstream--native (case)
  "Return raw fzf-native matches for CASE."
  (let* ((query (fzf-native-differential-case-query case))
         (fzf-native-case-mode
          (fzf-native-differential-query-case-mode query))
         (fzf-native-fuzzy
          (fzf-native-differential-query-fuzzy query))
         (fzf-native-batch-highlight nil)
         (fzf-native-filter-only-min-pool nil)
         (fzf-native-filter-only-length nil))
    (fzf-native-score-all
     (mapcar #'copy-sequence (fzf-native-upstream--candidate-texts case))
     (fzf-native-differential-case-rendered-query case))))

(defun fzf-native-upstream--utf8-sequence-length (string index)
  "Return valid UTF-8 sequence length in unibyte STRING at INDEX, or nil."
  (let* ((size (length string))
         (first (aref string index))
         (second (and (< (1+ index) size) (aref string (1+ index)))))
    (cond
     ((<= first #x7f) 1)
     ((and (<= #xc2 first) (<= first #xdf)
           second (fzf-native-differential--utf8-continuation-p second))
      2)
     ((and (<= #xe0 first) (<= first #xef)
           (< (+ index 2) size)
           (cond
            ((= first #xe0) (and (<= #xa0 second) (<= second #xbf)))
            ((= first #xed) (and (<= #x80 second) (<= second #x9f)))
            (t (fzf-native-differential--utf8-continuation-p second)))
           (fzf-native-differential--utf8-continuation-p
            (aref string (+ index 2))))
      3)
     ((and (<= #xf0 first) (<= first #xf4)
           (< (+ index 3) size)
           (cond
            ((= first #xf0) (and (<= #x90 second) (<= second #xbf)))
            ((= first #xf4) (and (<= #x80 second) (<= second #x8f)))
            (t (fzf-native-differential--utf8-continuation-p second)))
           (fzf-native-differential--utf8-continuation-p
            (aref string (+ index 2)))
           (fzf-native-differential--utf8-continuation-p
            (aref string (+ index 3))))
      4))))

(defun fzf-native-upstream--go-output-bytes (string)
  "Return Go range/output bytes for possibly malformed unibyte STRING."
  (let ((string (if (multibyte-string-p string)
                    (encode-coding-string string 'raw-text t)
                  string)))
    (let ((index 0)
          (size (length string))
          (replacement (unibyte-string #xef #xbf #xbd))
          pieces)
      (while (< index size)
        (let ((sequence-length
               (fzf-native-upstream--utf8-sequence-length string index)))
          (if sequence-length
              (progn
                (push (substring string index (+ index sequence-length))
                      pieces)
                (setq index (+ index sequence-length)))
            (push replacement pieces)
            (setq index (1+ index)))))
      (apply #'concat (nreverse pieces)))))

(defun fzf-native-upstream--identity-table (case &optional upstream-output)
  "Return a text-to-identity-queue table for CASE.

When UPSTREAM-OUTPUT is non-nil, key malformed input by Go's output bytes."
  (let ((table (make-hash-table :test #'equal)))
    (dolist (candidate (fzf-native-differential-case-candidates case))
      (let* ((raw (fzf-native-differential-candidate-text candidate))
             (text (if (and upstream-output
                            (not (plist-get
                                  (fzf-native-differential-case-dimensions
                                   case)
                                  :valid-utf8)))
                       (fzf-native-upstream--go-output-bytes raw)
                     raw))
             (identities (gethash text table)))
        (puthash text
                 (append identities
                         (list (fzf-native-differential-candidate-id
                                candidate)))
                 table)))
    table))

(defun fzf-native-upstream--identities (case strings &optional upstream-output)
  "Map returned STRINGS to stable candidate identities from CASE.

UPSTREAM-OUTPUT selects Go's malformed-byte output representation."
  (let ((table (fzf-native-upstream--identity-table case upstream-output))
        result)
    (dolist (string (append strings nil) (nreverse result))
      (let* ((plain (substring-no-properties string))
             (queue (gethash plain table)))
        (unless queue
          (error "Matcher returned an unknown candidate: %S" plain))
        (puthash plain (cdr queue) table)
        (push (car queue) result)))))

(defun fzf-native-upstream--membership (identities)
  "Return sorted membership for IDENTITIES."
  (sort (copy-sequence identities) #'<))

(defun fzf-native-upstream--membership-difference (left right)
  "Return sorted identities present in exactly one of LEFT and RIGHT."
  (sort
   (append (cl-set-difference left right :test #'=)
           (cl-set-difference right left :test #'=))
   #'<))

(defun fzf-native-upstream--record-exception (table exception)
  "Record EXCEPTION in count TABLE."
  (let ((name (plist-get exception :name)))
    (puthash name (1+ (gethash name table 0)) table)))

(defun fzf-native-upstream--accepted-exception-p (exception)
  "Return non-nil only for a documented, accepted EXCEPTION."
  (eq (plist-get exception :disposition) 'accepted))

(defun fzf-native-upstream--exception-alist (table)
  "Return sorted exception counts from TABLE."
  (let (entries)
    (maphash (lambda (name count) (push (cons name count) entries)) table)
    (sort entries (lambda (left right)
                    (string< (symbol-name (car left))
                             (symbol-name (car right)))))))

(defun fzf-native-upstream--dimension-value (case key)
  "Return dimension KEY from CASE, including query options."
  (let ((query (fzf-native-differential-case-query case))
        (dimensions (fzf-native-differential-case-dimensions case)))
    (pcase key
      (:case-mode (fzf-native-differential-query-case-mode query))
      (:fuzzy (fzf-native-differential-query-fuzzy query))
      (_ (plist-get dimensions key)))))

(defun fzf-native-upstream--assert-pairwise-coverage (cases specifications)
  "Assert pairwise SPECIFICATIONS coverage across CASES.

Each specification has the form (KEY VALUES)."
  (cl-loop
   for tail on specifications
   for left = (car tail)
   do
   (dolist (right (cdr tail))
     (dolist (left-value (cadr left))
       (dolist (right-value (cadr right))
         (should
          (cl-some
           (lambda (case)
             (and
              (equal (fzf-native-upstream--dimension-value
                      case (car left))
                     left-value)
              (equal (fzf-native-upstream--dimension-value
                      case (car right))
                     right-value)))
           cases)))))))

(ert-deftest fzf-native-fuzz-upstream-generator-is-deterministic ()
  "Cover query, text, length, rank, and identity dimensions deterministically."
  (let* ((seed 424242)
         (left
          (cl-loop for serial below 6000
                   collect (fzf-native-upstream--generated-case
                            seed serial 'common)))
         (right
          (cl-loop for serial below 6000
                   collect (fzf-native-upstream--generated-case
                            seed serial 'common)))
         (replay-start 733)
         (replay-count 20)
         (replay
          (cl-loop for serial from replay-start
                   below (+ replay-start replay-count)
                   collect (fzf-native-upstream--generated-case
                            seed serial 'common)))
         (long-cases
          (cl-loop for serial below 3
                   collect (fzf-native-upstream--generated-case
                            seed serial 'long)))
         text-classes length-targets query-shapes primary-kinds rank-shapes)
    (should
     (equal (mapcar #'fzf-native-differential-case-description left)
            (mapcar #'fzf-native-differential-case-description right)))
    (should
     (equal
      (mapcar #'fzf-native-differential-case-description replay)
      (mapcar #'fzf-native-differential-case-description
              (seq-take (nthcdr replay-start left) replay-count))))
    (dolist (case left)
      (let ((dimensions (fzf-native-differential-case-dimensions case))
            (ids (mapcar #'fzf-native-differential-candidate-id
                         (fzf-native-differential-case-candidates case)))
            (texts (fzf-native-upstream--candidate-texts case)))
        (push (plist-get dimensions :text-class) text-classes)
        (push (plist-get dimensions :needle-length) length-targets)
        (push (plist-get dimensions :query-shape) query-shapes)
        (push (plist-get dimensions :primary-kind) primary-kinds)
        (push (plist-get dimensions :rank-shape) rank-shapes)
        (should (equal ids (number-sequence 0 (1- (length ids)))))
        (should (> (length texts)
                   (length (delete-dups (copy-sequence texts)))))
        (should-not
         (string-search "\0"
                        (fzf-native-differential-case-rendered-query case)))))
    (dolist (class '(ascii unicode malformed))
      (should (memq class text-classes)))
    (dolist (target '(1 2 7 31 32 63 64))
      (should (memq target length-targets)))
    (dolist (shape '(empty single and or inverse mixed))
      (should (memq shape query-shapes)))
    (dolist (kind '(fuzzy exact prefix suffix equal))
      (should (memq kind primary-kinds)))
    (dolist (shape '(front middle tail ambiguous))
      (should (memq shape rank-shapes)))
    (fzf-native-upstream--assert-pairwise-coverage
     (cl-remove-if
      (lambda (case)
        (eq (fzf-native-upstream--dimension-value case :query-shape)
            'empty))
      left)
     '((:text-class (ascii unicode malformed))
       (:query-shape (single and or inverse mixed))
       (:primary-kind (fuzzy exact prefix suffix equal))
       (:case-mode (smart ignore respect))
       (:fuzzy (nil t))
       (:rank-shape (front middle tail ambiguous))
       (:needle-length (1 2 7 31 32 63 64))))
    (cl-loop for case in long-cases
             for expected in '(999 1000 1001)
             for primary = (caar
                            (fzf-native-differential-query-sets
                             (fzf-native-differential-case-query case)))
             do (should
                 (= expected
                    (length
                     (fzf-native-differential-term-literal primary)))))))

(provide 'fzf-native-upstream-test)
;;; fzf-native-upstream-test.el ends here
