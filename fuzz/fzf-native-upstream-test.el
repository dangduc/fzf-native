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
         (score-scheme
          (or (fzf-native-differential-query-score-scheme query) 'default))
         (valid-utf8
          (plist-get (fzf-native-differential-case-dimensions case)
                     :valid-utf8))
         (args
          (append
           (list "--read0" "--print0" "--no-color" "--no-multi-line"
                 (format "--scheme=%s" score-scheme)
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
         (fzf-native-normalize
          (fzf-native-differential-query-normalize query))
         (fzf-native-search-direction
          (or (fzf-native-differential-query-direction query) 'auto))
         (fzf-native-score-scheme
          (or (fzf-native-differential-query-score-scheme query) 'default))
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
      (:normalize (fzf-native-differential-query-normalize query))
      (:direction
       (or (fzf-native-differential-query-direction query) 'auto))
      (:forward (fzf-native-differential-query-forward query))
      (:score-scheme
       (or (fzf-native-differential-query-score-scheme query) 'default))
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
                            seed serial 'parity)))
         (right
          (cl-loop for serial below 6000
                   collect (fzf-native-upstream--generated-case
                            seed serial 'parity)))
         (replay-start 733)
         (replay-count 20)
         (replay
          (cl-loop for serial from replay-start
                   below (+ replay-start replay-count)
                   collect (fzf-native-upstream--generated-case
                            seed serial 'parity)))
         (long-cases
          (cl-loop for serial below 3
                   collect (fzf-native-upstream--generated-case
                            seed serial 'long)))
         text-classes length-targets query-shapes primary-kinds rank-shapes
         normalize-modes direction-modes directions score-schemes
         normalization-pairs)
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
        (push (fzf-native-upstream--dimension-value case :normalize)
              normalize-modes)
        (push (fzf-native-upstream--dimension-value case :direction)
              direction-modes)
        (push (fzf-native-upstream--dimension-value case :forward)
              directions)
        (push (fzf-native-upstream--dimension-value case :score-scheme)
              score-schemes)
        (push (plist-get dimensions :normalization-pair)
              normalization-pairs)
        (pcase (fzf-native-upstream--dimension-value case :direction)
          ('auto
           (should
            (eq (fzf-native-upstream--dimension-value case :forward)
                (not (eq (fzf-native-upstream--dimension-value
                          case :score-scheme)
                         'path)))))
          ('forward
           (should (fzf-native-upstream--dimension-value case :forward)))
          ('backward
           (should-not
            (fzf-native-upstream--dimension-value case :forward))))
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
    (dolist (normalize '(nil t))
      (should (memq normalize normalize-modes)))
    (dolist (forward '(nil t))
      (should (memq forward directions)))
    (dolist (direction '(auto forward backward))
      (should (memq direction direction-modes)))
    (dolist (scheme '(default path history))
      (should (memq scheme score-schemes)))
    (should (memq t normalization-pairs))
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
       (:normalize (nil t))
       (:direction (auto forward backward))
       (:score-scheme (default path history))
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

(ert-deftest fzf-native-fuzz-upstream-malformed-ranking-uses-membership ()
  "Do not rank byte strings that Go rewrites while decoding malformed UTF-8."
  (let ((fzf (or (getenv "FZF_REFERENCE") (executable-find "fzf")))
        (case
         (fzf-native-upstream--generated-case 3735928559 9885 'parity)))
    (skip-unless fzf)
    (fzf-native-upstream--verify-reference fzf)
    (should-not
     (plist-get (fzf-native-differential-case-dimensions case) :valid-utf8))
    (should (eq (fzf-native-differential-case-comparison case) 'membership))
    (let ((native
           (fzf-native-upstream--identities
            case (fzf-native-upstream--native case)))
          (upstream
           (fzf-native-upstream--identities
            case (fzf-native-upstream--fzf fzf case) t)))
      (should
       (equal (fzf-native-upstream--membership native)
              (fzf-native-upstream--membership upstream)))
      (should-not (equal native upstream)))))

(ert-deftest fzf-native-fuzz-upstream-exceptions-are-narrow ()
  "Reject broad exceptions for ordinary common-profile membership."
  (let* ((seed 73)
         (common
          (cl-loop for serial below 100
                   for case = (fzf-native-upstream--generated-case
                               seed serial 'common)
                   when (plist-get
                         (fzf-native-differential-case-dimensions case)
                         :valid-utf8)
                   return case))
         (boundary
          (cl-loop for serial below 1000
                   for case = (fzf-native-upstream--generated-case
                               seed serial 'parity)
                   when (and
                         (fzf-native-differential--case-has-kind-p
                          case 'boundary-exact)
                         (fzf-native-differential-query-forward
                          (fzf-native-differential-case-query case))
                         (not
                          (fzf-native-differential-query-normalize
                           (fzf-native-differential-case-query case))))
                   return case))
         (ranking
          (cl-loop for serial below 1000
                   for case = (fzf-native-upstream--generated-case
                               seed serial 'parity)
                   when (eq (fzf-native-differential-case-comparison case)
                            'ranking)
                   return case))
         (normalized
          (cl-loop for serial below 1000
                   for case = (fzf-native-upstream--generated-case
                               seed serial 'parity)
                   when (and
                         (plist-get
                          (fzf-native-differential-case-dimensions case)
                          :valid-utf8)
                         (fzf-native-differential-query-normalize
                          (fzf-native-differential-case-query case)))
                   return case))
         (backward
          (cl-loop for serial below 1000
                   for case = (fzf-native-upstream--generated-case
                               seed serial 'parity)
                   when (and
                         (plist-get
                          (fzf-native-differential-case-dimensions case)
                          :valid-utf8)
                         (not (fzf-native-differential-query-forward
                               (fzf-native-differential-case-query case))))
                   return case))
         (malformed
          (cl-loop for serial below 1000
                   for case = (fzf-native-upstream--generated-case
                               seed serial 'common)
                   unless (plist-get
                           (fzf-native-differential-case-dimensions case)
                           :valid-utf8)
                   return case))
         (forged (copy-fzf-native-differential-case common))
         (malformed-candidate
          (cl-find-if
           (lambda (candidate)
             (fzf-native-differential--malformed-utf8-string-p
              (fzf-native-differential-candidate-text candidate)))
           (fzf-native-differential-case-candidates malformed)))
         (malformed-identity
          (fzf-native-differential-candidate-id
           (or malformed-candidate
               (car (fzf-native-differential-case-candidates malformed)))))
         malformed-exception)
    (setf (fzf-native-differential-case-dimensions forged)
          (plist-put
           (copy-sequence
            (fzf-native-differential-case-dimensions forged))
           :valid-utf8 nil))
    (should-not
     (fzf-native-differential-classify common 'membership))
    (should-not
     (fzf-native-differential-classify forged 'membership))
    (should-not
     (fzf-native-differential-classify boundary 'membership))
    (should-not
     (fzf-native-differential-classify
      ranking 'ranking '(:membership-equal nil)))
    (should-not
     (fzf-native-differential-classify
      ranking 'ranking '(:membership-equal t)))
    (should-not
     (fzf-native-differential-classify normalized 'membership))
    (should-not
     (fzf-native-differential-classify backward 'membership))
    (setq malformed-exception
          (fzf-native-differential-classify
           malformed 'membership
           (list :differing-identities (list malformed-identity))))
    (should (eq (plist-get malformed-exception :name)
                'malformed-utf8-decoder))
    (should (eq (plist-get malformed-exception :disposition)
                'accepted))
    (should
     (equal
      (mapcar (lambda (entry) (plist-get entry :name))
              (cl-remove-if-not
               (lambda (entry)
                 (eq (plist-get entry :disposition) 'accepted))
               fzf-native-differential-exceptions))
      '(malformed-utf8-decoder)))
    (dolist (entry fzf-native-differential-exceptions)
      (should
       (equal (plist-get entry :upstream-revision)
              fzf-native-differential-upstream-revision))
      (should (stringp (plist-get entry :owner)))
      (should (stringp (plist-get entry :remove-when))))
    (should
     (equal (plist-get malformed-exception :scope)
            "Only differing malformed inputs for membership or positions."))))

(ert-deftest fzf-native-fuzz-upstream-malformed-exceptions-are-difference-scoped ()
  "Do not let an unrelated malformed candidate waive a valid difference."
  (let* ((malformed-bytes (unibyte-string #xff))
         (valid-candidate
          (make-fzf-native-differential-candidate
           :id 1 :text "valid" :role 'match))
         (malformed-candidate
          (make-fzf-native-differential-candidate
           :id 2 :text malformed-bytes :role 'decoy))
         (query
          (make-fzf-native-differential-query
           :sets nil :normalize nil :forward t))
         (candidate-case
          (make-fzf-native-differential-case
           :query query
           :rendered-query ""
           :candidates (list valid-candidate malformed-candidate)
           :dimensions '(:valid-utf8 nil)))
         (query-case (copy-fzf-native-differential-case candidate-case))
         exception)
    (setf (fzf-native-differential-case-rendered-query query-case)
          malformed-bytes)
    ;; Candidate 2 is an unrelated malformed decoy when valid candidate 1 is
    ;; the identity missing from one result.
    (let ((difference
           (fzf-native-upstream--membership-difference '(2) '(1 2))))
      (should (equal difference '(1)))
      (should-not
       (fzf-native-differential-classify
        candidate-case 'membership
        (list :differing-identities difference))))
    (setq exception
          (fzf-native-differential-classify
           candidate-case 'membership '(:differing-identities (2))))
    (should (eq (plist-get exception :name) 'malformed-utf8-decoder))
    (should-not
     (fzf-native-differential-classify candidate-case 'membership))
    (should-not
     (fzf-native-differential-classify
      candidate-case 'membership '(:differing-identities (99))))
    (should-not
     (fzf-native-differential-classify
      candidate-case 'membership '(:differing-identities (1 2))))
    ;; A malformed query alone cannot attribute a valid-candidate difference
    ;; to decoder policy.  A malformed differing candidate remains in scope,
    ;; while missing and unknown identity context still fail closed.
    (should-not
     (fzf-native-differential-classify
      query-case 'membership '(:differing-identities (1))))
    (should
     (fzf-native-differential-classify
      query-case 'membership '(:differing-identities (2))))
    (should-not
     (fzf-native-differential-classify query-case 'membership))
    (should-not
     (fzf-native-differential-classify
      query-case 'membership '(:differing-identities (99))))))

(ert-deftest fzf-native-fuzz-upstream-exact-boundary-membership ()
  "Compare parsed trailing-quote boundary terms with the pinned fzf CLI."
  (let* ((fzf (or (getenv "FZF_REFERENCE") (executable-find "fzf")))
         (seed (fzf-native-upstream--env-integer
                "FZF_NATIVE_FUZZ_SEED" 12648430))
         (checked 0))
    (skip-unless fzf)
    (fzf-native-upstream--verify-reference fzf)
    (cl-loop
     for serial below 2000
     until (= checked 32)
     for case = (fzf-native-upstream--generated-case seed serial 'parity)
     for query = (fzf-native-differential-case-query case)
     when (and
           (eq (fzf-native-differential-case-comparison case) 'membership)
           (plist-get (fzf-native-differential-case-dimensions case)
                      :valid-utf8)
           (fzf-native-differential-query-forward query)
           (not (fzf-native-differential-query-normalize query))
           (fzf-native-differential--case-has-kind-p case 'boundary-exact))
     do
     (let ((native
            (fzf-native-upstream--membership
             (fzf-native-upstream--identities
              case (fzf-native-upstream--native case))))
           (upstream
            (fzf-native-upstream--membership
             (fzf-native-upstream--identities
              case (fzf-native-upstream--fzf fzf case) t))))
       (ert-info ((fzf-native-differential-case-description case))
         (should (equal native upstream)))
       (setq checked (1+ checked))))
    (should (= checked 32))))

(defconst fzf-native-upstream--session-candidate-bodies
  '("alpha"
    "alphabet soup"
    "alphanumeric"
    "alpine trail"
    "beta alpha"
    "beta"
    "gamma"
    "FOO alpha"
    "foo beta"
    "Food"
    "prefix FOO suffix"
    "σ alpha"
    "Σ beta"
    "κόσμος"
    "中文 alpha"
    "中間 beta"
    "😀 alpha"
    "café"
    "CAFÉ beta"
    "kelvin k"
    "Kelvin K"
    "literal pipe"
    "inverse bang")
  "Stable candidate bodies for persistent-session comparisons.")

(defconst fzf-native-upstream--session-rounds
  '((:name broad :query "a" :case-mode smart :fuzzy t)
    (:name narrow :query "al" :case-mode smart :fuzzy t)
    (:name narrower :query "alp" :case-mode smart :fuzzy t)
    (:name broaden :query "a" :case-mode smart :fuzzy t)
    (:name repeat :query "a" :case-mode smart :fuzzy t)
    (:name or :query "alpha | beta" :case-mode smart :fuzzy t)
    (:name inverse :query "a !beta" :case-mode smart :fuzzy t)
    (:name case-ignore :query "FOO" :case-mode ignore :fuzzy t)
    (:name case-respect :query "FOO" :case-mode respect :fuzzy t)
    (:name global-exact :query "alpha" :case-mode smart :fuzzy nil)
    (:name unicode-fold :query "σ" :case-mode ignore :fuzzy t)
    (:name unicode-case :query "Σ" :case-mode respect :fuzzy t)
    (:name unicode-cjk :query "中" :case-mode smart :fuzzy t)
    (:name unicode-kelvin :query "K" :case-mode ignore :fuzzy t))
  "Ordered query rounds for one persistent native session.")

(defun fzf-native-upstream--shuffle (rng values)
  "Return a deterministic shuffled copy of VALUES using RNG."
  (let ((items (vconcat values)))
    (cl-loop for index downfrom (1- (length items)) above 0
             for swap = (fzf-native-differential-random rng (1+ index))
             do (cl-rotatef (aref items index) (aref items swap)))
    (append items nil)))

(defun fzf-native-upstream--session-candidates (seed serial)
  "Return a deterministic candidate pool for SEED and SERIAL."
  (let ((rng (fzf-native-upstream--case-rng seed serial)) decoys)
    (dotimes (index 7)
      (push (format "noise-%08x-%02d-%02d"
                    (fzf-native-differential-random rng #xffffffff)
                    serial index)
            decoys))
    (cl-loop
     for text in
     (fzf-native-upstream--shuffle
      rng (append fzf-native-upstream--session-candidate-bodies decoys))
     for id from 0
     collect (make-fzf-native-differential-candidate
              :id id :text text :role 'session))))

(defun fzf-native-upstream--session-case
    (seed serial round candidates)
  "Return an oracle case for SEED, SERIAL, ROUND, and CANDIDATES."
  (let ((query
         (make-fzf-native-differential-query
          :sets nil
          :case-mode (plist-get round :case-mode)
          :fuzzy (plist-get round :fuzzy)
          :normalize nil
          :forward t)))
    (make-fzf-native-differential-case
     :seed seed
     :serial serial
     :profile 'session
     :query query
     :rendered-query (plist-get round :query)
     :candidates candidates
     :dimensions (list :round (plist-get round :name)
                       :valid-utf8 t
                       :producer-eof t)
     :comparison 'membership)))

(defun fzf-native-upstream--wait-for-producer-eof (handle &optional timeout)
  "Wait for producer EOF on HANDLE for at most TIMEOUT seconds."
  (let ((deadline (+ (float-time) (or timeout 10.0))) status)
    (while (and (< (float-time) deadline)
                (progn
                  (setq status (fzf-native-async-status handle))
                  (not (plist-get status :reader-done))))
      (sleep-for 0.01))
    (unless (and status (plist-get status :reader-done))
      (error "Timed out waiting for fzf-native producer EOF: %S" status))
    status))

(defun fzf-native-upstream--wait-for-session-request
    (handle request-id &optional timeout)
  "Wait for REQUEST-ID on HANDLE for at most TIMEOUT seconds."
  (let ((deadline (+ (float-time) (or timeout 10.0))) status)
    (while (and (< (float-time) deadline)
                (progn
                  (setq status (fzf-native-async-status handle request-id))
                  (or (memq (plist-get status :state) '(queued running))
                      (and (eq (plist-get status :state) 'complete)
                           (plist-get status :stale)))))
      (sleep-for 0.01))
    (when (or (memq (plist-get status :state) '(queued running))
              (and (eq (plist-get status :state) 'complete)
                   (plist-get status :stale)))
      (error "Timed out waiting for fzf-native request %d: %S"
             request-id status))
    (fzf-native-async-snapshot handle request-id)))

(defun fzf-native-upstream--write-session-input (file candidates)
  "Write CANDIDATES as UTF-8 lines to FILE."
  (let ((coding-system-for-write 'utf-8-unix))
    (with-temp-file file
      (dolist (candidate candidates)
        (insert (fzf-native-differential-candidate-text candidate) "\n")))))

(ert-deftest fzf-native-fuzz-upstream-session-rounds ()
  "Compare identity sets from one native session with fresh fzf processes."
  (let* ((fzf (or (getenv "FZF_REFERENCE") (executable-find "fzf")))
         (cat (executable-find "cat"))
         (seed (fzf-native-upstream--env-integer
                "FZF_NATIVE_FUZZ_SEED" 12648430))
         (cases (fzf-native-upstream--env-integer
                 "FZF_NATIVE_UPSTREAM_SESSION_CASES" 4))
         (start (fzf-native-upstream--env-integer
                 "FZF_NATIVE_UPSTREAM_SESSION_START" 0)))
    (skip-unless (and fzf cat
                      (fboundp 'fzf-native-async-start)
                      (fboundp 'fzf-native-async-submit)
                      (fboundp 'fzf-native-async-snapshot)))
    (should (> cases 0))
    (fzf-native-upstream--verify-reference fzf)
    (dotimes (iteration cases)
      (let* ((serial (+ start iteration))
             (candidates
              (fzf-native-upstream--session-candidates seed serial))
             (input (make-temp-file "fzf-native-upstream-session-"
                                    nil ".txt"))
             (starts (make-temp-file "fzf-native-upstream-starts-"
                                     nil ".txt"))
             handle
             (last-request-id 0))
        (unwind-protect
            (let ((fzf-native-max-line-length nil)
                  (fzf-native-async-cache-size 40)
                  (fzf-native-async-cache-bytes (* 4 1024 1024))
                  (fzf-native-async-batch-cache-bytes (* 4 1024 1024))
                  (fzf-native-filter-only-min-pool nil)
                  (fzf-native-filter-only-length nil)
                  (fzf-native-async-highlight nil))
              (fzf-native-upstream--write-session-input input candidates)
              (setq handle
                    (fzf-native-async-start
                     (format "printf 'start\\n' >> %s; exec %s -- %s"
                             (shell-quote-argument starts)
                             (shell-quote-argument cat)
                             (shell-quote-argument input))))
              (let ((producer
                     (fzf-native-upstream--wait-for-producer-eof handle)))
                (should (eq (plist-get producer :producer-state) 'complete))
                (should (= (plist-get producer :producer-exit-status) 0))
                (should (= (plist-get producer :pool-generation)
                           (length candidates)))
                (with-temp-buffer
                  (insert-file-contents-literally starts)
                  (should (equal (buffer-string) "start\n"))))
              (cl-loop
               for round in fzf-native-upstream--session-rounds
               for round-index from 0
               do
               (let* ((case
                       (fzf-native-upstream--session-case
                        seed serial round candidates))
                      (query (plist-get round :query))
                      (fzf-native-case-mode (plist-get round :case-mode))
                      (fzf-native-fuzzy (plist-get round :fuzzy))
                      (request-id
                       (fzf-native-async-submit handle query 0))
                      (snapshot
                       (fzf-native-upstream--wait-for-session-request
                        handle request-id))
                      ;; Ranking remains explicit parity debt.  This lane is
                      ;; strict about the complete set of candidate identities.
                      (native
                       (fzf-native-upstream--membership
                        (fzf-native-upstream--identities
                         case (plist-get snapshot :candidates))))
                      (upstream
                       (fzf-native-upstream--membership
                        (fzf-native-upstream--identities
                         case (fzf-native-upstream--fzf fzf case) t))))
                 (ert-info
                     ((format
                       "seed=%d serial=%d round=%d name=%S query=%S mode=%S fuzzy=%S"
                       seed serial round-index (plist-get round :name)
                       query fzf-native-case-mode fzf-native-fuzzy))
                   (if (eq (plist-get round :name) 'repeat)
                       (should (= request-id last-request-id))
                     (should (> request-id last-request-id)))
                   (should (eq (plist-get snapshot :state) 'complete))
                   (should (= (plist-get snapshot :result-request-id)
                              request-id))
                   (should-not (plist-get snapshot :stale))
                   (should (plist-get snapshot :reader-done))
                   (should (eq (plist-get snapshot :producer-state)
                               'complete))
                   (should (= (plist-get snapshot :pool-generation)
                              (length candidates)))
                   (should (= (plist-get snapshot :result-pool-generation)
                              (length candidates)))
                   (should (= (plist-get snapshot :total)
                              (length candidates)))
                   (should (= (plist-get snapshot :filtered)
                              (length native)))
                   (should (equal (plist-get snapshot :query) query))
                   (should (eq (plist-get snapshot :case-mode)
                               fzf-native-case-mode))
                   (should (eq (plist-get snapshot :fuzzy)
                               fzf-native-fuzzy))
                   (should upstream)
                   (should (equal native upstream)))
                 (setq last-request-id request-id))))
          (when handle
            (fzf-native-async-stop handle))
          (when (file-exists-p input)
            (delete-file input))
          (when (file-exists-p starts)
            (delete-file starts)))))))

(ert-deftest fzf-native-fuzz-upstream-structured-query-results ()
  "Compare deterministic parsed-query results with a pinned fzf CLI."
  (let* ((fzf (or (getenv "FZF_REFERENCE") (executable-find "fzf")))
         (seed (fzf-native-upstream--env-integer
                "FZF_NATIVE_FUZZ_SEED" 12648430))
         (cases (fzf-native-upstream--env-integer
                 "FZF_NATIVE_UPSTREAM_CASES" 200))
         (start (fzf-native-upstream--env-integer
                 "FZF_NATIVE_UPSTREAM_START" 0))
         (profile (fzf-native-upstream--profile))
         (exceptions (make-hash-table :test #'eq)))
    (skip-unless fzf)
    (fzf-native-upstream--verify-reference fzf)
    (dotimes (iteration cases)
      (let* ((serial (+ start iteration))
             (case
              (fzf-native-upstream--generated-case seed serial profile))
             (native-order
              (fzf-native-upstream--identities
               case (fzf-native-upstream--native case)))
             (upstream-order
             (fzf-native-upstream--identities
               case (fzf-native-upstream--fzf fzf case) t))
             (native-membership
              (fzf-native-upstream--membership native-order))
             (upstream-membership
              (fzf-native-upstream--membership upstream-order))
             (membership-equal
              (equal native-membership upstream-membership)))
        (ert-info ((fzf-native-differential-case-description case))
          (when (plist-get (fzf-native-differential-case-dimensions case)
                           :valid-utf8)
            (should (memq 0 upstream-membership)))
          (unless membership-equal
            (let ((exception
                   (fzf-native-differential-classify
                    case 'membership
                    (list
                     :differing-identities
                     (fzf-native-upstream--membership-difference
                      native-membership upstream-membership)))))
              (if exception
                  (progn
                    (fzf-native-upstream--record-exception exceptions exception)
                    (unless (fzf-native-upstream--accepted-exception-p exception)
                      (should (equal native-membership upstream-membership))))
                (should (equal native-membership upstream-membership)))))
          (when (and membership-equal
                     (eq (fzf-native-differential-case-comparison case)
                         'ranking)
                     (not (equal native-order upstream-order)))
            (let ((exception
                   (fzf-native-differential-classify
                    case 'ranking '(:membership-equal t))))
              (if exception
                  (progn
                    (fzf-native-upstream--record-exception exceptions exception)
                    (unless (fzf-native-upstream--accepted-exception-p exception)
                      (should (equal native-order upstream-order))))
                (should (equal native-order upstream-order))))))))
    (let ((summary (fzf-native-upstream--exception-alist exceptions)))
      (when (eq profile 'common)
        (dolist (count summary)
          (let ((entry
                 (cl-find (car count) fzf-native-differential-exceptions
                          :key (lambda (item) (plist-get item :name)))))
            (should (fzf-native-upstream--accepted-exception-p entry)))))
      (message
       "fzf-native upstream differential seed=%d start=%d profile=%S cases=%d exceptions=%S"
       seed start profile cases summary))))

(provide 'fzf-native-upstream-test)
;;; fzf-native-upstream-test.el ends here
