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
