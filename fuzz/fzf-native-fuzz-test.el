;;; fzf-native-fuzz-test.el --- Randomized public-ABI properties -*- lexical-binding: t; -*-

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

;; Exercise randomized properties of the public scoring and highlighting API.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'fzf-native)

(declare-function fzf-native-score "fzf-native-module"
                  (string query &optional slab))
(declare-function fzf-native-score-all "fzf-native-module"
                  (collection query &optional slab))

(let ((module (getenv "FZF_NATIVE_TEST_MODULE")))
  (if (and module (not (string-empty-p module)))
      (progn
        (module-load module)
        (setq fzf-native-loaded t))
    (fzf-native-load-dyn)))

(defvar fzf-native-fuzz--state 1)

(defun fzf-native-fuzz--env-integer (name default)
  "Read non-negative integer NAME, or return DEFAULT."
  (let ((value (getenv name)))
    (if (and value (string-match-p "\\`[0-9]+\\'" value))
        (string-to-number value)
      default)))

(defun fzf-native-fuzz--seed (seed)
  "Initialize the deterministic generator with SEED."
  (setq fzf-native-fuzz--state (logand (max seed 1) #xffffffff)))

(defun fzf-native-fuzz--random (limit)
  "Return a deterministic integer in [0, LIMIT)."
  (let ((x fzf-native-fuzz--state))
    (setq x (logxor x (ash x 13)))
    (setq x (logxor x (ash x -17)))
    (setq x (logxor x (ash x 5)))
    (setq fzf-native-fuzz--state (logand x #xffffffff))
    (if (<= limit 0) 0 (% fzf-native-fuzz--state limit))))

(defconst fzf-native-fuzz--candidate-pieces
  ["a" "b" "c" "F" "K" "-" "_" "/" "." " " "\t"])

(defconst fzf-native-fuzz--query-pieces
  ["a" "b" "c" "f" "F" "k" "K" "foo" "bar"])

(defun fzf-native-fuzz--candidate ()
  "Generate one short ASCII candidate."
  (let ((count (fzf-native-fuzz--random 12)) pieces)
    (dotimes (_ count)
      (push (aref fzf-native-fuzz--candidate-pieces
                  (fzf-native-fuzz--random
                   (length fzf-native-fuzz--candidate-pieces)))
            pieces))
    (apply #'concat (nreverse pieces))))

(defun fzf-native-fuzz--literal ()
  "Generate a nonempty operator-free query literal."
  (let ((count (1+ (fzf-native-fuzz--random 3))) pieces)
    (dotimes (_ count)
      (push (aref fzf-native-fuzz--query-pieces
                  (fzf-native-fuzz--random
                   (length fzf-native-fuzz--query-pieces)))
            pieces))
    (apply #'concat (nreverse pieces))))

(defun fzf-native-fuzz--term ()
  "Generate one extended-search term."
  (concat (aref ["" "" "'" "^"]
                (fzf-native-fuzz--random 4))
          (fzf-native-fuzz--literal)
          ""))

(defun fzf-native-fuzz--query ()
  "Generate a small extended-search query."
  (let ((count (1+ (fzf-native-fuzz--random 4))) terms)
    (dotimes (i count)
      (push (fzf-native-fuzz--term) terms)
      (when (and (< i (1- count))
                 (zerop (fzf-native-fuzz--random 4)))
        (push "|" terms)))
    (mapconcat #'identity (nreverse terms) " ")))

(defun fzf-native-fuzz--copies (collection)
  "Return fresh copies of strings in COLLECTION."
  (mapcar #'copy-sequence collection))

(defun fzf-native-fuzz--keys (collection)
  "Return a sorted, property-free multiset for COLLECTION."
  (sort (mapcar (lambda (string)
                  (substring-no-properties string))
                (append collection nil))
        #'string<))

(defun fzf-native-fuzz--scalar-matches (collection query)
  "Return members of COLLECTION with positive scalar scores for QUERY."
  (let ((fzf-native-batch-highlight nil)
        (fzf-native-highlight-fn nil))
    (cl-loop for candidate in collection
             when (> (car (fzf-native-score candidate query)) 0)
             collect candidate)))

(defun fzf-native-fuzz--score-all (collection query highlight)
  "Score COLLECTION for QUERY with explicit HIGHLIGHT behavior."
  (let ((fzf-native-filter-only-min-pool nil)
        (fzf-native-filter-only-length nil)
        (fzf-native-batch-highlight (and highlight t))
        (fzf-native-highlight-fn
         (and highlight #'fzf-native-default-highlight-fn)))
    (fzf-native-score-all collection query)))

(ert-deftest fzf-native-fuzz-public-abi-properties ()
  "Check scalar, batch, collection-shape, filter, and highlight agreement."
  (let* ((seed (fzf-native-fuzz--env-integer
                "FZF_NATIVE_FUZZ_SEED" 12648430))
         (cases (fzf-native-fuzz--env-integer
                 "FZF_NATIVE_FUZZ_CASES" 250)))
    (fzf-native-fuzz--seed seed)
    (dotimes (iteration cases)
      (let* ((fzf-native-case-mode
              (aref [smart ignore respect]
                    (fzf-native-fuzz--random 3)))
             (fzf-native-fuzzy
              (not (zerop (fzf-native-fuzz--random 2))))
             (collection
              (cl-loop repeat (fzf-native-fuzz--random 24)
                       collect (fzf-native-fuzz--candidate)))
             (query (fzf-native-fuzz--query)))
        (ert-info ((format "seed=%d iteration=%d mode=%S fuzzy=%S query=%S collection=%S"
                           seed iteration fzf-native-case-mode
                           fzf-native-fuzzy query collection))
          (let* ((scalar (fzf-native-fuzz--keys
                          (fzf-native-fuzz--scalar-matches collection query)))
                 (list-full
                  (fzf-native-fuzz--keys
                   (fzf-native-fuzz--score-all
                    (fzf-native-fuzz--copies collection) query nil)))
                 (vector-full
                  (fzf-native-fuzz--keys
                   (fzf-native-fuzz--score-all
                    (vconcat (fzf-native-fuzz--copies collection))
                    query nil)))
                 (highlight-input (fzf-native-fuzz--copies collection))
                 (highlighted
                  (fzf-native-fuzz--score-all
                   highlight-input query t)))
            (should (equal scalar list-full))
            (should (equal list-full vector-full))
            (should (equal list-full
                           (fzf-native-fuzz--keys highlighted)))
            (dolist (candidate highlight-input)
              (should-not
               (text-property-not-all 0 (length candidate) 'face nil
                                      candidate)))))))))

(provide 'fzf-native-fuzz-test)
;;; fzf-native-fuzz-test.el ends here
