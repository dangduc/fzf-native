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
(require 'subr-x)

(declare-function fzf-native-score "fzf-native-module"
                  (string query &optional slab))
(declare-function fzf-native-score-all "fzf-native-module"
                  (collection query &optional slab))
(declare-function fzf-native-highlight-one "fzf-native-module" (cand query))
(declare-function fzf-native-make-slab "fzf-native-module" (size16 size32))
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

(defvar fzf-native-fuzz--state 1)

(defvar fzf-native-fuzz--allow-malformed t
  "When non-nil, generated strings can contain arbitrary unibyte data.")

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
  ["a" "b" "c" "F" "K" "-" "_" "/" "." " " "\t"
   "é" "é" "σ" "Σ" "你" "中文" "😀" "𐐷" "K" "Ⱥ" "ⱥ"])

(defconst fzf-native-fuzz--query-pieces
  ["a" "b" "c" "f" "F" "k" "K" "foo" "bar"
   "é" "é" "σ" "Σ" "你" "😀" "𐐷" "K" "Ⱥ" "ⱥ"])

(defconst fzf-native-fuzz--raw-bytes
  [1 9 32 65 127 128 191 192 193 224 237 240 245 254 255])

(defun fzf-native-fuzz--unibyte-string ()
  "Generate a short string with arbitrary bytes, including malformed UTF-8."
  (apply #'unibyte-string
         (cl-loop repeat (fzf-native-fuzz--random 9)
                  collect (aref fzf-native-fuzz--raw-bytes
                                (fzf-native-fuzz--random
                                 (length fzf-native-fuzz--raw-bytes))))))

(defun fzf-native-fuzz--candidate ()
  "Generate one short candidate across valid and malformed text classes."
  (if (and fzf-native-fuzz--allow-malformed
           (zerop (fzf-native-fuzz--random 8)))
      (fzf-native-fuzz--unibyte-string)
    (let ((count (fzf-native-fuzz--random 12)) pieces)
      (dotimes (_ count)
        (push (aref fzf-native-fuzz--candidate-pieces
                    (fzf-native-fuzz--random
                     (length fzf-native-fuzz--candidate-pieces)))
              pieces))
      (apply #'concat (nreverse pieces)))))

(defun fzf-native-fuzz--literal ()
  "Generate a nonempty operator-free query literal."
  (if (and fzf-native-fuzz--allow-malformed
           (zerop (fzf-native-fuzz--random 16)))
      (let ((raw (fzf-native-fuzz--unibyte-string)))
        (if (string-empty-p raw) (unibyte-string 255) raw))
    (let ((count (1+ (fzf-native-fuzz--random 3))) pieces)
      (dotimes (_ count)
        (push (aref fzf-native-fuzz--query-pieces
                    (fzf-native-fuzz--random
                     (length fzf-native-fuzz--query-pieces)))
              pieces))
      (apply #'concat (nreverse pieces)))))

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

(defun fzf-native-fuzz--common-part-positions (string)
  "Return character positions highlighted on STRING by fzf-native."
  (cl-loop for index below (length string)
           for face = (get-text-property index 'face string)
           when (or (eq face 'completions-common-part)
                    (and (listp face)
                         (memq 'completions-common-part face)))
           collect index))

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
            (dolist (candidate highlighted)
              (let ((scalar-highlight
                     (fzf-native-highlight-one
                      (substring-no-properties candidate) query)))
                (should
                 (equal
                  (fzf-native-fuzz--common-part-positions candidate)
                  (fzf-native-fuzz--common-part-positions
                   scalar-highlight)))))
            (dolist (candidate highlight-input)
              (should-not
               (text-property-not-all 0 (length candidate) 'face nil
                                      candidate)))))))))

(ert-deftest fzf-native-fuzz-public-abi-rejects-invalid-values ()
  "Generate invalid types and arities across public native entry points."
  (let ((bad-strings
         (vector nil t 17 1.5 'fzf-native-bad '("list") ["vector"]
                 (make-hash-table)))
        (bad-collections
         (vector t 17 1.5 'fzf-native-bad '("valid" . "bad-tail")
                 (make-hash-table)))
        (bad-pointers
         (vector t 17 1.5 'fzf-native-bad '("list") ["vector"]
                 (make-hash-table)))
        (bad-sizes
         (vector nil t -1 1.5 'fzf-native-bad '("list") ["vector"]
                 (make-hash-table))))
    (fzf-native-fuzz--seed
     (fzf-native-fuzz--env-integer "FZF_NATIVE_FUZZ_SEED" 12648430))
    (dotimes (_ (fzf-native-fuzz--env-integer
                 "FZF_NATIVE_FUZZ_ABI_CASES" 200))
      (let ((bad-string
             (aref bad-strings
                   (fzf-native-fuzz--random (length bad-strings))))
            (bad-collection
             (aref bad-collections
                   (fzf-native-fuzz--random (length bad-collections))))
            (bad-pointer
             (aref bad-pointers
                   (fzf-native-fuzz--random (length bad-pointers))))
            (bad-size
             (aref bad-sizes
                   (fzf-native-fuzz--random (length bad-sizes)))))
        (pcase (fzf-native-fuzz--random 11)
          (0 (should-error (fzf-native-score bad-string "a")))
          (1 (should-error (fzf-native-score "a" bad-string)))
          (2 (should-error (fzf-native-score "a" "a" bad-pointer)))
          (3 (should-error (fzf-native-score-all bad-collection "a")))
          (4 (should
              (listp
               (fzf-native-score-all (list "a" bad-string) "a"))))
          (5 (should-error (fzf-native-make-slab bad-size 1)))
          (6 (should-error (fzf-native-make-slab 1 bad-size)))
          (7 (when (fboundp 'fzf-native-async-submit)
               (should-error (fzf-native-async-submit bad-pointer "a" 1))))
          (8 (when (fboundp 'fzf-native-async-status)
               (should-error (fzf-native-async-status bad-pointer))))
          (9 (when (fboundp 'fzf-native-async-start)
               (should-error (fzf-native-async-start bad-string))))
          (10 (when (fboundp 'fzf-native-async-submit)
                (let ((handle
                       (fzf-native-async-start "printf '%s\\n' value")))
                  (unwind-protect
                      (should-error
                       (fzf-native-async-submit handle bad-string 1))
                    (fzf-native-async-stop handle))))))))
    (should-error (fzf-native-score "only-one-argument"))
    (should-error (fzf-native-score "a" "a" nil nil))
    (should-error (fzf-native-score-all '("a")))
    (should-error (fzf-native-make-slab 1 2 3))))

(ert-deftest fzf-native-fuzz-public-abi-rejects-embedded-nul ()
  "Keep the public module's explicit embedded-NUL boundary fail-closed."
  (let ((nul (concat "before" "\0" "after")))
    (should-error (fzf-native-score nul "a"))
    (should-error (fzf-native-score "a" nul))
    (should-error (fzf-native-score-all (list nul) "a"))
    (should-error (fzf-native-score-all '("a") nul))))

(defun fzf-native-fuzz--wait-for-producer (handle)
  "Return the terminal producer status for HANDLE, or signal on timeout."
  (let ((deadline (+ (float-time) 10.0)) status)
    (while (and (< (float-time) deadline)
                (progn
                  (setq status (fzf-native-async-status handle))
                  (not (plist-get status :reader-done))))
      (sleep-for 0.01))
    (unless (plist-get status :reader-done)
      (error "Timed out while waiting for the generated producer"))
    status))

(defun fzf-native-fuzz--wait-for-request (handle request-id)
  "Return the terminal snapshot for REQUEST-ID on HANDLE."
  (let ((deadline (+ (float-time) 10.0)) snapshot)
    (while (and (< (float-time) deadline)
                (progn
                  (setq snapshot
                        (fzf-native-async-snapshot handle request-id))
                  (memq (plist-get snapshot :state) '(queued running))))
      (sleep-for 0.01))
    (unless (eq (plist-get snapshot :state) 'complete)
      (error "Request %S did not complete: %S" request-id snapshot))
    snapshot))

(ert-deftest fzf-native-fuzz-interactive-abi-matches-batch-membership ()
  "Compare generated persistent-session rounds with the batch native API."
  (skip-unless (fzf-native--session-platform-p))
  (should (fzf-native--verify-session-abi))
  (let* ((seed (fzf-native-fuzz--env-integer
                "FZF_NATIVE_FUZZ_SEED" 12648430))
         (rounds (fzf-native-fuzz--env-integer
                  "FZF_NATIVE_FUZZ_SESSION_CASES" 100))
         (fzf-native-fuzz--allow-malformed nil)
         collection
         (input (make-temp-file "fzf-native-abi-fuzz-"))
         handle)
    (fzf-native-fuzz--seed (logxor seed #x51a7e))
    (setq collection (cl-loop repeat 128
                              collect (fzf-native-fuzz--candidate)))
    (unwind-protect
        (progn
          (let ((coding-system-for-write 'utf-8-unix))
            (write-region (concat (mapconcat #'identity collection "\n") "\n")
                          nil input nil 'silent))
          (setq handle
                (fzf-native-async-start
                 (concat "cat " (shell-quote-argument input))))
          (let ((producer (fzf-native-fuzz--wait-for-producer handle)))
            (should (eq (plist-get producer :producer-state) 'complete))
            (should (= (plist-get producer :pool-generation)
                       (length collection))))
          (dotimes (iteration rounds)
            (let* ((fzf-native-case-mode
                    (aref [smart ignore respect]
                          (fzf-native-fuzz--random 3)))
                   (fzf-native-fuzzy
                    (not (zerop (fzf-native-fuzz--random 2))))
                   (fzf-native-async-highlight nil)
                   (fzf-native-filter-only-min-pool nil)
                   (fzf-native-filter-only-length nil)
                   (query (fzf-native-fuzz--query))
                   (expected
                    (fzf-native-fuzz--keys
                     (fzf-native-fuzz--score-all
                      (fzf-native-fuzz--copies collection) query nil)))
                   (request-id (fzf-native-async-submit handle query 0))
                   (snapshot
                    (fzf-native-fuzz--wait-for-request handle request-id))
                   (actual
                    (fzf-native-fuzz--keys
                     (plist-get snapshot :candidates))))
              (ert-info ((format
                          "seed=%d round=%d mode=%S fuzzy=%S query=%S"
                          seed iteration fzf-native-case-mode
                          fzf-native-fuzzy query))
                (should-not (plist-get snapshot :stale))
                (should (equal expected actual)))))
          (fzf-native-async-stop handle)
          (setq handle nil))
      (when handle
        (ignore-errors (fzf-native-async-stop handle)))
      (when (file-exists-p input)
        (delete-file input)))))

(provide 'fzf-native-fuzz-test)
;;; fzf-native-fuzz-test.el ends here
