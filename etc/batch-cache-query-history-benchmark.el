;;; batch-cache-query-history-benchmark.el --- stable-cache scale probe -*- lexical-binding: t; -*-

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

;; Run `make benchmark-batch-cache-history' from the repository root.

;;; Code:

(require 'cl-lib)

(declare-function fzf-native-async-start "fzf-native-module" (command &optional dir))
(declare-function fzf-native-async-status "fzf-native-module"
                  (handle &optional request-id))
(declare-function fzf-native-async-stop "fzf-native-module" (handle))
(declare-function fzf-native-async-submit "fzf-native-module"
                  (handle query &optional limit))

(let* ((script-directory
        (file-name-directory (or load-file-name buffer-file-name)))
       (source-directory
        (or (getenv "FZF_NATIVE_SOURCE_DIR")
            (expand-file-name ".." script-directory)))
       (module
        (or (getenv "FZF_NATIVE_TEST_MODULE")
            (expand-file-name "build/bench-module/fzf-native-module.so"
                              source-directory))))
  (load-file (expand-file-name "fzf-native.el" source-directory))
  (module-load module))

(setq fzf-native-max-line-length nil
      fzf-native-async-highlight nil
      fzf-native-batch-highlight nil
      fzf-native-filter-only-min-pool nil
      fzf-native-filter-only-length nil
      fzf-native-async-cache-size 0
      fzf-native-async-cache-bytes 0
      fzf-native-async-batch-cache-bytes (* 64 1024 1024))

(defun fzf-native-bench--wait-for-producer (handle)
  "Wait for the producer owned by HANDLE to finish."
  (let ((deadline (+ (float-time) 30.0)) status)
    (while (and (< (float-time) deadline)
                (not (plist-get
                      (setq status (fzf-native-async-status handle))
                      :reader-done)))
      (sleep-for 0.0005))
    (unless (plist-get status :reader-done)
      (error "The producer did not finish: %S" status))))

(defun fzf-native-bench--run-query (handle index)
  "Run query INDEX through HANDLE and return elapsed milliseconds."
  (let* ((query (format "never-match-%08d-ZZ" index))
         (started (float-time))
         (request-id (fzf-native-async-submit handle query 20))
         (deadline (+ started 30.0))
         status)
    (while (and (< (float-time) deadline)
                (memq (plist-get
                       (setq status
                             (fzf-native-async-status handle request-id))
                       :state)
                      '(queued running)))
      (sleep-for 0.0001))
    (unless (and (eq (plist-get status :state) 'complete)
                 (zerop (plist-get status :filtered)))
      (error "Query %d failed: %S" index status))
    (* 1000.0 (- (float-time) started))))

(defun fzf-native-bench--report-window (samples start end)
  "Print timing summary for SAMPLES between START and END."
  (let* ((values (append (cl-subseq samples start end) nil))
         (sorted (sort (copy-sequence values) #'<)))
    (princ
     (format "range=%d..%d mean-ms=%.6f median-ms=%.6f max-ms=%.6f\n"
             start end
             (/ (apply #'+ values) (float (length values)))
             (nth (/ (length sorted) 2) sorted)
             (apply #'max values)))))

(let* ((rounds (string-to-number
                (or (getenv "FZF_NATIVE_BENCH_ROUNDS") "10000")))
       (handle
        (fzf-native-async-start
         (concat "awk 'BEGIN { for (i=0; i<2048; i++) "
                 "printf \"candidate-%06d-alpha\\n\", i }'"))))
  (unwind-protect
      (progn
        (fzf-native-bench--wait-for-producer handle)
        (let ((samples (make-vector rounds 0.0)))
          (dotimes (index rounds)
            (aset samples index
                  (fzf-native-bench--run-query handle index)))
          (dolist (start (delete-dups
                          (list 0
                                (max 0 (- (/ rounds 2) 50))
                                (max 0 (- rounds 100)))))
            (fzf-native-bench--report-window
             samples start (min rounds (+ start 100))))
          (let ((status (fzf-native-async-status handle)))
            (princ
             (format (concat "cache queries=%S entries=%S bytes=%S "
                             "hits=%S misses=%S evictions=%S\n")
                     (plist-get status :batch-cache-queries)
                     (plist-get status :batch-cache-entries)
                     (plist-get status :batch-cache-bytes)
                     (plist-get status :batch-cache-hits)
                     (plist-get status :batch-cache-misses)
                     (plist-get status :batch-cache-evictions))))))
    (fzf-native-async-stop handle)))

;;; batch-cache-query-history-benchmark.el ends here
