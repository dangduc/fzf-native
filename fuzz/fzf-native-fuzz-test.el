;;; fzf-native-fuzz-test.el --- Randomized module properties -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

(require 'cl-lib)
(require 'ert)
(require 'fzf-native)

(declare-function fzf-native-score "fzf-native-module" (string query &optional slab))
(declare-function fzf-native-score-all "fzf-native-module" (collection query &optional slab))
(declare-function fzf-native-async-start "fzf-native-module" (command &optional directory))
(declare-function fzf-native-async-stop "fzf-native-module" (handle))
(declare-function fzf-native-async-generation "fzf-native-module" (handle))
(declare-function fzf-native-async-candidates "fzf-native-module" (handle query &optional limit))
(declare-function fzf-native-async-result-fresh-p "fzf-native-module" (handle query))

(fzf-native-load-dyn)

(defvar fzf-native-fuzz--state 1)

(defun fzf-native-fuzz--env-integer (name default)
  "Read positive integer NAME from the environment, or return DEFAULT."
  (let ((value (getenv name)))
    (if (and value (string-match-p "\\`[0-9]+\\'" value))
        (string-to-number value)
      default)))

(defun fzf-native-fuzz--seed (seed)
  "Initialize the deterministic fuzz PRNG from SEED."
  (setq fzf-native-fuzz--state (logand (max seed 1) #xffffffff)))

(defun fzf-native-fuzz--next ()
  "Return the next deterministic unsigned 32-bit fuzz value."
  (let ((x fzf-native-fuzz--state))
    (setq x (logxor x (ash x 13)))
    (setq x (logxor x (ash x -17)))
    (setq x (logxor x (ash x 5)))
    (setq fzf-native-fuzz--state (logand x #xffffffff))))

(defun fzf-native-fuzz--random (limit)
  "Return a deterministic integer in [0, LIMIT)."
  (if (<= limit 0) 0 (% (fzf-native-fuzz--next) limit)))

(defconst fzf-native-fuzz--candidate-pieces
  ["a" "b" "c" "F" "K" "-" "_" "/" "." " " "\t"
   "é" "É" "K" "İ" "ı" "Σ" "σ" "ς" "ẞ" "ß" "Å" "å" "Å"
   "Ａ" "ａ" "𐐀" "𐐨" "你" "好" "界" "λ" "Λ" "ж" "Ж"
   "א" "ع" "가" "🚀" "🧑‍💻" "👩🏽‍💻" "é" "\0"])

(defconst fzf-native-fuzz--query-pieces
  ["a" "b" "c" "f" "F" "k" "K" "é" "É" "K" "İ" "ı" "Σ" "σ"
   "ς" "ẞ" "ß" "Å" "å" "Å" "Ａ" "ａ" "𐐀" "𐐨" "你" "好"
   "λ" "Λ" "ж" "Ж" "א" "ع" "가" "🚀" "👩🏽‍💻"])

(defun fzf-native-fuzz--valid-string (&optional max-pieces)
  "Generate a valid Emacs string of at most MAX-PIECES pieces."
  (let ((count (fzf-native-fuzz--random (1+ (or max-pieces 12))))
        pieces)
    (dotimes (_ count)
      (push (aref fzf-native-fuzz--candidate-pieces
                  (fzf-native-fuzz--random
                   (length fzf-native-fuzz--candidate-pieces)))
            pieces))
    (apply #'concat (nreverse pieces))))

(defun fzf-native-fuzz--unibyte-string ()
  "Generate an arbitrary short unibyte string, including invalid UTF-8."
  (let ((bytes [0 1 9 32 65 97 127 128 191 192 224 245 254 255])
        (count (fzf-native-fuzz--random 12))
        result)
    (dotimes (_ count)
      (push (aref bytes (fzf-native-fuzz--random (length bytes))) result))
    (apply #'unibyte-string (nreverse result))))

(defun fzf-native-fuzz--candidate ()
  "Generate a valid multibyte or arbitrary unibyte candidate."
  (if (zerop (fzf-native-fuzz--random 5))
      (fzf-native-fuzz--unibyte-string)
    (fzf-native-fuzz--valid-string)))

(defun fzf-native-fuzz--literal ()
  "Generate a non-empty operator-free query literal."
  (let ((count (1+ (fzf-native-fuzz--random 4))) pieces)
    (dotimes (_ count)
      (push (aref fzf-native-fuzz--query-pieces
                  (fzf-native-fuzz--random
                   (length fzf-native-fuzz--query-pieces)))
            pieces))
    (apply #'concat (nreverse pieces))))

(defun fzf-native-fuzz--term ()
  "Generate one extended-search term."
  (let* ((literal (fzf-native-fuzz--literal))
         (prefix (aref ["" "" "'" "^" "!"]
                       (fzf-native-fuzz--random 5)))
         (suffix (if (zerop (fzf-native-fuzz--random 5)) "$" "")))
    (concat prefix literal suffix)))

(defun fzf-native-fuzz--query ()
  "Generate a non-empty basic or extended fzf query."
  (let ((terms (1+ (fzf-native-fuzz--random 4))) result)
    (dotimes (i terms)
      (push (fzf-native-fuzz--term) result)
      (when (and (< i (1- terms))
                 (zerop (fzf-native-fuzz--random 4)))
        (push "|" result)))
    (mapconcat #'identity (nreverse result) " ")))

(defun fzf-native-fuzz--copies (collection)
  "Return fresh copies of every string in COLLECTION."
  (mapcar #'copy-sequence collection))

(defun fzf-native-fuzz--keys (collection)
  "Return a sorted, property-free representation of COLLECTION."
  (sort (mapcar (lambda (string)
                  (prin1-to-string (substring-no-properties string)))
                (append collection nil))
        #'string<))

(defun fzf-native-fuzz--scalar-matches (collection query)
  "Return COLLECTION members whose scalar score for QUERY is positive."
  ;; Scalar scoring can apply the optional highlight hook directly to its
  ;; argument.  Disable that independent side effect while checking match-set
  ;; equivalence; batch copy isolation is asserted separately below.
  (let ((fzf-native-batch-highlight nil)
        (fzf-native-highlight-fn nil))
    (cl-loop for candidate in collection
             when (> (car (fzf-native-score candidate query)) 0)
             collect candidate)))

(defun fzf-native-fuzz--score-all (collection query filter-only highlight)
  "Score COLLECTION for QUERY with explicit FILTER-ONLY and HIGHLIGHT modes."
  (let ((fzf-native-filter-only-min-pool (and filter-only 1))
        (fzf-native-filter-only-length nil)
        (fzf-native-filter-only-logic 'or)
        (fzf-native-batch-highlight (and highlight t))
        (fzf-native-highlight-fn
         (and highlight #'fzf-native-default-highlight-fn)))
    (fzf-native-score-all collection query)))

(ert-deftest fzf-native-fuzz-module-properties ()
  "Randomized scalar/batch/filter/highlight properties at the module ABI."
  (let* ((seed (fzf-native-fuzz--env-integer "FZF_NATIVE_FUZZ_SEED" 12648430))
         (cases (fzf-native-fuzz--env-integer "FZF_NATIVE_FUZZ_CASES" 250))
         (trace (fzf-native-fuzz--env-integer
                 "FZF_NATIVE_FUZZ_TRACE_ITERATION" -1)))
    (fzf-native-fuzz--seed seed)
    (dotimes (iteration cases)
      (let* ((fzf-native-case-mode
              (aref [smart ignore respect] (fzf-native-fuzz--random 3)))
             (fzf-native-fuzzy (not (zerop (fzf-native-fuzz--random 2))))
             (count (fzf-native-fuzz--random 24))
             (collection (cl-loop repeat count
                                  collect (fzf-native-fuzz--candidate)))
             (query (fzf-native-fuzz--query)))
        (when (= iteration trace)
          (message "fzf-native fuzz trace: seed=%d iteration=%d mode=%S fuzzy=%S query=%S collection=%S"
                   seed iteration fzf-native-case-mode fzf-native-fuzzy
                   query collection))
        (ert-info ((format "seed=%d iteration=%d mode=%S fuzzy=%S query=%S collection=%S"
                           seed iteration fzf-native-case-mode fzf-native-fuzzy
                           query collection))
          (let* ((scalar (fzf-native-fuzz--keys
                          (fzf-native-fuzz--scalar-matches collection query)))
                 (list-full (fzf-native-fuzz--keys
                             (fzf-native-fuzz--score-all
                              (fzf-native-fuzz--copies collection)
                              query nil nil)))
                 (vector-full (fzf-native-fuzz--keys
                               (fzf-native-fuzz--score-all
                                (vconcat (fzf-native-fuzz--copies collection))
                                query nil nil)))
                 (filter-only (fzf-native-fuzz--keys
                               (fzf-native-fuzz--score-all
                                (fzf-native-fuzz--copies collection)
                                query t nil)))
                 (highlighted-input (fzf-native-fuzz--copies collection))
                 (highlighted (fzf-native-fuzz--score-all
                               highlighted-input query nil t)))
            (should (equal scalar list-full))
            (should (equal list-full vector-full))
            (should (equal list-full filter-only))
            (should (equal list-full (fzf-native-fuzz--keys highlighted)))
            ;; Highlighting must never mutate caller-owned face properties.
            (dolist (candidate highlighted-input)
              (should-not
               (text-property-not-all 0 (length candidate) 'face nil
                                      candidate)))))))))

(defun fzf-native-fuzz--wait-until (predicate timeout)
  "Wait up to TIMEOUT seconds for PREDICATE to become non-nil."
  (let ((deadline (+ (float-time) timeout)) result)
    (while (and (not (setq result (funcall predicate)))
                (< (float-time) deadline))
      (accept-process-output nil 0.005))
    result))

(defun fzf-native-fuzz--async-result (handle query limit)
  "Return a fresh async result from HANDLE for QUERY, capped at LIMIT."
  (fzf-native-async-candidates handle query limit)
  (unless (fzf-native-fuzz--wait-until
           (lambda ()
             (fzf-native-async-candidates handle query limit)
             (fzf-native-async-result-fresh-p handle query))
           5.0)
    (error "async result did not become fresh for query %S" query))
  (fzf-native-async-candidates handle query limit))

(ert-deftest fzf-native-fuzz-async-state-machine ()
  "Exercise repeated/refined/broadened/reordered queries, GC, and stop."
  (skip-unless (and (fboundp 'fzf-native-async-start)
                    (executable-find "cat")))
  (let* ((seed (fzf-native-fuzz--env-integer "FZF_NATIVE_FUZZ_SEED" 12648430))
         (sessions (fzf-native-fuzz--env-integer
                    "FZF_NATIVE_FUZZ_ASYNC_SESSIONS" 8)))
    (fzf-native-fuzz--seed (logxor seed #x9e3779b9))
    (dotimes (iteration sessions)
      (let* ((fzf-native-case-mode
              (aref [smart ignore respect] (fzf-native-fuzz--random 3)))
             (fzf-native-fuzzy t)
             (fzf-native-filter-only-min-pool nil)
             (fzf-native-filter-only-length nil)
             (fzf-native-batch-highlight nil)
             (fzf-native-async-highlight nil)
             (fzf-native-max-line-length nil)
             (collection
              (cl-loop repeat (+ 12 (fzf-native-fuzz--random 24))
                       for candidate = (replace-regexp-in-string
                                        "[\n\0]" "-"
                                        (fzf-native-fuzz--valid-string 10))
                       collect (if (zerop (length candidate)) "-" candidate)))
             (file (make-temp-file "fzf-native-fuzz-" nil ".txt"))
             (queries (list "a" "ab" "a" "foo bar" "bar foo" "你" "K"))
             handle)
        (unwind-protect
            (progn
              (let ((coding-system-for-write 'utf-8-unix))
                (write-region (concat (mapconcat #'identity collection "\n") "\n")
                              nil file nil 'silent))
              (setq handle
                    (fzf-native-async-start
                     (concat "cat " (shell-quote-argument file))))
              (should (fzf-native-fuzz--wait-until
                       (lambda ()
                         (= (fzf-native-async-generation handle)
                            (length collection)))
                       5.0))
              (let ((previous-generation
                     (or (fzf-native-async-generation handle) 0)))
                (dolist (query queries)
                  (let* ((async (fzf-native-fuzz--async-result
                                 handle query (length collection)))
                         (sync (fzf-native-score-all
                                (fzf-native-fuzz--copies collection) query))
                         (generation (fzf-native-async-generation handle)))
                    (ert-info ((format "seed=%d async-iteration=%d query=%S"
                                       seed iteration query))
                      (should (equal (fzf-native-fuzz--keys async)
                                     (fzf-native-fuzz--keys sync)))
                      (should (>= generation previous-generation)))
                    (setq previous-generation generation)))
              ;; The cached repeated query must be stable across a collection.
              (let ((first (fzf-native-fuzz--async-result
                            handle "a" (length collection))))
                (garbage-collect)
                (should (equal (fzf-native-fuzz--keys first)
                               (fzf-native-fuzz--keys
                                (fzf-native-fuzz--async-result
                                 handle "a" (length collection))))))
              (fzf-native-async-stop handle)
              (should-not (fzf-native-async-result-fresh-p handle "a"))
              (setq handle nil)))
          (when handle (ignore-errors (fzf-native-async-stop handle)))
          (ignore-errors (delete-file file)))))))

(provide 'fzf-native-fuzz-test)
;;; fzf-native-fuzz-test.el ends here
