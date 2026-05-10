;;; fzf-native-test.el --- `fzf-native' test. -*- lexical-binding: t; -*-
(require 'ert)
(require 'fzf-native)

(fzf-native-load-dyn)



(ert-deftest fzf-native-score-with-default-slab-test ()
  "Test slab can be reused."
  (let* ((slab (fzf-native-make-default-slab))
         (_result (fzf-native-score "abcdefghi" "acef" slab)))
    (should
     (equal (fzf-native-score "abcdefghi" "acef" slab)
            '(78)))
    (should
     (equal (fzf-native-score "abc" "acef" slab)
            '(0)))
    (should
     (equal (fzf-native-score "zzzzzabc" "z" slab)
            '(32)))
    (should
     (equal (fzf-native-score "sfsjoc" "jo" slab)
            '(36)))))

(ert-deftest fzf-native-score-with-slab-test ()
  "Test slab can be reused."
  (let* ((slab (fzf-native-make-slab (* 100 1024) 2048))
         (_result (fzf-native-score "abcdefghi" "acef" slab)))
    (should
     (equal (fzf-native-score "abcdefghi" "acef" slab)
            '(78)))
    (should
     (equal (fzf-native-score "abc" "acef" slab)
            '(0)))
    (should
     (equal (fzf-native-score "zzzzzabc" "z" slab)
            '(32)))
    (should
     (equal (fzf-native-score "sfsjoc" "jo" slab)
            '(36)))))

(ert-deftest fzf-native-score-empty-query-test ()
  (let ((result (fzf-native-score "abcdefghi" "")))
    (should (equal result '(0)))))

(ert-deftest fzf-native-score-empty-str-test ()
  (let ((result (fzf-native-score "" "acef")))
    (should (equal result '(0)))))

(ert-deftest fzf-native-score-str-wrong-type-int-test ()
  (should-error (fzf-native-score 1 "1")
                :type 'wrong-type-argument))

(ert-deftest fzf-native-score-query-wrong-type-int-test ()
  (should-error (fzf-native-score "1" 1)
                :type 'wrong-type-argument))

(ert-deftest fzf-native-score-str-wrong-type-nil-test ()
  (should-error (fzf-native-score nil "1")
                :type 'wrong-type-argument))

(ert-deftest fzf-native-score-query-wrong-type-nil-test ()
  (should-error (fzf-native-score "1" nil)
                :type 'wrong-type-argument))

(ert-deftest fzf-native-score-long-str-test ()
  (let* ((len 4096)
         (str (concat (make-string len ?s) "d"))
         (result (fzf-native-score str "d")))
    (should (equal result '(16)))))

(ert-deftest fzf-native-score-very-long-str-test ()
  (let* ((len 65536)
         (str (concat (make-string len ?s) "d"))
         (result (fzf-native-score str "d")))
    (should (equal result '(16)))))

(ert-deftest fzf-native-score-case-mode-smart-test ()
  "Default `fzf-native-case-mode' is smart: lowercase query is
case-insensitive, query with any uppercase becomes case-sensitive."
  (should (eq fzf-native-case-mode 'smart))
  ;; Lowercase query → insensitive: matches uppercase target.
  (should (equal (fzf-native-score "Foo" "foo") '(80)))
  ;; Uppercase query → sensitive: lowercase target no longer matches.
  (should (equal (fzf-native-score "foo" "Foo") '(0))))

(ert-deftest fzf-native-score-case-mode-ignore-test ()
  "`fzf-native-case-mode' = ignore matches regardless of case."
  (let ((fzf-native-case-mode 'ignore))
    (should (equal (fzf-native-score "foo" "Foo") '(80)))
    (should (equal (fzf-native-score "Foo" "foo") '(80)))))

(ert-deftest fzf-native-score-case-mode-respect-test ()
  "`fzf-native-case-mode' = respect requires exact case."
  (let ((fzf-native-case-mode 'respect))
    (should (equal (fzf-native-score "Foo" "foo") '(0)))
    (should (equal (fzf-native-score "foo" "foo") '(80)))))

(ert-deftest fzf-native-score-with-default-slab-benchmark-test ()
  "Test scoring with slab is faster."
  (let* ((slab (fzf-native-make-default-slab))
         (str "aaaaaasdfas;ldfjalsdjfasdfaourioquruwrqrqwruqaaaaaafffffffaadf31230")
         (query "asldfjasldfasdsfofjadf"))
    (should
     (<
      (car
       (benchmark-run 10000
         (fzf-native-score str query slab)))
      (car
       (benchmark-run 10000
         (fzf-native-score str query)))))))

(ert-deftest fzf-native-score-with-small-slab-versus-large-slab-benchmark-test ()
  "Test scoring which slab is faster."
  (let* ((small-slab (fzf-native-make-slab (* 1 1024) (* 1 2048)))
         (large-slab (fzf-native-make-slab (* 100 1024) (* 1 2048)))
         (str (concat (make-string 4096 ?s) "d"))
         (query "d"))
    (should
     (>
      (car
       (benchmark-run 10000
         (fzf-native-score str query small-slab)))
      (car
       (benchmark-run 10000
         (fzf-native-score str query large-slab)))))))



(defun fzf-native-generate-random-string (length)
  "Generate a random string of LENGTH using alphanumeric characters."
  (let ((chars "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"))
    (apply #'string
           (cl-loop repeat length
                    collect (elt chars (random (length chars)))))))

(defun fzf-native-generate-random-string-list (list-size string-length)
  "Generate a list of LIST-SIZE random strings, each of STRING-LENGTH."
  (cl-loop repeat list-size
           collect (fzf-native-generate-random-string string-length)))

(defvar fzf-native-large-random-string-list
  (fzf-native-generate-random-string-list 50000 10)
  "A mock list of 50,000 random strings, each of length 10.")

(ert-deftest fzf-native-score-all-big-collection-test ()
  (let ((collection (all-completions
                     "" fzf-native-large-random-string-list nil)))
    (should
     (fzf-native-score-all collection "a"))))

(ert-deftest fzf-native-score-all-benchmark-test ()
  (let ((collection (all-completions
                     "" fzf-native-large-random-string-list nil)))
    (should
     (<
      (car (benchmark-run 10 (fzf-native-score-all collection "a")))
      (car (benchmark-run 10 (dolist (c collection)
                               (fzf-native-score c "a"))))))))

(ert-deftest fzf-native-score-all-basic-tests ()
  (let ((_ (should
            (equal '("a")
                   (fzf-native-score-all '("a" "b" "c") "a"))))
        (_ (should
            (equal '("a" "adsfdsa")
                   (fzf-native-score-all '("a" "b" "c" "adsfdsa") "a"))))
        (_ (should
            (equal '("a" "FAST" "Fast")
                   (fzf-native-score-all '("a" "b" "c" "FAST" "Fast") "a"))))
        (_ (should
            (equal '("FAST" "Fast")
                   (fzf-native-score-all '("a" "b" "c" "FAST" "Fast") "at"))))
        (_ (should
            (equal '("abc.txt" "ポケモン.txt" "tビビxt")
                   (fzf-native-score-all
                    '("abc.txt" "ポケモン.txt" "tビビxt" "tビ") "txt")))))
    t))

(ert-deftest fzf-native-score-all-empty-string-candidate-test ()
  (let ((result (fzf-native-score-all '("") "")))
    (should (equal result '("")))))

;;
;; Multibyte / invalid unibyte handling
;;
;; These exercise the C-side `copy_emacs_string' fallback through
;; `encode-coding-string'. Pre-fix, an invalid-unibyte input made
;; `copy_string_contents' signal `unicode-string-p' and abort the whole
;; batch.
;;
;; On Emacs 30+ the coercion path almost always succeeds: raw bytes get
;; round-tripped to a valid byte sequence that fzf can score normally.
;; The interesting guarantee is therefore "no input causes the call to
;; signal", not any specific score value. We previously assigned a
;; sentinel score of 1 to uncoerceable inputs; that path is now treated
;; the same as "did not match" and the candidate is silently dropped.

(defconst fzf-native-test--bad-bytes
  (string-as-multibyte ";; Copyright 2022 Jo Be�����")
  "Raw-byte string used as a reproducer for the `unicode-string-p' bug.
Note: on Emacs 30+ this WILL coerce successfully through
`encode-coding-string', so it scores like any other string. The tests
below assert the absence of a signal, not any particular score.")

(ert-deftest fzf-native-score-invalid-unibyte-test ()
  "`fzf-native-score' does not signal on a byte-junk candidate."
  (let ((result (fzf-native-score fzf-native-test--bad-bytes "C")))
    (should (listp result))
    (should (numberp (car result)))))

(ert-deftest fzf-native-score-invalid-unibyte-query-test ()
  "`fzf-native-score' does not signal when the QUERY is byte-junk."
  (let ((result (fzf-native-score "hello" fzf-native-test--bad-bytes)))
    (should (listp result))
    (should (numberp (car result)))))

(ert-deftest fzf-native-score-chinese-match-test ()
  "`fzf-native-score' scores a Chinese substring match."
  (let ((result (fzf-native-score "你好世界 hello" "你好")))
    (should (listp result))
    (should (> (car result) 0))))

(ert-deftest fzf-native-score-all-invalid-unibyte-test ()
  "`fzf-native-score-all' handles a byte-junk candidate without signaling."
  (let* ((result (fzf-native-score-all
                  (list "CCCCC" fzf-native-test--bad-bytes "xyzzy")
                  "C"))
         (good (car (member "CCCCC" result))))
    (should (listp result))
    ;; The clean match survives and gets a numeric score attached.
    (should good)
    (should (numberp (get-text-property 0 'completion-score good)))
    ;; Non-matching candidates are filtered as usual.
    (should-not (member "xyzzy" result))))

(ert-deftest fzf-native-score-all-chinese-test ()
  "`fzf-native-score-all' scores Chinese candidates against a Chinese query."
  (let ((result (fzf-native-score-all '("你好世界" "Hello" "你是") "你")))
    (should (member "你好世界" result))
    (should (member "你是" result))
    (should-not (member "Hello" result))))

(ert-deftest fzf-native-score-all-preserves-object-identity-test ()
  "Returned strings are the same object as the input (for text properties)."
  (let* ((orig "你好")
         (result (fzf-native-score-all (list orig) "你")))
    (should (eq (car result) orig))
    ;; `completion-score' is attached to the original object.
    (should (get-text-property 0 'completion-score (car result)))))

(ert-deftest fzf-native-score-all-empty-query-test ()
  (let* ((coll ["a" "b" "c"])
         (result (fzf-native-score-all coll "")))
    (should (equal result coll))))

;;
;; Async path (fzf-native-async-*)
;;

(defun fzf-native-test--wait-for-data (handle &optional timeout)
  "Poll HANDLE until its generation advances past 0.
Returns t when data has arrived, nil if TIMEOUT seconds elapsed (default 5)."
  (let ((deadline (+ (float-time) (or timeout 5))))
    (while (and (zerop (fzf-native-async-generation handle))
                (< (float-time) deadline))
      (sleep-for 0.05)))
  (> (fzf-native-async-generation handle) 0))

(defun fzf-native-test--wait-for-scoring (handle filter &optional limit timeout)
  "Dispatch FILTER and poll until scoring completes; return candidates.
Scoring is considered done when stats total > 0.  Polls for up to
TIMEOUT seconds (default 5), calling candidates each iteration."
  (let ((deadline (+ (float-time) (or timeout 5.0))))
    (while (and (= (cdr (or (fzf-native-async-stats handle) '(0 . 0))) 0)
                (< (float-time) deadline))
      (if limit
          (fzf-native-async-candidates handle filter limit)
        (fzf-native-async-candidates handle filter))
      (sleep-for 0.05)))
  (if limit
      (fzf-native-async-candidates handle filter limit)
    (fzf-native-async-candidates handle filter)))

(ert-deftest fzf-native-async-lifecycle-test ()
  "Start → wait for data → generation advances → stop."
  (let ((handle (fzf-native-async-start "printf '%s\\n' foo bar baz")))
    (unwind-protect
        (should (fzf-native-test--wait-for-data handle))
      (fzf-native-async-stop handle))))

(ert-deftest fzf-native-async-stop-invalidates-handle-test ()
  "After stop, generation returns nil (handle is invalidated)."
  (let ((handle (fzf-native-async-start "printf '%s\\n' foo")))
    (fzf-native-test--wait-for-data handle)
    (fzf-native-async-stop handle)
    (should (null (fzf-native-async-generation handle)))))

(ert-deftest fzf-native-async-candidates-empty-filter-test ()
  "Empty filter returns all candidates."
  (let ((handle (fzf-native-async-start "printf '%s\\n' foo bar baz")))
    (unwind-protect
        (progn
          (fzf-native-test--wait-for-data handle)
          (let ((result (fzf-native-test--wait-for-scoring handle "")))
            (should (= (length result) 3))
            (should (member "foo" result))
            (should (member "bar" result))
            (should (member "baz" result))))
      (fzf-native-async-stop handle))))

(ert-deftest fzf-native-async-candidates-filter-test ()
  "Filter keeps matching candidates and drops non-matches."
  (let ((handle (fzf-native-async-start "printf '%s\\n' foo bar baz foobaz")))
    (unwind-protect
        (progn
          (fzf-native-test--wait-for-data handle)
          (let ((result (fzf-native-test--wait-for-scoring handle "foo")))
            (should (member "foo" result))
            (should (member "foobaz" result))
            (should-not (member "bar" result))
            (should-not (member "baz" result))))
      (fzf-native-async-stop handle))))

(ert-deftest fzf-native-async-candidates-no-match-test ()
  "Filter that matches nothing returns nil."
  (let ((handle (fzf-native-async-start "printf '%s\\n' foo bar baz")))
    (unwind-protect
        (progn
          (fzf-native-test--wait-for-data handle)
          (should (null (fzf-native-async-candidates handle "zzz"))))
      (fzf-native-async-stop handle))))

(ert-deftest fzf-native-async-candidates-limit-test ()
  "LIMIT argument caps returned candidates."
  (let ((handle (fzf-native-async-start "printf '%s\\n' foo bar baz foobaz")))
    (unwind-protect
        (progn
          (fzf-native-test--wait-for-data handle)
          (let ((result (fzf-native-test--wait-for-scoring handle "" 2)))
            (should (= (length result) 2))))
      (fzf-native-async-stop handle))))

(ert-deftest fzf-native-async-stats-test ()
  "Stats return (filtered . total) after scoring."
  (let ((handle (fzf-native-async-start "printf '%s\\n' foo bar baz foobaz")))
    (unwind-protect
        (progn
          (fzf-native-test--wait-for-data handle)
          (fzf-native-test--wait-for-scoring handle "foo")
          (let ((stats (fzf-native-async-stats handle)))
            (should (consp stats))
            (should (= (car stats) 2))    ; filtered: foo + foobaz
            (should (= (cdr stats) 4))))  ; total: 4 candidates
      (fzf-native-async-stop handle))))

(ert-deftest fzf-native-async-candidates-same-filter-no-livelock ()
  "Same-filter repeated calls must not prevent scoring from completing.
Previously score_abort=true was set unconditionally on every call; with
large candidate sets the pre-work exceeded the 50ms timer interval so
workers always aborted immediately (livelock: scoring never completed).
The fix skips setting abort when the incoming filter matches the one
currently being scored.  Stats are only written on completion, so
(car (fzf-native-async-stats handle)) > 0 proves scoring finished."
  (skip-unless (fboundp 'fzf-native-async-start))
  (let ((handle (fzf-native-async-start "seq 1 1000000")))
    (unwind-protect
        (progn
          ;; Wait up to 15s for 1M candidates to arrive
          (should (fzf-native-test--wait-for-data handle 15.0))
          (sleep-for 3.0)
          ;; Rapid same-filter calls simulating the 50ms UI refresh timer
          (dotimes (_ 40)
            (fzf-native-async-candidates handle "1" 100)
            (sleep-for 0.02))
          ;; Stats > 0 means scoring completed; zero throughout = livelock
          (let ((deadline (+ (float-time) 15.0))
                done)
            (while (and (not done) (< (float-time) deadline))
              (fzf-native-async-candidates handle "1" 100)
              (when (> (car (or (fzf-native-async-stats handle) '(0 . 0))) 0)
                (setq done t))
              (unless done (sleep-for 0.1)))
            (should done)))
      (fzf-native-async-stop handle))))

(ert-deftest fzf-native-async-cache-prefix-refinement-test ()
  "Cache returns consistent results across a typing progression and on
backspace.  Setup: a small corpus where 'fo'/'foo'/'food' produce
predictably-different result sets.  We type the progression, verify
each query's results, then backspace back to 'fo' and verify it
returns the same set as the original 'fo' call.

This exercises:
- Phase-1 exact lookup (each first call inserts; second call hits)
- Phase-2 prefix refinement (typing extends matched_idx)
- Backspace coverage (LRU keeps prior queries)"
  (skip-unless (fboundp 'fzf-native-async-start))
  (let ((handle (fzf-native-async-start
                 "printf '%s\\n' food foo foobar fool bar baz")))
    (unwind-protect
        (progn
          (fzf-native-test--wait-for-data handle)
          (let ((r-fo-1   (sort (copy-sequence
                                 (fzf-native-test--wait-for-scoring handle "fo"))
                                #'string<)))
            (should (member "foo"    r-fo-1))
            (should (member "food"   r-fo-1))
            (should (member "foobar" r-fo-1))
            (should (member "fool"   r-fo-1))
            (should-not (member "bar" r-fo-1))
            ;; Type "foo": narrower than "fo" — refinement scenario
            (let ((r-foo (fzf-native-test--wait-for-scoring handle "foo")))
              (should (member "foo"    r-foo))
              (should (member "food"   r-foo))
              (should (member "foobar" r-foo))
              ;; "fool" doesn't fuzzy-match "foo" cleanly; just check non-foo
              ;; candidates are absent
              (should-not (member "bar" r-foo))
              (should-not (member "baz" r-foo)))
            ;; Backspace to "fo" — should hit cached entry, return same set
            (let ((r-fo-2 (sort (copy-sequence
                                 (fzf-native-async-candidates handle "fo"))
                                #'string<)))
              (should (equal r-fo-2 r-fo-1)))))
      (fzf-native-async-stop handle))))

(ert-deftest fzf-native-async-cache-term-reorder-test ()
  "Term reordering: \"foo bar\" and \"bar foo\" are semantically equal
in fzf and the cache should treat them so via term-set subsumption
(v2).  Both queries should return the same candidates."
  (skip-unless (fboundp 'fzf-native-async-start))
  (let ((handle (fzf-native-async-start
                 "printf '%s\\n' foobar fooXbar bar foo barfoo barXfoo")))
    (unwind-protect
        (progn
          (fzf-native-test--wait-for-data handle)
          (let ((r1 (sort (copy-sequence
                           (fzf-native-test--wait-for-scoring handle "foo bar"))
                          #'string<))
                (r2 (sort (copy-sequence
                           (fzf-native-test--wait-for-scoring handle "bar foo"))
                          #'string<)))
            (should r1)
            (should (equal r1 r2))))
      (fzf-native-async-stop handle))))

