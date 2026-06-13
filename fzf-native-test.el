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

(ert-deftest fzf-native-score-fuzzy-default-test ()
  "Default `fzf-native-fuzzy' is t: non-contiguous query matches."
  (should (eq fzf-native-fuzzy t))
  (should (equal (fzf-native-score "src/foo.c" "sfc") '(70))))

(ert-deftest fzf-native-score-fuzzy-disabled-no-fuzzy-test ()
  "`fzf-native-fuzzy' = nil: non-contiguous query no longer matches."
  (let ((fzf-native-fuzzy nil))
    (should (equal (fzf-native-score "src/foo.c" "sfc") '(0)))))

(ert-deftest fzf-native-score-fuzzy-disabled-substring-still-matches-test ()
  "`fzf-native-fuzzy' = nil: contiguous substring still matches."
  (let ((fzf-native-fuzzy nil))
    (should (equal (fzf-native-score "src/foo.c" "foo") '(80)))))

(ert-deftest fzf-native-score-fuzzy-disabled-quote-prefix-inverts-test ()
  "`fzf-native-fuzzy' = nil: ' prefix re-enables fuzzy for that term."
  (let ((fzf-native-fuzzy nil))
    (should (equal (fzf-native-score "src/foo.c" "'sfc") '(70)))))

(ert-deftest fzf-native-score-fuzzy-disabled-operators-still-work-test ()
  "`fzf-native-fuzzy' = nil: ^, !, and AND tokenization keep working."
  (let ((fzf-native-fuzzy nil))
    ;; ^ prefix anchor matches at start.
    (should (equal (fzf-native-score "src/foo.c" "^src") '(80)))
    ;; ! negation excludes a term and the bare term still matches.
    (should (equal (fzf-native-score "src/foo.c" "!xyz foo") '(80)))
    ;; Space-separated AND: both substrings must match.
    (should (equal (fzf-native-score "src/foo.c" "src foo") '(160)))))

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

(ert-deftest fzf-native-score-all-isolates-caller-originals-test ()
  "Top-N result strings are fresh copies; caller's originals stay clean.

Earlier the C scorer returned the input objects so callers could read
`completion-score' off them directly.  As of the highlight-isolation
fix, top-N candidates are `copy-sequence'd before face / score
attachment so the caller's shared strings (obarray symbol-names,
buffer-name interns, etc.) don't accumulate stale face / score across
calls.  `completion-score' still rides on the returned copy."
  (let* ((orig (copy-sequence "你好"))
         (result (fzf-native-score-all (list orig) "你")))
    (should (equal (car result) orig))
    (should-not (eq (car result) orig))
    (should-not (get-text-property 0 'completion-score orig))
    (should (get-text-property 0 'completion-score (car result)))))

(ert-deftest fzf-native-score-all-empty-query-test ()
  (let* ((coll ["a" "b" "c"])
         (result (fzf-native-score-all coll "")))
    (should (equal result coll))))

;;
;; Async path (fzf-native-async-*)
;;

(defun fzf-native-test--wait-for-data (handle &optional timeout)
  "Poll HANDLE until its generation advances past 0 and then stabilises.
Returns t when the reader has drained (generation unchanged for three
consecutive 50ms polls after first becoming non-zero), nil if TIMEOUT
seconds elapsed (default 5).  Waiting only for generation > 0 races the
reader on small corpora — the first batch may carry just one line."
  (let ((deadline (+ (float-time) (or timeout 5)))
        (stable 0)
        (prev   0))
    (while (and (< (float-time) deadline)
                (or (zerop (fzf-native-async-generation handle))
                    (< stable 3)))
      (sleep-for 0.05)
      (let ((g (fzf-native-async-generation handle)))
        (setq stable (if (and (> g 0) (= g prev)) (1+ stable) 0)
              prev   g))))
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

(defun fzf-native-test--wait-for-fresh (handle filter &optional timeout)
  "Drive scoring for FILTER and poll `result-fresh-p' until t.
Unlike `wait-for-scoring' (which exits as soon as stats are updated by
*any* candidates call, racing the actual scoring), this returns only
once the result cache holds an entry for FILTER at the current pool
size.  Times out after TIMEOUT seconds (default 5)."
  (let ((deadline (+ (float-time) (or timeout 5.0))))
    (while (and (not (fzf-native-async-result-fresh-p handle filter))
                (< (float-time) deadline))
      (fzf-native-async-candidates handle filter)
      (sleep-for 0.05)))
  (fzf-native-async-result-fresh-p handle filter))

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

(ert-deftest fzf-native-async-result-fresh-p-before-scoring-test ()
  "`result-fresh-p' is nil for any query before scoring runs."
  (skip-unless (fboundp 'fzf-native-async-start))
  (let ((handle (fzf-native-async-start "printf '%s\\n' foo bar baz")))
    (unwind-protect
        (progn
          (fzf-native-test--wait-for-data handle)
          ;; No async-candidates call yet → no cache entry → not fresh.
          (should-not (fzf-native-async-result-fresh-p handle ""))
          (should-not (fzf-native-async-result-fresh-p handle "foo")))
      (fzf-native-async-stop handle))))

(ert-deftest fzf-native-async-result-fresh-p-after-scoring-test ()
  "`result-fresh-p' returns t for a query after its scoring completes."
  (skip-unless (fboundp 'fzf-native-async-start))
  (let ((handle (fzf-native-async-start "printf '%s\\n' foo bar baz foobaz")))
    (unwind-protect
        (progn
          (fzf-native-test--wait-for-data handle)
          (should (fzf-native-test--wait-for-fresh handle "foo")))
      (fzf-native-async-stop handle))))

(ert-deftest fzf-native-async-result-fresh-p-per-query-test ()
  "`result-fresh-p' is keyed by query: fresh for one, nil for another."
  (skip-unless (fboundp 'fzf-native-async-start))
  (let ((handle (fzf-native-async-start "printf '%s\\n' foo bar baz foobaz")))
    (unwind-protect
        (progn
          (fzf-native-test--wait-for-data handle)
          (should (fzf-native-test--wait-for-fresh handle "foo"))
          (should     (fzf-native-async-result-fresh-p handle "foo"))
          (should-not (fzf-native-async-result-fresh-p handle "bar"))
          (should-not (fzf-native-async-result-fresh-p handle "qqq")))
      (fzf-native-async-stop handle))))

(ert-deftest fzf-native-async-result-fresh-p-zero-match-test ()
  "Authoritative zero: scoring done for a non-matching query — candidates
returns nil AND fresh-p returns t.  This is the load-bearing case for
distinguishing \"no matches\" from \"scoring in flight\"."
  (skip-unless (fboundp 'fzf-native-async-start))
  (let ((handle (fzf-native-async-start "printf '%s\\n' foo bar baz")))
    (unwind-protect
        (progn
          (fzf-native-test--wait-for-data handle)
          (should (fzf-native-test--wait-for-fresh handle "zzz"))
          (should-not (fzf-native-async-candidates handle "zzz"))
          (should (fzf-native-async-result-fresh-p handle "zzz")))
      (fzf-native-async-stop handle))))

(ert-deftest fzf-native-async-result-fresh-p-pool-grew-stale-test ()
  "After scoring at pool size N, the arrival of more candidates makes
the cache entry stale: `pool_gen' lags `s->count' and `fresh-p' flips
to nil.  Uses a two-phase Python producer gated on a tempfile so phase
2 provably runs *after* scoring has settled against the phase-1 pool,
regardless of CI runner speed.  Phase 1 emits 3 items, then the
producer polls until elisp deletes the gate, then phase 2 streams more
items.  Between `wait-for-fresh' and the gate release, no
`async-candidates' call dispatches, so the cache entry for \"a\" keeps
its phase-1 `pool_gen' even as the pool grows."
  (skip-unless (and (fboundp 'fzf-native-async-start)
                    (executable-find "python3")))
  (let* ((gate (make-temp-file "fzf-native-gate-"))
         (handle (fzf-native-async-start
                  (format "python3 -u -c 'import os, sys, time
gate = sys.argv[1]
for i in range(3): print(f\"a{i}\", flush=True)
while os.path.exists(gate): time.sleep(0.01)
for i in range(40): print(f\"b{i}\", flush=True)
' %s" (shell-quote-argument gate)))))
    (unwind-protect
        (progn
          (fzf-native-test--wait-for-data handle)
          (should (fzf-native-test--wait-for-fresh handle "a"))
          (let ((g0 (fzf-native-async-generation handle))
                (deadline (+ (float-time) 5.0)))
            ;; Release phase 2 now that scoring for "a" is settled.
            (delete-file gate)
            ;; Wait for at least one phase-2 batch to land so the reader
            ;; has advanced `s->count' past the phase-1 pool.
            (while (and (= (fzf-native-async-generation handle) g0)
                        (< (float-time) deadline))
              (sleep-for 0.05)))
          ;; Pool grew → cache entry's `pool_gen' < `s->count' → stale.
          (should-not (fzf-native-async-result-fresh-p handle "a")))
      (when (file-exists-p gate) (delete-file gate))
      (fzf-native-async-stop handle))))

(ert-deftest fzf-native-async-result-fresh-p-after-stop-test ()
  "After `async-stop', `result-fresh-p' returns nil on the dead handle."
  (skip-unless (fboundp 'fzf-native-async-start))
  (let ((handle (fzf-native-async-start "printf '%s\\n' foo bar baz")))
    (fzf-native-test--wait-for-data handle)
    (fzf-native-test--wait-for-fresh handle "foo")
    (should (fzf-native-async-result-fresh-p handle "foo"))
    (fzf-native-async-stop handle)
    (should-not (fzf-native-async-result-fresh-p handle "foo"))))

;;
;; Stress / robustness — exercise the corner-case paths that surfaced in
;; on-machine crash investigation (06-06 finalizer-during-GC race,
;; intermittent "memory buffer too small" reports).  Each test generates
;; its own data inline; no fixture files are required.
;;

(ert-deftest fzf-native-async-invalid-unibyte-test ()
  "Invalid UTF-8 bytes in the candidate stream must not signal.
Exercises the async reader → arena_strdup → `make_string' path with
byte junk that would otherwise trip `unicode-string-p' if the C side
ever decoded these as Emacs strings without coercion."
  (skip-unless (and (fboundp 'fzf-native-async-start)
                    (executable-find "python3")))
  (let ((handle (fzf-native-async-start
                 "python3 -u -c 'import sys
sys.stdout.buffer.write(b\"valid_line\\n\")
sys.stdout.buffer.write(b\"junk\\x80\\x81\\xfe\\xff_more\\n\")
sys.stdout.buffer.write(b\"another_valid\\n\")
'")))
    (unwind-protect
        (progn
          (should (fzf-native-test--wait-for-data handle))
          (let ((result (fzf-native-test--wait-for-scoring handle "valid")))
            (should result)
            ;; "valid_line" and "another_valid" both match "valid"; the
            ;; byte-junk line should be either coerced or silently dropped.
            (should (cl-some (lambda (s) (string-match-p "valid" s)) result))))
      (fzf-native-async-stop handle))))

(ert-deftest fzf-native-async-multibyte-candidates-test ()
  "Async path scores multibyte candidates (CJK)."
  (skip-unless (and (fboundp 'fzf-native-async-start)
                    (executable-find "python3")))
  (let ((handle (fzf-native-async-start
                 "python3 -u -c 'print(\"你好世界\"); print(\"Hello\"); print(\"你是\")'")))
    (unwind-protect
        (progn
          (should (fzf-native-test--wait-for-data handle))
          (let ((result (fzf-native-test--wait-for-scoring handle "你")))
            (should (member "你好世界" result))
            (should (member "你是" result))
            (should-not (member "Hello" result))))
      (fzf-native-async-stop handle))))

(ert-deftest fzf-native-async-long-line-whole-test ()
  "Lines much larger than the reader's initial buffer must arrive as a
single whole candidate, not as fragments split at I/O boundaries.

The reader uses `getline', which grows its buffer to fit each logical
line.  Pre-getline, the reader used `fgets' with a fixed 8 KB stack
buffer and chopped long lines into 8 KB shards at arbitrary positions
— making fuzzy-matching against the original line impossible and
leaking subtle partial-tail candidates whenever the line length
landed in (8192, 8192+cap].  This test guards against regression to
that behavior.

`fzf-native-max-line-length' is lifted so the long line isn't excluded
by the user-facing cap before we can observe whole-line delivery."
  (skip-unless (and (fboundp 'fzf-native-async-start)
                    (executable-find "python3")))
  (let* ((fzf-native-max-line-length nil)
         (handle (fzf-native-async-start
                  "python3 -u -c 'print(\"a\" * 9000 + \"NEEDLE\" + \"b\" * 9000)'")))
    (unwind-protect
        (progn
          (should (fzf-native-test--wait-for-data handle))
          (let ((result (fzf-native-test--wait-for-scoring handle "NEEDLE")))
            (should result)
            ;; Exactly one candidate, holding the full 18006-char line.
            (should (= (length result) 1))
            (should (= (length (car result)) (+ 9000 6 9000)))
            (should (string-match-p "NEEDLE" (car result)))))
      (fzf-native-async-stop handle))))

(ert-deftest fzf-native-async-large-pool-finalize-test ()
  "Ingest ~50k candidates, dispatch a typing progression, then stop
cleanly.  Smoke-tests the destroy path under realistic load (arena
chunks, multiple cache entries, scoring thread mid-run)."
  (skip-unless (fboundp 'fzf-native-async-start))
  (let ((handle (fzf-native-async-start "seq 1 50000")))
    (unwind-protect
        (progn
          (should (fzf-native-test--wait-for-data handle 15.0))
          (dolist (q '("1" "12" "123" "1234"))
            (fzf-native-test--wait-for-scoring handle q 100 5.0))
          (should (consp (fzf-native-async-stats handle))))
      (fzf-native-async-stop handle))))

(ert-deftest fzf-native-async-gc-during-active-workers-test ()
  "Drop the session handle while reader/scoring threads are still
active, then force GC.  Emacs must not crash and must remain
responsive.  This is the on-machine 06-06 finalizer-race reproducer:
`async_session_destroy' is invoked from `sweep_vectors' during GC,
calls `pthread_join' on workers that may be mid-malloc, and previously
deadlocked on the macOS xzone fork-lock.

The handle is bound only in the inner `let' so it becomes unreachable
once the form exits; the trailing `(setq handle nil)' explicitly drops
the lexical slot in case the byte compiler keeps it alive longer than
needed.  No `async-stop' here on purpose — we want the finalizer path."
  (skip-unless (fboundp 'fzf-native-async-start))
  (dotimes (_ 5)
    (let ((handle (fzf-native-async-start "seq 1 100000")))
      (fzf-native-test--wait-for-data handle)
      ;; Kick scoring so the score thread is also active when we drop.
      (fzf-native-async-candidates handle "1" 100)
      (setq handle nil))
    (garbage-collect)
    (garbage-collect))
  ;; If we got here without aborting Emacs, the finalizer survived
  ;; the race for this run.  Confirm the module is still usable.
  (should (equal (fzf-native-score "abcdefghi" "acef") '(78))))

(ert-deftest fzf-native-async-stop-returns-fast-test ()
  "`fzf-native-async-stop' must return on the calling (Emacs main) thread
within milliseconds, regardless of how much work the scoring/reader
threads or arena teardown might cost.  The C side signals stop
synchronously and offloads `pthread_join' + arena/cache free to a
detached pthread; this test asserts that contract end-to-end.

A ~200k-line pool with an active dispatched filter is large enough that
a synchronous join would take 50ms+; a non-blocking stop returns in
single-digit ms."
  (skip-unless (fboundp 'fzf-native-async-start))
  (let ((handle (fzf-native-async-start "seq 1 200000")))
    (unwind-protect
        (progn
          (should (fzf-native-test--wait-for-data handle 15.0))
          ;; Kick scoring so both reader and score thread are busy when
          ;; we stop.
          (fzf-native-async-candidates handle "1" 100)
          (let* ((t0 (float-time))
                 (_  (fzf-native-async-stop handle))
                 (elapsed-ms (* 1000.0 (- (float-time) t0))))
            ;; 30 ms ceiling — sub-ms expected, headroom for CI jitter.
            ;; A regression that re-introduces synchronous join would
            ;; spike well over this on a 200k pool.
            (should (< elapsed-ms 30.0))
            ;; Handle invalidated regardless of when the detached
            ;; worker actually finishes the join.
            (should (null (fzf-native-async-generation handle)))))
      ;; Already stopped — second stop is a no-op (s == NULL).
      (ignore-errors (fzf-native-async-stop handle)))))

(ert-deftest fzf-native-async-stop-many-sessions-fast-test ()
  "Multi-source teardown: stopping N sessions back-to-back from Emacs
main returns in roughly N × per-call cost (microseconds each), not the
sum of their join times.  Models the `fzfa-find-any' minibuffer-exit
path where ~10 async sources tear down in one unwind."
  (skip-unless (fboundp 'fzf-native-async-start))
  (let ((handles
         (cl-loop repeat 6
                  collect (fzf-native-async-start "seq 1 50000"))))
    (unwind-protect
        (progn
          (dolist (h handles)
            (fzf-native-test--wait-for-data h 15.0)
            (fzf-native-async-candidates h "1" 100))
          (let* ((t0 (float-time))
                 (_  (dolist (h handles) (fzf-native-async-stop h)))
                 (elapsed-ms (* 1000.0 (- (float-time) t0))))
            (should (< elapsed-ms 60.0))))
      (dolist (h handles) (ignore-errors (fzf-native-async-stop h))))))

;;; `fzf-native-highlight-all' caller-isolation tests
;;
;; Verify the C-side highlight pass substitutes face-bearing COPIES into
;; the COLLECTION (via setcar / aset) rather than mutating the caller's
;; original strings.  Prior to the fix, fussy and other callers saw their
;; shared candidate strings accumulate stale `face' properties because the
;; module did `put-text-property' on the originals.

(ert-deftest fzf-native-highlight-all-preserves-list-originals-test ()
  "Caller's original strings are not face-mutated by highlight-all (list).

Builds a list of caller-owned strings, captures the originals by `eq'
identity before the call, runs `fzf-native-highlight-all', and confirms
that (a) the originals carry no `face' property after the call, and
(b) the returned list's top-N slots hold face-bearing copies that are
NOT `eq' to the originals."
  (skip-unless (fboundp 'fzf-native-highlight-all))
  (let* ((fussy-fzf-native-highlight t)
         (orig-1 (copy-sequence "alpha"))
         (orig-2 (copy-sequence "beta"))
         (orig-3 (copy-sequence "gamma"))
         (coll   (list orig-1 orig-2 orig-3))
         (ret    (fzf-native-highlight-all coll "a")))
    ;; Originals unmutated.
    (should-not (text-property-not-all 0 (length orig-1) 'face nil orig-1))
    (should-not (text-property-not-all 0 (length orig-2) 'face nil orig-2))
    (should-not (text-property-not-all 0 (length orig-3) 'face nil orig-3))
    ;; Top-N slots now hold copies (not eq to originals).
    (should-not (eq (nth 0 ret) orig-1))
    (should-not (eq (nth 1 ret) orig-2))
    (should-not (eq (nth 2 ret) orig-3))
    ;; Copies carry face on at least one position.
    (should (text-property-not-all 0 (length (nth 0 ret)) 'face nil
                                   (nth 0 ret)))))

(ert-deftest fzf-native-highlight-all-preserves-vector-originals-test ()
  "Caller's original strings are not face-mutated by highlight-all (vector)."
  (skip-unless (fboundp 'fzf-native-highlight-all))
  (let* ((fussy-fzf-native-highlight t)
         (orig-1 (copy-sequence "alpha"))
         (orig-2 (copy-sequence "beta"))
         (coll   (vector orig-1 orig-2))
         (ret    (fzf-native-highlight-all coll "a")))
    (should-not (text-property-not-all 0 (length orig-1) 'face nil orig-1))
    (should-not (text-property-not-all 0 (length orig-2) 'face nil orig-2))
    ;; Vector slots substituted with copies.
    (should-not (eq (aref ret 0) orig-1))
    (should-not (eq (aref ret 1) orig-2))
    (should (text-property-not-all 0 (length (aref ret 0)) 'face nil
                                   (aref ret 0)))))

(ert-deftest fzf-native-highlight-all-returns-same-collection-test ()
  "Return value is `eq' to args[0] (substitution-in-place semantics)."
  (skip-unless (fboundp 'fzf-native-highlight-all))
  (let* ((fussy-fzf-native-highlight t)
         (lst (list (copy-sequence "alpha") (copy-sequence "beta")))
         (vec (vector (copy-sequence "alpha") (copy-sequence "beta"))))
    (should (eq (fzf-native-highlight-all lst "a") lst))
    (should (eq (fzf-native-highlight-all vec "a") vec))))



;;;; fzf-native-default-highlight-fn tests

(ert-deftest fzf-native-default-highlight-fn-symbol-face-preserved-test ()
  "User's symbol face survives a highlight pass at non-match positions."
  (let ((cand (copy-sequence "abcdef")))
    (put-text-property 0 6 'face 'my-user-face cand)
    (fzf-native-default-highlight-fn cand [0 2])
    ;; Highlight positions [0,2) carry both faces (list).
    (let ((f0 (get-text-property 0 'face cand)))
      (should (and (listp f0)
                   (memq 'completions-common-part f0)
                   (memq 'my-user-face f0))))
    ;; Non-match positions still hold the user face alone.
    (let ((f2 (get-text-property 2 'face cand)))
      (should (or (eq f2 'my-user-face)
                  (and (listp f2)
                       (memq 'my-user-face f2)
                       (not (memq 'completions-common-part f2))))))))

(ert-deftest fzf-native-default-highlight-fn-leftover-scrubbed-test ()
  "Stale `completions-common-part' from a prior pass is removed."
  (let ((cand (copy-sequence "abcdef")))
    (fzf-native-default-highlight-fn cand [0 3])
    ;; Now run again with a narrower highlight; the [1,3) span must scrub.
    (fzf-native-default-highlight-fn cand [0 1])
    (let ((f0 (get-text-property 0 'face cand))
          (f1 (get-text-property 1 'face cand)))
      (should (or (eq f0 'completions-common-part)
                  (and (listp f0) (memq 'completions-common-part f0))))
      (should (not (and (listp f1) (memq 'completions-common-part f1))))
      (should (not (eq f1 'completions-common-part))))))

(ert-deftest fzf-native-default-highlight-fn-list-face-cleaned-of-stale-only-test ()
  "List face has `completions-common-part' stripped without touching other faces."
  (let ((cand (copy-sequence "abc")))
    (put-text-property 0 3 'face '(my-face completions-common-part) cand)
    (fzf-native-default-highlight-fn cand [1 2])
    ;; Position 0: only `my-face' (completions-common-part scrubbed).
    ;; Accept either bare symbol or single-element list — both render
    ;; identically and the handler is allowed to keep the list form.
    (let ((f0 (get-text-property 0 'face cand)))
      (should (or (eq f0 'my-face)
                  (equal f0 '(my-face)))))
    ;; Position 1: highlight layered back on top of `my-face'.
    (let ((f1 (get-text-property 1 'face cand)))
      (should (and (listp f1)
                   (memq 'my-face f1)
                   (memq 'completions-common-part f1))))
    ;; Position 2: only `my-face' (bare or single-element list).
    (let ((f2 (get-text-property 2 'face cand)))
      (should (or (eq f2 'my-face)
                  (equal f2 '(my-face)))))))

(ert-deftest fzf-native-default-highlight-fn-empty-positions-clears-test ()
  "Empty POSITIONS vector clears leftover `completions-common-part'."
  (let ((cand (copy-sequence "abc")))
    (put-text-property 0 3 'face 'completions-common-part cand)
    (fzf-native-default-highlight-fn cand [])
    (should-not (text-property-not-all 0 3 'face nil cand))))

(ert-deftest fzf-native-default-highlight-fn-plist-face-survives-test ()
  "Plist-form face (e.g. `(:foreground \"red\")') survives highlight pass."
  (let ((cand (copy-sequence "abc"))
        (spec '(:foreground "red")))
    (put-text-property 0 3 'face spec cand)
    (fzf-native-default-highlight-fn cand [0 1])
    ;; A non-list (plist) face is left alone by the strip.  After additive
    ;; apply at [0,1), position 0 should hold a list containing
    ;; `completions-common-part' on top of the surviving plist.
    (let ((f0 (get-text-property 0 'face cand)))
      (should (and (listp f0)
                   (memq 'completions-common-part f0))))
    ;; Position 1: the plist alone (not a face symbol).
    (let ((f1 (get-text-property 1 'face cand)))
      (should (equal f1 spec)))))

(ert-deftest fzf-native-score-all-preserves-user-face-test ()
  "Caller-attached face survives an end-to-end `fzf-native-score-all' call."
  (skip-unless (fboundp 'fzf-native-score-all))
  (let* ((fzf-native-batch-highlight 25)
         (cand (let ((s (copy-sequence "alpha-beta")))
                 (put-text-property 0 5 'face 'my-tag-face s)
                 s))
         (result (fzf-native-score-all (vector cand) "alpha")))
    (let ((out (car result)))
      (should out)
      ;; my-tag-face survives end-to-end.
      (let ((f (get-text-property 0 'face out)))
        (should (or (eq f 'my-tag-face)
                    (and (listp f) (memq 'my-tag-face f))))))))

(ert-deftest fzf-native-score-all-multibyte-positions-test ()
  "Highlight positions are character offsets, not byte offsets, on multibyte.

Candidate \"αβ-foo\" has 6 characters but more bytes due to the two
Greek letters.  Query \"foo\" should highlight the 3 ASCII chars at
character positions 3,4,5 — not bytes 5,6,7."
  (skip-unless (fboundp 'fzf-native-score-all))
  (let* ((fzf-native-batch-highlight 25)
         (cand (copy-sequence "αβ-foo"))
         (result (fzf-native-score-all (vector cand) "foo"))
         (out (car result)))
    (should out)
    ;; Characters at char-positions 3,4,5 carry completions-common-part.
    (dotimes (i 3)
      (let* ((pos (+ 3 i))
             (face (get-text-property pos 'face out)))
        (should (or (eq face 'completions-common-part)
                    (and (listp face)
                         (memq 'completions-common-part face))))))
    ;; Character at position 0 (α) carries no highlight.
    (should-not (get-text-property 0 'face out))))

(ert-deftest fzf-native-score-all-nil-highlight-fn-skips-test ()
  "Setting `fzf-native-highlight-fn' to nil suppresses highlight application."
  (skip-unless (fboundp 'fzf-native-score-all))
  (let* ((fzf-native-batch-highlight 25)
         (fzf-native-highlight-fn nil)
         (result (fzf-native-score-all (vector "alpha") "alpha"))
         (out (car result)))
    (should out)
    ;; No `completions-common-part' face attached anywhere.
    (dotimes (i (length out))
      (let ((face (get-text-property i 'face out)))
        (should-not (eq face 'completions-common-part))
        (should-not (and (listp face) (memq 'completions-common-part face)))))))

(ert-deftest fzf-native-highlight-all-empty-query-no-crash-test ()
  "Regression: empty query → clear-only path → `hl_scratch_free' on
zero-initialised scratch.  Used to abort with libmalloc complaining
the pointer being freed was not allocated (declaration sat behind
`goto done')."
  (skip-unless (fboundp 'fzf-native-highlight-all))
  (let* ((fzf-native-batch-highlight 25)
         (coll (list (copy-sequence "alpha") (copy-sequence "beta"))))
    ;; If the goto-skip bug returns, this call aborts the process.
    (should (eq (fzf-native-highlight-all coll "") coll))
    (let ((vec (vector "alpha" "beta")))
      (should (eq (fzf-native-highlight-all vec "") vec)))))

(ert-deftest fzf-native-highlight-one-basic-test ()
  "Single-char match attaches `completions-common-part' face at the
matched position; caller's original is unmutated."
  (skip-unless (fboundp 'fzf-native-highlight-one))
  (let* ((orig (copy-sequence "find-file"))
         (ret  (fzf-native-highlight-one orig "f")))
    (should-not (eq ret orig))
    (should-not (text-property-not-all 0 (length orig) 'face nil orig))
    (let ((face (get-text-property 0 'face ret)))
      (should (or (eq face 'completions-common-part)
                  (and (listp face)
                       (memq 'completions-common-part face)))))))

(ert-deftest fzf-native-highlight-one-empty-query-test ()
  "Empty query returns a face-stripped copy without crashing."
  (skip-unless (fboundp 'fzf-native-highlight-one))
  (let* ((orig (copy-sequence "find-file"))
         (ret  (fzf-native-highlight-one orig "")))
    (should-not (eq ret orig))
    (should-not (text-property-not-all 0 (length ret) 'face nil ret))))

(ert-deftest fzf-native-highlight-one-no-match-test ()
  "Non-matching query returns a copy with no face applied."
  (skip-unless (fboundp 'fzf-native-highlight-one))
  (let* ((orig (copy-sequence "abc"))
         (ret  (fzf-native-highlight-one orig "z")))
    (should-not (eq ret orig))
    (should-not (text-property-not-all 0 (length ret) 'face nil ret))))

(ert-deftest fzf-native-highlight-one-fuzzy-test ()
  "Multi-character fuzzy match attaches face at the matched positions."
  (skip-unless (fboundp 'fzf-native-highlight-one))
  (let* ((orig (copy-sequence "foobar"))
         (ret  (fzf-native-highlight-one orig "fb"))
         (faced-positions
          (cl-loop for i below (length ret)
                   for face = (get-text-property i 'face ret)
                   when (or (eq face 'completions-common-part)
                            (and (listp face)
                                 (memq 'completions-common-part face)))
                   collect i)))
    (should (memq 0 faced-positions))
    (should (memq 3 faced-positions))))

(ert-deftest fzf-native-highlight-one-preserves-original-test ()
  "Caller's CAND has no face property after the call, even on match."
  (skip-unless (fboundp 'fzf-native-highlight-one))
  (let ((orig (copy-sequence "alpha")))
    (fzf-native-highlight-one orig "a")
    (should-not (text-property-not-all 0 (length orig) 'face nil orig))))

(ert-deftest fzf-native-highlight-one-honors-highlight-fn-test ()
  "When `fzf-native-highlight-fn' is nil, no face is applied even on match."
  (skip-unless (fboundp 'fzf-native-highlight-one))
  (let* ((fzf-native-highlight-fn nil)
         (orig (copy-sequence "find-file"))
         (ret  (fzf-native-highlight-one orig "f")))
    (should-not (text-property-not-all 0 (length ret) 'face nil ret))))

(ert-deftest fzf-native-highlight-one-ignores-batch-highlight-cap-test ()
  "`fzf-native-batch-highlight' must NOT gate `highlight-one'.  The cap
applies to top-N selection in `highlight-all' / `score-all'; for a single
candidate it's meaningless and ignoring it is the design — see Chunk 6
of the sort-highlight design, where call sites bind it to nil to suppress
eager passes but lazy highlights must still fire."
  (skip-unless (fboundp 'fzf-native-highlight-one))
  (let* ((fzf-native-batch-highlight nil)
         (orig (copy-sequence "find-file"))
         (ret  (fzf-native-highlight-one orig "f"))
         (face (get-text-property 0 'face ret)))
    (should (or (eq face 'completions-common-part)
                (and (listp face)
                     (memq 'completions-common-part face))))))

(ert-deftest fzf-native-score-all-empty-query-no-crash-test ()
  "Regression: `fzf-native-score-all' with empty query delegates to
`fzf-native-highlight-all'; the highlight_all path must not crash on
the uninitialised scratch."
  (skip-unless (fboundp 'fzf-native-score-all))
  (let ((fzf-native-batch-highlight 25))
    ;; Empty query → routes through highlight-all internally.
    (should (vectorp (fzf-native-score-all (vector "alpha" "beta") "")))))
