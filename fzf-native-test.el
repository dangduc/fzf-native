;;; fzf-native-test.el --- `fzf-native' test. -*- lexical-binding: t; -*-
(require 'ert)
(require 'fzf-native)

(unless (fboundp 'fzf-native-score)
  (if-let* ((module (getenv "FZF_NATIVE_TEST_MODULE")))
      (progn
        (module-load module)
        (setq fzf-native-loaded t))
    (fzf-native-load-dyn)))
(when (fboundp 'fzf-native--verify-session-abi)
  (fzf-native--verify-session-abi))



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

(ert-deftest fzf-native-make-slab-rejects-invalid-size-test ()
  "Invalid public slab sizes signal an error instead of aborting Emacs."
  (should-error (fzf-native-make-slab -1 1))
  (should-error (fzf-native-make-slab most-positive-fixnum
                                      most-positive-fixnum)))

(ert-deftest fzf-native-score-empty-query-test ()
  (let ((result (fzf-native-score "abcdefghi" "")))
    (should (equal result '(0)))))

(ert-deftest fzf-native-score-empty-str-test ()
  (let ((result (fzf-native-score "" "acef")))
    (should (equal result '(0)))))

(ert-deftest fzf-native-score-empty-str-inverse-query-test ()
  "An empty candidate can match a non-empty inverse query."
  (should (> (car (fzf-native-score "" "!acef")) 0))
  (should (equal (fzf-native-score-all '("") "!acef") '(""))))

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

;;
;; Exact-value oracle tests for the operators that the rest of the
;; CI-run main suite only exercises as booleans: suffix ($), equal
;; (^...$), and OR (|).  Every expected number is derived BY HAND from
;; the fzf scoring constants below; the binary merely confirms it.
;;
;; Constants (fzf.c:52-59):
;;   ScoreMatch=16  ScoreGapStart=-3  ScoreGapExtention=-1
;;   BonusBoundary=8  BonusNonWord=8  BonusCamel123=7
;;   BonusConsecutive=4  BonusFirstCharMultiplier=2
;; char_class_of (ASCII): a-z=Lower, A-Z=Upper, 0-9=Number, else=NonWord
;;   ('.', '/', ' ' are all NonWord).
;; bonus_for(prev,cur): NonWord->word = BonusBoundary(8); else 0 between
;;   two word letters.  In a run, char k>0 takes
;;   max(bonus, first_bonus, BonusConsecutive); first_bonus carries the
;;   run's opening boundary bonus.  The first matched char's bonus is
;;   doubled (BonusFirstCharMultiplier).
;;
;; Two closed forms used repeatedly:
;;   * contiguous run of M chars whose first char sits at a word boundary:
;;       (ScoreMatch + 2*BonusBoundary) + (M-1)*(ScoreMatch + BonusBoundary)
;;       = 32 + 24*(M-1) = 24*M + 8.
;;   * equal match (^X$): hardcoded (ScoreMatch+BonusBoundary)*M +
;;       (BonusFirstCharMultiplier-1)*BonusBoundary = 24*M + 8 (fzf.c:1280).

(ert-deftest fzf-native-score-suffix-operator-test ()
  "Suffix ($) exact scores; boundary vs non-boundary first char.

\"foobar\" \"bar$\": suffix \"bar\" sits at byte range [3,6).  The char
before it is 'o' (Lower), so the run opens with NO boundary bonus.
  b: ScoreMatch + 0*2          = 16
  a: ScoreMatch + max(0,0,4)=4 = 20
  r: ScoreMatch + 4            = 20   -> total 56.
General form (no opening boundary): 16*M + 4*(M-1) = 16*3 + 4*2 = 56.

\"foo.bar\" \"bar$\": suffix \"bar\" at [4,7); the char before it is '.'
\(NonWord), so the run opens at a word boundary.
  b: ScoreMatch + 2*BonusBoundary = 32
  a: ScoreMatch + 8               = 24
  r: ScoreMatch + 8               = 24   -> total 80 = 24*3 + 8."
  (should (equal (fzf-native-score "foobar"  "bar$") '(56)))
  (should (equal (fzf-native-score "foo.bar" "bar$") '(80)))
  ;; A suffix that isn't actually at the end does not match.
  (should (equal (fzf-native-score "barfoo"  "bar$") '(0))))

(ert-deftest fzf-native-score-equal-operator-test ()
  "Equal (^...$) exact scores: closed form 24*M + 8, length-exact.

equal_match returns (ScoreMatch+BonusBoundary)*M +
\(BonusFirstCharMultiplier-1)*BonusBoundary = 24*M + 8 (fzf.c:1280).
  M=3 \"^abc$\"  -> 24*3 + 8 = 80
  M=4 \"^abcd$\" -> 24*4 + 8 = 104
The candidate length must equal the pattern length exactly, so a
shorter pattern against a longer candidate scores 0."
  (should (equal (fzf-native-score "abc"  "^abc$")  '(80)))
  (should (equal (fzf-native-score "abcd" "^abcd$") '(104)))
  ;; Length mismatch -> no equal match.
  (should (equal (fzf-native-score "foobar" "^foo$")    '(0)))
  (should (equal (fzf-native-score "abc"    "^abcd$")  '(0))))

(ert-deftest fzf-native-score-or-operator-test ()
  "OR (|) takes the FIRST matching term's score within the term-set.

In a term-set the evaluator breaks on the first term that matches and
uses that term's score (fzf.c:2410-2419); it is NOT the max.  Proof:
the same two terms reordered give different totals.

text \"abcdef\":
  \"^abc | ^abcdef$\": term 1 is prefix \"abc\" -> contiguous run of 3 at
     the start boundary = 24*3 + 8 = 80.  Matches first, so total 80
     even though equal \"^abcdef$\" (24*6+8=152) would score higher.
  \"^abcdef$ | ^abc\": term 1 is equal \"abcdef\" = 24*6 + 8 = 152.
     Matches first -> total 152.
  \"zzz | ^abc\": term 1 \"zzz\" does not match; falls through to prefix
     \"abc\" = 80.
  \"zzz | qqq\": neither term matches -> 0."
  (should (equal (fzf-native-score "abcdef" "^abc | ^abcdef$") '(80)))
  (should (equal (fzf-native-score "abcdef" "^abcdef$ | ^abc") '(152)))
  (should (equal (fzf-native-score "abcdef" "zzz | ^abc")      '(80)))
  (should (equal (fzf-native-score "abcdef" "zzz | qqq")       '(0))))

(ert-deftest fzf-native-score-and-sum-of-operators-test ()
  "AND (space) sums the per-term-set scores; combine new operators.

text \"foo.bar\", query \"^foo bar$\" = two term-sets ANDed:
  prefix \"foo\" : contiguous run of 3 at start boundary = 24*3 + 8 = 80.
  suffix \"bar\" : opens after '.' (boundary)            = 24*3 + 8 = 80.
Sum = 160."
  (should (equal (fzf-native-score "foo.bar" "^foo bar$") '(160)))
  ;; If either ANDed term fails, the whole pattern scores 0.
  (should (equal (fzf-native-score "foo.bar" "^foo zzz$") '(0))))

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
;; Filter-only fast path (sync) — end-to-end guard.
;;
;; `fzf-native-score-all' switches to the cheap `fzf_has_match' path when
;; `fzf-native-filter-only-p' fires (see the defcustoms
;; `fzf-native-filter-only-min-pool' / `-length' / `-logic').  The fast
;; path skips scoring and top-K sorting, so the *order* of results and the
;; `completion-score' property differ from full scoring — but the matched
;; candidate SET must be identical.
;;
;; This guards a real regression where the filter-only path used the
;; byte-wise ASCII matcher for every term: it dropped ALL UTF-8 matches
;; and let inverted UTF-8 terms (`!café') through.  The fix defers any
;; non-ASCII / inverted term-set to the full scorer inside
;; `fzf_has_match'.  We assert the active-vs-disabled sets are `equal'
;; for ASCII, case-folded UTF-8, Greek, CJK, and inverted-non-ASCII
;; queries.

(defun fzf-native-test--score-all-set (coll query)
  "Sorted, property-stripped match set for `fzf-native-score-all'.
Neutralises the order / `completion-score' differences between the
filter-only and full paths so only set membership is compared."
  (sort (mapcar #'substring-no-properties
                (fzf-native-score-all coll query))
        #'string<))

(ert-deftest fzf-native-score-all-filter-only-matches-full-test ()
  "Filter-only active vs disabled return the SAME matched set.

With `fzf-native-filter-only-min-pool' = 1 and OR logic, the predicate
fires for any non-empty pool, so the fast path is genuinely exercised;
with min-pool = 10000000 (default-ish) it never fires on this tiny
collection.  Both settings must agree on the match set for every query,
including the UTF-8 / inverted cases that the regression broke."
  (skip-unless (fboundp 'fzf-native-filter-only-p))
  (let ((coll '("café" "CAFÉ" "résumé" "naïve"
                "中文" "测试中文" "中文测试"
                "Θεσσαλονίκη" "θεωρία"
                "hello" "HELLO" "world" "test")))
    (dolist (query '("hello"     ; ASCII, case-fold (matches hello + HELLO)
                     "café"      ; UTF-8, case-fold (matches café + CAFÉ)
                     "θε"        ; Greek, case-fold (matches both Greek words)
                     "中文"      ; CJK (matches all three 中文* candidates)
                     "!café"     ; inverted non-ASCII term
                     "résumé"    ; UTF-8 exact-ish
                     "zzz"))     ; matches nothing
      ;; Sanity: the predicate really does flip between the two settings,
      ;; so a green assertion can't be vacuous (path never engaged).
      (let ((fzf-native-filter-only-min-pool 1)
            (fzf-native-filter-only-length nil)
            (fzf-native-filter-only-logic 'or))
        (should (fzf-native-filter-only-p (length query) (length coll))))
      (let ((fzf-native-filter-only-min-pool 10000000)
            (fzf-native-filter-only-length nil)
            (fzf-native-filter-only-logic 'or))
        (should-not (fzf-native-filter-only-p (length query) (length coll))))
      ;; Same matched set under both settings.
      (let ((active
             (let ((fzf-native-filter-only-min-pool 1)
                   (fzf-native-filter-only-length nil)
                   (fzf-native-filter-only-logic 'or))
               (fzf-native-test--score-all-set coll query)))
            (disabled
             (let ((fzf-native-filter-only-min-pool 10000000)
                   (fzf-native-filter-only-length nil)
                   (fzf-native-filter-only-logic 'or))
               (fzf-native-test--score-all-set coll query))))
        (should (equal active disabled))
        ;; Spot-check the load-bearing cases actually carry content / are
        ;; correctly empty, so "equal but both wrong" can't pass silently.
        (cond
         ((equal query "café")
          (should (equal active '("CAFÉ" "café"))))
         ((equal query "中文")
          (should (equal active '("中文" "中文测试" "测试中文"))))
         ((equal query "!café")
          ;; Everything EXCEPT the two café candidates.
          (should-not (member "café" active))
          (should-not (member "CAFÉ" active))
          (should (member "résumé" active))
          (should (member "中文" active)))
         ((equal query "zzz")
          (should (null active))))))))

(ert-deftest fzf-native-score-all-filter-only-length-counts-characters-test ()
  "Filter-only query thresholds count characters, not UTF-8 bytes."
  (let ((fzf-native-filter-only-min-pool nil)
        (fzf-native-filter-only-length 1)
        (fzf-native-filter-only-logic 'or)
        (fzf-native-batch-highlight nil))
    (let ((ascii (car (fzf-native-score-all (vector "abc") "a")))
          (utf8 (car (fzf-native-score-all (vector "你好") "你"))))
      ;; Filter-only deliberately omits `completion-score'.  Both one-character
      ;; queries must therefore take that path despite differing byte lengths.
      (should ascii)
      (should utf8)
      (should-not (get-text-property 0 'completion-score ascii))
      (should-not (get-text-property 0 'completion-score utf8)))))

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

(defun fzf-native-test--wait-for-request
    (handle request-id &optional timeout require-candidates)
  "Wait for REQUEST-ID on HANDLE and return its terminal snapshot.

When REQUIRE-CANDIDATES is non-nil, skip stale or empty warm-up completions.
Reentry tests use this option because their callback needs one candidate."
  (let ((deadline (+ (float-time) (or timeout 5.0)))
        snapshot)
    (while (and (< (float-time) deadline)
                (progn
                  (setq snapshot
                        (fzf-native-async-snapshot handle request-id))
                  (or (memq (plist-get snapshot :state) '(queued running))
                      (and require-candidates
                           (eq (plist-get snapshot :state) 'complete)
                           (or (plist-get snapshot :stale)
                               (null (plist-get snapshot :candidates)))))))
      (sleep-for 0.01))
    snapshot))

(defun fzf-native-test--wait-for-producer (handle &optional timeout)
  "Wait for HANDLE's producer to finish and return its status."
  (let ((deadline (+ (float-time) (or timeout 5.0)))
        status)
    (while (and (< (float-time) deadline)
                (progn
                  (setq status (fzf-native-async-status handle))
                  (not (plist-get status :reader-done))))
      (sleep-for 0.01))
    status))

(ert-deftest fzf-native-async-lifecycle-test ()
  "Start → wait for data → generation advances → stop."
  (let ((handle (fzf-native-async-start "printf '%s\\n' foo bar baz")))
    (unwind-protect
        (should (fzf-native-test--wait-for-data handle))
      (fzf-native-async-stop handle))))

(ert-deftest fzf-native-session-abi-handshake-test ()
  "The loaded module and Elisp session contracts must agree exactly."
  (skip-unless (fboundp 'fzf-native-session-abi-version))
  (should (= (fzf-native-session-abi-version)
             fzf-native-session-abi-required))
  (should (fzf-native--verify-session-abi)))

(ert-deftest fzf-native-native-handles-are-kind-checked-test ()
  "Slab and async-session user pointers must never be cross-cast.
Every public async entry point rejects a slab without mutating it, and the
batch scorer rejects a live session in its optional slab position."
  (skip-unless (fboundp 'fzf-native-async-status))
  (let ((slab (fzf-native-make-default-slab))
        (handle (fzf-native-async-start "printf '%s\\n' alpha")))
    (unwind-protect
        (progn
          (dolist (call (list
                         (lambda () (fzf-native-async-stop slab))
                         (lambda () (fzf-native-async-generation slab))
                         (lambda () (fzf-native-async-submit slab "a" 10))
                         (lambda () (fzf-native-async-snapshot slab))
                         (lambda () (fzf-native-async-status slab))
                         (lambda () (fzf-native-async-candidates slab "a" 10))
                         (lambda () (fzf-native-async-stats slab))
                         (lambda () (fzf-native-async-result-fresh-p slab "a"))))
            (should-error (funcall call)))
          ;; Rejected calls must leave both objects usable as their real kind.
          (should (numberp (car (fzf-native-score "alpha" "a" slab))))
          (should-error (fzf-native-score "alpha" "a" handle))
          (should (listp (fzf-native-async-status handle))))
      (fzf-native-async-stop handle))))

(ert-deftest fzf-native-async-producer-clean-completion-test ()
  "A zero producer exit is visible as a clean terminal state."
  (skip-unless (fboundp 'fzf-native-async-status))
  (let ((handle (fzf-native-async-start "printf '%s\\n' foo")))
    (unwind-protect
        (let ((status (fzf-native-test--wait-for-producer handle)))
          (should (plist-get status :reader-done))
          (should (eq (plist-get status :producer-state) 'complete))
          (should (= (plist-get status :producer-exit-status) 0))
          (should-not (plist-get status :producer-error)))
      (fzf-native-async-stop handle))))

(ert-deftest fzf-native-async-producer-honors-working-directory-test ()
  "The child runs in DIR, rather than silently inheriting Emacs's cwd."
  (skip-unless (memq system-type '(darwin gnu/linux berkeley-unix)))
  (let* ((dir (make-temp-file "fzf-native-producer-dir-" t))
         (handle (fzf-native-async-start "pwd" dir)))
    (unwind-protect
        (progn
          (should (plist-get (fzf-native-test--wait-for-producer handle)
                             :reader-done))
          (let* ((request-id (fzf-native-async-submit handle "" 10))
                 (snapshot (fzf-native-test--wait-for-request
                            handle request-id))
                 (candidate (car (plist-get snapshot :candidates))))
            (should (stringp candidate))
            (should (equal (file-truename candidate)
                           (file-truename dir)))))
      (when handle (fzf-native-async-stop handle))
      (delete-directory dir))))

(ert-deftest fzf-native-async-producer-rejects-missing-directory-test ()
  "A failed child chdir is reported synchronously and creates no session."
  (skip-unless (memq system-type '(darwin gnu/linux berkeley-unix)))
  (let ((missing (make-temp-name
                  (expand-file-name "fzf-native-missing-dir-"
                                    temporary-file-directory))))
    (should-not (file-exists-p missing))
    (should-error (fzf-native-async-start "pwd" missing)
                  :type 'error)))

(ert-deftest fzf-native-async-producer-rejects-missing-shell-test ()
  "An exec failure is reported before a session handle can escape."
  (skip-unless (memq system-type '(darwin gnu/linux berkeley-unix)))
  (let ((shell-file-name
         (make-temp-name
          (expand-file-name "fzf-native-missing-shell-"
                            temporary-file-directory)))
        (shell-command-switch "-c"))
    (should-error (fzf-native-async-start "printf unreachable")
                  :type 'error)))

(ert-deftest fzf-native-async-producer-resolves-shell-through-exec-path-test ()
  "A bare `shell-file-name' is resolved like Emacs subprocess commands."
  (skip-unless (memq system-type '(darwin gnu/linux berkeley-unix)))
  (let ((shell-file-name "sh")
        (shell-command-switch "-c")
        (exec-path '("/bin" "/usr/bin")))
    (let ((handle (fzf-native-async-start "printf '%s\\n' shell-ok")))
      (unwind-protect
          (progn
            (should (plist-get (fzf-native-test--wait-for-producer handle)
                               :reader-done))
            (let* ((request-id (fzf-native-async-submit handle "shell" 10))
                   (snapshot (fzf-native-test--wait-for-request
                              handle request-id)))
              (should (equal (plist-get snapshot :candidates)
                             '("shell-ok")))))
        (fzf-native-async-stop handle)))))

(ert-deftest fzf-native-async-producer-rejects-embedded-nul-inputs-test ()
  "Producer strings cannot be silently truncated at an embedded NUL."
  (skip-unless (fboundp 'fzf-native-async-start))
  (should-error (fzf-native-async-start (concat "printf before" "\0" "after"))
                :type 'error)
  (should-error (fzf-native-async-start "pwd" (concat "/tmp" "\0" "/missing"))
                :type 'error)
  (let ((shell-command-switch (concat "-c" "\0" "ignored")))
    (should-error (fzf-native-async-start "printf unreachable")
                  :type 'error))
  (let ((exec-path (list (concat "/bin" "\0" "/ignored"))))
    (should-error (fzf-native-async-start "printf unreachable")
                  :type 'error)))

(ert-deftest fzf-native-async-producer-rejects-nul-output-test ()
  "A producer NUL byte is a visible failure, never a truncated candidate."
  (skip-unless (and (fboundp 'fzf-native-async-submit)
                    (executable-find "python3")))
  (let ((handle
         (fzf-native-async-start
          "python3 -u -c 'import os
os.write(1, b\"valid\\nab\\x00cd\\nlate\\n\")
'")))
    (unwind-protect
        (let ((status (fzf-native-test--wait-for-producer handle)))
          (should (plist-get status :reader-done))
          (should (eq (plist-get status :producer-state) 'failed))
          (should (equal (plist-get status :producer-error)
                         "producer output contains a NUL byte"))
          ;; Lines published before the malformed one remain available, but
          ;; neither its truncated prefix nor any later bytes may escape.
          (let* ((request-id (fzf-native-async-submit handle "" 10))
                 (snapshot (fzf-native-test--wait-for-request
                            handle request-id)))
            (should (eq (plist-get snapshot :state) 'complete))
            (should (equal (plist-get snapshot :candidates) '("valid")))
            (should (equal (plist-get snapshot :producer-error)
                           "producer output contains a NUL byte"))))
      (fzf-native-async-stop handle))))

(ert-deftest fzf-native-async-producer-nonzero-exit-test ()
  "A nonzero producer exit reports an error without losing its candidates."
  (skip-unless (fboundp 'fzf-native-async-status))
  (let ((handle (fzf-native-async-start
                 "printf '%s\\n' foo; exit 7")))
    (unwind-protect
        (let ((status (fzf-native-test--wait-for-producer handle)))
          (should (plist-get status :reader-done))
          (should (eq (plist-get status :producer-state) 'failed))
          (should (= (plist-get status :producer-exit-status) 7))
          (should (equal (plist-get status :producer-error)
                         "producer exited with status 7"))
          (let* ((request-id (fzf-native-async-submit handle "foo" 10))
                 (snapshot (fzf-native-test--wait-for-request
                            handle request-id)))
            (should (eq (plist-get snapshot :state) 'complete))
            (should (equal (plist-get snapshot :candidates) '("foo")))
            (should (equal (plist-get snapshot :producer-error)
                           "producer exited with status 7"))))
      (fzf-native-async-stop handle))))

(ert-deftest fzf-native-async-delayed-producer-failure-bumps-generation-test ()
  "A failure after stdout EOF is a new poll-visible session event."
  (skip-unless (fboundp 'fzf-native-async-status))
  (let* ((gate-directory
          (make-temp-file "fzf-native-producer-gate-" t))
         (gate (expand-file-name "release" gate-directory))
         handle)
    (unwind-protect
        (progn
          (setq handle
                (fzf-native-async-start
                 (format
                  (concat "printf '%%s\\n' partial; exec 1>&-; "
                          "while test ! -e %s; do sleep 0.05; done; exit 7")
                  (shell-quote-argument gate))))
          (should (fzf-native-test--wait-for-data handle))
          (let* ((before (fzf-native-async-status handle))
                 (before-generation
                  (plist-get before :snapshot-generation)))
            (should-not (plist-get before :reader-done))
            (write-region "" nil gate nil 'silent)
            (let ((final (fzf-native-test--wait-for-producer handle)))
              (should (eq (plist-get final :producer-state) 'failed))
              (should (equal (plist-get final :producer-error)
                             "producer exited with status 7"))
              (should (> (plist-get final :snapshot-generation)
                         before-generation)))))
      (when handle
        (fzf-native-async-stop handle))
      (delete-directory gate-directory t))))

(ert-deftest fzf-native-async-stop-invalidates-handle-test ()
  "After stop, generation returns nil (handle is invalidated)."
  (let ((handle (fzf-native-async-start "printf '%s\\n' foo")))
    (fzf-native-test--wait-for-data handle)
    (fzf-native-async-stop handle)
    (should (null (fzf-native-async-generation handle)))))

(ert-deftest fzf-native-async-submit-snapshot-lifecycle-test ()
  "Submit once, poll snapshots, and receive an owned completed result."
  (skip-unless (fboundp 'fzf-native-async-submit))
  (let ((handle (fzf-native-async-start
                 "printf '%s\\n' foo bar baz foobaz"))
        (fzf-native-async-highlight nil))
    (unwind-protect
        (progn
          (should (fzf-native-test--wait-for-data handle))
          (let* ((request-id (fzf-native-async-submit handle "foo" 10))
                 (initial (fzf-native-async-snapshot handle request-id))
                 (final (fzf-native-test--wait-for-request
                         handle request-id)))
            (should (integerp request-id))
            (should (> request-id 0))
            (should (= (plist-get initial :request-id) request-id))
            (should (memq (plist-get initial :state)
                          '(queued running complete)))
            (should (eq (plist-get final :state) 'complete))
            (should (= (plist-get final :result-request-id) request-id))
            (should-not (plist-get final :stale))
            (should (equal (plist-get final :query) "foo"))
            (should (member "foo" (plist-get final :candidates)))
            (should (member "foobaz" (plist-get final :candidates)))
            (should-not (member "bar" (plist-get final :candidates)))
            (should (= (plist-get final :pool-generation)
                       (plist-get final :result-pool-generation)))
            (should (> (plist-get final :progress-total) 0))
            (should (= (plist-get final :progress-completed)
                       (plist-get final :progress-total)))
            (let ((status (fzf-native-async-status handle)))
              (should (eq (plist-get status :state) 'complete))
              (should (= (plist-get status :request-id) request-id)))))
      (fzf-native-async-stop handle))))

(ert-deftest fzf-native-async-snapshot-retains-prior-result-while-running-test ()
  "Keep the prior completed list while a new request scans the pool."
  (skip-unless (fboundp 'fzf-native-async-submit))
  (let ((handle (fzf-native-async-start "seq 1 500000"))
        (fzf-native-async-highlight nil))
    (unwind-protect
        (progn
          (let ((producer (fzf-native-test--wait-for-producer handle 15.0)))
            (should (plist-get producer :reader-done))
            (should (= (plist-get producer :pool-generation) 500000)))
          (let* ((old-id (fzf-native-async-submit handle "1" 100))
                 (old (fzf-native-test--wait-for-request handle old-id 10.0))
                 (old-candidates (plist-get old :candidates))
                 (new-id (fzf-native-async-submit handle "999999" 100))
                 (pending (fzf-native-async-snapshot handle new-id)))
            (should (eq (plist-get old :state) 'complete))
            (should (= (length old-candidates) 100))
            (should (memq (plist-get pending :state) '(queued running)))
            (should (= (plist-get pending :request-id) new-id))
            (should (= (plist-get pending :result-request-id) old-id))
            (should (plist-get pending :stale))
            (should (equal (plist-get pending :query) "1"))
            (should (equal (plist-get pending :candidates) old-candidates))
            (let ((final
                   (fzf-native-test--wait-for-request handle new-id 10.0)))
              (should (eq (plist-get final :state) 'complete))
              (should (= (plist-get final :result-request-id) new-id))
              (should-not (plist-get final :stale)))))
      (fzf-native-async-stop handle))))

(ert-deftest fzf-native-async-snapshot-preserves-request-query-test ()
  "Pattern parsing must not mutate the query recorded in a snapshot."
  (skip-unless (fboundp 'fzf-native-async-submit))
  (let ((handle (fzf-native-async-start "printf '%s\\n' foo foobar"))
        (fzf-native-async-highlight nil))
    (unwind-protect
        (progn
          (should (fzf-native-test--wait-for-data handle))
          (let* ((query "foo ")
                 (request-id (fzf-native-async-submit handle query 10))
                 (snapshot (fzf-native-test--wait-for-request
                            handle request-id)))
            (should (eq (plist-get snapshot :state) 'complete))
            (should (equal (plist-get snapshot :query) query))
            (should (= (plist-get snapshot :result-request-id)
                       request-id))))
      (fzf-native-async-stop handle))))

(ert-deftest fzf-native-async-submit-supersedes-queued-request-test ()
  "A new query supersedes queued obsolete work and owns the final snapshot."
  (skip-unless (fboundp 'fzf-native-async-submit))
  (let ((handle (fzf-native-async-start "seq 1 100000"))
        (fzf-native-async-highlight nil))
    (unwind-protect
        (progn
          (should (fzf-native-test--wait-for-data handle 10.0))
          (let* ((old-id (fzf-native-async-submit handle "11111" 20))
                 (new-id (fzf-native-async-submit handle "99999" 20))
                 (final (fzf-native-test--wait-for-request
                         handle new-id 10.0)))
            (should (> new-id old-id))
            (should (eq (plist-get final :state) 'complete))
            (should (= (plist-get final :result-request-id) new-id))
            (should (member "99999" (plist-get final :candidates)))
            (should (eq (plist-get
                         (fzf-native-async-snapshot handle old-id)
                         :state)
                        'superseded))))
      (fzf-native-async-stop handle))))

(ert-deftest fzf-native-async-submit-retries-after-input-growth-test ()
  "Candidate growth retries the latest request without another submission."
  (skip-unless (and (fboundp 'fzf-native-async-submit)
                    (executable-find "python3")))
  (let* ((gate (make-temp-file "fzf-native-growth-gate-"))
         (handle (fzf-native-async-start
                  (format "python3 -u -c 'import os, sys, time
gate = sys.argv[1]
for i in range(3): print(f\"a{i}\", flush=True)
while os.path.exists(gate): time.sleep(0.01)
print(\"a-late\", flush=True)
for i in range(40): print(f\"b{i}\", flush=True)
' %s" (shell-quote-argument gate))))
         (fzf-native-async-highlight nil))
    (unwind-protect
        (progn
          (should (fzf-native-test--wait-for-data handle))
          (let* ((request-id (fzf-native-async-submit handle "a" 100))
                 (first (fzf-native-test--wait-for-request
                         handle request-id)))
            (should (eq (plist-get first :state) 'complete))
            (should (> (plist-get first :snapshot-generation) 0))
            (should-not (member "a-late" (plist-get first :candidates)))

            ;; Release the producer.  From here onward, poll only snapshots.
            ;; A second submit or compatibility candidates call would hide a
            ;; missing native growth retry.
            (delete-file gate)
            (let ((deadline (+ (float-time) 10.0))
                  snapshot)
              (while (and (< (float-time) deadline)
                          (progn
                            (setq snapshot
                                  (fzf-native-async-snapshot
                                   handle request-id))
                            (not (and
                                  (eq (plist-get snapshot :state) 'complete)
                                  (not (plist-get snapshot :stale))
                                  (member "a-late"
                                          (plist-get snapshot :candidates))))))
                (sleep-for 0.01))
              (should (eq (plist-get snapshot :state) 'complete))
              (should-not (plist-get snapshot :stale))
              (should (= (plist-get snapshot :request-id) request-id))
              (should (= (plist-get snapshot :result-request-id)
                         request-id))
              (should (> (plist-get snapshot :snapshot-generation)
                         (plist-get first :snapshot-generation)))
              (should (member "a-late"
                              (plist-get snapshot :candidates)))
              (should (> (plist-get snapshot :progress-total) 0))
              (should (= (plist-get snapshot :progress-completed)
                         (plist-get snapshot :progress-total)))
              (should (= (plist-get snapshot :pool-generation)
                         (plist-get snapshot :result-pool-generation))))))
      (when (file-exists-p gate) (delete-file gate))
      (fzf-native-async-stop handle))))

(ert-deftest fzf-native-async-cache-expands-result-capacity-test ()
  "A cached limit-1 result does not satisfy a later limit-3 request."
  (skip-unless (fboundp 'fzf-native-async-submit))
  (let ((handle (fzf-native-async-start
                 "printf '%s\\n' alpha alpine alps beta"))
        (fzf-native-async-highlight nil))
    (unwind-protect
        (progn
          (should (fzf-native-test--wait-for-data handle))
          (let* ((small-id (fzf-native-async-submit handle "al" 1))
                 (small (fzf-native-test--wait-for-request handle small-id))
                 (large-id (fzf-native-async-submit handle "al" 3))
                 (large (fzf-native-test--wait-for-request handle large-id)))
            (should (eq (plist-get small :state) 'complete))
            (should (= (length (plist-get small :candidates)) 1))
            (should (> large-id small-id))
            (should (eq (plist-get large :state) 'complete))
            (should (= (length (plist-get large :candidates)) 3))))
      (fzf-native-async-stop handle))))

(ert-deftest fzf-native-async-cache-narrows-result-capacity-test ()
  "A cached unlimited result is capped for a later limit-1 request."
  (skip-unless (fboundp 'fzf-native-async-submit))
  (let ((handle (fzf-native-async-start
                 "printf '%s\\n' alpha alpine alps beta"))
        (fzf-native-async-highlight nil))
    (unwind-protect
        (progn
          (should (fzf-native-test--wait-for-data handle))
          (let* ((all-id (fzf-native-async-submit handle "al" 0))
                 (all (fzf-native-test--wait-for-request handle all-id))
                 (small-id (fzf-native-async-submit handle "al" 1))
                 (small (fzf-native-test--wait-for-request handle small-id)))
            (should (eq (plist-get all :state) 'complete))
            (should (= (length (plist-get all :candidates)) 3))
            (should (> small-id all-id))
            (should (eq (plist-get small :state) 'complete))
            (should (= (plist-get small :limit) 1))
            (should (= (length (plist-get small :candidates)) 1))))
      (fzf-native-async-stop handle))))

(ert-deftest fzf-native-async-cache-rescores-filter-only-limit-window-test ()
  "A larger filter-only window is not authoritative for a smaller limit."
  (skip-unless (fboundp 'fzf-native-async-submit))
  (let* ((fzf-native-filter-only-min-pool 1)
         (fzf-native-filter-only-length nil)
         (fzf-native-filter-only-logic 'or)
         (fzf-native-async-highlight nil)
         (handle (fzf-native-async-start
                  "printf '%s\\n' zzza alpha beta")))
    (unwind-protect
        (progn
          (let ((producer (fzf-native-test--wait-for-producer handle)))
            (should (plist-get producer :reader-done))
            (should (= (plist-get producer :pool-generation) 3)))
          (let* ((all-id (fzf-native-async-submit handle "a" 0))
                 (all (fzf-native-test--wait-for-request handle all-id))
                 (small-id (fzf-native-async-submit handle "a" 1))
                 (small (fzf-native-test--wait-for-request handle small-id)))
            ;; Ranking the unlimited window puts the stronger later match
            ;; first.  Limit 1 selects its window in input order, so it must
            ;; rescore and return the first match instead of truncating ALL.
            (should (equal (car (plist-get all :candidates)) "alpha"))
            (should (> small-id all-id))
            (should (eq (plist-get small :state) 'complete))
            (should (equal (plist-get small :candidates) '("zzza")))))
      (fzf-native-async-stop handle))))

(ert-deftest fzf-native-async-cache-separates-case-mode-test ()
  "Changing case mode for one query cannot reuse an incompatible result."
  (skip-unless (fboundp 'fzf-native-async-submit))
  (let ((handle (fzf-native-async-start
                 "printf '%s\\n' foo FOO Food bar"))
        (fzf-native-async-highlight nil))
    (unwind-protect
        (progn
          (should (fzf-native-test--wait-for-data handle))
          (let* ((fzf-native-case-mode 'ignore)
                 (ignore-id (fzf-native-async-submit handle "FOO" 10))
                 (ignore (fzf-native-test--wait-for-request
                          handle ignore-id)))
            (should (> (length (plist-get ignore :candidates)) 1)))
          (let* ((fzf-native-case-mode 'respect)
                 (respect-id (fzf-native-async-submit handle "FOO" 10))
                 (respect (fzf-native-test--wait-for-request
                           handle respect-id)))
            (should (eq (plist-get respect :state) 'complete))
            (should (eq (plist-get respect :case-mode) 'respect))
            (should (equal (plist-get respect :candidates) '("FOO")))))
      (fzf-native-async-stop handle))))

(ert-deftest fzf-native-async-bounded-top-k-crosses-coordinator-window-test ()
  "A positive limit returns exact stable top-K across multiple windows.
The corpus exceeds 64 native batches, so this exercises the second
coordinator window and verifies that dense matches are counted separately
from the bounded ranked result."
  (skip-unless (fboundp 'fzf-native-async-submit))
  (let* ((fzf-native-filter-only-min-pool nil)
         (fzf-native-filter-only-length nil)
         (fzf-native-async-highlight nil)
         (handle (fzf-native-async-start
                  (concat "awk 'BEGIN { for (i = 0; i < 140000; i++) "
                          "printf \"item%06d\\n\", i }'"))))
    (unwind-protect
        (progn
          (should (plist-get (fzf-native-test--wait-for-producer handle 20.0)
                             :reader-done))
          (let* ((request-id (fzf-native-async-submit handle "item" 100))
                 (snapshot (fzf-native-test--wait-for-request
                            handle request-id 20.0))
                 (expected (cl-loop for i below 100
                                    collect (format "item%06d" i))))
            (should (eq (plist-get snapshot :state) 'complete))
            (should (= (plist-get snapshot :filtered) 140000))
            (should (= (plist-get snapshot :total) 140000))
            (should (equal (plist-get snapshot :candidates) expected))))
      (fzf-native-async-stop handle))))

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

(ert-deftest fzf-native-async-candidates-rejects-invalid-query-test ()
  "A bad compatibility query signals before it can reuse stale results."
  (skip-unless (fboundp 'fzf-native-async-submit))
  (let ((handle (fzf-native-async-start "printf '%s\\n' alpha beta")))
    (unwind-protect
        (progn
          (should (plist-get (fzf-native-test--wait-for-producer handle)
                             :reader-done))
          (let* ((request-id (fzf-native-async-submit handle "a" 10))
                 (snapshot (fzf-native-test--wait-for-request
                            handle request-id)))
            (should (eq (plist-get snapshot :state) 'complete))
            ;; Omit LIMIT.  This exact shape used to continue after the
            ;; pending type error and read a zero-byte allocation in strcmp.
            (should-error (fzf-native-async-candidates handle 42)
                          :type 'wrong-type-argument)
            (should (= (plist-get (fzf-native-async-status handle)
                                  :latest-request-id)
                       request-id))))
      (fzf-native-async-stop handle))))

(ert-deftest fzf-native-async-query-apis-reject-embedded-nul-test ()
  "All asynchronous query APIs reject NUL without changing ownership."
  (skip-unless (fboundp 'fzf-native-async-submit))
  (let ((handle (fzf-native-async-start "printf '%s\\n' alpha beta"))
        (nul-query (concat "a" (string 0) "tail")))
    (unwind-protect
        (progn
          (should (plist-get (fzf-native-test--wait-for-producer handle)
                             :reader-done))
          (let* ((request-id (fzf-native-async-submit handle "a" 10))
                 (snapshot (fzf-native-test--wait-for-request
                            handle request-id)))
            (should (eq (plist-get snapshot :state) 'complete))
            ;; A completed result makes the stale-result failure observable:
            ;; the compatibility wrapper must signal, not return that result.
            (should-error (fzf-native-async-candidates handle nul-query 10)
                          :type 'error)
            (should-error (fzf-native-async-submit handle nul-query 10)
                          :type 'error)
            (should-error (fzf-native-async-result-fresh-p handle nul-query)
                          :type 'error)
            (should (= (plist-get (fzf-native-async-status handle)
                                  :latest-request-id)
                       request-id))))
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

(ert-deftest fzf-native-async-cache-inverse-extension-broadens-test ()
  "Extending an inverse term must not refine from the narrower old set."
  (skip-unless (fboundp 'fzf-native-async-submit))
  (let ((handle (fzf-native-async-start
                 "printf '%s\\n' foo foobar bar"))
        (fzf-native-async-highlight nil))
    (unwind-protect
        (progn
          (should (fzf-native-test--wait-for-data handle))
          (let* ((first-id (fzf-native-async-submit handle "!foo" 10))
                 (first (fzf-native-test--wait-for-request handle first-id))
                 (second-id (fzf-native-async-submit handle "!foobar" 10))
                 (second (fzf-native-test--wait-for-request
                          handle second-id)))
            (should (equal (plist-get first :candidates) '("bar")))
            (should (equal (sort (copy-sequence
                                  (plist-get second :candidates))
                                 #'string<)
                           '("bar" "foo")))))
      (fzf-native-async-stop handle))))

(ert-deftest fzf-native-async-cache-positive-quote-refinement-test ()
  "A longer positive quoted term scans only its cached ancestor's matches.

The leading quote toggles a positive term between exact and fuzzy matching.
Extending either form is monotone, so both global fuzzy modes may safely reuse
the prior membership set.  Compare both rounds with batch scoring to guard the
optimization's semantics, and assert `progress-total' to prove that the second
round actually took the refinement path."
  (skip-unless (fboundp 'fzf-native-async-submit))
  (let ((collection '("alpha" "alphabet" "alphanumeric"
                      "beta" "gamma" "你好-alpha")))
    (dolist (fuzzy '(t nil))
      (let* ((fzf-native-fuzzy fuzzy)
             (fzf-native-case-mode 'smart)
             (fzf-native-async-cache-size 40)
             (fzf-native-async-cache-bytes (* 1024 1024))
             (fzf-native-async-batch-cache-bytes 0)
             (fzf-native-async-highlight nil)
             (fzf-native-batch-highlight nil)
             (handle
              (fzf-native-async-start
               "printf '%s\\n' alpha alphabet alphanumeric beta gamma 你好-alpha")))
        (unwind-protect
            (progn
              (let ((producer (fzf-native-test--wait-for-producer handle)))
                (should (plist-get producer :reader-done))
                (should (= (plist-get producer :pool-generation)
                           (length collection))))
              (let* ((first-query "'alpha")
                     (second-query "'alphab")
                     (first-id
                      (fzf-native-async-submit handle first-query 20))
                     (first
                      (fzf-native-test--wait-for-request handle first-id))
                     (second-id
                      (fzf-native-async-submit handle second-query 20))
                     (second
                      (fzf-native-test--wait-for-request handle second-id))
                     (expected-first
                      (mapcar #'substring-no-properties
                              (append (fzf-native-score-all
                                       collection first-query)
                                      nil)))
                     (expected-second
                      (mapcar #'substring-no-properties
                              (append (fzf-native-score-all
                                       collection second-query)
                                      nil))))
                (should (eq (plist-get first :state) 'complete))
                (should (eq (plist-get second :state) 'complete))
                (should-not (plist-get first :stale))
                (should-not (plist-get second :stale))
                (should
                 (equal (mapcar #'substring-no-properties
                                (plist-get first :candidates))
                        expected-first))
                (should
                 (equal (mapcar #'substring-no-properties
                                (plist-get second :candidates))
                        expected-second))
                (should (= (plist-get first :filtered)
                           (length expected-first)))
                (should (= (plist-get second :filtered)
                           (length expected-second)))
                (should (= (plist-get second :progress-total)
                           (plist-get first :filtered)))
                (should (< (plist-get second :progress-total)
                           (plist-get second :pool-generation)))))
          (fzf-native-async-stop handle))))))

(ert-deftest fzf-native-async-stable-batch-cache-narrowing-test ()
  "A narrower request consumes whole-result membership before batch evidence."
  (skip-unless (fboundp 'fzf-native-async-submit))
  (let ((handle (fzf-native-async-start "seq 1 8192"))
        (fzf-native-async-highlight nil)
        (fzf-native-batch-highlight nil))
    (unwind-protect
        (let ((deadline (+ (float-time) 10.0)) first)
          (should (fzf-native-test--wait-for-data handle 10.0))
          (let ((request-id (fzf-native-async-submit handle "9" 200)))
            (while (and (< (float-time) deadline)
                        (progn
                          (setq first
                                (fzf-native-async-snapshot handle request-id))
                          (not (and (eq (plist-get first :state) 'complete)
                                    (plist-get first :reader-done)
                                    (not (plist-get first :stale))))))
              (sleep-for 0.01)))
          (should (eq (plist-get first :state) 'complete))
          (should (plist-get first :reader-done))
          (should (> (plist-get first :batch-cache-entries) 0))
          (let* ((request-id (fzf-native-async-submit handle "99" 200))
                 (final (fzf-native-test--wait-for-request
                         handle request-id 10.0))
                 (expected-all
                  (fzf-native-score-all
                   (mapcar #'number-to-string
                           (number-sequence 1 8192))
                   "99"))
                 (expected
                  (mapcar #'substring-no-properties
                          (cl-subseq expected-all 0
                                     (min 200 (length expected-all))))))
            (should (eq (plist-get final :state) 'complete))
            ;; The full matched-index set retained for "9" is smaller and
            ;; more precise than per-batch evidence, so only those prior
            ;; matches are scanned for the narrower "99" query.
            (should (= (plist-get final :progress-total)
                       (plist-get first :filtered)))
            (should (< (plist-get final :progress-total)
                       (plist-get final :pool-generation)))
            (should (equal (plist-get final :candidates) expected))))
      (fzf-native-async-stop handle))))

(ert-deftest fzf-native-async-batch-cache-progress-has-fixed-logical-total-test ()
  "Cached omissions advance completion without changing the progress total."
  (skip-unless (fboundp 'fzf-native-async-submit))
  (let* ((fzf-native-async-cache-size 0)
         (fzf-native-async-cache-bytes 0)
         (fzf-native-async-batch-cache-bytes (* 1024 1024))
         (fzf-native-async-highlight nil)
         (handle (fzf-native-async-start "seq 1 8192")))
    (unwind-protect
        (progn
          (should (fzf-native-test--wait-for-producer handle 10.0))
          (let* ((first-id (fzf-native-async-submit handle "9" 200))
                 (first (fzf-native-test--wait-for-request
                         handle first-id 10.0)))
            (should (eq (plist-get first :state) 'complete))
            (should (> (plist-get first :batch-cache-entries) 0)))
          (let* ((second-id (fzf-native-async-submit handle "99" 200))
                 (second (fzf-native-test--wait-for-request
                          handle second-id 10.0)))
            (should (eq (plist-get second :state) 'complete))
            (should (> (plist-get second :batch-cache-hits) 0))
            (should (= (plist-get second :progress-total)
                       (plist-get second :pool-generation)))
            (should (= (plist-get second :progress-completed)
                       (plist-get second :progress-total)))))
      (fzf-native-async-stop handle))))

(ert-deftest fzf-native-async-dense-growth-scores-only-delta-test ()
  "Exact dense streaming growth does not repeatedly rescan the old pool."
  (skip-unless (fboundp 'fzf-native-async-submit))
  (let* ((gate (make-temp-file "fzf-native-dense-growth-"))
         (handle (fzf-native-async-start
                  (format (concat "seq 1 5000; while [ -e %s ]; do "
                                  "sleep 0.01; done; seq 5001 10000")
                          (shell-quote-argument gate))))
         (fzf-native-async-highlight nil))
    (unwind-protect
        (progn
          (should (fzf-native-test--wait-for-data handle 10.0))
          (let* ((request-id (fzf-native-async-submit handle "" 100))
                 (first (fzf-native-test--wait-for-request
                         handle request-id 10.0)))
            (should (eq (plist-get first :state) 'complete))
            (should (= (plist-get first :pool-generation) 5000))
            (delete-file gate)
            (let ((deadline (+ (float-time) 10.0)) final)
              (while (and (< (float-time) deadline)
                          (progn
                            (setq final
                                  (fzf-native-async-snapshot
                                   handle request-id))
                            (not (and (plist-get final :reader-done)
                                      (eq (plist-get final :state) 'complete)
                                      (not (plist-get final :stale))
                                      (= (plist-get final :pool-generation)
                                         10000)))))
                (sleep-for 0.01))
              (should (= (plist-get final :filtered) 10000))
              (should (<= (plist-get final :progress-total) 5000))
              (should (< (plist-get final :progress-total)
                         (plist-get final :pool-generation)))
              (should (= (length (plist-get final :candidates)) 100)))))
      (when (file-exists-p gate) (delete-file gate))
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

(ert-deftest fzf-native-async-result-fresh-p-pool-growth-auto-refresh-test ()
  "Input growth automatically refreshes the latest compatibility query.
The second phase starts after the first result is fresh.  Snapshot and
freshness polling do not submit more scoring work."
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
          (let* ((request-id (plist-get (fzf-native-async-status handle)
                                        :request-id))
                 (initial (fzf-native-async-snapshot handle request-id))
                 (initial-pool
                  (plist-get initial :result-pool-generation))
                 (deadline (+ (float-time) 5.0))
                 snapshot)
            ;; Release phase 2 now that scoring for "a" is settled.
            (delete-file gate)
            ;; Poll only ownership and freshness.  Neither function submits
            ;; the query, so the native reader/coordinator path must retry it.
            (while (and (< (float-time) deadline)
                        (progn
                          (setq snapshot
                                (fzf-native-async-snapshot
                                 handle request-id))
                          (not (and
                                (> (plist-get snapshot :pool-generation)
                                   initial-pool)
                                (= (plist-get snapshot :pool-generation)
                                   (plist-get
                                    snapshot :result-pool-generation))
                                (fzf-native-async-result-fresh-p
                                 handle "a")))))
              (sleep-for 0.05))
            (should (> (plist-get snapshot :pool-generation)
                       initial-pool))
            (should (= (plist-get snapshot :pool-generation)
                       (plist-get snapshot :result-pool-generation)))
            (should-not (plist-get snapshot :stale))
            (should (fzf-native-async-result-fresh-p handle "a"))))
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

(ert-deftest fzf-native-async-raw-bytes-round-trip-losslessly-test ()
  "Raw producer bytes and a raw query survive submit and snapshot exactly."
  (skip-unless (and (fboundp 'fzf-native-async-submit)
                    (executable-find "python3")))
  (let* ((expected (unibyte-string ?c ?a ?f #xe9 ?- ?m ?a ?i ?n))
         (query (unibyte-string #xe9))
         (fzf-native-fuzzy nil)
         (fzf-native-case-mode 'respect)
         (fzf-native-async-highlight nil)
         (handle (fzf-native-async-start
                  "python3 -u -c 'import sys
sys.stdout.buffer.write(b\"caf\\xe9-main\\nplain\\n\")
'")))
    (unwind-protect
        (progn
          (should (plist-get (fzf-native-test--wait-for-producer handle)
                             :reader-done))
          (let* ((request-id (fzf-native-async-submit handle query 10))
                 (snapshot (fzf-native-test--wait-for-request
                            handle request-id))
                 (candidates (plist-get snapshot :candidates)))
            (should (eq (plist-get snapshot :state) 'complete))
            (should (equal (plist-get snapshot :query) query))
            (should-not (multibyte-string-p (plist-get snapshot :query)))
            (should (equal candidates (list expected)))
            (should-not (multibyte-string-p (car candidates)))
            (should (= (plist-get snapshot :filtered)
                       (length candidates)))
            (should (= (plist-get snapshot :total) 2))))
      (fzf-native-async-stop handle))))

(ert-deftest fzf-native-async-raw-byte-highlight-uses-unibyte-offsets-test ()
  "A match after mixed valid UTF-8 and raw bytes highlights its byte index."
  (skip-unless (and (fboundp 'fzf-native-async-submit)
                    (executable-find "python3")))
  (let* ((fzf-native-async-highlight 10)
         (handle (fzf-native-async-start
                  "python3 -u -c 'import sys
sys.stdout.buffer.write(b\"\\xe4\\xbd\\xa0\\xe9x\\n\")
'")))
    (unwind-protect
        (progn
          (should (plist-get (fzf-native-test--wait-for-producer handle)
                             :reader-done))
          (let* ((request-id (fzf-native-async-submit handle "x" 10))
                 (snapshot (fzf-native-test--wait-for-request
                            handle request-id))
                 (candidate (car (plist-get snapshot :candidates))))
            (should candidate)
            (should-not (multibyte-string-p candidate))
            (should-not (get-text-property 2 'face candidate))
            (let ((face (get-text-property 4 'face candidate)))
              (should (or (eq face 'completions-common-part)
                          (and (listp face)
                               (memq 'completions-common-part face)))))))
      (fzf-native-async-stop handle))))

(ert-deftest fzf-native-async-two-sessions-share-workers-safely-test ()
  "Two scoring coordinators may overlap on the process-wide worker pool."
  (skip-unless (fboundp 'fzf-native-async-submit))
  (let ((a (fzf-native-async-start "seq 1 200000"))
        (b (fzf-native-async-start "seq 1 200000"))
        (fzf-native-async-highlight nil))
    (unwind-protect
        (progn
          (should (plist-get (fzf-native-test--wait-for-producer a 15.0)
                             :reader-done))
          (should (plist-get (fzf-native-test--wait-for-producer b 15.0)
                             :reader-done))
          (let* ((a-id (fzf-native-async-submit a "not-present-alpha" 100))
                 (b-id (fzf-native-async-submit b "not-present-beta" 100))
                 (a-final (fzf-native-test--wait-for-request a a-id 15.0))
                 (b-final (fzf-native-test--wait-for-request b b-id 15.0)))
            (should (eq (plist-get a-final :state) 'complete))
            (should (eq (plist-get b-final :state) 'complete))
            (should (= (plist-get a-final :progress-completed)
                       (plist-get a-final :progress-total)))
            (should (= (plist-get b-final :progress-completed)
                       (plist-get b-final :progress-total)))))
      (fzf-native-async-stop a)
      (fzf-native-async-stop b))))

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

(ert-deftest fzf-native-async-max-line-length-counts-characters-test ()
  "Async line limits count characters and truncate at UTF-8 boundaries."
  (skip-unless (and (fboundp 'fzf-native-async-start)
                    (executable-find "python3")))
  ;; Positive cap: keep the two-character line and exclude the three-character
  ;; line, regardless of their six- and nine-byte UTF-8 encodings.
  (let* ((fzf-native-max-line-length 2)
         (handle (fzf-native-async-start
                  "python3 -u -c 'print(\"你好\"); print(\"你好吗\")'")))
    (unwind-protect
        (progn
          (should (fzf-native-test--wait-for-data handle))
          (should (fzf-native-test--wait-for-fresh handle ""))
          (should (equal (fzf-native-async-candidates handle "") '("你好"))))
      (fzf-native-async-stop handle)))
  ;; Negative cap: retain two complete characters instead of two raw bytes.
  (let* ((fzf-native-max-line-length -2)
         (handle (fzf-native-async-start
                  "python3 -u -c 'print(\"你好吗\")'")))
    (unwind-protect
        (progn
          (should (fzf-native-test--wait-for-data handle))
          (should (fzf-native-test--wait-for-fresh handle ""))
          (should (equal (fzf-native-async-candidates handle "") '("你好"))))
      (fzf-native-async-stop handle))))

(ert-deftest fzf-native-async-filter-only-length-counts-characters-test ()
  "Async filter-only thresholds count characters, not UTF-8 bytes."
  (skip-unless (and (fboundp 'fzf-native-async-start)
                    (executable-find "python3")))
  (let* ((fzf-native-filter-only-min-pool nil)
         (fzf-native-filter-only-length 1)
         (fzf-native-filter-only-logic 'or)
         (fzf-native-max-line-length nil)
         (fzf-native-async-highlight nil)
         (handle (fzf-native-async-start
                  "python3 -u -c 'print(\"zzz你\"); print(\"zz你\"); print(\"你\")'")))
    (unwind-protect
        (progn
          (should (fzf-native-test--wait-for-data handle))
          ;; Drive the first request with LIMIT=2.  Filter-only evaluates all
          ;; three memberships but ranks only the producer-order emit window;
          ;; full scoring would select the later, higher-scoring exact
          ;; candidate "你".  Excluding it proves the one-character threshold
          ;; fired without relying on the display order within that window.
          (let ((deadline (+ (float-time) 5.0)))
            (while (and (not (fzf-native-async-result-fresh-p handle "你"))
                        (< (float-time) deadline))
              (fzf-native-async-candidates handle "你" 2)
              (sleep-for 0.05)))
          (should (fzf-native-async-result-fresh-p handle "你"))
          (should (equal (fzf-native-async-candidates handle "你" 2)
                         '("zzz你" "zz你"))))
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

(ert-deftest fzf-native-highlight-one-raw-byte-offset-test ()
  "Mixed valid UTF-8/raw input maps logical match positions to unibyte bytes."
  (skip-unless (fboundp 'fzf-native-highlight-one))
  (let* ((orig (unibyte-string #xe4 #xbd #xa0 #xe9 ?x))
         (ret (fzf-native-highlight-one orig "x")))
    (should-not (multibyte-string-p ret))
    (should-not (get-text-property 2 'face ret))
    (let ((face (get-text-property 4 'face ret)))
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

(ert-deftest fzf-native-bundled-module-path-is-architecture-aware-test ()
  "Bundled paths must distinguish supported architectures explicitly."
  (let ((system-type 'darwin)
        (system-configuration "x86_64-apple-darwin"))
    (should (equal (fzf-native--bundled-module-relative-path)
                   "Darwin/fzf-native-module.so")))
  (let ((system-type 'darwin)
        (system-configuration "arm64-apple-darwin"))
    (should (equal (fzf-native--bundled-module-relative-path)
                   "Darwin/arm64/fzf-native-module.so")))
  (let ((system-type 'gnu/linux)
        (system-configuration "x86_64-pc-linux-gnu"))
    (should (equal (fzf-native--bundled-module-relative-path)
                   "Linux/fzf-native-module.so")))
  (let ((system-type 'berkeley-unix)
        (system-configuration "x86_64-unknown-freebsd14.1"))
    (should (equal (fzf-native--bundled-module-relative-path)
                   "FreeBSD/fzf-native-module.so"))))

(ert-deftest fzf-native-bundled-module-path-rejects-unsupported-arch-test ()
  "A wrong-architecture artifact must not reach `module-load'."
  (let ((system-type 'gnu/linux)
        (system-configuration "aarch64-unknown-linux-gnu"))
    (should-error (fzf-native--bundled-module-relative-path)
                  :type 'user-error)))

(ert-deftest fzf-native-bundled-module-path-rejects-other-bsd-test ()
  "A FreeBSD artifact must not load on another Berkeley Unix target."
  (dolist (configuration '("x86_64-unknown-dragonfly6.4"
                           "x86_64-unknown-netbsd10.0"
                           "x86_64-unknown-openbsd7.6"))
    (let ((system-type 'berkeley-unix)
          (system-configuration configuration))
      (should-not (fzf-native--freebsd-target-p))
      (should-error (fzf-native--bundled-module-relative-path)
                    :type 'user-error))))

(ert-deftest fzf-native-session-abi-platform-matches-bundled-freebsd-test ()
  "The ABI handshake and bundled loader must agree on FreeBSD support."
  (let ((calls 0))
    (cl-letf (((symbol-function 'fzf-native-session-abi-version)
               (lambda ()
                 (cl-incf calls)
                 fzf-native-session-abi-required)))
      (let ((system-type 'berkeley-unix)
            (system-configuration "amd64-portbld-freebsd13.2"))
        (should (fzf-native--session-platform-p))
        (should (fzf-native--verify-session-abi))
        (should (= calls 1)))
      (let ((system-type 'berkeley-unix)
            (system-configuration "x86_64-unknown-netbsd10.0"))
        (should-not (fzf-native--session-platform-p))
        (should (fzf-native--verify-session-abi))
        (should (= calls 1))))))

;;; Review regression gates (PR #39 multi-agent review)

(ert-deftest fzf-native-stale-abi-requires-restart-after-initialization-test ()
  "A stale initialized module must not enter a fake in-process rebuild loop."
  (let ((fzf-native-loaded nil)
        (fzf-native-always-compile-module t)
        (compile-calls 0))
    (cl-letf (((symbol-function 'featurep)
               (lambda (feature)
                 (eq feature 'fzf-native-module)))
              ((symbol-function 'fzf-native--verify-session-abi)
               (lambda ()
                 (error "module has ABI 2, Elisp requires ABI 1")))
              ((symbol-function 'fzf-native-module-compile)
               (lambda () (cl-incf compile-calls))))
      (let ((message
             (error-message-string
              (should-error (fzf-native-load-own-build-dyn)))))
        (should (string-match-p "cannot be replaced safely" message))
        (should (string-match-p "restart Emacs" message))
        (should (= compile-calls 0))
        (should-not fzf-native-loaded)))))

(ert-deftest fzf-native-bundled-loader-reports-post-init-restart-test ()
  "ABI rejection after `module-load' must state the real recovery contract."
  (let ((fzf-native-loaded nil)
        (module-loads 0))
    (cl-letf (((symbol-function 'fzf-native--bundled-module-relative-path)
               (lambda () "stale-module.so"))
              ((symbol-function 'featurep)
               (lambda (_feature) nil))
              ((symbol-function 'module-load)
               (lambda (_path) (cl-incf module-loads)))
              ((symbol-function 'fzf-native--verify-session-abi)
               (lambda ()
                 (error "module has ABI 2, Elisp requires ABI 1"))))
      (let ((message
             (error-message-string (should-error (fzf-native-load-dyn)))))
        (should (= module-loads 1))
        (should (string-match-p "cannot be replaced safely" message))
        (should (string-match-p "restart Emacs" message))
        (should-not fzf-native-loaded)))))

(ert-deftest fzf-native-bundled-loader-refuses-stale-reinitialization-test ()
  "The bundled loader verifies an existing module before another initializer."
  (let ((fzf-native-loaded nil)
        (module-loads 0))
    (cl-letf (((symbol-function 'featurep)
               (lambda (feature)
                 (eq feature 'fzf-native-module)))
              ((symbol-function 'fzf-native--bundled-module-relative-path)
               (lambda () "stale-module.so"))
              ((symbol-function 'module-load)
               (lambda (_path) (cl-incf module-loads)))
              ((symbol-function 'fzf-native--verify-session-abi)
               (lambda ()
                 (error "module has ABI 2, Elisp requires ABI 1"))))
      (let ((message
             (error-message-string (should-error (fzf-native-load-dyn)))))
        (should (= module-loads 0))
        (should (string-match-p "cannot be replaced safely" message))
        (should (string-match-p "restart Emacs" message))
        (should-not fzf-native-loaded)))))

(ert-deftest fzf-native-async-preserves-empty-line-candidates-test ()
  "The async line protocol preserves blank, CR-only, and ANSI-only records."
  (skip-unless (fboundp 'fzf-native-async-submit))
  (let* ((expected '("" "alpha" "" "" "" "omega" ""))
         (handle
          (fzf-native-async-start
           "printf '\nalpha\n\n\r\n\033[31m\033[0m\nomega\n\n'"))
         (fzf-native-async-highlight nil))
    (unwind-protect
        (progn
          (should (plist-get (fzf-native-test--wait-for-producer handle)
                             :reader-done))
          (dolist (query '("" "!x"))
            (let* ((request-id (fzf-native-async-submit handle query 20))
                   (snapshot (fzf-native-test--wait-for-request
                              handle request-id))
                   (actual
                    (mapcar #'substring-no-properties
                            (plist-get snapshot :candidates)))
                   (batch
                    (mapcar #'substring-no-properties
                            (fzf-native-score-all expected query))))
              (should (eq (plist-get snapshot :state) 'complete))
              (should-not (plist-get snapshot :stale))
              (should (equal actual expected))
              (should (equal actual batch)))))
      (fzf-native-async-stop handle))))

(defun fzf-native-test--new-complete-session ()
  "Return (HANDLE REQUEST-ID) for a small completed async request."
  (let* ((handle
          (fzf-native-async-start "printf '%s\\n' alpha beta gamma"))
         (request-id (fzf-native-async-submit handle "a" 10))
         (snapshot (fzf-native-test--wait-for-request
                    handle request-id nil t)))
    (unless (eq (plist-get snapshot :state) 'complete)
      (fzf-native-async-stop handle)
      (error "fzf-native test session did not complete: %S" snapshot))
    (list handle request-id)))

(defun fzf-native-test--call-with-symbol-value-stop (handle function)
  "Call FUNCTION while the first `symbol-value' call stops HANDLE.

Return (FIRED RESULT ERROR-DATA)."
  (let ((original (symbol-function 'symbol-value))
        fired result error-data)
    (unwind-protect
        (progn
          (fset 'symbol-value
                (lambda (symbol)
                  (unless fired
                    (setq fired t)
                    (fzf-native-async-stop handle)
                    (sleep-for 0.05))
                  (funcall original symbol)))
          (condition-case error
              (setq result (funcall function))
            (error (setq error-data error))))
      (fset 'symbol-value original))
    (list fired result error-data)))

(defun fzf-native-test--call-with-cons-stop (handle function)
  "Call FUNCTION while the first `cons' call stops HANDLE.

Return (FIRED RESULT ERROR-DATA)."
  (let ((original (symbol-function 'cons))
        fired result error-data)
    (unwind-protect
        (progn
          (fset 'cons
                (lambda (car cdr)
                  (unless fired
                    (setq fired t)
                    (fzf-native-async-stop handle)
                    (sleep-for 0.05))
                  (funcall original car cdr)))
          (condition-case error
              (setq result (funcall function))
            (error (setq error-data error))))
      (fset 'cons original))
    (list fired result error-data)))

(ert-deftest fzf-native-async-public-results-own-strings-across-reentry-test ()
  "Stopping from reentrant Lisp cannot invalidate a public result copy.

Both snapshot entry points build Lisp lists through the mutable `cons' symbol.
The first call stops the same session and gives detached teardown time to free
its arena.  Every later candidate must still come from memory owned by the
in-flight public call.  Run this test against an ASan module to make a borrowed
candidate pointer a deterministic heap-use-after-free."
  (skip-unless (fboundp 'fzf-native-async-submit))
  (dolist (entry '(snapshot candidates))
    (let* ((handle
            (fzf-native-async-start
             "awk 'BEGIN { for (i = 1; i <= 20000; i++) print i }'"))
           request-id
           deadline
           (fzf-native-async-highlight nil)
           status result fired)
      (unwind-protect
          (progn
            ;; Do not accept the valid but transient complete result over an
            ;; empty/growing pool.  Populate the fixed 20k arena first so both
            ;; public entry points must traverse copied candidate strings.
            (should (plist-get (fzf-native-test--wait-for-producer handle)
                               :reader-done))
            (setq request-id (fzf-native-async-submit handle "" 20000))
            (setq deadline (+ (float-time) 10.0))
            (while (and (< (float-time) deadline)
                        (not (eq (plist-get
                                  (setq status
                                        (fzf-native-async-status
                                         handle request-id))
                                  :state)
                                 'complete)))
              (sleep-for 0.005))
            (should (eq (plist-get status :state) 'complete))
            (let ((original-cons (symbol-function 'cons)))
              (unwind-protect
                  (progn
                    (fset 'cons
                          (lambda (car cdr)
                            (unless fired
                              (setq fired t)
                              (fzf-native-async-stop handle)
                              (sleep-for 0.2))
                            (funcall original-cons car cdr)))
                    (setq result
                          (if (eq entry 'snapshot)
                              (fzf-native-async-snapshot handle request-id)
                            (fzf-native-async-candidates
                             handle "" 20000))))
                (fset 'cons original-cons)))
            (should fired)
            (should (= (length (if (eq entry 'snapshot)
                                   (plist-get result :candidates)
                                 result))
                       20000)))
        (ignore-errors (fzf-native-async-stop handle))))))

(ert-deftest fzf-native-async-submit-pins-session-across-lisp-reentry-test ()
  "Stopping during defcustom lookup cannot free an in-flight submit call.

`fzf-native-async-submit' resolves matching options through the mutable Lisp
function `symbol-value'.  This replacement stops the same handle during that
call and waits long enough for detached teardown to expose a borrowed session
pointer.  An ASan module made the old code fail with a heap-use-after-free."
  (skip-unless (fboundp 'fzf-native-async-submit))
  (let* ((handle (fzf-native-async-start "printf '%s\\n' alpha"))
         (original-symbol-value (symbol-function 'symbol-value))
         fired submit-error)
    (unwind-protect
        (progn
          (should (plist-get (fzf-native-test--wait-for-producer handle)
                             :reader-done))
          (unwind-protect
              (progn
                (fset 'symbol-value
                      (lambda (symbol)
                        (unless fired
                          (setq fired t)
                          (fzf-native-async-stop handle)
                          (sleep-for 0.3))
                        (funcall original-symbol-value symbol)))
                (setq submit-error
                      (condition-case error-data
                          (progn
                            (fzf-native-async-submit handle "a" 10)
                            nil)
                        (error error-data))))
            (fset 'symbol-value original-symbol-value))
          (should fired)
          (should submit-error)
          (should (string-match-p
                   "fzf-native-async-submit failed"
                   (error-message-string submit-error)))
          (should-not (fzf-native-async-generation handle)))
      (fset 'symbol-value original-symbol-value)
      (ignore-errors (fzf-native-async-stop handle)))))

(ert-deftest fzf-native-async-candidates-pins-session-across-lisp-reentry-test ()
  "Candidate scoring must survive stop during matching-option lookup."
  (skip-unless (fboundp 'fzf-native-async-candidates))
  (pcase-let* ((`(,handle ,_) (fzf-native-test--new-complete-session))
               (`(,fired ,_result ,_error)
                (fzf-native-test--call-with-symbol-value-stop
                 handle
                 (lambda ()
                   (fzf-native-async-candidates handle "a" 10)))))
    (should fired)
    (should-not (fzf-native-async-generation handle))))

(ert-deftest fzf-native-async-result-fresh-pins-session-across-lisp-reentry-test ()
  "Freshness checks must survive stop during matching-option lookup."
  (skip-unless (fboundp 'fzf-native-async-result-fresh-p))
  (pcase-let* ((`(,handle ,_) (fzf-native-test--new-complete-session))
               (`(,fired ,result ,error-data)
                (fzf-native-test--call-with-symbol-value-stop
                 handle
                 (lambda ()
                   (fzf-native-async-result-fresh-p handle "a")))))
    (should fired)
    (should-not error-data)
    (should-not result)
    (should-not (fzf-native-async-generation handle))))

(ert-deftest fzf-native-async-snapshot-pins-session-across-lisp-reentry-test ()
  "Snapshot highlighting must survive a hook that stops its session."
  (skip-unless (fboundp 'fzf-native-async-snapshot))
  (pcase-let ((`(,handle ,request-id)
               (fzf-native-test--new-complete-session)))
    (let ((fzf-native-async-highlight t)
          fired)
      (let ((fzf-native-highlight-fn
             (lambda (_candidate _positions)
               (unless fired
                 (setq fired t)
                 (fzf-native-async-stop handle)
                 (sleep-for 0.05)))))
        (should (plistp (fzf-native-async-snapshot handle request-id)))
        (should fired)
        (should-not (fzf-native-async-generation handle))))))

(ert-deftest fzf-native-async-status-pins-session-across-lisp-reentry-test ()
  "Status plist construction must survive a reentrant stop."
  (skip-unless (fboundp 'fzf-native-async-status))
  (pcase-let* ((`(,handle ,request-id)
                (fzf-native-test--new-complete-session))
               (`(,fired ,result ,error-data)
                (fzf-native-test--call-with-cons-stop
                 handle
                 (lambda () (fzf-native-async-status handle request-id)))))
    (should fired)
    (should-not error-data)
    (should (plistp result))
    (should-not (fzf-native-async-generation handle))))

(ert-deftest fzf-native-async-stats-pins-session-across-lisp-reentry-test ()
  "Stats cons construction must survive a reentrant stop."
  (skip-unless (fboundp 'fzf-native-async-stats))
  (pcase-let* ((`(,handle ,_) (fzf-native-test--new-complete-session))
               (`(,fired ,result ,error-data)
                (fzf-native-test--call-with-cons-stop
                 handle
                 (lambda () (fzf-native-async-stats handle)))))
    (should fired)
    (should-not error-data)
    (should (consp result))
    (should-not (fzf-native-async-generation handle))))

(ert-deftest fzf-native-async-empty-pool-result-not-final-test ()
  "A result completed over a still-empty pool is not authoritative.
Gate for review JO2-1/DL2-1: with the warmup clause removed from the
snapshot's :stale rule, the first submit at picker-open reports a
final empty result while the producer is still starting up."
  (skip-unless (fboundp 'fzf-native-async-submit))
  (let ((handle (fzf-native-async-start
                 "sleep 0.5; printf '%s\\n' alpha beta"))
        (fzf-native-async-highlight nil))
    (unwind-protect
        (let ((request-id (fzf-native-async-submit handle "al" 10)))
          ;; The producer sleeps 0.5s before emitting anything; during
          ;; the first 0.25s finality must never hold.
          (let ((deadline (+ (float-time) 0.25)))
            (while (< (float-time) deadline)
              (let ((status (fzf-native-async-status handle request-id)))
                (when (and (eq (plist-get status :state) 'complete)
                           (not (plist-get status :stale)))
                  (ert-fail (format "empty-pool result reported final: %S"
                                    status))))
              (sleep-for 0.01)))
          ;; Once the producer finishes, the same request becomes final
          ;; and non-empty through growth retries alone.
          (let ((deadline (+ (float-time) 10.0)) snapshot)
            (while (and (< (float-time) deadline)
                        (progn
                          (setq snapshot (fzf-native-async-snapshot
                                          handle request-id))
                          (not (and (eq (plist-get snapshot :state) 'complete)
                                    (not (plist-get snapshot :stale))
                                    (plist-get snapshot :candidates)))))
              (sleep-for 0.01))
            (should (equal (mapcar #'substring-no-properties
                                   (plist-get snapshot :candidates))
                           '("alpha")))))
      (fzf-native-async-stop handle))))

(ert-deftest fzf-native-async-idempotent-resubmit-quiesces-test ()
  "Identical resubmits on a settled session reuse the request id.
Gate for review KK-4: no fresh request id and no :snapshot-generation
bump, so generation-driven pollers go quiet on an idle session."
  (skip-unless (fboundp 'fzf-native-async-submit))
  (let ((handle (fzf-native-async-start "printf '%s\\n' alpha beta"))
        (fzf-native-async-highlight nil))
    (unwind-protect
        (progn
          (fzf-native-test--wait-for-producer handle)
          (let* ((request-id (fzf-native-async-submit handle "al" 10))
                 (final (fzf-native-test--wait-for-request handle request-id)))
            (should (eq (plist-get final :state) 'complete))
            (should-not (plist-get final :stale))
            (let ((gen0 (plist-get (fzf-native-async-status handle)
                                   :snapshot-generation)))
              (dotimes (_ 20)
                (should (= (fzf-native-async-submit handle "al" 10)
                           request-id)))
              (should (= (plist-get (fzf-native-async-status handle)
                                    :snapshot-generation)
                         gen0)))))
      (fzf-native-async-stop handle))))

(ert-deftest fzf-native-async-status-request-aware-test ()
  "`fzf-native-async-status' answers about a specific request id.
Gate for review JO-1/DL-3: the metadata-only call takes the same
optional REQUEST-ID as snapshot and never carries :candidates."
  (skip-unless (fboundp 'fzf-native-async-submit))
  (let ((handle (fzf-native-async-start "printf '%s\\n' alpha beta"))
        (fzf-native-async-highlight nil))
    (unwind-protect
        (progn
          (fzf-native-test--wait-for-producer handle)
          (let* ((old-id (fzf-native-async-submit handle "al" 10)))
            (fzf-native-test--wait-for-request handle old-id)
            (let* ((new-id (fzf-native-async-submit handle "be" 10))
                   (old-status (fzf-native-async-status handle old-id))
                   (new-status (fzf-native-async-status handle new-id)))
              (should (= (plist-get old-status :request-id) old-id))
              (should (= (plist-get new-status :request-id) new-id))
              (should-not (plist-member old-status :candidates))
              (should-not (plist-member new-status :candidates))
              (should (memq (plist-get new-status :state)
                            '(queued running complete))))))
      (fzf-native-async-stop handle))))

(ert-deftest fzf-native-async-submit-signals-on-failure-test ()
  "`fzf-native-async-submit' signals instead of returning nil.
Gate for review JO-2/JO2-3: a failed submit must never produce a nil
that snapshot would silently read as \"latest\"."
  (skip-unless (fboundp 'fzf-native-async-submit))
  (let ((handle (fzf-native-async-start "printf '%s\\n' alpha")))
    (unwind-protect
        (progn
          (should (eq (car (should-error
                            (fzf-native-async-submit handle "a" -1)))
                      'wrong-type-argument))
          (should (eq (car (should-error
                            (fzf-native-async-snapshot handle -1)))
                      'wrong-type-argument)))
      (fzf-native-async-stop handle))
    ;; After stop, submit is loud; read-only calls stay soft.
    (should (should-error (fzf-native-async-submit handle "a")))
    (should-not (fzf-native-async-snapshot handle))))

(ert-deftest fzf-native-score-all-raw-byte-candidate-test ()
  "Invalid bytes no longer blind the matcher past them.
Gate for review LT-1/KK2-3 at the Emacs module boundary: a unibyte
candidate with a raw 0xE9 byte still matches a query that lands after
the bad byte."
  (skip-unless (fboundp 'fzf-native-score-all))
  (let* ((cand (unibyte-string ?s ?r ?c ?/ ?c ?a ?f #xe9 ?/ ?m ?a ?i ?n))
         (fzf-native-async-highlight nil)
         (results (fzf-native-score-all (list cand) "main")))
    (should (= (length results) 1))))

(ert-deftest fzf-native-rejects-embedded-nul-query-test ()
  "NUL-containing queries cannot alias at the C-string matcher boundary."
  (let ((query (concat "a" (string 0) "x")))
    (should-error (fzf-native-score "alpha" query))
    (should-error (fzf-native-score-all '("alpha") query))
    (should-error (fzf-native-highlight-one "alpha" query))
    (let ((handle (fzf-native-async-start "printf '%s\\n' alpha")))
      (unwind-protect
          (progn
            (should-error (fzf-native-async-submit handle query 10))
            ;; Rejection publishes no request and does not poison a later one.
            (should (= (fzf-native-async-submit handle "a" 10) 1)))
        (fzf-native-async-stop handle)))))

(ert-deftest fzf-native-rejects-embedded-nul-candidate-test ()
  "Synchronous candidate APIs reject strings the C scorer would truncate."
  (let ((candidate (concat "a" (string 0) "x")))
    (should-error (fzf-native-score candidate "a"))
    (should-error (fzf-native-score-all (list candidate) "a"))
    (should-error (fzf-native-highlight-one candidate "a"))))
