;;; fzf-native-utf8-test.el --- UTF-8 tests for fzf-native -*- lexical-binding: t -*-

;; This file contains all UTF-8 test strings from the C test suite (fzf-test.c)
;; to ensure the Emacs module handles them correctly

(require 'ert)
(require 'fzf-native)

;; Load the dynamic module
(fzf-native-load-dyn)

;; Helper function to get score from result
(defun fzf-native-utf8-test--get-score (str query)
  "Get the score for STR matching QUERY."
  (car (fzf-native-score str query)))

;; UTF-8 test strings from C tests
(defconst fzf-native-utf8-test-strings
  '(;; European languages with diacritics
    "café" "résumé" "naïve" "Åström"
    "über" "größe" "straße" 
    "café_résumé.pdf" "café restaurant"
    "café.txt" "naïve.doc"
    
    ;; CJK characters
    "中文" "测试中文" "中文测试"
    "test中文file.txt" "测试中文text"
    "こんにちは" "世界" "ポケモン"
    "こんにちは世界" "ポケモン.txt"
    "Hello世界café"
    
    ;; Greek
    "Αριστοτέλης" "Θεσσαλονίκη"
    
    ;; Cyrillic
    "Москва" 
    
    ;; Arabic
    "مرحبا" "مرحبا بالعالم"
    
    ;; Hebrew
    "שלום"
    
    ;; Emojis
    "🚀" "😀" "🌍"
    "file🚀rocket.txt" "hello 🚀 world"
    "😀 hello 🌍 world"
    
    ;; Mathematical symbols
    "∫dx" "∂y"
    
    ;; Mixed ASCII/UTF-8
    "hello café" "test_café_file.txt"
    "restaurant café" "ÜBER größe")
  "List of UTF-8 test strings from C test suite.")

;; Test exact matching with UTF-8 strings
(ert-deftest fzf-native-utf8-exact-match-test ()
  "Test exact matching with UTF-8 strings."
  ;; Test café matches café
  (should (> (fzf-native-utf8-test--get-score "café" "café") 0))
  (should (> (fzf-native-utf8-test--get-score "café restaurant" "café") 0))
  (should (= (fzf-native-utf8-test--get-score "restaurant" "café") 0))
  
  ;; Test Chinese characters
  (should (> (fzf-native-utf8-test--get-score "中文" "中文") 0))
  (should (> (fzf-native-utf8-test--get-score "测试中文text" "中文") 0))
  
  ;; Test Japanese
  (should (> (fzf-native-utf8-test--get-score "こんにちは世界" "こんにちは") 0))
  (should (> (fzf-native-utf8-test--get-score "ポケモン.txt" "ポケモン") 0))
  
  ;; Test emojis
  (should (> (fzf-native-utf8-test--get-score "🚀" "🚀") 0))
  (should (> (fzf-native-utf8-test--get-score "hello 🚀 world" "🚀") 0)))

;; Test prefix matching with UTF-8
(ert-deftest fzf-native-utf8-prefix-match-test ()
  "Test prefix matching with UTF-8 strings."
  (should (> (fzf-native-utf8-test--get-score "café.txt" "^café") 0))
  (should (> (fzf-native-utf8-test--get-score "naïve.doc" "^naï") 0))
  (should (= (fzf-native-utf8-test--get-score "résumé" "^café") 0))
  
  ;; CJK prefix
  (should (> (fzf-native-utf8-test--get-score "中文测试" "^中文") 0))
  (should (> (fzf-native-utf8-test--get-score "こんにちは世界" "^こん") 0)))

;; Test suffix matching with UTF-8
(ert-deftest fzf-native-utf8-suffix-match-test ()
  "Test suffix matching with UTF-8 strings."
  (should (> (fzf-native-utf8-test--get-score "café.txt" "txt$") 0))
  (should (> (fzf-native-utf8-test--get-score "ポケモン.txt" "txt$") 0))
  (should (> (fzf-native-utf8-test--get-score "restaurant café" "café$") 0))
  (should (= (fzf-native-utf8-test--get-score "café restaurant" "café$") 0)))

;; Test fuzzy matching with UTF-8
(ert-deftest fzf-native-utf8-fuzzy-match-test ()
  "Test fuzzy matching with UTF-8 strings."
  ;; Fuzzy match across UTF-8 characters
  (should (> (fzf-native-utf8-test--get-score "café résumé" "cr") 0))
  (should (> (fzf-native-utf8-test--get-score "über größe" "üg") 0))
  
  ;; Mixed scripts
  (should (> (fzf-native-utf8-test--get-score "Hello世界café" "H世c") 0))
  
  ;; Emoji fuzzy match
  (should (> (fzf-native-utf8-test--get-score "😀 hello 🌍 world" "😀🌍") 0)))

;; Test case-insensitive matching with UTF-8
(ert-deftest fzf-native-utf8-case-insensitive-test ()
  "Test case-insensitive matching with UTF-8."
  ;; The module uses CaseSmart by default, so lowercase patterns match both cases
  (should (> (fzf-native-utf8-test--get-score "CAFÉ" "café") 0))
  ;; But uppercase patterns are case-sensitive
  (should (= (fzf-native-utf8-test--get-score "café" "CAFÉ") 0))
  (should (> (fzf-native-utf8-test--get-score "CAFÉ" "CAFÉ") 0))
  (should (> (fzf-native-utf8-test--get-score "ÜBER größe" "über") 0))
  
  ;; Greek case folding
  (should (> (fzf-native-utf8-test--get-score "Θεσσαλονίκη" "θε") 0))
  
  ;; Cyrillic case folding
  (should (> (fzf-native-utf8-test--get-score "Москва" "мос") 0)))

;; Test with all UTF-8 strings from C tests
(ert-deftest fzf-native-utf8-comprehensive-test ()
  "Test scoring with all UTF-8 strings from C test suite."
  ;; Test that each UTF-8 string matches itself
  (dolist (str fzf-native-utf8-test-strings)
    (let ((score (fzf-native-utf8-test--get-score str str)))
      (should (> score 0))))
  
  ;; Test specific patterns from C tests
  (should (> (fzf-native-utf8-test--get-score "café_résumé.pdf" "café") 0))
  (should (> (fzf-native-utf8-test--get-score "test_café_file.txt" "café") 0))
  (should (> (fzf-native-utf8-test--get-score "file🚀rocket.txt" "🚀") 0))
  (should (> (fzf-native-utf8-test--get-score "test中文file.txt" "中文") 0))
  (should (> (fzf-native-utf8-test--get-score "hello 🚀 world" "🚀") 0))
  (should (> (fzf-native-utf8-test--get-score "测试中文text" "中文") 0)))

;; Test score-all with UTF-8 collections
(ert-deftest fzf-native-utf8-score-all-test ()
  "Test score-all with collections containing UTF-8 strings."
  (let* ((collection '("café" "résumé" "naïve" "über" "größe"))
         (results (fzf-native-score-all collection "café")))
    ;; Should match café
    (should (= (length results) 1))
    (should (string= (substring-no-properties (car results)) "café")))
  
  ;; Test with CJK
  (let* ((collection '("中文测试" "测试中文" "test中文file"))
         (results (fzf-native-score-all collection "中文")))
    ;; Should match all three
    (should (= (length results) 3)))
  
  ;; Test with emojis
  (let* ((collection '("🚀 rocket" "😀 smile" "🌍 world"))
         (results (fzf-native-score-all collection "🚀")))
    (should (= (length results) 1))
    (should (string-match "🚀" (car results)))))

;; Test specific cases from the C test suite
(ert-deftest fzf-native-utf8-c-test-cases ()
  "Test specific cases from the C test suite."
  ;; Test from test_emacs_interface_cases
  (should (> (fzf-native-utf8-test--get-score "café.txt" "café") 0))
  (should (> (fzf-native-utf8-test--get-score "naïve.doc" "naï") 0))
  (should (> (fzf-native-utf8-test--get-score "ポケモン.txt" "txt") 0))
  
  ;; Test from test_utf8_fuzzy_match_v1
  (should (> (fzf-native-utf8-test--get-score "café résumé" "cr") 0))
  (should (> (fzf-native-utf8-test--get-score "Über Größe" "üg") 0))
  (should (> (fzf-native-utf8-test--get-score "😀 hello 🌍 world" "😀🌍") 0))
  
  ;; Test from test_utf8_fuzzy_match_v2
  (should (> (fzf-native-utf8-test--get-score "مرحبا بالعالم" "مب") 0))
  (should (> (fzf-native-utf8-test--get-score "Hello世界café" "H世c") 0))
  (should (> (fzf-native-utf8-test--get-score "ÜBER größe" "üg") 0)))

;; Test normalization cases
(ert-deftest fzf-native-utf8-normalization-test ()
  "Test UTF-8 strings that might need normalization."
  ;; Note: Current implementation doesn't strip accents, so café != cafe
  (should (> (fzf-native-utf8-test--get-score "café" "café") 0))  ; exact match works
  
  ;; Full-width vs half-width (Japanese)
  (should (> (fzf-native-utf8-test--get-score "ｈｅｌｌｏ" "ｈｅ") 0)))

;; Test character position reporting with UTF-8
(defun fzf-native-utf8-test--highlight-positions (cand query)
  "Char positions in CAND highlighted by `fzf-native-score-all' for QUERY.
Returns a sorted list of 0-indexed character offsets carrying the
`completions-common-part' face.  Exercises the v2.x highlight API, where
match positions are applied as faces by `fzf-native-highlight-fn' rather
than returned in the score list."
  (let* ((fzf-native-batch-highlight 25)
         (out (car (fzf-native-score-all (vector (copy-sequence cand)) query)))
         (positions '()))
    (when out
      (dotimes (i (length out))
        (let ((face (get-text-property i 'face out)))
          (when (or (eq face 'completions-common-part)
                    (and (listp face) (memq 'completions-common-part face)))
            (push i positions)))))
    (nreverse positions)))

(ert-deftest fzf-native-utf8-position-test ()
  "Highlight positions are character offsets, not byte offsets, on UTF-8.
\\='hello-世界-world\\=': h=0 e=1 l=2 l=3 o=4 -=5 世=6 界=7 -=8 w=9 o=10 r=11 l=12 d=13"
  (skip-unless (fboundp 'fzf-native-score-all))
  ;; Single multibyte query char '界' at char position 7.
  (let ((pos (fzf-native-utf8-test--highlight-positions "hello-世界-world" "界")))
    (should (member 7 pos)))
  ;; Multi-char match '世界' at char positions 6,7.
  (let ((pos (fzf-native-utf8-test--highlight-positions "hello-世界-world" "世界")))
    (should (member 6 pos))
    (should (member 7 pos)))
  ;; Fuzzy match across the string 'h界w': h=0, 界=7, w=9.
  (let ((pos (fzf-native-utf8-test--highlight-positions "hello-世界-world" "h界w")))
    (should (member 0 pos))
    (should (member 7 pos))
    (should (member 9 pos))))

;; Test character vs byte position with advice
;; DISABLED: We now return character positions natively, so advice is not needed
;; (ert-deftest fzf-native-utf8-char-position-test ()
;;   "Test character position conversion with multibyte strings."
;;   ;; This test demonstrates the difference between byte and character positions
;;   ;; The advice function `fzf-native--fix-score-indices` converts byte to char positions
;;   ;; DISABLED because we now return character positions natively
;;   (advice-add 'fzf-native-score :around #'fzf-native--fix-score-indices)
;;   
;;   ;; Test 'hello-世界-world' with search '界'
;;   ;; Character position: h=0, e=1, l=2, l=3, o=4, -=5, 世=6, 界=7, -=8, w=9...
;;   (let ((result (fzf-native-score "hello-世界-world" "界")))
;;     (should (> (car result) 0))  ; Should match
;;     ;; With advice, '界' should be at character position 7
;;     (should (equal (cadr result) 7)))  ; Character position
;;   
;;   ;; Test with '世界'
;;   (let ((result (fzf-native-score "hello-世界-world" "世界")))
;;     (should (> (car result) 0))  ; Should match
;;     ;; With advice: '世' at char 6, '界' at char 7
;;     (should (member 6 (cdr result)))  ; '世' character position
;;     (should (member 7 (cdr result))))  ; '界' character position
;;   
;;   ;; Clean up: remove advice
;;   (advice-remove 'fzf-native-score #'fzf-native--fix-score-indices))

;; Test edge cases with UTF-8
(ert-deftest fzf-native-utf8-edge-cases-test ()
  "Test edge cases with UTF-8 strings."
  ;; Empty pattern should return 0 score
  (should (= (fzf-native-utf8-test--get-score "café" "") 0))
  
  ;; Single UTF-8 character patterns
  (should (> (fzf-native-utf8-test--get-score "café" "é") 0))
  (should (> (fzf-native-utf8-test--get-score "中文" "中") 0))
  (should (> (fzf-native-utf8-test--get-score "🚀" "🚀") 0))
  
  ;; Very long UTF-8 strings
  (let ((long-str "很长的中文字符串包含许多不同的汉字用于测试"))
    (should (> (fzf-native-utf8-test--get-score long-str "中文") 0)))
  
  ;; Mixed direction text (LTR and RTL)
  (should (> (fzf-native-utf8-test--get-score "Hello שלום مرحبا" "שלום") 0)))

;; Test pattern operators with UTF-8
(ert-deftest fzf-native-utf8-operators-test ()
  "Test pattern operators with UTF-8 strings."
  ;; Negation with UTF-8
  (should (= (fzf-native-utf8-test--get-score "café" "!café") 0))
  (should (> (fzf-native-utf8-test--get-score "résumé" "!café") 0))
  
  ;; OR patterns with UTF-8
  (should (> (fzf-native-utf8-test--get-score "café" "café | résumé") 0))
  (should (> (fzf-native-utf8-test--get-score "résumé" "café | résumé") 0))
  
  ;; Complex patterns
  (should (> (fzf-native-utf8-test--get-score "test_ñ.rb" "ñ .rb$") 0)))

;;
;; Exact-value oracle tests for UTF-8.  The boolean (> score 0) checks
;; above prove "matched"; these assert the precise score, derived BY HAND
;; from the fzf constants and CONFIRMED against the binary.
;;
;; Key fact (fzf.c): multibyte match positions and lengths are counted in
;; CHARACTERS, not bytes.  The equal-match closed form is therefore
;;   (ScoreMatch+BonusBoundary)*M + (BonusFirstCharMultiplier-1)*BonusBoundary
;;   = 24*M + 8,  where M is the CHARACTER count (fzf.c:1704).
;; A contiguous prefix/suffix run whose first char sits at a word boundary
;; uses the same closed form: 32 + 24*(M-1) = 24*M + 8.
;; Constants: ScoreMatch=16, BonusBoundary=8, BonusConsecutive=4,
;;            BonusFirstCharMultiplier=2.

(ert-deftest fzf-native-utf8-equal-exact-score-test ()
  "Equal match (^X$) on UTF-8: score = 24*M + 8 for M = char count.

  café       M=4 -> 24*4 + 8 = 104
  résumé     M=6 -> 24*6 + 8 = 152   (r é s u m é, 6 codepoints)
  中文       M=2 -> 24*2 + 8 = 56    (CJK, 2 codepoints)
  测试中文   M=4 -> 24*4 + 8 = 104
  θεωρία     M=6 -> 24*6 + 8 = 152   (Greek, 6 codepoints)
M counts characters, so the multibyte é / CJK / Greek codepoints each
count as one even though they occupy 2-3 bytes."
  ;; (length "café") = 4, (length "résumé") = 6, (length "中文") = 2,
  ;; (length "测试中文") = 4, (length "θεωρία") = 6 — verified in Emacs.
  (should (equal (fzf-native-score "café"     "^café$")     '(104)))
  (should (equal (fzf-native-score "résumé"   "^résumé$")   '(152)))
  (should (equal (fzf-native-score "中文"     "^中文$")     '(56)))
  (should (equal (fzf-native-score "测试中文" "^测试中文$") '(104)))
  (should (equal (fzf-native-score "θεωρία"   "^θεωρία$")   '(152)))
  ;; Length is compared in CHARACTERS: a 3-char pattern against a 4-char
  ;; candidate is a length mismatch -> 0 (not a byte-length coincidence).
  (should (equal (fzf-native-score "café" "^caf$") '(0))))

(ert-deftest fzf-native-utf8-prefix-suffix-exact-score-test ()
  "Prefix/suffix on UTF-8: contiguous boundary run = 24*M + 8.

\"café.txt\" \"^café\": prefix \"café\" (4 chars) is a contiguous run
from the start boundary -> 24*4 + 8 = 104.

\"restaurant café\" \"café$\": suffix \"café\" (4 chars, multibyte term
-> UTF-8 suffix matcher).  The char before it is a space (NonWord), so
the run opens at a word boundary -> 24*4 + 8 = 104."
  (should (equal (fzf-native-score "café.txt"        "^café") '(104)))
  (should (equal (fzf-native-score "restaurant café" "café$") '(104))))

(ert-deftest fzf-native-utf8-or-exact-score-test ()
  "OR (|) on UTF-8 takes the FIRST matching term's score, not the max.

Both terms are anchored so each score is a closed form, and the same
pair reordered yields different totals — proving first-match semantics.

text \"café\":
  \"^café$ | ^caf\": term 1 equal \"café\" = 24*4 + 8 = 104. Wins first.
  \"^caf | ^café$\": term 1 prefix \"caf\" (3-char boundary run) =
     24*3 + 8 = 80. Wins first, even though equal \"^café$\" = 104.
  \"zzz | ^café$\": term 1 \"zzz\" misses; falls through to equal = 104."
  (should (equal (fzf-native-score "café" "^café$ | ^caf") '(104)))
  (should (equal (fzf-native-score "café" "^caf | ^café$") '(80)))
  (should (equal (fzf-native-score "café" "zzz | ^café$")  '(104))))

;; Performance test with UTF-8 strings
(ert-deftest fzf-native-utf8-performance-test ()
  "Test performance doesn't degrade significantly with UTF-8."
  (let ((ascii-str "hello world test file")
        (utf8-str "café résumé naïve über")
        (cjk-str "中文测试字符串")
        (query "test"))
    
    ;; Just verify they all work - actual benchmarking would be separate
    (should (numberp (fzf-native-utf8-test--get-score ascii-str query)))
    (should (numberp (fzf-native-utf8-test--get-score utf8-str query)))
    (should (numberp (fzf-native-utf8-test--get-score cjk-str query)))))

(provide 'fzf-native-utf8-test)
;;; fzf-native-utf8-test.el ends here