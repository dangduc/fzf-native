;;; fzf-native-test.el --- `fzf-native' test. -*- lexical-binding: t; -*-
(require 'ert)
(require 'fzf-native)

(fzf-native-load-dyn)

(ert-deftest fzf-native-score-indices-order-test ()
  (let ((result (fzf-native-score "abcdefghi" "acef")))
    (should (= (nth 1 result) 0))
    (should (= (nth 2 result) 2))
    (should (= (nth 3 result) 4))
    (should (= (nth 4 result) 5))))

(ert-deftest score-with-default-slab-indices-order-test ()
  (let* ((slab (fzf-native-make-default-slab))
         (result (fzf-native-score "abcdefghi" "acef" slab)))
    (should (= (nth 1 result) 0))
    (should (= (nth 2 result) 2))
    (should (= (nth 3 result) 4))
    (should (= (nth 4 result) 5))))

(ert-deftest fzf-native-score-with-default-slab-test ()
  "Test slab can be reused."
  (let* ((slab (fzf-native-make-default-slab))
         (_result (fzf-native-score "abcdefghi" "acef" slab)))
    (should
     (equal (fzf-native-score "abcdefghi" "acef" slab)
            '(78 0 2 4 5)))
    (should
     (equal (fzf-native-score "abc" "acef" slab)
            '(0)))
    (should
     (equal (fzf-native-score "zzzzzabc" "z" slab)
            '(32 0)))
    (should
     (equal (fzf-native-score "sfsjoc" "jo" slab)
            '(36 3 4)))))

(ert-deftest fzf-native-score-with-slab-test ()
  "Test slab can be reused."
  (let* ((slab (fzf-native-make-slab (* 100 1024) 2048))
         (_result (fzf-native-score "abcdefghi" "acef" slab)))
    (should
     (equal (fzf-native-score "abcdefghi" "acef" slab)
            '(78 0 2 4 5)))
    (should
     (equal (fzf-native-score "abc" "acef" slab)
            '(0)))
    (should
     (equal (fzf-native-score "zzzzzabc" "z" slab)
            '(32 0)))
    (should
     (equal (fzf-native-score "sfsjoc" "jo" slab)
            '(36 3 4)))))

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
    (should (equal result `(16 ,len)))))

(ert-deftest fzf-native-score-very-long-str-test ()
  (let* ((len 65536)
         (str (concat (make-string len ?s) "d"))
         (result (fzf-native-score str "d")))
    (should (equal result `(16 ,len)))))

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

(ert-deftest fzf-native-score-indices-multibyte-not-supported-test ()
  ;; Force `str' to be unambiguously multibyte regardless of the coding
  ;; system used to load this file (eask may differ from an interactive
  ;; session). Without the advice, the C module returns BYTE positions.
  (let ((str (decode-coding-string
              (encode-coding-string "ポケモン.txt" 'utf-8) 'utf-8)))
    (should (multibyte-string-p str))
    (should
     (equal (cdr (fzf-native-score str "txt"))
            '(13 14 15)))))

(ert-deftest fzf-native-score-indices-multibyte-support-through-advice-test ()
  ;; Force `str' to be multibyte (see `...not-supported-test' above).
  ;; With the advice, byte positions are mapped back to character
  ;; positions, so we expect (5 6 7) instead of (13 14 15).
  (advice-add 'fzf-native-score :around #'fzf-native--fix-score-indices)
  (unwind-protect
      (let ((str (decode-coding-string
                  (encode-coding-string "ポケモン.txt" 'utf-8) 'utf-8)))
        (should (multibyte-string-p str))
        (should
         (equal (cdr (fzf-native-score str "txt"))
                '(5 6 7))))
    ;; Always remove the advice, even if the assertions above failed.
    ;; Otherwise the advice leaks into subsequent tests.
    (advice-remove 'fzf-native-score #'fzf-native--fix-score-indices)))

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
