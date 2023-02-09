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
         (result (fzf-native-score "abcdefghi" "acef" slab)))
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
         (result (fzf-native-score "abcdefghi" "acef" slab)))
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
  (should
   (equal (cdr (fzf-native-score "ポケモン.txt" "txt"))
          '(13 14 15))))

(ert-deftest fzf-native-score-indices-multibyte-support-through-advice-test ()
  ; Assume advice not yet added. Setup advice environment.
  (advice-add 'fzf-native-score :around #'fzf-native--fix-score-indices)
  (should
   (equal (cdr (fzf-native-score "ポケモン.txt" "txt"))
          '(5 6 7)))
  ; Reset advice environment.
  (advice-remove 'fzf-native-score #'fzf-native--fix-score-indices))
