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
