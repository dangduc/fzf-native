;;; fzf-native.el --- Fuzzy completion style  -*- lexical-binding: t; -*-

;; Copyright 2021 Duc Dang
;; Author: Duc Dang <me@dangduc.com>
;; Version: 0.1
;; Package-Requires: ((emacs "27.1"))
;; Keywords: matching
;; Homepage: https://github.com/dangduc/fzf-native
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; This is a fuzzy Emacs completion style similar to the built-in
;; `flex' style, but with a better scoring algorithm.

(require 'cl-lib)

(defgroup fzf-native nil
  "Fuzzy completion style."
  :group 'minibuffer
  :link '(url-link :tag "GitHub" "https://github.com/dangduc/fzf-native"))

(declare-function fzf-native--score-c "fzf-native-module")
;; If the dynamic module is available: Load it

(defconst fzf-native--dyn-name "fzf-native-module"
  "Dynamic module name.")

(defconst fzf-native--bin-dir
  (concat (file-name-directory load-file-name) "bin/")
  "Pre-built binaries directory path.")

;(require 'fzf-native-module nil nil)

;;;###autoload
(defun fzf-native-load-dyn ()
  "Load dynamic module."
  (interactive)
  (let* ((dyn-name (cl-case system-type
                     ((windows-nt ms-dos cygwin) (concat fzf-native--dyn-name ".dll"))
                     (`darwin (concat "Darwin/" fzf-native--dyn-name ".so"))
                     (t (concat fzf-native--dyn-name ".so"))))
         (dyn-path (concat fzf-native--bin-dir dyn-name)))
    (module-load dyn-path)
    (message "[INFO] Successfully load dynamic module, `%s`" dyn-name)))

(defun fzf-native-score (str query)
  (fzf-native--score-c str query))

(provide 'fzf-native)
;;; fzf-native.el ends here
