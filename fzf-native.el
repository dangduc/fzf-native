;;; fzf-native.el --- Fuzzy completion style  -*- lexical-binding: t; -*-

;; Copyright 2021 Duc Dang
;; Author: Duc Dang <me@dangduc.com>
;; Version: 0.2
;; Package-Requires: ((emacs "27.1"))
;; Keywords: matching
;; Homepage: https://github.com/dangduc/fzf-native
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; This is a package that provides fuzzy match scoring
;; based on the fzf algorithm by junegunn.

(require 'cl-lib)

(defgroup fzf-native nil
  "Fuzzy completion style."
  :group 'minibuffer
  :link '(url-link :tag "GitHub" "https://github.com/dangduc/fzf-native"))

(declare-function fzf-native-score "fzf-native-module")
(declare-function fzf-native-make-default-slab "fzf-native-module")
(declare-function fzf-native-make-slab "fzf-native-module")

(defconst fzf-native--dyn-name "fzf-native-module"
  "Dynamic module name.")

(defconst fzf-native--bin-dir
  (concat (file-name-directory load-file-name) "bin/")
  "Pre-built binaries directory path.")

(defvar fzf-native-module-install-buffer-name " *Install fzf-native-module* "
  "Name of the buffer used for compiling fzf-native-module.")

(defcustom fzf-native-module-cmake-args
  "-DCMAKE_C_FLAGS='-O3'"
  "Arguments given to CMake to compile fzf-native-module."
  :type 'string
  :group 'fzf-native)

(defcustom fzf-native-always-compile-module nil
  "If not nil, if `fzf-native-module' is not found, compile it without asking.

When `fzf-native-always-compile-module' is nil, fzf-native will ask for
confirmation before compiling."
  :type  'boolean
  :group 'fzf-native)

(defun fzf-native-module--cmake-is-available ()
  "Return t if cmake is available.
CMake is needed to build fzf-native, here we check that we can find
the executable."
  (unless (executable-find "cmake")
    (error "Fzf-Native needs CMake to be compiled.  Please, install CMake"))
  t)

;;;###autoload
(defun fzf-native-module-compile ()
  "Compile fzf-native-module."
  (interactive)
  (when (fzf-native-module--cmake-is-available)
    (let* ((fzf-native-directory
            (shell-quote-argument
             ;; NOTE: This is a workaround to fix an issue with how the Emacs
             ;; feature/native-comp branch changes the result of
             ;; `(locate-library "fzf-native")'. See emacs-devel thread
             ;; https://lists.gnu.org/archive/html/emacs-devel/2020-07/msg00306.html
             ;; for a discussion.
             (file-name-directory (locate-library "fzf-native.el" t))))
           (make-commands
            (concat
             "cd " fzf-native-directory " ; "
             "cmake -B build/ " fzf-native-module-cmake-args " && "
             "cmake --build build/"))
           (buffer (get-buffer-create fzf-native-module-install-buffer-name)))
      (pop-to-buffer buffer)
      (compilation-mode)
      (if (zerop (let ((inhibit-read-only t))
                   (call-process "sh" nil buffer t "-c" make-commands)))
          (message "Compilation of `fzf-native' module succeeded")
        (error "Compilation of `fzf-native' module failed!")))))

;;;###autoload
(defun fzf-native-load-dyn ()
  "Load dynamic module."
  (interactive)
  (let* ((dyn-name (cl-case system-type
                     ((windows-nt ms-dos cygwin) (concat "Windows/Release/" fzf-native--dyn-name ".dll"))
                     ('darwin (if (string-prefix-p "x86_64" system-configuration)
                                  ; Intel
                                  (concat "Darwin/" fzf-native--dyn-name ".so")
                                ; Apple Silicon
                                (concat "Darwin/arm64/" fzf-native--dyn-name ".so")))
                     (t (concat "Linux/" fzf-native--dyn-name ".so"))))
         (dyn-path (concat fzf-native--bin-dir dyn-name)))
    (module-load dyn-path)
    (message "[INFO] Successfully load dynamic module, `%s`" dyn-name)))

;;;###autoload
(defun fzf-native-load-own-build-dyn ()
  "Loads user-compiled version of module, building it if necessary"
  (unless (require 'fzf-native-module nil t)
    (if (or fzf-native-always-compile-module
            (y-or-n-p "Fzf-Native needs `fzf-native-module' to work.  Compile it now? "))
        (progn
          (let ((fzf-native-module-cmake-args (concat "-DFZF_NATIVE_MODULE_OUTPUT_DIR=''"
                                                      " "
                                                      fzf-native-module-cmake-args)))
            (fzf-native-module-compile))
          (require 'fzf-native-module))
      (error "Fzf-Native will not work until `fzf-native-module' is compiled!"))))

(defun fzf-native--fix-score-indices (fn str &rest args)
  "An around advice to fix score indices if STR is multibyte.
FN should be `fzf-native-score'."
  (let ((score (apply fn str args)))
    (if (or (null score) (not (multibyte-string-p str)))
        score
      ;; fzf-native makes score indices as byte position.
      ;; But we want it as character position.
      (let ((idx (cl-loop for i from 0 to (1- (length str))
                          vconcat (make-vector (string-bytes (char-to-string (aref str i))) i))))
        (cons (car score) (mapcar (lambda (x) (aref idx x)) (cdr score)))))))

;; Work around the lib's lack of support for multibyte chars. Keep
;; this advice if you want accurate indices for multibyte chars.
;; Remove this advice if you want better performance or you don't need
;; accurate indices for multibyte chars.
(advice-add 'fzf-native-score :around #'fzf-native--fix-score-indices)

(provide 'fzf-native)
;;; fzf-native.el ends here
