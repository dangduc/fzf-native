;;; fzf-native-module-init-probe.el --- Fresh-process module init probes -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Duc Dang
;; Author: Duc Dang <me@dangduc.com>
;; Assisted-by: Codex:gpt-5
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is part of fzf-native.

;; fzf-native is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; fzf-native is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with fzf-native.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Exercise module initialization in a fresh Emacs process.  The test suite
;; selects a probe with FZF_NATIVE_INIT_PROBE and supplies the module path in
;; FZF_NATIVE_TEST_MODULE.

;;; Code:

(declare-function fzf-native-score-all "fzf-native-module"
                  (collection query &optional slab))

(let ((module (getenv "FZF_NATIVE_TEST_MODULE"))
      (mode (getenv "FZF_NATIVE_INIT_PROBE")))
  (unless (and module (file-readable-p module))
    (error "FZF_NATIVE_TEST_MODULE is not readable: %S" module))
  (pcase mode
    ("reentry"
     (let (seen)
       (advice-add
        'defalias :around
        (lambda (original symbol definition &optional docstring)
          (prog1 (funcall original symbol definition docstring)
            (when (eq symbol 'fzf-native-score-all)
              (setq seen t)
              (unless (equal (fzf-native-score-all ["alpha"] "a")
                             '("alpha"))
                (error "Reentrant score returned the wrong result"))))))
       (module-load module)
       (unless (and seen (featurep 'fzf-native-module))
         (error "Module initialization did not complete after reentry"))))
    ("partial-error"
     (let (load-error)
       (advice-add
        'defalias :around
        (lambda (original symbol definition &optional docstring)
          (if (eq symbol 'fzf-native-score)
              (error "Injected defalias failure for %S" symbol)
            (funcall original symbol definition docstring))))
       (condition-case err
           (module-load module)
         (error (setq load-error err)))
       (unless load-error
         (error "Injected module initialization failure did not signal"))
       (when (featurep 'fzf-native-module)
         (error "A partial initializer published the module feature"))
       (unless (equal (fzf-native-score-all ["alpha"] "a") '("alpha"))
         (error "A partial callable alias observed incomplete module state"))))
    (_ (error "Unknown FZF_NATIVE_INIT_PROBE mode: %S" mode))))

;;; fzf-native-module-init-probe.el ends here
