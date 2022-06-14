(package "fzf-native"
         "0.2"
         "Fuzzy completion style")

(package-file "fzf-native.el")

(files "fzf-native.el" "bin")

(source "gnu")
(source "melpa")

(depends-on "emacs" "27.1")

(development
  (depends-on "f")
  (depends-on "ert-runner")
  (depends-on "package-lint"))
 
; see https://github.com/jcs090218/setup-emacs-windows/issues/156#issuecomment-932956432
(setq network-security-level 'low)