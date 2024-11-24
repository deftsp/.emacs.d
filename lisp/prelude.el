;;; prelude.el ---                                 -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>
;;; Commentary:

;;; Code:

(defvar dottl-scratch-mode 'lisp-interaction-mode
  "Default major mode of the scratch buffer.")

(defvar dottl-leader-key "SPC"
  "The leader key.")

(defvar dottl-distinguish-gui-tab t
  "If non nil, distinguish C-i and tab in the GUI version of
emacs.")

(defvar dottl-emacs-leader-key "M-m"
  "The leader key accessible in `emacs state' and `insert state'")

(defvar dottl-major-mode-leader-key ","
  "Major mode leader key is a shortcut key which is the equivalent of
 pressing `<leader> m`. Set it to `nil` to disable it.")

;; in terminal C-m is RET, M-Ret is C-M-m
(defvar dottl-major-mode-emacs-leader-key "C-M-m"
  "Major mode leader key accessible in `emacs state' and `insert state'")

(defvar dottl-emacs-command-key "SPC"
  "The key used for Emacs commands (M-x) (after pressing on the leader key).")

;;  "BenQ G2220HD"
(defvar dottl-org-agenda-screen-name "DELL U2718Q")
(defvar dottl-build-in-screen-name "Color LCD")
(defvar dottl-external-screen-name "DELL P2715Q")

;;; cnfonts
(defvar tl/default-cnfonts-profile-name "program"
  "Default cnfonts profile name used by chinse-font-setup.")

(defvar tl/default-cnfonts-fontsize (cl-case system-type
                                      (darwin 14)
                                      (gnu/linux 10)
                                      (t 14))
  "Default cnfonts fontsize step used by chinse-font-setup.")

(defvar dottl-active-transparency 96
  "A value from the range (0..100), in increasing opacity, which describes the
transparency level of a frame when it's active or selected. Transparency
can be toggled through `toggle-transparency'.")

;; 90 looks better, but bad for screenshot
(defvar dottl-inactive-transparency 96
  "A value from the range (0..100), in increasing opacity, which describes the
transparency level of a frame when it's inactive or deselected. Transparency
can be toggled through `toggle-transparency'.")

(defvar dottl-lsp-client 'lsp-bridge "lsp client `lsp-mode', `lsp-bridge' or `lspce'")

(provide 'prelude)
;;; 00prelude.el ends here
