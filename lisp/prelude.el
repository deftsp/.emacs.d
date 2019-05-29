;;; prelude.el ---                                 -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>
;;; Commentary:

;;; Code:

(defvar dotpaloryemacs-scratch-mode 'lisp-interaction-mode
  "Default major mode of the scratch buffer.")

(defvar dotpaloryemacs-leader-key "SPC"
  "The leader key.")

(defvar dotpaloryemacs-distinguish-gui-tab t
  "If non nil, distinguish C-i and tab in the GUI version of
emacs.")

(defvar dotpaloryemacs-emacs-leader-key "M-m"
  "The leader key accessible in `emacs state' and `insert state'")

(defvar dotpaloryemacs-major-mode-leader-key ","
  "Major mode leader key is a shortcut key which is the equivalent of
 pressing `<leader> m`. Set it to `nil` to disable it.")

;; in terminal C-m is RET, M-Ret is C-M-m
(defvar dotpaloryemacs-major-mode-emacs-leader-key "C-M-m"
  "Major mode leader key accessible in `emacs state' and `insert state'")

(defvar dotpaloryemacs-emacs-command-key "SPC"
  "The key used for Emacs commands (M-x) (after pressing on the leader key).")

;;  "BenQ G2220HD"
(defvar dotpaloryemacs-org-agenda-screen-name "DELL U2718Q")
(defvar dotpaloryemacs-build-in-screen-name "Color LCD")
(defvar dotpaloryemacs-external-screen-name "DELL P2715Q")

;;; cnfonts
(defvar paloryemacs/default-cnfonts-profile-name "program"
  "Default cnfonts profile name used by chinse-font-setup.")

(defvar paloryemacs/default-cnfonts-fontsize-step 6
  "Default cnfonts fontsize step used by chinse-font-setup.")


(defvar dotpaloryemacs-active-transparency 96
  "A value from the range (0..100), in increasing opacity, which describes the
transparency level of a frame when it's active or selected. Transparency
can be toggled through `toggle-transparency'.")

(defvar dotpaloryemacs-inactive-transparency 90
  "A value from the range (0..100), in increasing opacity, which describes the
transparency level of a frame when it's inactive or deselected. Transparency
can be toggled through `toggle-transparency'.")



(provide 'prelude)
;;; 00prelude.el ends here
