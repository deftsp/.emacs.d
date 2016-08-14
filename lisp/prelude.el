;;; prelude.el ---                                 -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>
;;; Commentary:

;;; Code:

(defvar dotpaloryemacs-scratch-mode 'lisp-interaction-mode
  "Default major mode of the scratch buffer.")

(defvar dotpaloryemacs-leader-key "SPC"
  "The leader key.")

(defvar dotpaloryemacs-emacs-leader-key "M-m"
  "The leader key accessible in `emacs state' and `insert state'")

(defvar dotpaloryemacs-major-mode-leader-key ","
  "Major mode leader key is a shortcut key which is the equivalent of
 pressing `<leader> m`. Set it to `nil` to disable it.")

(defvar dotpaloryemacs-major-mode-emacs-leader-key "C-M-m"
  "Major mode leader key accessible in `emacs state' and `insert state'")

(defvar dotpaloryemacs-emacs-command-key "SPC"
  "The key used for Emacs commands (M-x) (after pressing on the leader key).")


;;; chinese-font-setup
(defvar paloryemacs/default-cfs-profile-name "program"
  "Default cfs profile name used by chinse-font-setup.")

(defvar paloryemacs/default-cfs-fontsize-step 4
  "Default cfs fontsize step used by chinse-font-setup.")


(provide 'prelude)
;;; 00prelude.el ends here
