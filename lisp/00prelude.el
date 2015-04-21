;;; 00prelude.el ---                                 -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>
;;; Commentary:

;;; Code:

(defvar pl/leader-key "SPC" "The leader key.")

(defvar pl/emacs-leader-key "M-m"
  "The leader key accessible in `emacs state' and `insert state'")

(defvar pl/major-mode-leader-key ","
  "Major mode leader key is a shortcut key which is the equivalent of
pressing `<leader> m`. Set it to `nil` to disable it.")

(defvar pl/major-mode-emacs-leader-key "C-M-m"
  "Major mode leader key accessible in `emacs state' and `insert state'")

(defvar pl/default-cfs-fontsizes-list '(12.5 14.5 14.5)
  "Default cfs fontsizes list used by chinse-font-setup.")


(provide '00prelude)
;;; 00prelude.el ends here
