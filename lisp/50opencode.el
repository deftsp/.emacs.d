;;; 50opencode.el ---                                 -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>
;; Keywords:

;; https://codeberg.org/sczi/opencode.el
(use-package opencode
  :straight (opencode :type git :host codeberg :repo "sczi/opencode.el")
  :commands (opencode opencode-connect)
  :bind (:map opencode-session-mode-map
         ("RET" . newline)
         ("C-c C-c" . comint-send-input)
         ;; change the reasoning/thinking effort (thought level), OpenCode TUI is Ctrl-t
         ("C-c C-t" . opencode-select-variant)
         ("C-c C-v" . opencode-select-model)
         ("C-c C-k" . opencode-abort-session)))

(provide '50opencode)
