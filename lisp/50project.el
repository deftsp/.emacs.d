;;; 50project.el ---                                 -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>
;; Keywords:

(use-package project
  :defer t
  :init
  (tl/set-leader-keys
    "p!" 'project-shell-command
    "p&" 'project-async-shell-command
    ;; "pf" 'project-find-file
    "pF" 'project-or-external-find-file
    "pb" 'project-switch-to-buffer
    "ps" 'project-shell
    "pd" 'project-find-dir
    "pD" 'project-dired
    "pv" 'project-vc-dir
    "pc" 'project-compile
    "pe" 'project-eshell
    "pk" 'project-kill-buffers
    "pp" 'project-switch-project
    "pg" 'project-find-regexp
    "pG" 'project-or-external-find-regexp
    "pr" 'project-query-replace-regexp
    "px" 'project-execute-extended-command))

(use-package find-file-in-project
  :commands (find-file-in-project-at-point find-file-in-project)
  :init
  (setq ffip-use-rust-fd t)
  (tl/set-leader-keys "pf" 'find-file-in-project))


(provide '50project)
