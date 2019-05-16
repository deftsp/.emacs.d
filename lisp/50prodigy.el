;;; 50prodigy.el ---                                 -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>
;; Keywords:

(use-package prodigy
  :init
  (progn
    (setq prodigy-completion-system 'default))
  :config
  (progn
    (load "~/.emacs.d/private-prodigy-service.el")
    (with-eval-after-load "evil-evilified-state"
      (evilified-state-evilify prodigy-view-mode prodigy-view-mode-map
        "x"  'prodigy-view-clear-buffer
        "gf" 'find-file-at-point
        "q" 'quit-window)
      (evilified-state-evilify prodigy-mode prodigy-mode-map
        "c" 'prodigy-view-clear-buffer

        ;; quit
        "q" 'quit-window

        "j" 'prodigy-next
        "k" 'prodigy-prev
        "gg" 'prodigy-first
        "G" 'prodigy-last

        ;; mark
        "m" 'prodigy-mark
        "*t" 'prodigy-mark-tag
        "M" 'prodigy-mark-all
        "u" 'prodigy-unmark
        "*T" 'prodigy-unmark-tag
        "U" 'prodigy-unmark-all

        "s" 'prodigy-start
        "S" 'prodigy-stop

        ;; refresh
        "gr" 'prodigy-refresh
        "r" 'prodigy-restart

        "`" 'prodigy-display-process
        (kbd "<return>") 'prodigy-browse
        "it" 'prodigy-add-tag-filter
        "in" 'prodigy-add-name-filter
        "I" 'prodigy-clear-filters
        "Jm" 'prodigy-jump-magit
        "Jd" 'prodigy-jump-dired

        "gj" 'prodigy-next-with-status
        "gk" 'prodigy-prev-with-status
        (kbd "C-j") 'prodigy-next-with-status
        (kbd "C-k") 'prodigy-prev-with-status
        (kbd "Y") 'prodigy-copy-cmd))))


  (provide '50prodigy)
