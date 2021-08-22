;;; 50prodigy.el ---                                 -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>
;; Keywords:

(use-package prodigy
  :defer t
  :init
  (setq prodigy-completion-system 'default)
  :config
  (require 'private-prodigy-service)

  (defun tl/prodigy-reload-define ()
    "Reload prodigy define"
    (interactive)
    (setq prodigy-services nil)

    (when (featurep 'private-prodigy-service)
      (unload-feature 'private-prodigy-service))
    (require 'private-prodigy-service)

    (prodigy-refresh))

  (general-define-key
   :states 'normal
   :keymaps 'prodigy-view-mode-map
   "x"  'prodigy-view-clear-buffer
   "gf" 'find-file-at-point
   "q" 'quit-window)

  (general-define-key
   :states 'normal
   :keymaps 'prodigy-mode-map
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
   "gR" 'tl/prodigy-reload-define
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
   (kbd "Y") 'prodigy-copy-cmd))

(provide '50prodigy)
