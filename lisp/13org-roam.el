;;; 13org-roam.el ---                               -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>
;; Keywords:

;; org-roam-db-build-cache
(use-package org-roam
  :diminish org-roam-mode
  :hook (after-init . org-roam-mode)
  :custom (org-roam-directory "~/org/roam")
  :init
  (setq org-roam-completion-system 'ivy
        org-roam-index-file "index.org")
  (setq org-roam-capture-templates
        '(("d" "default" plain
           (function org-roam-capture--get-point)
           "%?"
           :file-name "%<%Y%m%d%H%M%S>-${slug}"
           :head "#+TITLE: ${title}\n#+CREATED: %u\n#+LAST_MODIFIED: %U\n\n"
           :unnarrowed t))
        org-roam-capture-ref-templates
        '(("r" "ref" plain
           (function org-roam-capture--get-point)
           ""
           :file-name "caps/${slug}"
           :head "#+TITLE: ${title}\n#+ROAM_KEY: ${ref}\n#+CREATED: %u\n#+LAST_MODIFIED: %U\n\n"
           :unnarrowed t)))
  :config
  (tl/set-leader-keys-for-major-mode 'org-mode
    "mi" 'org-roam-insert
    "mI" 'org-roam-insert-immediate)

  (tl/set-leader-keys-for-minor-mode 'org-mode
    "ml" 'org-roam
    "mf" 'org-roam-find-file
    "mg" 'org-roam-graph-show
    "mr" 'org-roam-find-ref
    "md" 'org-roam-find-directory
    "mj" 'org-roam-jump-to-index
    "mb" 'org-roam-switch-to-buffer
    "mg" 'org-roam-graph))


(provide '13org-roam)
