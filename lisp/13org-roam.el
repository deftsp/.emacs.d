;;; 13org-roam.el ---                               -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>
;; Keywords:

;; org-roam-db-build-cache
(use-package org-roam
  :diminish org-roam-mode
  :hook (after-init . org-roam-mode)
  :custom (org-roam-directory (concat org-directory "/roam/"))
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
        '(("a" "Annotation" plain (function org-roam-capture--get-point)
           "%U ${body}\n"
           :file-name "${slug}"
           :head "#+TITLE: ${title}\n#+ROAM_KEY: ${ref}\n#+ROAM_ALIAS:\n\n"
           :immediate-finish t
           :unnarrowed t)
          ("r" "ref" plain
           (function org-roam-capture--get-point)
           ""
           :file-name "caps/${slug}"
           :head "#+TITLE: ${title}\n#+ROAM_KEY: ${ref}\n#+CREATED: %u\n#+LAST_MODIFIED: %U\n\n"
           :unnarrowed t)))
  :config
  (require 'org-roam-protocol)

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



(use-package org-roam-server
  :defer t
  :commands (org-roam-server-mode)
  :config
  (setq org-roam-server-host "127.0.0.1"
        org-roam-server-port 5050
        org-roam-server-export-inline-images t
        org-roam-server-authenticate nil
        org-roam-server-network-poll t
        org-roam-server-network-arrows nil
        org-roam-server-network-label-truncate t
        org-roam-server-network-label-truncate-length 60
        org-roam-server-network-label-wrap-length 20))

(provide '13org-roam)
