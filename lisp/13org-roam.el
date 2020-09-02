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
        org-roam-index-file "index.org"
        org-roam-graph-executable "neato" ; dot
        org-roam-graph-viewer #'tl/open-with-firefox
        org-roam-graph-exclude-matcher '("____-__-__.org") ; in sql like wildcard
        org-roam-graph-max-title-length    50
        org-roam-graph-extra-config '(("overlap" . "false")
                                      ("concentrate" . "true")
                                      ("bgcolor" . "grey80"))
        org-roam-graph-node-extra-config
        '(("shape"      . "underline")
          ("style"      . "rounded,filled")
          ("fontsize"   . "10")
          ("margin"     . "0.02,0.02")
          ("width"      . "0.2")
          ("height"      . "0.2")
          ("fillcolor"  . "#43586d")
          ("color"      . "#43586d")
          ("fontcolor"  . "#40d9f5"))
        org-roam-graph-edge-extra-config
        '(("color" . "#33485d"))
        org-roam-graph-edge-cites-extra-config
        '(("color" . "gray")
          ("style" . "dashed")
          ("sep" . "20")))

  (setq org-roam-capture-templates
        '(("d" "default" plain
           (function org-roam-capture--get-point)
           "%?"
           :file-name "%<%Y%m%d%H%M%S>-${slug}"
           :head "#+TITLE: ${title}\n#+CREATED: %u\n#+LAST_MODIFIED: %U\n#+ROAM_TAGS: other\n\n"
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

  (tl/declare-prefix-for-mode 'org-mode "mr" "org-roam")

  (tl/set-leader-keys-for-major-mode 'org-mode
    "ri" 'org-roam-insert
    "rI" 'org-roam-insert-immediate)

  (tl/set-leader-keys-for-minor-mode 'org-mode
    "rb" 'org-roam-switch-to-buffer
    "rc" 'org-roam-capture
    "rf" 'org-roam-find-file
    "rg" 'org-roam-graph-show
    "rl" 'org-roam
    "rd" 'org-roam-find-directory
    "rj" 'org-roam-jump-to-index
    "rg" 'org-roam-graph
    "rr" 'org-roam-find-ref))

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

(defun tl/open-with-firefox (path)
  (let ((process-connection-type nil))
    (start-process "" nil "/usr/bin/open" "-a" "Firefox" path)))

(provide '13org-roam)
