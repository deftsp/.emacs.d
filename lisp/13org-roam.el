;;; 13org-roam.el ---                               -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>
;; Keywords:

;; org-roam-db-build-cache
(use-package org-roam
  :diminish org-roam-mode
  :after org
  :commands
  (org-roam-buffer
   org-roam-setup
   org-roam-capture
   org-roam-node-find)
  :init
  (setq org-roam-directory (concat org-directory "/roam/"))
  (setq org-roam-file-extensions '("org"))
  (setq org-roam-node-display-template "${title:48}   ${tags:42}")
  (setq org-roam-v2-ack t)
  (setq org-roam-completion-everywhere t)
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
        '(("d" "default" plain "%?" :if-new
           (file+head
            "%<%Y%m%d%H%M%S>-${slug}.org"
            "#+TITLE: ${title}\n#+CREATED: %u\n#+LAST_MODIFIED: %U\n#+FILETAGS: other\n\n")
           :unnarrowed t)))

  ;; The value of key ROAM_REFS in PROPERTIES will contain the URL.

  ;; ${foo} will look for the foo property in the `org-roam-note',if the property does not exist, the user will be
  ;; prompted to fill in the string value.
  ;; FIXME: Using %:body to the value from URL is not work
  (setq org-roam-capture-ref-templates
        '(("a" "Annotation" plain "%U %:body\n%?"
           :if-new (file+head
                    "caps/${slug}.org"
                    "#+TITLE: ${title}\n#+CREATED: %u\n#+LAST_MODIFIED: %U\n#+FILETAGS: other\n\n\n")
           :immediate-finish t
           :unnarrowed t)
          ("r" "ref" plain "%?"
           :if-new (file+head
                    "caps/${slug}.org"
                    "#+TITLE: ${title}\n#+CREATED: %u\n#+LAST_MODIFIED: %U\n#+FILETAGS: other\n\n")
           :unnarrowed t)))
  :config
  (use-package org-roam-protocol)

  ;; for org-roam-buffer-toggle
  (add-to-list 'display-buffer-alist
               '(("\\*org-roam\\*"
                  (display-buffer-in-direction)
                  (direction . right)
                  (window-width . 0.33)
                  (window-height . fit-window-to-buffer))))

  (tl/declare-prefix-for-mode 'org-mode "mr" "org-roam")

  (tl/set-leader-keys-for-mode 'org-mode
    "rc" 'org-roam-capture
    "rf" 'org-roam-node-find
    "ri" 'org-roam-node-insert
    "rl" 'org-roam-buffer-toggle
    "rr" 'org-roam-ref-find)

  (org-roam-db-autosync-mode +1))

;; (use-package org-roam-server
;;   :defer t
;;   :commands (org-roam-server-mode)
;;   :config
;;   (setq org-roam-server-host "127.0.0.1"
;;         org-roam-server-port 5050
;;         org-roam-server-export-inline-images t
;;         org-roam-server-authenticate nil
;;         org-roam-server-network-poll t
;;         org-roam-server-network-arrows nil
;;         org-roam-server-network-label-truncate t
;;         org-roam-server-network-label-truncate-length 60
;;         org-roam-server-network-label-wrap-length 20))

(defun tl/open-with-firefox (path)
  (let ((process-connection-type nil))
    (start-process "" nil "/usr/bin/open" "-a" "Firefox" path)))

(provide '13org-roam)
