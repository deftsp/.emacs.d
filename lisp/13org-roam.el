;;; 13org-roam.el ---                               -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>
;; Keywords:

;; org-roam-db-build-cache
(use-package org-roam
  :hook (after-init . org-roam-mode)
  :custom (org-roam-directory "~/org/roam")
  :init
  (setq org-roam-completion-system 'ivy)
  :bind (:map org-roam-mode-map
         (("C-c n l" . org-roam)
          ("C-c n f" . org-roam-find-file)
          ("C-c n g" . org-roam-graph-show))
         :map org-mode-map
         (("C-c n i" . org-roam-insert))
         (("C-c n I" . org-roam-insert-immediate))))


(provide '13org-roam)
