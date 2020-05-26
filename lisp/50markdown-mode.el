;;; 50markdown-mode.el ---                           -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>
;; Keywords:


(defun tl/markdown-refresh-inline-images ()
  (interactive)
  (when markdown-inline-image-overlays
    (markdown-remove-inline-images))
  (markdown-display-inline-images))

(defun tl/markdown-refresh ()
  (interactive)
  (tl/markdown-refresh-inline-images))


(use-package markdown-mode
  :defer t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown")
  :config
  (setq markdown-max-image-size '(600 . 400))
  (tl/set-leader-keys-for-major-mode 'markdown-mode
    "r" 'tl/markdown-refresh
    "Ti" 'markdown-toggle-inline-images))

(provide '50markdown-mode)
