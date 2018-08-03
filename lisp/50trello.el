;;; 50trello.el ---                                  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>
;; Keywords:


(defun paloryemacs//enable-org-trello-mode-maybe ()
  (let ((filename (buffer-file-name (current-buffer))))
    (when (and filename (string= "trello" (file-name-extension filename)))
      (org-trello-mode))))


(use-package org-trello
  :init
  (progn
    (add-to-list 'auto-mode-alist '("\\.trello$" . org-mode))
    (add-hook 'org-mode-hook 'paloryemacs//enable-org-trello-mode-maybe)))


(provide '50trello)
