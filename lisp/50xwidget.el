;;; 50xwidget.el ---

;; Copyright (C) 2022  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>
;; Keywords:

(defun pl/xwidget-webkit-mode-init ()
  (display-line-numbers-mode -1))

(use-package xwidget
  :defer t
  :config
  (add-hook 'xwidget-webkit-mode-hook 'pl/xwidget-webkit-mode-init))

(provide '50xwidget)
