;;; 50javascript.el ---                         -*- lexical-binding: t; -*-

;; Copyright (C) 2013  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>

;;; js2-mode
;; (autoload 'js2-mode "js2-mode" nil t)
;; (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))


(defun pl/js2-mode-hook ()
  (when (fboundp 'moz-minor-mode)
   (moz-minor-mode +1)))

(add-hook 'js2-mode-hook 'pl/js2-mode-hook)


(provide '50javascript)
;;; 50javascript.el ends here
