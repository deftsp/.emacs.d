;;; 50javascript.el ---                         -*- lexical-binding: t; -*-

;; Copyright (C) 2013  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>

;;; js2-mode
;; TODO: try skewer-mode https://github.com/skeeto/skewer-mode

;; (autoload 'js2-mode "js2-mode" nil t)
;; (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

(defun paloryemacs/js2-mode-hook ()
  (when (fboundp 'moz-minor-mode)
    (define-key js2-mode-map (kbd "C-c C-z") 'run-mozilla)
    (moz-minor-mode +1)))

(add-hook 'js2-mode-hook 'paloryemacs/js2-mode-hook)


(provide '50javascript)
;;; 50javascript.el ends here
