;;; 50lua.el ---                                     -*- lexical-binding: t; -*-

;; Copyright (C) 2013  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>

;;; Lua mode
;; (eval-after-load "lua-mode"
;;   '(progn
;;      (setq lua-search-url-prefix
;;            (concat "file://" (expand-file-name "~/") "share/doc/lua/5.1/manual.html#pdf-"))))

(setq lua-indent-level 4)

;; lua2-mode
(eval-after-load "lua-mode"
  '(progn
     ;; (require 'lua2-mode nil t) ; Note: it will cause lua mode very slow.
     (add-hook 'lua-mode-hook 'pl/lua-mode-setup)
     (define-key lua-mode-map (kbd "C-c C-c") 'pl/relaunch-quick-cocos2d-x)))


(defun pl/lua-mode-setup ()
  (flycheck-mode +1)
  (subword-mode +1))

(defun pl/relaunch-quick-cocos2d-x ()
  (interactive)
  (shell-command "relaunch-quick-cocos2d-x"))


(provide '50lua)
;;; 50lua.el ends here
