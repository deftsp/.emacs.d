;;; 50lua.el ---                                     -*- lexical-binding: t; -*-

;; Copyright (C) 2013  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>

;;; Lua mode

(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp/mobdebug-mode"))

(eval-after-load "lua-mode"
  '(progn
     (setq )
     ;; (require 'lua2-mode nil t) ; Note: it will cause lua mode very slow.
     (require 'mobdebug-mode nil t)
     (setq mobdebug-use-evil-binding t)
     (add-hook 'lua-mode-hook 'pl/lua-mode-setup)
     (define-key lua-mode-map (kbd "C-c C-c") 'pl/relaunch-quick-cocos2d-x)))

(setq lua-indent-level 4
      lua-search-url-prefix (concat "file://"
                                    (expand-file-name "~/")
                                    "share/doc/lua/5.1/manual.html#pdf-"))

(defun pl/lua-mode-setup ()
  (if (fboundp 'flycheck-mode)
      (flycheck-mode +1))
  (subword-mode +1))

(defun pl/relaunch-quick-cocos2d-x ()
  (interactive)
  (shell-command "relaunch-quick-cocos2d-x"))


(provide '50lua)
;;; 50lua.el ends here
