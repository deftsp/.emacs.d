;;; 50lua.el ---                                     -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>

(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/mobdebug-mode"))

;; https://github.com/sumneko/lua-language-server
;; https://github.com/sumneko/lua-language-server/wiki/Build-and-Run-(Standalone)
(use-package lua-mode
  :defer t
  :init
  (setq lua-default-application "luajit")
  (setq lua-indent-level 4
        lua-search-url-prefix (concat "file://"
                                      (expand-file-name "~/")
                                      "share/doc/lua/5.1/manual.html#pdf-"))
  :config
  ;; (require 'lua2-mode nil t) ; Note: it will cause lua mode very slow.
  ;; (require 'mobdebug-mode nil t)
  ;; (setq mobdebug-use-evil-binding t)

  (add-hook 'lua-mode-hook 'tl/lua-mode-init)
  (define-key lua-mode-map (kbd "C-c C-z") 'lua-show-process-buffer)

  (require 'f))

(defun tl/lua-mode-init ()
  (if (fboundp 'flycheck-mode)
      (flycheck-mode +1))
  (if (fboundp 'flyspell-mode)
      (flyspell-mode -1))
  (subword-mode +1))

(provide '50lua)
;;; 50lua.el ends here
