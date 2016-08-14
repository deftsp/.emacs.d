;;; 50emacs-lisp-mode.el ---                         -*- lexical-binding: t; -*-

;; Copyright (C) 2014  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>

;;; Commentary:
;;; Code:

;;; tooltip-help
;; (require 'tooltip-help)
;; (define-key emacs-lisp-mode-map (kbd "<f1>") 'th-show-help)

;;; eldoc
;; mini-buffer 中显示 point 处 eLisp 函数的定义格式。
(autoload 'turn-on-eldoc-mode "eldoc" nil t)
(eval-after-load "eldoc"
  '(progn
     (require 'eldoc-extension nil t)
     ;; make ElDoc aware of ParEdit's most used commands. Whenever the listed commands are used,
     ;; ElDoc will automatically refresh the minibuffer.
     ;; (eldoc-add-command
     ;;  'paredit-backward-delete
     ;;  'paredit-close-round)
     ))

(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)
;; (add-hook 'rcirc-mode-hook 'turn-on-eldoc-mode)

;;; do not considered '-' as word-delimiter
;; (add-hook 'emacs-lisp-mode-hook '(lambda () (modify-syntax-entry ?- "w")))
;; (modify-syntax-entry ?- "w" emacs-lisp-mode-syntax-table)


;;; elisp-slime-nav
;; install with el-get
;; Elisp go-to-definition with M-. and back again with M-,
(add-hook 'emacs-lisp-mode-hook 'turn-on-elisp-slime-nav-mode)
(eval-after-load 'elisp-slime-nav '(diminish 'elisp-slime-nav-mode))

;;; auto compile el file
;; (defun byte-compile-visited-file ()
;;   (let ((byte-compile-verbose t))
;;     (unless (eq major-mode 'sawfish-mode)
;;       (byte-compile-file buffer-file-name))))

;; (add-hook 'emacs-lisp-mode-hook
;;           (lambda ()
;;             (when buffer-file-name
;;               (add-hook 'after-save-hook 'byte-compile-visited-file nil t))))


(font-lock-add-keywords
 'emacs-lisp-mode
 '(("\\<\\(set\\|setq\\|require-soft\\|quote\\|when-available\\|add-hook\\)\\>" .
    font-lock-function-name-face)
   ("\\<\\(nil\\|\\t\\)\\_>" . font-lock-constant-face)))

;;; imenu
(setq paloryemacs/emacs-lisp-imenu-generic-expression
      '((nil "^\\s-*(def\\(un\\|subst\\|macro\\|advice\\)\
\\s-+\\([-A-Za-z0-9+/]+\\)" 2)
        ("*Vars*" "^\\s-*(def\\(var\\|const\\)\
\\s-+\\([-A-Za-z0-9+/]+\\)" 2)
        ("*Types*"
         "^\\s-*\
 (def\\(type\\|struct\\|class\\|ine-condition\\)\
\\s-+\\([-A-Za-z0-9+/]+\\)" 2)
        (nil ";;[;]\\{1,8\\} \\(.*$\\)" 1) ; the default one
        ("Sections" "^;;;; \\(.+\\)$" 1)))


(defun paloryemacs/imenu-elisp-init ()
  (setq imenu-prev-index-position-function nil)
  (setq imenu-generic-expression paloryemacs/emacs-lisp-imenu-generic-expression))


(add-hook 'emacs-lisp-mode-hook 'paloryemacs/imenu-elisp-init)

(with-eval-after-load "lispy"
  (define-key lispy-mode-map-lispy (kbd "M-o") nil)

  (defun paloryemacs/enable-evil-lispy-mode ()
    (when (fboundp 'evil-lispy-mode)
      (evil-lispy-mode +1)))
  (add-hook 'emacs-lisp-mode-hook 'paloryemacs/enable-evil-lispy-mode))


(provide '50emacs-lisp-mode)
;;; 50emacs-lisp-mode.el ends here
