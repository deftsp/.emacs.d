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

;;;
(paloryemacs|define-jump-handlers emacs-lisp-mode)
(paloryemacs|define-jump-handlers lisp-interaction-mode)


(dolist (mode '(emacs-lisp-mode lisp-interaction-mode))
  (let ((jumpl (intern (format "paloryemacs-jump-handlers-%S" mode))))
    (add-to-list jumpl 'elisp-slime-nav-find-elisp-thing-at-point)))

;; Idea from http://www.reddit.com/r/emacs/comments/312ge1/i_created_this_function_because_i_was_tired_of/
(defun paloryemacs/eval-current-form ()
  "Find and evaluate the current def* or set* command.
Unlike `eval-defun', this does not go to topmost function."
  (interactive)
  (save-excursion
    (search-backward-regexp "(def\\|(set")
    (forward-list)
    (call-interactively 'eval-last-sexp)))

;; smartparens integration

(defun paloryemacs/eval-current-form-sp (&optional arg)
  "Call `eval-last-sexp' after moving out of one level of
parentheses. Will exit any strings and/or comments first.
An optional ARG can be used which is passed to `sp-up-sexp' to move out of more
than one sexp.
Requires smartparens because all movement is done using `sp-up-sexp'."
  (interactive "p")
  (require 'smartparens)
  (let ((evil-move-beyond-eol t))
    ;; evil-move-beyond-eol disables the evil advices around eval-last-sexp
    (save-excursion
      (let ((max 10))
        (while (and (> max 0)
                    (sp-point-in-string-or-comment))
          (decf max)
          (sp-up-sexp)))
      (sp-up-sexp arg)
      (call-interactively 'eval-last-sexp))))

(defun paloryemacs/eval-current-symbol-sp ()
  "Call `eval-last-sexp' on the symbol around point.
Requires smartparens because all movement is done using `sp-forward-symbol'."
  (interactive)
  (require 'smartparens)
  (let ((evil-move-beyond-eol t))
    ;; evil-move-beyond-eol disables the evil advices around eval-last-sexp
    (save-excursion
      (sp-forward-symbol)
      (call-interactively 'eval-last-sexp))))

(defun paloryemacs/init-emacs-lisp ()
  (dolist (mode '(emacs-lisp-mode lisp-interaction-mode))
    (paloryemacs/declare-prefix-for-mode mode "mc" "compile")
    (paloryemacs/declare-prefix-for-mode mode "me" "eval")
    (paloryemacs/declare-prefix-for-mode mode "mt" "tests")
    (paloryemacs/set-leader-keys-for-major-mode mode
      "cc" 'emacs-lisp-byte-compile
      "e$" 'lisp-state-eval-sexp-end-of-line
      "eb" 'eval-buffer
      "eC" 'paloryemacs/eval-current-form
      "ee" 'eval-last-sexp
      "er" 'eval-region
      "ef" 'eval-defun
      "el" 'lisp-state-eval-sexp-end-of-line
      ","  'lisp-state-toggle-lisp-state
      "tb" 'paloryemacs/ert-run-tests-buffer
      "tq" 'ert
      "f" 'describe-function
      "k" 'describe-key
      "hh" 'elisp-slime-nav-describe-elisp-thing-at-point
      "gg" 'elisp-slime-nav-find-elisp-thing-at-point
      "v" 'describe-variable)))

(paloryemacs/init-emacs-lisp)



(provide '50emacs-lisp-mode)
;;; 50emacs-lisp-mode.el ends here
