;;; 50company-mode.el ---                            -*- lexical-binding: t; -*-

;; Copyright (C) 2014  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>
;; Keywords:

;;; Code:
;; el-get install company company-ghc company-cabal
;; when the completion candidates are shown,
;; press <f1> to display the documentation for the selected candidate,
;; or C-w to see its source. Not all back-ends support this.

(add-hook 'after-init-hook 'global-company-mode)

(with-eval-after-load "company"
  (setq company-show-numbers t
        company-clang-insert-arguments nil
        company-require-match nil ; company-explicit-action-p
        company-dabbrev-downcase nil
        company-tooltip-limit 20
        ;; company-backends (delete 'company-ropemacs company-backends)
        ;; company-backends (delete 'company-capf company-backends)
        company-idle-delay 0.2)

  (define-key company-active-map (kbd "M-j") 'company-select-next)
  (define-key company-active-map (kbd "M-k") 'company-select-previous)

  ;; https://github.com/tj64/outshine/issues/38
  ;; company-mode explicitly lists all commands which should trigger idle
  ;; completion. Among this list is self-insert-command, which is rebound to
  ;; outshine-self-insert-command by outshine.
  (add-to-list 'company-begin-commands 'outshine-self-insert-command)
  ;; Enable key-chord "jk" which binded to `company-complete' work,
  ;; `company-manual-begin' will not begin when `last-command' started with
  ;; "company-"
  (add-to-list 'company-begin-commands 'company-complete)
  (add-to-list 'company-backends 'company-cmake))

(defvar paloryemacs/company-common-backends
  '(company-capf
    company-files
    company-dabbrev
    company-yasnippet))

(defvar paloryemacs/company-prog-common-backends
  (cons '(company-dabbrev-code
          company-gtags
          company-etags
          company-keywords)
        paloryemacs/company-common-backends)
  "common company backends(as grouped) for editing programming language source code.")

(add-hook 'c-mode-common-hook 'paloryemacs/company-c-mode-common-setup)
(defun paloryemacs/company-c-mode-common-setup ()
  (let ((backends paloryemacs/company-prog-common-backends))
    (push 'company-clang backends)
    (push 'company-semantic backends)
    (push 'company-xcode backends)
    (push 'company-cmake backends)
    (set (make-local-variable 'company-backends) backends)))


(add-hook 'haskell-mode-hook 'paloryemacs/company-haskell-mode-setup)
(defun paloryemacs/company-haskell-mode-setup ()
  (let ((backends paloryemacs/company-prog-common-backends))
    (push 'company-cabal backends)
    (push 'company-ghc backends)
    (set (make-local-variable 'company-backends) backends)))


(add-hook 'scheme-mode-hook 'paloryemacs/company-scheme-mode-setup)
(defun paloryemacs/company-scheme-mode-setup ()
  (let ((backends paloryemacs/company-prog-common-backends))
    (set (make-local-variable 'company-backends) backends)))

(add-hook 'clojure-mode-hook 'paloryemacs/company-clojure-mode-setup)
(add-hook 'nrepl-mode-hook 'paloryemacs/company-clojure-mode-setup)
(defun paloryemacs/company-clojure-mode-setup ()
  (let ((backends paloryemacs/company-prog-common-backends))
    (set (make-local-variable 'company-backends) backends)))


(add-hook 'ruby-mode-hook 'paloryemacs/company-ruby-mode-setup)
(defun paloryemacs/company-ruby-mode-setup ()
  (let ((backends paloryemacs/company-prog-common-backends))
    (set (make-local-variable 'company-backends) backends)))

(add-hook 'python-mode-hook 'paloryemacs/company-python-mode-setup)
(defun paloryemacs/company-python-mode-setup ()
  (let ((backends paloryemacs/company-prog-common-backends))
    (push 'company-ropemacs backends)
    (push 'company-anaconda backends)
    (set (make-local-variable 'company-backends) backends)))

(add-hook 'css-mode-hook 'paloryemacs/company-css-mode-setup)
(defun paloryemacs/company-css-mode-setup ()
  (let ((backends paloryemacs/company-prog-common-backends))
    (push 'company-css backends)
    (set (make-local-variable 'company-backends) backends)))

(add-hook 'nxml-mode-hook 'paloryemacs/company-nxml-mode-setup)
(defun paloryemacs/company-nxml-mode-setup ()
  (let ((backends paloryemacs/company-prog-common-backends))
    (push 'company-nxml backends)
    (set (make-local-variable 'company-backends) backends)))

(add-hook 'emacs-lisp-mode-hook 'paloryemacs/company-emacs-lisp-mode-setup)
(add-hook 'lisp-interaction-mode-hook 'paloryemacs/company-emacs-lisp-mode-setup)
(defun paloryemacs/company-emacs-lisp-mode-setup ()
  (let ((backends paloryemacs/company-prog-common-backends))
    (set (make-local-variable 'company-backends) backends)))

(add-hook 'org-mode-hook 'paloryemacs/company-org-mode-setup)
(defun paloryemacs/company-org-mode-setup ()
  (let ((backends paloryemacs/company-common-backends))
    (set (make-local-variable 'company-backends) backends)))



(provide '50company-mode)
;;; 50company-mode.el ends here
