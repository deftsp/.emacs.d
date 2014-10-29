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

(eval-after-load 'company
  '(progn
     company-require-match nil ; company-explicit-action-p
     (setq company-show-numbers t
           company-clang-insert-arguments nil
           company-dabbrev-downcase nil
           company-tooltip-limit 20
           ;; company-begin-commands '(self-insert-command)
           ;; company-backends (delete 'company-ropemacs company-backends)
           ;; company-backends (delete 'company-capf company-backends)
           company-idle-delay 0.2)

     (define-key company-active-map (kbd "M-j") 'company-select-next)
     (define-key company-active-map (kbd "M-k") 'company-select-previous)
     (add-to-list 'company-backends 'company-cmake)))

(defvar pl/company-common-backends
  '(company-capf
    company-files
    company-dabbrev))

(defvar pl/company-prog-common-backends
  (cons '(company-dabbrev-code
          company-gtags
          company-etags
          company-keywords)
        pl/company-common-backends)
  "common company backends(as grouped) for editing programming language source code.")

(add-hook 'c-mode-common-hook 'pl/company-c-mode-common-setup)
(defun pl/company-c-mode-common-setup ()
  (let ((backends pl/company-prog-common-backends))
    (push 'company-clang backends)
    (push 'company-semantic backends)
    (push 'company-xcode backends)
    (push 'company-cmake backends)
    (set (make-local-variable 'company-backends) backends)))


(add-hook 'haskell-mode-hook 'pl/company-haskell-mode-setup)
(defun pl/company-haskell-mode-setup ()
  (let ((backends pl/company-prog-common-backends))
    (push 'company-cabal backends)
    (push 'company-ghc backends)
    (set (make-local-variable 'company-backends) backends)))


(add-hook 'scheme-mode-hook 'pl/company-scheme-mode-setup)
(defun pl/company-scheme-mode-setup ()
  (let ((backends pl/company-prog-common-backends))
    (set (make-local-variable 'company-backends) backends)))

(add-hook 'clojure-mode-hook 'pl/company-clojure-mode-setup)
(add-hook 'nrepl-mode-hook 'pl/company-clojure-mode-setup)
(defun pl/company-clojure-mode-setup ()
  (let ((backends pl/company-prog-common-backends))
    (set (make-local-variable 'company-backends) backends)))


(add-hook 'ruby-mode-hook 'pl/company-ruby-mode-setup)
(defun pl/company-ruby-mode-setup ()
  (let ((backends pl/company-prog-common-backends))
    (set (make-local-variable 'company-backends) backends)))

(add-hook 'python-mode-hook 'pl/company-python-mode-setup)
(defun pl/company-python-mode-setup ()
  (let ((backends pl/company-prog-common-backends))
    (push 'company-ropemacs backends)
    (set (make-local-variable 'company-backends) backends)))

(add-hook 'css-mode-hook 'pl/company-css-mode-setup)
(defun pl/company-css-mode-setup ()
  (let ((backends pl/company-prog-common-backends))
    (push 'company-css backends)
    (set (make-local-variable 'company-backends) backends)))

(add-hook 'nxml-mode-hook 'pl/company-nxml-mode-setup)
(defun pl/company-nxml-mode-setup ()
  (let ((backends pl/company-prog-common-backends))
    (push 'company-nxml backends)
    (set (make-local-variable 'company-backends) backends)))

(add-hook 'emacs-lisp-mode-hook 'pl/company-emacs-lisp-mode-setup)
(add-hook 'lisp-interaction-mode-hook 'pl/company-emacs-lisp-mode-setup)
(defun pl/company-emacs-lisp-mode-setup ()
  (let ((backends pl/company-prog-common-backends))
    (set (make-local-variable 'company-backends) backends)))

(add-hook 'org-mode-hook 'pl/company-org-mode-setup)
(defun pl/company-org-mode-setup ()
  (let ((backends pl/company-common-backends))
    (set (make-local-variable 'company-backends) backends)))



(provide '50company-mode)
;;; 50company-mode.el ends here
