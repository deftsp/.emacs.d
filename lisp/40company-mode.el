;;; 40company-mode.el ---                            -*- lexical-binding: t; -*-

;; Copyright (C) 2014  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>
;; Keywords:

;;; Code:
;; el-get install company company-ghc company-cabal
;; when the completion candidates are shown,
;; press <f1> to display the documentation for the selected candidate,
;; or C-w to see its source. Not all back-ends support this.

(add-hook 'after-init-hook 'global-company-mode)

(use-package company
  :defer t
  :init
  (progn
    (setq company-show-numbers t
          company-clang-insert-arguments nil
          company-require-match nil ; company-explicit-action-p; Don't require match, so you can still move your cursor as expected.
          company-tooltip-align-annotations t ; Align annotation to the right side.
          ;; company-eclim-auto-save nil ; Stop eclim auto save.
          company-dabbrev-downcase nil
          company-tooltip-limit 20
          company-minimum-prefix-length 2
          company-selection-wrap-around t
          ;; company-backends (delete 'company-ropemacs company-backends)
          ;; company-backends (delete 'company-capf company-backends)
          company-idle-delay 0.9))
  :config
  (progn
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
    (add-to-list 'company-backends 'company-cmake)

    ;; Use the tab-and-go frontend.
    ;; Allows TAB to select and complete at the same time.
    ;; 'tng' means 'tab and go'
    (company-tng-configure-default)

    ;; https://github.com/TommyX12/company-tabnine
    ;; workaround for company-transformers
    ;; company-transformers or plugins that use it (such as company-flx-mode) can interfere with TabNine's sorting.
    (setq company-tabnine--disable-next-transform nil)
    (defun paloryemacs-company--transform-candidates (func &rest args)
      (if (not company-tabnine--disable-next-transform)
          (apply func args)
        (setq company-tabnine--disable-next-transform nil)
        (car args)))

    (defun paloryemacs-company-tabnine (func &rest args)
      (when (eq (car args) 'candidates)
        (setq company-tabnine--disable-next-transform t))
      (apply func args))

    (advice-add #'company--transform-candidates :around #'paloryemacs-company--transform-candidates)
    (advice-add #'company-tabnine :around #'paloryemacs-company-tabnine)

    ;;;
    (use-package company-quickhelp
      :config
      (progn
        (company-quickhelp-mode +1)
        (define-key company-active-map (kbd "C-c h") #'company-quickhelp-manual-begin)))))

;;; company-flx -- instead by company-fuzzy
;; (use-package company-flx
;;   :init
;;   (setq company-flx-limit 600)
;;   :config
;;   (company-flx-mode +1))


;; TODO: when enable company-fuzzy, nothing pop out. Maybe be conflict with company-tabnine
;; (use-package company-fuzzy
;;   :after company
;;   :init
;;   (setq company-fuzzy-sorting-backend 'flx)
;;   (setq company-fuzzy-prefix-ontop nil)
;;   :config
;;   (setq company-fuzzy-show-annotation t)
;;   (global-company-fuzzy-mode +1))

(defvar paloryemacs/company-common-backends
  '(company-tabnine
    company-capf
    company-files
    ;; company-dabbrev ; which complete some string sypmbol
    company-yasnippet))

(defvar paloryemacs/company-prog-common-backends
  (list paloryemacs/company-common-backends
        '(company-dabbrev-code
          company-gtags
          company-etags
          company-keywords))
  "common company backends(as grouped) for editing programming language source code.")

(add-hook 'c-mode-common-hook 'paloryemacs/company-c-mode-common-setup)
(defun paloryemacs/company-c-mode-common-setup ()
  (let ((backends paloryemacs/company-prog-common-backends))
    (set (make-local-variable 'company-backends) backends)
    ;; (add-to-list 'backends
    ;;              '(company-clang company-semantic company-xcode company-cmake))
    ))

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
    ;; (push 'company-ropemacs backends)
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
    (setq-local company-backends backends)))

(add-hook 'lua-mode-hook 'paloryemacs/company-lua-mode-setup)
(defun paloryemacs/company-lua-mode-setup ()
  (let ((backends paloryemacs/company-common-backends))
    (setq-local company-backends backends)))

(add-hook 'web-mode-hook 'paloryemacs/company-web-mode-setup)
(defun paloryemacs/company-web-mode-setup ()
  (let ((backends paloryemacs/company-common-backends))
    (setq-local company-backends backends)
    (when (fboundp 'company-tide)
      (add-to-list 'company-backends 'company-tide))))


(provide '40company-mode)
;;; 40company-mode.el ends here
