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
  (setq company-show-numbers t
        company-clang-insert-arguments nil
        company-require-match nil ; company-explicit-action-p; Don't require match, so you can still move your cursor as expected.
        company-tooltip-align-annotations t ; Align annotation to the right side.
        company-tooltip-minimum-width 60
        company-tooltip-maximum-width 60
        ;; company-eclim-auto-save nil ; Stop eclim auto save.
        company-dabbrev-downcase nil
        company-tooltip-limit 20
        company-minimum-prefix-length 1
        company-selection-wrap-around t
        company-format-margin-function #'company-text-icons-margin
        ;; company-backends (delete 'company-ropemacs company-backends)
        ;; company-backends (delete 'company-capf company-backends)
        company-idle-delay 0.2)
  :config
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

  ;; (use-package company-quickhelp
  ;;   :config
  ;;   (progn
  ;;     (company-quickhelp-mode +1)
  ;;     (define-key company-active-map (kbd "C-c h") #'company-quickhelp-manual-begin)))

  (use-package company-posframe
    :diminish company-posframe-mode
    :init
    (setq company-posframe-quickhelp-delay nil
          company-posframe-show-indicator nil)
    :config
    (define-key company-active-map (kbd "C-c h") #'company-posframe-quickhelp-show))

  (if (window-system)
      (company-posframe-mode +1)
    ;; Use the tab-and-go frontend.
    ;; Allows TAB to select and complete at the same time.
    ;; 'tng' means 'tab and go'
    (company-tng-configure-default))


  ;; https://github.com/TommyX12/company-tabnine
  ;; workaround for company-transformers
  ;; company-transformers or plugins that use it (such as company-flx-mode) can interfere with TabNine's sorting.
  (setq company-tabnine--disable-next-transform nil)
  (defun tl-company--transform-candidates (func &rest args)
    (if (not company-tabnine--disable-next-transform)
        (apply func args)
      (setq company-tabnine--disable-next-transform nil)
      (car args)))

  (defun tl-company-tabnine (func &rest args)
    (when (eq (car args) 'candidates)
      (setq company-tabnine--disable-next-transform t))
    (apply func args))

  (advice-add #'company--transform-candidates :around #'tl-company--transform-candidates)
  (advice-add #'company-tabnine :around #'tl-company-tabnine))

(use-package company-emoji
  :after company)

;;; company-flx -- instead by company-fuzzy
;; (use-package company-flx
;;   :init
;;   (setq company-flx-limit 600)
;;   :config
;;   (company-flx-mode +1))


;; TODO: when enable company-fuzzy, nothing pop out. Maybe be conflict with company-tabnine
;; https://github.com/jcs-elpa/company-fuzzy
;; (use-package company-fuzzy
;;   :after company
;;   :init
;;   (setq company-fuzzy-sorting-backend 'flx)
;;   (setq company-fuzzy-prefix-ontop nil)
;;   :config
;;   (setq company-fuzzy-show-annotation t)
;;   (global-company-fuzzy-mode +1))

(defvar tl/company-common-backends
  '(company-tabnine
    company-capf
    company-files
    company-emoji
    ;; company-dabbrev ; which complete some string sypmbol
    company-yasnippet))

(defvar tl/company-prog-common-backends
  (list tl/company-common-backends
        '(company-dabbrev-code
          company-gtags
          company-etags
          company-keywords))
  "common company backends(as grouped) for editing programming language source code.")

(add-hook 'c-mode-common-hook 'tl/company-c-mode-common-setup)
(defun tl/company-c-mode-common-setup ()
  (let ((backends tl/company-prog-common-backends))
    (set (make-local-variable 'company-backends) backends)
    ;; (add-to-list 'backends
    ;;              '(company-clang company-semantic company-xcode company-cmake))
    ))

(add-hook 'scheme-mode-hook 'tl/company-scheme-mode-setup)
(defun tl/company-scheme-mode-setup ()
  (let ((backends tl/company-prog-common-backends))
    (set (make-local-variable 'company-backends) backends)))

(add-hook 'clojure-mode-hook 'tl/company-clojure-mode-setup)
(add-hook 'nrepl-mode-hook 'tl/company-clojure-mode-setup)
(defun tl/company-clojure-mode-setup ()
  (let ((backends tl/company-prog-common-backends))
    (set (make-local-variable 'company-backends) backends)))


(add-hook 'ruby-mode-hook 'tl/company-ruby-mode-setup)
(defun tl/company-ruby-mode-setup ()
  (let ((backends tl/company-prog-common-backends))
    (set (make-local-variable 'company-backends) backends)))

(add-hook 'python-mode-hook 'tl/company-python-mode-setup)
(defun tl/company-python-mode-setup ()
  (let ((backends tl/company-prog-common-backends))
    ;; (push 'company-ropemacs backends)
    (push 'company-anaconda backends)
    (set (make-local-variable 'company-backends) backends)))

(add-hook 'css-mode-hook 'tl/company-css-mode-setup)
(defun tl/company-css-mode-setup ()
  (let ((backends tl/company-prog-common-backends))
    (push 'company-css backends)
    (set (make-local-variable 'company-backends) backends)))

(add-hook 'nxml-mode-hook 'tl/company-nxml-mode-setup)
(defun tl/company-nxml-mode-setup ()
  (let ((backends tl/company-prog-common-backends))
    (push 'company-nxml backends)
    (set (make-local-variable 'company-backends) backends)))

(add-hook 'emacs-lisp-mode-hook 'tl/company-emacs-lisp-mode-setup)
(add-hook 'lisp-interaction-mode-hook 'tl/company-emacs-lisp-mode-setup)
(defun tl/company-emacs-lisp-mode-setup ()
  (let ((backends tl/company-prog-common-backends))
    (set (make-local-variable 'company-backends) backends)))

(add-hook 'org-mode-hook 'tl/company-org-mode-setup)
(defun tl/company-org-mode-setup ()
  (let ((backends tl/company-common-backends))
    (setq-local company-backends backends)))

(add-hook 'lua-mode-hook 'tl/company-lua-mode-setup)
(defun tl/company-lua-mode-setup ()
  (let ((backends tl/company-common-backends))
    (setq-local company-backends backends)))

(add-hook 'web-mode-hook 'tl/company-web-mode-setup)
(defun tl/company-web-mode-setup ()
  (let ((backends tl/company-common-backends))
    (setq-local company-backends backends)
    (when (fboundp 'company-tide)
      (add-to-list 'company-backends 'company-tide))))


(add-hook 'rustic-mode-hook 'tl/company-rustic-mode-setup)
(defun tl/company-rustic-mode-setup ()
  (let ((backends tl/company-common-backends))
    (setq-local company-backends backends)))

(provide '40company-mode)
;;; 40company-mode.el ends here
