;;; snippets.el ---                                  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>

(use-package evil-lispy
  :config
  (evil-define-key 'insert evil-lispy-mode-map ";" nil)

  (evil-define-key 'normal evil-lispy-mode-map
    "gm" #'evil-lispy/enter-marked-state ; "gm" default to evil-middle-of-visual-line
    (kbd "C-SPC") nil)

  (defun tl/enable-evil-lispy-mode ()
    (when (fboundp 'evil-lispy-mode)
      (evil-lispy-mode +1)))

  (dolist (l '(emacs-lisp-mode-hook clojure-mode-hook))
    (add-hook l 'tl/enable-evil-lispy-mode)))

;; TODO: dired will be require when el-get sync and here dired+ require slow.
;; (use-package dired+
;;   :after dired
;;   :init
;;   (progn
;;     (setq diredp-hide-details-initially-flag nil)
;;     (setq diredp-hide-details-propagate-flag nil)))


(use-package rust-mode
  :config

  (defun tl/rust-mode-init ()
    (racer-mode +1)
    (eldoc-mode +1)
    (smartparens-strict-mode +1)
    (when (fboundp 'org-link-minor-mode)
      (org-link-minor-mode +1)))

  (add-hook 'rust-mode-hook 'tl/rust-mode-init)

  (setq rust-indent-method-chain t
        rust-format-on-save t)


  (defun tl/toggle-mut ()
    "Toggles the mutability of the variable defined on the current line"
    (interactive)
    (save-excursion
      (back-to-indentation)
      (forward-word)
      (if (string= " mut" (buffer-substring (point) (+ (point) 4)))
          (delete-region (point) (+ (point) 4))
        (insert " mut"))))

  (tl/declare-prefix-for-mode 'rust-mode "mv" "variable")

  (tl/set-leader-keys-for-major-mode 'rust-mode
    "cm" 'tl/maximize-cargo-window
    "vm" 'tl/toggle-mut)

  (use-package flycheck-rust
    :after flycheck
    :init
    (add-hook 'rust-mode-hook #'flycheck-rust-setup)))

;; cargo-mode: execute cargo commands easily
;; https://github.com/kwrooijen/cargo.el
(use-package cargo
  :after rust-mode
  :hook ((rust-mode . cargo-minor-mode))
  :config
  (tl/declare-prefix-for-mode 'rust-mode "mc" "cargo")

  (tl/set-leader-keys-for-major-mode 'rust-mode
    "c." 'cargo-process-repeat
    "ca" 'cargo-process-add
    "cC" 'cargo-process-clean
    "cX" 'cargo-process-run-example
    "cc" 'cargo-process-build
    "cd" 'cargo-process-doc
    "cD" 'cargo-process-doc-open
    "ce" 'cargo-process-bench
    "cf" 'cargo-process-fmt
    "ci" 'cargo-process-init
    "cl" 'cargo-process-clippy
    "cn" 'cargo-process-new
    "co" 'cargo-process-current-file-tests
    "cR" 'cargo-process-rm
    "cs" 'cargo-process-search
    "ct" 'cargo-process-current-test

    "cu" 'cargo-process-update
    "cU" 'cargo-process-upgrade
    "cx" 'cargo-process-run
    "cv" 'cargo-process-check
    "t" 'cargo-process-test)

  (defun tl/cargo-process-quit ()
    (interactive)
    (if (and (= 1 (length (window-list)))
             (assoc ?_ register-alist))
        (jump-to-register ?_)
      (quit-window)))

  (with-eval-after-load "evil-evilified-state"
    (evilified-state-evilify-map cargo-process-mode-map
      :mode cargo-process-mode
      :bindings
      "q" 'tl/cargo-process-quit)))

(use-package racer
  :defer t
  :diminish
  :after rust-mode
  :commands racer-mode
  :init
  (tl/set-leader-keys-for-major-mode 'rust-mode
    "hh" 'tl/racer-describe)
  :config
  (defun tl/racer-describe ()
    "Show a *Racer Help* buffer for the function or type at point.
If `help-window-select' is non-nil, also select the help window."
    (interactive)
    (let ((window (racer-describe)))
      (when help-window-select
        (select-window window)))))

(defun tl/maximize-cargo-window ()
  (interactive)
  (let ((bufs (-filter
               (lambda (s) (string-prefix-p "*Cargo " s))
               (-map (lambda (win) (buffer-name (window-buffer win)) )
                     (-mapcat 'window-list (frame-list))))))
    (if (> (length bufs) 0)
        (tl/toggle-maximize-buffer
         (car bufs))
      (message "No cargo process buffer found"))))


(use-package nox
  :init
  (dolist (hook (list
                 ;; 'js-mode-hook
                 ;; 'python-mode-hook
                 ;; 'ruby-mode-hook
                 ;; 'java-mode-hook
                 ;; 'sh-mode-hook
                 ;; 'php-mode-hook
                 ;; 'c-mode-common-hook
                 ;; 'c-mode-hook
                 ;; 'csharp-mode-hook
                 ;; 'c++-mode-hook
                 ;; 'haskell-mode-hook
                 ;; 'rust-mode-hook
                 'rustic-mode-hook
                 ))
    (add-hook hook #'nox-ensure))
  :config
  ;; (add-to-list 'nox-server-programs '(rustic-mode . (nox-rls "rls")))
  (add-to-list 'nox-server-programs '(rustic-mode . (nox-rls "rust-analyzer"))))


;;; [[https://github.com/manateelazycat/color-rg][manateelazycat/color-rg: Search and refactoring tool based on ripgrep.]]
(use-package color-rg
  :commands (color-rg-search-input
             color-rg-search-input-in-project
             color-rg-search-symbol
             color-rg-search-symbol-in-project)
  :bind (("M-s g" . color-rg-search-input-in-project)
         ("M-s r" . color-rg-search-input))
  :init
  (with-eval-after-load "evil-evilified-state"
    (evilified-state-evilify color-rg-mode color-rg-mode-map
      "^" 'color-rg-beginning-of-line
      "j" 'color-rg-jump-next-keyword
      "k" 'color-rg-jump-prev-keyword
      "h" 'color-rg-jump-next-file
      "l" 'color-rg-jump-prev-file

      "e" 'color-rg-switch-to-edit-mode
      ;; Quit
      "q" 'color-rg-quit
      "ZQ" 'evil-quit))
  :config
  (define-key isearch-mode-map (kbd "M-s M-s") 'isearch-toggle-color-rg))


;;; Run 'save-buffers-kill-emacs' without process-killing query
;; seems not work on emacs26
;; (defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
;;   "Prevent annoying \"Active processes exist\" query when you quit Emacs."
;;   (cl-flet ((process-list () nil)) ad-do-it))


;; The Silver Searcher (ag)
;; http://thetrafficstat.net/
;; run `wgrep-change-to-wgrep-mode' and edit the *ag* buffer. Press C-x C-s when you're done to make the changes to
;; buffers.
(setq ag-highlight-search t)
(setq ag-reuse-window 't)


(use-package info+
  :after (info)
  :init
  (progn
    (setq Info-breadcrumbs-in-header-flag t
          Info-fontify-angle-bracketed-flag nil)))
