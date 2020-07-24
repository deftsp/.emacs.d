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

;;; Switch rg to color-rg for following reason:
;; FIXME: in rg-mode (search result), press `n' cause below message:
;; Error in pre-command-hook (evilified-state--pre-command-hook): (wrong-type-argument listp compilation-button-map)
;; when result the cursor keep at the bottom of the window
(use-package rg
  :commands (rg-menu)
  :bind (("M-s g" . tl/rg-vc-or-dir)
         ("M-s r" . tl/rg-ref-in-dir)
         :map rg-mode-map
         ("s" . tl/rg-save-search-as-name))
  :init
  (setq rg-hide-command t
        rg-show-columns nil)
  (global-set-key (kbd "C-c s") #'rg-menu)
  :config
  (with-eval-after-load "evil-evilified-state"
    (evilified-state-evilify rg-mode rg-mode-map
      "g>" 'rg-forward-history
      "g<" 'rg-back-history

      "j" 'compilation-next-error
      "k" 'compilation-previous-error

      "h" 'rg-next-file
      "l" 'rg-prev-file
      "m" 'rg-menu

      "gg" 'evil-goto-first-line
      "gr" 'rg-recompile
      "G" 'evil-goto-line

      "e" 'wgrep-change-to-wgrep-mode
      ;; Quit
      "q" 'color-rg-quit
      "ZQ" 'evil-quit))

  (rg-define-search rg-emacs-config
                    "Search the emacs config."
                    :dir "~/.emacs.d"
                    :files "*.{el,el.gz}"
                    :menu ("Custom" "e" "emacs config"))

  (rg-define-search tl/rg-vc-or-dir
                    "RipGrep in project root or present directory."
                    :query ask
                    :format regexp
                    :files "everything"
                    :dir (let ((vc (vc-root-dir)))
                           (if vc
                               vc                       ; search root project dir
                             default-directory))        ; or from the current dir
                    :confirm prefix
                    :flags ("--hidden -g !.git"))


  ;; https://protesilaos.com/dotemacs
  (rg-define-search tl/rg-ref-in-dir
                    "RipGrep for thing at point in present directory."
                    :query point
                    :format regexp
                    :files "everything"
                    :dir default-directory
                    :confirm prefix
                    :flags ("--hidden -g !.git"))

  (defun tl/rg-save-search-as-name ()
    "Save `rg' buffer, naming it after the current search query.

 This function is meant to be mapped to a key in `rg-mode-map'."
    (interactive)
    (let ((pattern (car rg-pattern-history)))
      (rg-save-search-as-name (concat "«" pattern "»")))))

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


;;; Run 'save-buffers-kill-emacs' without process-killing query
;; seems not work on emacs26
;; (defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
;;   "Prevent annoying \"Active processes exist\" query when you quit Emacs."
;;   (cl-flet ((process-list () nil)) ad-do-it))
