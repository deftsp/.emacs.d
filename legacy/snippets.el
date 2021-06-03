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


;; Since macOS 10.14, can not send keystroke to Firefox with system event. Use
;; hammerspoon instead
(defun pl/org-as-mac-firefox-get-frontmost-url ()
  (let ((result
	     (do-applescript
	      (concat
	       "set oldClipboard to the clipboard\n"
	       "set frontmostApplication to path to frontmost application\n"
           "do shell script \"~/bin/hs -c 'ff_url.cp_firefox_frontmost_url_to_clipboard()'\"\n"
           "delay 0.4\n"
	       "set links to the clipboard\n"
	       "set the clipboard to oldClipboard\n"
	       "activate application (frontmostApplication as text)\n"
	       "return links as string\n"))))
    (replace-regexp-in-string
     "^\"\\| - Mozilla Firefox\"$\\|\"$" ""
     (car (split-string result "[\r\n]+" t)))))

;; (advice-add 'org-as-mac-firefox-get-frontmost-url :override 'pl/org-as-mac-firefox-get-frontmost-url)
;; (advice-remove 'org-as-mac-firefox-get-frontmost-url 'pl/org-as-mac-firefox-get-frontmost-url)



;; Python
(defvar python-auto-set-local-pyenv-version 'on-visit
  "Automatically set pyenv version from \".python-version\".

Possible values are `on-visit', `on-project-switch' or `nil'.")

(defvar python-auto-set-local-pyvenv-virtualenv 'on-visit
  "Automatically set pyvenv virtualenv from \".venv\".

Possible values are `on-visit', `on-project-switch' or `nil'.")

(defun tl/pyenv-executable-find (command)
  "Find executable taking pyenv shims into account."
  (if (executable-find "pyenv")
      (progn
        (let ((pyenv-string (shell-command-to-string (concat "pyenv which " command))))
          (unless (string-match "not found" pyenv-string)
            (string-trim pyenv-string))))
    (executable-find command)))


(defun tl//pyenv-mode-set-local-version ()
  "Set pyenv version from \".python-version\" by looking in parent directories."
  (interactive)
  (let ((root-path (locate-dominating-file default-directory
                                           ".python-version")))
    (when root-path
      (let* ((file-path (expand-file-name ".python-version" root-path))
             (version
              (with-temp-buffer
                (insert-file-contents-literally file-path)
                (nth 0 (split-string (buffer-substring-no-properties
                                      (line-beginning-position)
                                      (line-end-position)))))))
        (if (member version (pyenv-mode-versions))
            (pyenv-mode-set version)
          (message "pyenv: version `%s' is not installed (set by %s)"
                   version file-path))))))

(defun tl//pyvenv-mode-set-local-virtualenv ()
  "Set pyvenv virtualenv from \".venv\" by looking in parent directories."
  (interactive)
  (let ((root-path (locate-dominating-file default-directory
                                           ".venv")))
    (when root-path
      (let* ((file-path (expand-file-name ".venv" root-path))
             (virtualenv
              (with-temp-buffer
                (insert-file-contents-literally file-path)
                (buffer-substring-no-properties (line-beginning-position)
                                                (line-end-position)))))
        (pyvenv-workon virtualenv)))))


(use-package pyenv-mode
  :if (executable-find "pyenv")
  :commands (pyenv-mode-versions)
  :init
  (progn
    (pcase python-auto-set-local-pyenv-version
      (`on-visit
       (add-hook 'python-mode-hook 'tl//pyenv-mode-set-local-version))
      (`on-project-switch
       (add-hook 'projectile-after-switch-project-hook
                 'tl//pyenv-mode-set-local-version)))
    ;; setup shell correctly on environment switch
    (dolist (func '(pyenv-mode-set pyenv-mode-unset))
      (advice-add func :after 'tl/python-setup-everything))
    (tl/set-leader-keys-for-major-mode 'python-mode
      "vu" 'pyenv-mode-unset
      "vs" 'pyenv-mode-set))
  :config
  (progn
    ;; For a venv named 'venv3.6.2' located at '~/.pyenv/versions/venv3.6.2'
    ;; which is linked to '~/.pyenv/versions/3.6.2/envs/venv',
    ;; `python-shell-calculate-process-environment' will set VIRTUAL_ENV to
    ;; '~/.pyenv/versions/venv3.6.2'. But the function init_virtualenv in
    ;; interactiveshell.py (in IPython source) don't accept the virtualenv path
    ;; as link, and emit warning. Here we advice `pyenv-mode-full-path' to
    ;; return the true path instead of link.

    ;; See also
    ;; https://github.com/pyenv/pyenv-virtualenv/issues/113
    ;; https://github.com/ipython/ipython/issues/9774
    ;; https://github.com/ipython/ipython/pull/5939
    (defun tl//chase-virtualenv-root (p)
      (file-truename p))
    ;; (advice-remove 'pyenv-mode-full-path
    ;;                #'tl//chase-virtualenv-root)
    (advice-add 'pyenv-mode-full-path
                :filter-return #'tl//chase-virtualenv-root)))

(use-package anaconda-mode
  :defer t
  :diminish anaconda-mode
  :init
  (progn
    (add-hook 'python-mode-hook 'anaconda-mode))
  :config
  (progn
    (tl/set-leader-keys-for-major-mode 'python-mode
      ;; use gtags
      "gd" 'anaconda-mode-find-definitions
      "ga" 'anaconda-mode-find-assignments
      "gr" 'anaconda-mode-find-references
      "gu" 'anaconda-mode-find-references
      "gb" 'anaconda-mode-go-back
      "g*" 'anaconda-mode-go-back
      "hh" 'anaconda-mode-show-doc)

    (defadvice anaconda-mode-goto (before python/anaconda-mode-goto activate)
      (evil--jumps-push))))


(defun tl//python-setup-shell (&rest args)
  (if (tl/pyenv-executable-find "ipython")
      (progn (setq python-shell-interpreter "ipython")
             (if (version< (replace-regexp-in-string "\n$" "" (shell-command-to-string "ipython --version")) "5")
                 (setq python-shell-interpreter-args "-i")
               (setq python-shell-interpreter-args "--simple-prompt -i")))
    (progn
      (setq python-shell-interpreter-args "-i")
      (setq python-shell-interpreter "python"))))


(defun tl//python-setup-checkers (&rest args)
  (when (fboundp 'flycheck-set-checker-executable)
    (let ((pylint (tl/pyenv-executable-find "pylint"))
          (flake8 (tl/pyenv-executable-find "flake8")))
      (when pylint
        (flycheck-set-checker-executable "python-pylint" pylint))
      (when flake8
        (flycheck-set-checker-executable "python-flake8" flake8)))))

(defun tl/python-setup-everything (&rest args)
  (apply 'tl//python-setup-shell args)
  (apply 'tl//python-setup-checkers args))


(defun tl/python-toggle-breakpoint ()
  "Add a break point, highlight it."
  (interactive)
  (let ((trace (cond ((tl/pyenv-executable-find "wdb") "import wdb; wdb.set_trace()")
                     ((tl/pyenv-executable-find "ipdb") "import ipdb; ipdb.set_trace()")
                     ((tl/pyenv-executable-find "pudb") "import pudb; pudb.set_trace()")
                     ((tl/pyenv-executable-find "ipdb3") "import ipdb; ipdb.set_trace()")
                     ((tl/pyenv-executable-find "pudb3") "import pudb; pudb.set_trace()")
                     (t "import pdb; pdb.set_trace()")))
        (line (thing-at-point 'line)))
    (if (and line (string-match trace line))
        (kill-whole-line)
      (progn
        (back-to-indentation)
        (insert trace)
        (insert "\n")
        (python-indent-line)))))


(defun tl/on-select-previous-input-source (data)
  "Swith to insert when switch to Rime input method"
  (let ((source-id (plist-get data :source-id)))
    (when (string-equal source-id "im.rime.inputmethod.Squirrel.Rime")
      (let ((state (bound-and-true-p evil-state)))
        (when (and state
                   (eq state 'normal)
                   (not (minibufferp))
                   (not isearch-mode))
          (call-interactively 'evil-insert))))))


;; https://github.com/yasuyk/web-beautify
(use-package web-beautify
  :defer t
  ;; :init
  ;; (progn
  ;;   (tl/set-leader-keys-for-major-mode 'js2-mode
  ;;     "=" 'web-beautify-js)
  ;;   (tl/set-leader-keys-for-major-mode 'json-mode
  ;;     "=" 'web-beautify-js)
  ;;   (tl/set-leader-keys-for-major-mode 'web-mode
  ;;     "=" 'web-beautify-html)
  ;;   (tl/set-leader-keys-for-major-mode 'css-mode
  ;;     "=" 'web-beautify-css))
  )

;; Live evaluation of JS buffer change.
(use-package livid-mode
  :commands (livid-mode)
  :defer t
  :init
  (progn
    (with-eval-after-load 'js2-mode
      (tl/set-leader-keys-for-major-mode 'js2-mode
        "sa" 'livid-mode))))

;;; lispyville: [[https://github.com/noctuid/lispyville][noctuid/lispyville: lispy + evil = lispyville]]
(use-package lispyville
  :after (evil)
  :diminish
  :hook (lispy-mode . lispyville-mode)
  :config
  (lispyville-set-key-theme
   '((operators normal)
     ;; c-w
     (prettify insert)
     (atom-movement normal visual)
     slurp/barf-lispy
     (wrap normal insert)
     ;; additional
     ;; additional-insert
     (additional-wrap normal insert)
     (escape insert))))

;; evil-iedit-state
(use-package iedit
  :defer 3
  :config
  (use-package evil-iedit-state
    :init
    (setq iedit-current-symbol-default t
          iedit-only-at-symbol-boundaries t
          iedit-toggle-key-default nil)
    :config
    ;; activate leader in iedit and iedit-insert states
    (define-key evil-iedit-state-map
      (kbd dottl-leader-key) tl-default-map)))

;;; mutiple cursors
(use-package multiple-cursors
  :defer t
  :init
  (progn
    ;; Mark more like this
    (global-set-key (kbd "C-*") 'mc/mark-all-like-this)
    (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
    (global-set-key (kbd "C->") 'mc/mark-next-like-this)
    ;; (global-set-key (kbd "M-å") 'mc/mark-all-in-region)
    (global-set-key (kbd "C-M-m") 'mc/mark-more-like-this-extended)

    (defvar tl/mutiple-cursors-keymap (make-sparse-keymap)
      "Keymap for key chord prefix commands in haskell mode.")
    (define-key tl/mutiple-cursors-keymap (kbd "l") 'mc/edit-lines)
    (define-key tl/mutiple-cursors-keymap (kbd "c") 'mc/edit-lines)
    (define-key tl/mutiple-cursors-keymap (kbd "M-C") 'mc/edit-lines)
    (define-key tl/mutiple-cursors-keymap (kbd "C-e") 'mc/edit-ends-of-lines)
    (define-key tl/mutiple-cursors-keymap (kbd "e") 'mc/edit-ends-of-lines)
    (define-key tl/mutiple-cursors-keymap (kbd "C-a") 'mc/edit-beginnings-of-lines)
    (define-key tl/mutiple-cursors-keymap (kbd "a") 'mc/edit-beginnings-of-lines)
    (define-key tl/mutiple-cursors-keymap (kbd "SPC") 'set-rectangular-region-anchor)
    (define-key tl/mutiple-cursors-keymap (kbd "m") 'set-rectangular-region-anchor)

    (global-set-key (kbd "M-C") tl/mutiple-cursors-keymap)
    (key-chord-define-global ";c" tl/mutiple-cursors-keymap))
  :config
  (progn
    (with-eval-after-load "evil"
      (add-hook 'multiple-cursors-mode-enabled-hook
                'tl/evil-switch-to-insert-maybe))

    (add-to-list 'mc/unsupported-minor-modes 'autopair-mode)
    (add-to-list 'mc/unsupported-minor-modes 'smartparens-mode)
    ;; (add-to-list 'mc/unsupported-minor-modes 'evil-mode)
    (add-to-list 'mc/unsupported-minor-modes 'smartparens-strict-mode)))

(global-set-key (kbd "C-S-SPC") 'tl/set-rectangular-region-anchor)
;; Rectangular region mode
(defun tl/evil-switch-to-insert-maybe ()
  (when (and (fboundp 'evil-mode) evil-mode)
    (if (not (memq evil-state '(insert emacs)))
        (evil-insert 1))))

(defun tl/set-rectangular-region-anchor ()
  (interactive)
  (tl/evil-switch-to-insert-maybe)
  (set-rectangular-region-anchor))


;; https://github.com/brotzeit/rustic/pull/244
;;   (defun tl//rustic-buffer-crate (&optional nodefault)
;;     "Get the workspace root.
;; If NODEFAULT is t, return nil instead of `default-directory' if directory is
;; not in a rust project."
;;     (let ((dir (locate-dominating-file
;;                 (or buffer-file-name default-directory) "Cargo.toml")))
;;       (if dir
;;           (expand-file-name dir)
;;         (if nodefault
;;             nil default-directory))))

;;   ;; Basically equal to`rust-compilation', but use buffer crate instead of buffer workspace
;;   (defun tl//rustic-compilation-buffer-crate (command &optional args)
;;     "Start a compilation process with COMMAND.

;; :no-display - don't display buffer when starting compilation process
;; :buffer - name for process buffer
;; :process - name for compilation process
;; :mode - mode for process buffer
;; :directory - set `default-directory'
;; :sentinel - process sentinel
;; "
;;     (let ((buf (get-buffer-create
;;                 (or (plist-get args :buffer) rustic-compilation-buffer-name)))
;;           (process (or (plist-get args :process) rustic-compilation-process-name))
;;           (mode (or (plist-get args :mode) 'rustic-compilation-mode))
;;           (directory (or (plist-get args :directory) (tl//rustic-buffer-crate)))
;;           (sentinel (or (plist-get args :sentinel) #'compilation-sentinel)))
;;       (rustic-compilation-setup-buffer buf directory mode)
;;       (setq next-error-last-buffer buf)
;;       (unless (plist-get args :no-display)
;;         (funcall rustic-compile-display-method buf))
;;       (with-current-buffer buf
;;         (rustic-make-process :name process
;;                              :buffer buf
;;                              :command command
;;                              :filter #'rustic-compilation-filter
;;                              :sentinel sentinel))))


;;   ;; (advice-remove #'rustic-compilation #'tl//rustic-compilation-buffer-crate)
;;   (advice-add #'rustic-compilation :override #'tl//rustic-compilation-buffer-crate)

;;; comment current line
;; Original idea from
;; http://www.opensubscriber.com/message/emacs-devel@gnu.org/10971693.html
(global-set-key (kbd "M-;") 'tl/comment-dwim)
(defun tl/comment-dwim (&optional arg)
  "Replacement for the comment-dwim command.
   If no region is selected and current line is not blank and we are not at the end of the line,
   then comment current line.
   Replaces default behaviour of comment-dwim, when it inserts comment at the end of the line."
  (interactive "*P")
  (comment-normalize-vars)

  (if (and (not (region-active-p)) (not (looking-at "[ \t]*$")))
      (comment-or-uncomment-region (line-beginning-position) (line-end-position))
    (comment-dwim arg)))

;; A fun startup message, somewhat reminiscent of "The Matrix: Reloaded"
(defconst animate-n-steps 3)
;; (defun emacs-reloaded ()
;;   (animate-string (concat ";; Initialization successful, welcome to "
;;                           (substring (emacs-version) 0 14) ".") 0 0)
;;   (newline-and-indent) (newline-and-indent))

;; (add-hook 'after-init-hook 'emacs-reloaded)

;; (mapc 'load (directory-files "~/.emacs.d/site-lisp" t "\.el$"))


;;; temporary fix bug
;; when el-get initialize ace-window, it'll compain can not find ace-jump-mode
;; (let ((p (expand-file-name"~/.emacs.d/el-get/ace-jump-mode")))
;;   (when (file-exists-p p)
;;     (add-to-list 'load-path p)))

;; (use-package org-bullets
;;   :defer t
;;   :init
;;   (progn
;;     (setq org-bullets-face-name nil
;;           org-bullets-invisible-leading-stars t)
;;     ;; http://nadeausoftware.com/articles/2007/11/latency_friendly_customized_bullets_using_unicode_characters
;;     ;; https://zhangda.wordpress.com/2016/02/15/configurations-for-beautifying-emacs-org-mode/
;;     ;; "⬢" "⭓" "■"
;;     ;; "◉" "◎" "⚫" "○" "►" "◇"
;;     ;; "✺" "✹" "✸" "✷" "✶" "✭" "✦" "■" "▼" "●"
;;     ;; "⊢" "⋮" "⋱" "⋱" "⋱"
;;     ;; "☯" "☰" "☱" "☲" "☳" "☴" "☵" "☶" "☷"
;;     (setq org-bullets-bullet-list
;;           '("☱" "☲" "☳" "☴" "☵" "☶" "☷"))
;;     (add-hook 'org-mode-hook 'org-bullets-mode)))
