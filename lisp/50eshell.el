;;; 50eshell.el ---

;; Copyright (C) 2008  Shihpin Tseng
;; Author: Shihpin Tseng <deftsp@gmail.com>

(defvar shell-default-shell 'eshell
  "Default shell to use in Paloryemacs. Possible values are `eshell', `shell',
`term' and `ansi-term'.")

(defvar shell-default-position 'bottom
  "Position of the shell. Possible values are `top', `bottom', `full',
`left' and `right'.")

(defvar shell-default-height 30
  "Height in percents for the shell window.")

(defvar shell-default-term-shell shell-file-name
  "Default shell to use in `term' and `ansi-term' shells.")

(defvar shell-enable-smart-eshell nil
  "If non-nil then `em-smart' is enabled. `em-smart' allows to quickly review
commands, modify old commands or enter a new one.")

(defvar shell-protect-eshell-prompt t
  "If non-nil then eshell's prompt is protected. This means that
movement to the prompt is inhibited like for `comint-mode'
prompts and the prompt is made read-only")

(defvar shell-default-full-span t
  "If non-nil, the `shell' buffer spans full width of a frame.")


(defun paloryemacs/default-pop-shell ()
  "Open the default shell in a popup."
  (interactive)
  (let ((shell (if (eq 'multi-term shell-default-shell)
                   'multiterm
                 shell-default-shell)))
    (call-interactively (intern (format "paloryemacs/shell-pop-%S" shell)))))

(defmacro make-shell-pop-command (func &optional shell)
  "Create a function to open a shell via the function FUNC.
SHELL is the SHELL function to use (i.e. when FUNC represents a terminal)."
  (let* ((name (symbol-name func)))
    `(defun ,(intern (concat "paloryemacs/shell-pop-" name)) (index)
       ,(format (concat "Toggle a popup window with `%S'.\n"
                        "Multiple shells can be opened with a numerical prefix "
                        "argument. Using the universal prefix argument will "
                        "open the shell in the current buffer instead of a "
                        "popup buffer.") func)
       (interactive "P")
       (require 'shell-pop)
       (if (equal '(4) index)
           ;; no popup
           (,func ,shell)
         (shell-pop--set-shell-type
          'shell-pop-shell-type
          (backquote (,name
                      ,(concat "*" name "*")
                      (lambda nil (,func ,shell)))))
         (shell-pop index)))))


(defun paloryemacs/default-pop-shell ()
  "Open the default shell in a popup."
  (interactive)
  (let ((shell (if (eq 'multi-term shell-default-shell)
                   'multiterm
                 shell-default-shell)))
    (call-interactively (intern (format "paloryemacs/shell-pop-%S" shell)))))

(use-package shell-pop
  :defer t
  :init
  (progn
    (setq shell-pop-window-position shell-default-position
          shell-pop-window-size     shell-default-height
          shell-pop-term-shell      shell-default-term-shell
          shell-pop-full-span       shell-default-full-span)
    (make-shell-pop-command eshell)
    (make-shell-pop-command shell)
    (make-shell-pop-command term shell-pop-term-shell)
    (make-shell-pop-command multiterm)
    (make-shell-pop-command ansi-term shell-pop-term-shell)

    (add-hook 'term-mode-hook 'ansi-term-handle-close)
    (add-hook 'term-mode-hook (lambda () (linum-mode -1)))

    (paloryemacs/set-leader-keys
      "'"   'paloryemacs/default-pop-shell
      "ase" 'paloryemacs/shell-pop-eshell
      "asi" 'paloryemacs/shell-pop-shell
      "asm" 'paloryemacs/shell-pop-multiterm
      "ast" 'paloryemacs/shell-pop-ansi-term
      "asT" 'paloryemacs/shell-pop-term)))


(defun paloryemacs//protect-eshell-prompt ()
  "Protect Eshell's prompt like Comint's prompts.

E.g. `evil-change-whole-line' won't wipe the prompt. This
is achieved by adding the relevant text properties."
  (let ((inhibit-field-text-motion t))
    (add-text-properties
     (point-at-bol)
     (point)
     '(rear-nonsticky t
                      inhibit-line-move-field-capture t
                      field output
                      read-only t
                      front-sticky (field inhibit-line-move-field-capture)))))

(defun paloryemacs/eshell-mode-init ()
  (set (make-local-variable 'scroll-margin) 0)
  (setq pcomplete-cycle-completions nil)
  (unless shell-enable-smart-eshell
    ;; we don't want auto-jump to prompt when smart eshell is enabled.
    ;; Idea: maybe we could make auto-jump smarter and jump only if
    ;; point is not on a prompt line
    (add-hook 'evil-insert-state-entry-hook
              'paloryemacs//eshell-auto-end nil t))

  (define-key eshell-mode-map (kbd "C-u") 'eshell-kill-input)
  (define-key eshell-mode-map (kbd "C-a") 'paloryemacs/eshell-maybe-bol)
  ;; Caution! this will erase buffer's content at C-l
  (define-key eshell-mode-map (kbd "C-l") 'paloryemacs/eshell-clear-keystroke)
  ;; (define-key eshell-mode-map (kbd "C-d") 'paloryemacs/eshell-delchar-or-exit)
  (define-key eshell-mode-map (kbd "C-d") 'eshell-delchar-or-maybe-eof))

(defun paloryemacs/init-eshell-xterm-color ()
  "Initialize xterm coloring for eshell"
  (setq-local xterm-color-preserve-properties t)
  (make-local-variable 'eshell-preoutput-filter-functions)
  (add-hook 'eshell-preoutput-filter-functions 'xterm-color-filter)
  (setq-local eshell-output-filter-functions
              (remove 'eshell-handle-ansi-color
                      eshell-output-filter-functions)))

(defun paloryemacs//eshell-switch-company-frontend ()
  "Sets the company frontend to `company-preview-frontend' in e-shell mode."
  (require 'company)
  (setq-local company-frontends '(company-preview-frontend)))

(with-eval-after-load 'company
  (add-hook 'eshell-mode-hook 'paloryemacs/company-eshell-mode-setup)
  (defun paloryemacs/company-eshell-mode-setup ()
    (let ((backends paloryemacs/company-common-backends))
      (setq-local company-backends backends))))

(defun paloryemacs//eshell-auto-end ()
  "Move point to end of current prompt when switching to insert state."
  (when (and (eq major-mode 'eshell-mode)
             ;; Not on last line, we might want to edit within it.
             (not (eq (point) (point-max))))
    (end-of-buffer)))

(defface epe-user-face
  '((t :foreground "red"))
  "Face of user in prompt."
  :group 'epe)

(defface epe-host-face
  '((t :foreground "blue"))
  "Face of host in prompt."
  :group 'epe)

(defface epe-time-face
  '((t :foreground "yellow"))
  "Face of time in prompt."
  :group 'epe)

(defface epe-delimiter-face
  '((t :foreground "yellow"))
  "Face of delimiter in prompt."
  :group 'epe)

(defun epe-theme-palory ()
  "A eshell-prompt theme with full path, smiliar to oh-my-zsh theme."
  (setq eshell-prompt-regexp "^[^#\nλ]* λ[#]* ")
  (concat
   (if (epe-remote-p)
       (progn
	     (concat
	      (epe-colorize-with-face (epe-remote-user) 'epe-user-face)
	      (epe-colorize-with-face "@" 'epe-host-face)
	      (epe-colorize-with-face (epe-remote-host) 'epe-host-face)))
     (progn
       (concat
        (epe-colorize-with-face (format-time-string "%H:%M:%S" (current-time)) 'epe-time-face)
        " "
	    (epe-colorize-with-face (user-login-name) 'epe-user-face)
	    (epe-colorize-with-face "@" 'epe-host-face)
	    (epe-colorize-with-face (system-name) 'epe-host-face))))
   (concat
    (epe-colorize-with-face ":" 'epe-dir-face)
    (epe-colorize-with-face (concat (epe-fish-path (eshell/pwd))) 'epe-dir-face)
    (epe-colorize-with-face  "\n" 'epe-delimiter-face))
   (when epe-show-python-info
     (when (fboundp 'epe-venv-p)
       (when (and (epe-venv-p) venv-current-name)
	     (epe-colorize-with-face (concat "(" venv-current-name ") ") 'epe-venv-face))))
   (when (epe-git-p)
     (concat
      (epe-colorize-with-face ":" 'epe-dir-face)
      (epe-colorize-with-face
       (concat (epe-git-branch)
	           (epe-git-dirty)
	           (epe-git-untracked)
	           (let ((unpushed (epe-git-unpushed-number)))
		         (unless (= unpushed 0)
		           (concat ":" (number-to-string unpushed)))))
       'epe-git-face)))
   (epe-colorize-with-face " λ" 'epe-symbol-face)
   (epe-colorize-with-face (if (= (user-uid) 0) "#" "") 'epe-sudo-symbol-face)
   " "))


(use-package eshell
  :defer t
  :init
  (progn
    (setq eshell-error-if-no-glob t
          eshell-history-size 500
          ;; auto truncate after 20k lines
          eshell-buffer-maximum-lines 20000
          eshell-hist-ignoredups t
          ;; buffer shorthand -> echo foo > #'buffer
          ;; eshell-buffer-shorthand t ; seem removed in Emacs26
          ;; my prompt is easy enough to see
          eshell-highlight-prompt nil
          ;; treat 'echo' like shell echo
          eshell-plain-echo-behavior t
          ;; cache directory
          eshell-directory-name (concat paloryemacs/cache-directory "eshell/"))

    (autoload 'eshell-delchar-or-maybe-eof "em-rebind")
    (add-hook 'eshell-after-prompt-hook 'paloryemacs//protect-eshell-prompt)
    (add-hook 'eshell-mode-hook 'paloryemacs/eshell-mode-init)
    :config
    (progn
      ;; Work around bug in eshell's preoutput-filter code.
      ;; Eshell doesn't call preoutput-filter functions in the context of the eshell
      ;; buffer. This breaks the xterm color filtering when the eshell buffer is updated
      ;; when it's not currently focused.
      ;; To remove if/when fixed upstream.
      (defun eshell-output-filter@paloryemacs-with-buffer (fn process string)
        (let ((proc-buf (if process (process-buffer process)
                          (current-buffer))))
          (when proc-buf
            (with-current-buffer proc-buf
              (funcall fn process string)))))
      (advice-add
       #'eshell-output-filter
       :around
       #'eshell-output-filter@paloryemacs-with-buffer)

      (require 'esh-opt)

      ;; ;; quick commands
      (defalias 'eshell/e 'find-file-other-window)
      (defalias 'eshell/d 'dired)
      (setenv "PAGER" "cat")

      ;; support `em-smart'
      (when shell-enable-smart-eshell
        (require 'em-smart)
        (setq eshell-where-to-jump 'begin
              eshell-review-quick-commands nil
              eshell-smart-space-goes-to-end t)
        (add-hook 'eshell-mode-hook 'eshell-smart-initialize))

      ;; Visual commands
      (require 'em-term)
      (mapc (lambda (x) (push x eshell-visual-commands))
            '("el" "elinks" "htop" "less" "ssh" "tmux" "top"))

      ;; automatically truncate buffer after output
      (when (boundp 'eshell-output-filter-functions)
        (push 'eshell-truncate-buffer eshell-output-filter-functions))

      ;; These don't work well in normal state
      ;; due to evil/emacs cursor incompatibility
      (with-eval-after-load 'evil
        (evil-define-key 'insert eshell-mode-map
          (kbd "C-k") 'eshell-previous-matching-input-from-input
          (kbd "C-j") 'eshell-next-matching-input-from-input))

      (use-package eshell-autojump
        :defer t
        :init
        (with-eval-after-load 'eshell
          (require 'eshell-autojump)))

      (use-package eshell-did-you-mean
        :config
        (eshell-did-you-mean-setup))

      (use-package eshell-fringe-status
        :init
        (add-hook 'eshell-mode-hook 'eshell-fringe-status-mode))

      (use-package eshell-prompt-extras
        :init
        (setq eshell-highlight-prompt nil
              eshell-prompt-function 'epe-theme-palory)))))

(use-package xterm-color
  :init
  (progn
    ;; Comint and Shell
    (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter)
    (setq comint-output-filter-functions
          (remove 'ansi-color-process-output comint-output-filter-functions))
    (add-hook 'eshell-mode-hook 'paloryemacs/init-eshell-xterm-color)))

(defun paloryemacs/eshell-delchar-or-exit (arg)
  (interactive "P")
  (let ((cur-point (point))
        (delchar-flag t))
    (save-excursion
      (eshell-bol)
      (when (and (= cur-point (point))
                 (= cur-point (point-max)))
        (setq delchar-flag nil)))
    (if delchar-flag
        (delete-char (if (null arg) 1 arg) (if (null arg) nil t))
      (eshell-bol)
      (insert "exit")
      (eshell-send-input)
      (delete-window))))


;; C-a to beginning of command line or beginning of line?
;; I use the following code. It makes C-a go to the beginning of the command line, unless it is already there, in which
;; case it goes to the beginning of the line. So if you are at the end of the command line and want to go to the real
;; beginning of line, hit C-a twice:
(defun paloryemacs/eshell-maybe-bol ()
  (interactive)
  (let ((p (point)))
    (eshell-bol)
    (if (= p (point))
        (beginning-of-line))))

;;; eshell here
;; http://www.howardism.org/Technical/Emacs/eshell-fun.html
(defun paloryemacs/eshell-here ()
  "Opens up a new shell in the directory associated with the
current buffer's file. The eshell is renamed to match that
directory to make multiple eshell windows easier."
  (interactive)
  (let* ((parent (if (buffer-file-name)
                     (file-name-directory (buffer-file-name))
                   default-directory))
         (height (/ (window-total-height) 3))
         (name   (car (last (split-string parent "/" t)))))
    (split-window-vertically (- height))
    (other-window 1)
    (eshell "new")
    (rename-buffer (concat "*eshell: " name "*"))

    (insert (concat "ls"))
    (eshell-send-input)))

(global-set-key (kbd "C-!") 'paloryemacs/eshell-here)

;;; alias
(defun eshell/l (&rest args)
  "ls -ltr alias"
  (eshell/ls "-ltr" args))

(defun eshell/ll (&rest args)
  "ls -alh alias"
  (eshell/ls "-alh" args))

(defun eshell/lla (&rest args)
  "ls -lA alias"
  (eshell/ls "-lA" args))

;; This is an eshell alias
(defun eshell/clear ()
  (let ((inhibit-read-only t))
    (erase-buffer)))

;; This is a key-command
(defun paloryemacs/eshell-clear-keystroke ()
  "Allow for keystrokes to invoke eshell/clear"
  (interactive)
  (eshell/clear)
  (eshell-send-input))


(defun eshell/dired () (dired (eshell/pwd)))
(defalias 'eshell/emacs 'find-file)
(defalias 's 'magit-status)

(provide '50eshell)
