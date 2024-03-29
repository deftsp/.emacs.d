;;; 50eshell.el ---

;; Copyright (C) 2008  Shihpin Tseng
;; Author: Shihpin Tseng <deftsp@gmail.com>

;;; Links
;; https://github.com/xuchunyang/eshell-git-prompt
;; https://github.com/manateelazycat/aweshell
;; https://github.com/howardabrams/dot-files/blob/master/emacs-eshell.org

;;; shell-pop
(defvar shell-default-shell 'eshell
  "Default shell to use. Possible values are `eshell', `shell',
`term' and `ansi-term'.")

(defvar shell-default-position 'bottom
  "Position of the shell. Possible values are `top', `bottom', `full',
`left' and `right'.")

(defvar shell-default-height 30
  "Height in percents for the shell window.")

(defvar shell-default-width 30
  "Width in percents for the shell window.")

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

(defvar close-window-with-terminal nil
  "If non-nil, the window is closed when the terminal is stopped.
This is only applied to `term' and `ansi-term' modes.")

(defun tl/default-pop-shell ()
  "Open the default shell in a popup."
  (interactive)
  (let ((shell (cl-case shell-default-shell
                 ('multi-term 'multiterm)
                 ('shell 'inferior-shell)
                 (t shell-default-shell))))
    (call-interactively (intern (format "tl/shell-pop-%S" shell)))))

(defun tl/resize-shell-to-desired-width ()
  (when (and (string= (buffer-name) shell-pop-last-shell-buffer-name)
             (memq shell-pop-window-position '(left right)))
    (enlarge-window-horizontally (- (/ (* (frame-width) shell-default-width)
                                       100)
                                    (window-width)))))

(defmacro make-shell-pop-command (func &optional shell)
  "Create a function to open a shell via the function FUNC.
SHELL is the SHELL function to use (i.e. when FUNC represents a terminal)."
  (let* ((name (symbol-name func)))
    `(defun ,(intern (concat "tl/shell-pop-" name)) (index)
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
         (shell-pop index)
         (tl/resize-shell-to-desired-width)))))

(defun ansi-term-handle-close ()
  "Close current term buffer when `exit' from term buffer."
  (when (ignore-errors (get-buffer-process (current-buffer)))
    (set-process-sentinel (get-buffer-process (current-buffer))
                          (lambda (proc change)
                            (when (string-match "\\(finished\\|exited\\)"
                                                change)
                              (kill-buffer (process-buffer proc))
                              (when (and close-window-with-terminal
                                         (> (count-windows) 1))
                                (delete-window)))))))

(use-package shell-pop
  :defer t
  :init
  (progn
    (setq shell-pop-window-position shell-default-position
          shell-pop-window-size     shell-default-height
          shell-pop-term-shell      shell-default-term-shell
          shell-pop-full-span       shell-default-full-span)
    (make-shell-pop-command eshell)
    (make-shell-pop-command term shell-pop-term-shell)
    (make-shell-pop-command ansi-term shell-pop-term-shell)
    (make-shell-pop-command inferior-shell)
    (make-shell-pop-command multiterm)

    (add-hook 'term-mode-hook 'ansi-term-handle-close)

    (tl/declare-prefix "'" "open shell")
    (tl/declare-prefix "as" "shells")

    (tl/set-leader-keys
      "'"   'tl/default-pop-shell
      "ase" 'tl/shell-pop-eshell
      "asi" 'tl/shell-pop-shell
      "asm" 'tl/shell-pop-multiterm
      "ast" 'tl/shell-pop-ansi-term
      "asT" 'tl/shell-pop-term)))

(defun tl//protect-eshell-prompt ()
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

(defun tl//eshell-auto-end ()
  "Move point to end of current prompt when switching to insert state."
  (when (and (eq major-mode 'eshell-mode)
             ;; Not on last line, we might want to edit within it.
             (not (eq (point) (point-max))))
    (end-of-buffer)))

;; C-a to beginning of command line or beginning of line?
;; I use the following code. It makes C-a go to the beginning of the command line, unless it is already there, in which
;; case it goes to the beginning of the line. So if you are at the end of the command line and want to go to the real
;; beginning of line, hit C-a twice:
(defun tl/eshell-maybe-bol ()
  (interactive)
  (let ((p (point)))
    (eshell-bol)
    (if (= p (point))
        ;; with `tl//protect-eshell-prompt', `beginning-of-line' will
        ;; not work
        (beginning-of-line))))

;; This is a key-command
(defun tl/eshell-clear-keystroke ()
  "Allow for keystrokes to invoke eshell/clear"
  (interactive)
  (eshell/clear)
  (eshell-send-input))


;; clear the buffer while preserving unsent input. This is more comint-like.
(defun tl/eshell-clear-buffer ()
  "Clear `eshell' buffer, comint-style."
  (interactive)
  (let ((input (eshell-get-old-input)))
    (eshell/clear-scrollback)
    (eshell-emit-prompt)
    (insert input)))


(defun tl/eshell-mode-init ()
  (display-line-numbers-mode -1)
  (setq pcomplete-cycle-completions nil)
  (buffer-face-set 'tl/eshell-base-face)
  (unless shell-enable-smart-eshell
    ;; we don't want auto-jump to prompt when smart eshell is enabled. Idea:
    ;; maybe we could make auto-jump smarter and jump only if point is not on a
    ;; prompt line
    (add-hook 'evil-insert-state-entry-hook
              'tl//eshell-auto-end nil t))

  (define-key eshell-mode-map (kbd "C-u") 'eshell-kill-input)
  (define-key eshell-mode-map (kbd "C-a") 'tl/eshell-maybe-bol)

  (define-key eshell-mode-map (kbd "C-d") 'eshell-delchar-or-maybe-eof)
  ;; Caution! this will erase buffer's content at C-l
  ;; (define-key eshell-mode-map (kbd "C-l") 'tl/eshell-clear-keystroke)
  (define-key eshell-mode-map (kbd "C-l") 'tl/eshell-clear-buffer)
  (with-eval-after-load 'evil
    ;; These don't work well in normal state due to evil/emacs cursor
    ;; incompatibility
    (evil-define-key 'insert eshell-mode-map
      (kbd "C-k") 'eshell-previous-matching-input-from-input
      (kbd "C-j") 'eshell-next-matching-input-from-input)))

(use-package eshell
  :defer t
  :init
  (setq eshell-cmpl-cycle-completions nil
        eshell-error-if-no-glob t
        eshell-history-size 500
        eshell-save-history-on-exit t
        eshell-scroll-to-bottom-on-input 'all
        ;; auto truncate after 20k lines
        eshell-buffer-maximum-lines 20000
        eshell-hist-ignoredups t
        eshell-prefer-lisp-functions nil
        eshell-destroy-buffer-when-process-dies nil
        ;; buffer shorthand -> echo foo > #'buffer
        ;; eshell-buffer-shorthand t ; seem removed in Emacs26
        ;; my prompt is easy enough to see
        eshell-highlight-prompt nil
        ;; treat 'echo' like shell echo
        eshell-plain-echo-behavior t
        eshell-directory-name (concat user-emacs-directory "eshell/"))

  (when shell-protect-eshell-prompt
    (add-hook 'eshell-after-prompt-hook 'tl//protect-eshell-prompt))

  (autoload 'eshell-delchar-or-maybe-eof "em-rebind")
  (add-hook 'eshell-mode-hook 'tl/eshell-mode-init)
  :config
  ;; Work around bug in eshell's preoutput-filter code.
  ;; Eshell doesn't call preoutput-filter functions in the context of the eshell
  ;; buffer. This breaks the xterm color filtering when the eshell buffer is updated
  ;; when it's not currently focused.
  ;; To remove if/when fixed upstream.
  (defun eshell-output-filter@tl-with-buffer (fn process string)
    (let ((proc-buf (if process (process-buffer process)
                      (current-buffer))))
      (when proc-buf
        (with-current-buffer proc-buf
          (funcall fn process string)))))
  (advice-add
   #'eshell-output-filter
   :around
   #'eshell-output-filter@tl-with-buffer)

  (require 'esh-opt)

  ;; quick commands
  (defalias 'eshell/e 'find-file-other-window)
  (defalias 'eshell/d 'dired)

  ;; don't pause the output through the $PAGER variable
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
  ;; Eshell would get somewhat confused if I ran the following commands
  ;; directly through the normal Elisp library, as these need the better
  ;; handling of ansiterm
  (mapc (lambda (x) (push x eshell-visual-commands))
        '("el" "elinks" "htop" "less" "ssh" "tmux" "top" "tail"))

  (setq eshell-visual-subcommands
        '(("git" "log" "diff" "show"
           "l" "lol" "d" "dc") ; aliases
          ("sudo" "vi" "visudo")))
  ;; automatically truncate buffer after output
  (when (boundp 'eshell-output-filter-functions)
    (push 'eshell-truncate-buffer eshell-output-filter-functions))

  (use-package eshell-autojump)

  ;; http://www.modernemacs.com/post/custom-eshell/
  (use-package eshell-prompt-extras
    :init
    (defface tl/eshell-base-face
      '((t :foreground "black"
           :background "aquamarine"
           :font "Knack Nerd Font"))
      "Base face for shell."
      :group 'eshell-prompt)

    (defface epe-user-face
      '((t :inherit tl/eshell-base-face :foreground "red"))
      "Face of user in prompt."
      :group 'eshell-prompt)

    (defface epe-host-face
      '((t :inherit tl/eshell-base-face  :foreground "blue"))
      "Face of host in prompt."
      :group 'eshell-prompt)

    (defface epe-time-face
      '((t :inherit tl/eshell-base-face :foreground "yellow"))
      "Face of time in prompt."
      :group 'eshell-prompt)

    (defface epe-delimiter-face
      '((t :inherit tl/eshell-base-face  :foreground "yellow"))
      "Face of delimiter in prompt."
      :group 'eshell-prompt)

    (defun paloryemac//eshell-prompt-function ()
      "A eshell-prompt theme with full path, smiliar to oh-my-zsh theme."
      (setq eshell-prompt-regexp "^\n┌─.*\n.* λ[#]* ")
      (concat
       (if (epe-remote-p)
           (progn
             (concat
              (epe-colorize-with-face (epe-remote-user) 'epe-user-face)
              (epe-colorize-with-face "@" 'epe-host-face)
              (epe-colorize-with-face (epe-remote-host) 'epe-host-face)))
         (progn
           (concat
            (epe-colorize-with-face  "\n┌─" 'epe-delimiter-face)
            (epe-colorize-with-face (format-time-string "%H:%M:%S" (current-time)) 'epe-time-face)
            (epe-colorize-with-face  " " 'epe-delimiter-face)
            (epe-colorize-with-face (user-login-name) 'epe-user-face)
            (epe-colorize-with-face "@" 'epe-host-face)
            (epe-colorize-with-face (system-name) 'epe-host-face))))
       (concat
        (epe-colorize-with-face ":" 'epe-dir-face)
        (epe-colorize-with-face (concat (epe-fish-path (eshell/pwd))) 'epe-dir-face)
        (epe-colorize-with-face  "\n" 'epe-delimiter-face))
       (epe-colorize-with-face  "└─" 'epe-delimiter-face)
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

    (setq eshell-highlight-prompt nil
          eshell-prompt-function 'paloryemac//eshell-prompt-function))

  (use-package eshell-fringe-status
    :defer t
    :commands (eshell-fringe-status-mode)
    :init
    (add-hook 'eshell-mode-hook 'eshell-fringe-status-mode)))



(use-package esh-mode
  :defer t
  :config
  (progn
    (use-package eshell-did-you-mean
      :config
      (eshell-did-you-mean-setup))))


(defun tl/init-eshell-xterm-color ()
  "Initialize xterm coloring for eshell"
  (setq-local xterm-color-preserve-properties t)
  (make-local-variable 'eshell-preoutput-filter-functions)
  (add-hook 'eshell-preoutput-filter-functions 'xterm-color-filter)
  (setq-local eshell-output-filter-functions
              (remove 'eshell-handle-ansi-color
                      eshell-output-filter-functions)))



(use-package xterm-color
  :init
  ;; Comint and Shell
  (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter)
  (setq comint-output-filter-functions
        (remove 'ansi-color-process-output comint-output-filter-functions))
  (add-hook 'eshell-mode-hook 'tl/init-eshell-xterm-color))

;;; eshell here
;; http://www.howardism.org/Technical/Emacs/eshell-fun.html
(defun tl/eshell-here ()
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

(global-set-key (kbd "C-!") 'tl/eshell-here)

;;; alias
(defun eshell/l (&rest args)
  "ls -ltr alias"
  (eshell/ls "-ltr" args))

;; (defun eshell/ll (&rest args)
;;   "ls -alh alias"
;;   (eshell/ls "-alh" args))

(defun eshell/lla (&rest args)
  "ls -lA alias"
  (eshell/ls "-lA" args))

(defun eshell/lld (&rest args)
  "ls -lA alias"
  (eshell/ls "-ld" *(/)))


;; This is an eshell alias
(defun eshell/clear ()
  (let ((inhibit-read-only t))
    (erase-buffer)))

;; using the alias doesn't pull in the current working directory, use function instead
(defun eshell/gst (&rest args)
  (magit-status (pop args) nil)
  (eshell/echo))   ;; The echo command suppresses output

(defun eshell/f (filename &optional dir try-count)
  "Searches for files matching FILENAME in either DIR or the
current directory. Just a typical wrapper around the standard
`find' executable.

Since any wildcards in FILENAME need to be escaped, this wraps the shell command.

If not results were found, it calls the `find' executable up to
two more times, wrapping the FILENAME pattern in wildcat
matches. This seems to be more helpful to me."
  (let* ((cmd (concat
               (executable-find "find")
               " " (or dir ".")
               "      -not -path '*/.git*'"
               " -and -not -path '*node_modules*'"
               " -and -not -path '*classes*'"
               " -and "
               " -type f -and "
               "-iname '" filename "'"))
         (results (shell-command-to-string cmd)))

    (if (not (s-blank-str? results))
        results
      (cond
       ((or (null try-count) (= 0 try-count))
        (eshell/f (concat filename "*") dir 1))
       ((or (null try-count) (= 1 try-count))
        (eshell/f (concat "*" filename) dir 2))
       (t "")))))


(defun eshell/ef (filename &optional dir)
  "Searches for the first matching filename and loads it into a
file to edit."
  (let* ((files (eshell/f filename dir))
         (file (car (s-split "\n" files))))
    (find-file file)))

(defun eshell/find (&rest args)
  "Wrapper around the ‘find’ executable."
  (let ((cmd (concat "find " (string-join args))))
    (shell-command-to-string cmd)))


(defun eshell/dired ()
  (dired (eshell/pwd)))


;; (require 'em-alias)
;; (require 'esh-io)
;; have already permanent to ~/.emacs.d/eshell/alias
;; N.B. every `eshell/alias' will cause the file ~/.emacs.d/eshell/alias be save once
;; (eshell/alias "e" "find-file $1")
;; (eshell/alias "ff" "find-file $1")
;; (eshell/alias "emacs" "find-file $1")
;; (eshell/alias "ee" "find-file-other-window $1")
;; (eshell/alias "gd" "magit-diff-unstaged")
;; (eshell/alias "gds" "magit-diff-staged")
;; (eshell/alias "d" "dired $1")

;; ;; The 'ls' executable requires the Gnu version on the Mac
;; (let ((ls (if (file-exists-p "/usr/local/bin/gls")
;;               "/usr/local/bin/gls"
;;             "/bin/ls")))
;;   (eshell/alias "ll" (concat ls " -AlohG --color=always")))



;;; Predicate Filters and Modifiers

;; The T predicate filter allows me to limit file results that have
;; have internal org-mode tags. For instance, files that have a
;; #+TAGS: header with a mac label will be given to the grep
;; function:

;; $ grep brew *.org(T'mac')
(defun eshell-org-file-tags ()
  "Helps the eshell parse the text the point is currently on,
looking for parameters surrounded in single quotes. Returns a
function that takes a FILE and returns nil if the file given to
it doesn't contain the org-mode #+TAGS: entry specified."

  (if (looking-at "'\\([^)']+\\)'")
      (let* ((tag (match-string 1))
             (reg (concat "^#\\+TAGS:.* " tag "\\b")))
        (goto-char (match-end 0))

        `(lambda (file)
           (with-temp-buffer
             (insert-file-contents file)
             (re-search-forward ,reg nil t 1))))
    (error "The `T' predicate takes an org-mode tag value in single quotes.")))

;; Note: We can’t add it to the list until after we start our first eshell
;; session, so we just add it to the eshell-pred-load-hook which is sufficient.
(add-hook 'eshell-pred-load-hook
          (lambda ()
            (add-to-list 'eshell-predicate-alist '(?T . (eshell-org-file-tags)))))

;; http://whyarethingsthewaytheyare.com/fishlike-autosuggestions-in-eshell/
;; hit RET to accept the autosuggest, or you can bind a key of your choosing in company-active-map
(defun company-eshell-autosuggest-candidates (prefix)
  (let* ((history
          (delete-dups
           (mapcar (lambda (str)
                     (string-trim (substring-no-properties str)))
                   (ring-elements eshell-history-ring))))
         (most-similar (cl-find-if
                        (lambda (str)
                          (string-prefix-p prefix str))
                        history)))
    (when most-similar
      `(,most-similar))))

(defun company-eshell-autosuggest--prefix ()
  (let ((prefix
         (string-trim-left
          (save-excursion
            (buffer-substring-no-properties
             (progn
               (eshell-bol)
               (point))
             (progn
               (end-of-line)
               (point))))
          )))
    (if (not (string-empty-p prefix))
        prefix
      'stop)))

(defun company-eshell-autosuggest (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-eshell))
    (prefix (and (eq major-mode 'eshell-mode)
                 (company-eshell-autosuggest--prefix)))
    (candidates (company-eshell-autosuggest-candidates arg))))

(with-eval-after-load 'company
  (defun setup-eshell-autosuggest ()
    (setq-local company-backends '(company-eshell-autosuggest))
    (setq-local company-frontends '(company-preview-frontend)))

  (add-hook 'eshell-mode-hook 'tl/company-eshell-mode-setup)
  (defun tl/company-eshell-mode-setup ()
    (setq-local company-backends '(company-eshell-autosuggest))
    (setq-local company-frontends '(company-preview-frontend))))



(provide '50eshell)
