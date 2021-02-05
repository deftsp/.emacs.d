;;; file 50shell.el

;; To use ansi-term with ZShell on a Mac, make sure you run
;; tic -o ~/.terminfo /Applications/Emacs.app/Contents/Resources/etc/e/eterm-color.ti
;; to get rid of weird characters

;; zsh --help
;; `-c' take first argument as a command to execute
;; `-f' equivalent to --no-rcs and it is equivalent to th e--norc option in Bash.
;;      it will prevents the zsh startup files from being sourced.

;; N.B. `call-process' and `call-process-shell-command' will not use value of `process-variable'.
;; It just run the command from the shell in separate process. If we set `-f' option to `shell-command-switch',
;; the environment variables set in `~/.zprofile' will not be read.
(setq shell-command-switch "-c")        ; "-cf"
(setq shell-command-completion-mode t)  ; Allow completion for some shell-command functions.

(defun tl/shell-mode-init  ()
  (toggle-truncate-lines 1)
  (ansi-color-for-comint-mode-on)
  (local-set-key [home] 'comint-bol)      ; move to beginning of line, after prompt
  (local-set-key [up] '(lambda () (interactive) ; cycle backward through command history
                         (if (comint-after-pmark-p)
                             (comint-previous-input 1)
                           (previous-line 1))))
  (local-set-key [down] '(lambda () (interactive) ; cycle forward through command history
                           (if (comint-after-pmark-p)
                               (comint-next-input 1)
                             (forward-line 1))))
  ;; (set (make-local-variable 'scroll-margin) 0)

  ;; truncate shell buffer to comint-buffer-maximum-size.
  (add-hook 'comint-output-filter-functions 'comint-truncate-buffer nil t)
  ;; disalllow passwords to be shown in clear text (this is useful, for example,
  ;; if you use the shell and then, don't echo passwords when communicating with
  ;; interactive programs login/telnet/ftp/scp etc. to other machines).
  (add-hook 'comint-output-filter-functions 'comint-watch-for-password-prompt nil t)

  (set-process-sentinel (get-buffer-process (current-buffer))
                        #'tl/shell-mode-kill-buffer-on-exit))

(add-hook 'shell-mode-hook 'tl/shell-mode-init)

;; auto close shell buffer, after execute `exit' exit shell
(defun tl/shell-mode-kill-buffer-on-exit (process state)
  (message "%s" state)
  (if (or
       (string-match "exited abnormally with code.*" state)
       (string-match "finished" state))
      (kill-buffer (current-buffer))))

;;; ansi-term
(defun tl/ansi-term ()
  "Use bash for ansi term"
  (interactive)
  (ansi-term "/bin/zsh"))


;;; vterm
;; [[https://github.com/akermu/emacs-libvterm][akermu/emacs-libvterm: Emacs libvterm integration]]
;; [[https://github.com/jixiuf/vterm-toggle][jixiuf/vterm-toggle: toggles between the vterm buffer and whatever buffer you are editing.]]
(defun tl/vterm-mode-init ()
  (setq confirm-kill-processes nil
        ;; Prevent premature horizontal scrolling
        hscroll-margin 0))

(use-package vterm
  :commands (vterm vterm-other-window)
  :init
  ;; (setq vterm-keymap-exceptions nil)
  (setq vterm-max-scrollback 20000)
  (setq vterm-kill-buffer-on-exit t)
  :config
  (add-hook 'vterm-mode-hook 'tl/vterm-mode-init))

(use-package multi-vterm
  :defer t
  :init
  (with-eval-after-load 'evil
	(evil-define-key 'normal vterm-mode-map (kbd ",c")       #'multi-vterm)
	(evil-define-key 'normal vterm-mode-map (kbd ",n")       #'multi-vterm-next)
    (evil-define-key 'normal vterm-mode-map (kbd ",p")       #'multi-vterm-prev)))

(use-package vterm-toggle
  :defer t
  :commands (vterm-toggle vterm-toggle-cd))

(provide '50shell)
