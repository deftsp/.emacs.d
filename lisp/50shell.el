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

;; ghostel terminal backend (libghostty):
;; https://github.com/dakra/ghostel
;; TODO: try https://github.com/CsBigDataHub/popterm.el
(use-package ghostel
  :straight (:host github :repo "dakra/ghostel"
             :files ("lisp/*.el" "extensions/evil-ghostel/*.el" ("etc" "etc/*")))
  :config
  (setq ghostel-module-auto-install 'ask)
  ;; (setq ghostel-glyph-scale-floor 0.0)

  ;; Pin to spinner (errors with a hint if spinner.el isn't installed):
  (setq ghostel-progress-function #'ghostel-spinner-progress)
  ;; Or stay on the plain text indicator:
  ;; (setq ghostel-progress-function #'ghostel-default-progress)

  ;; Pick a different spinner style — see `spinner-types' in spinner.el:
  (setq ghostel-spinner-type 'horizontal-moving)
  ;; (setq ghostel-prompt-regexp "➜ ")

  (setq ghostel-max-scrollback (* 20 1024 1024)) ; 20 MB
  (setq ghostel-kill-buffer-on-exit t))

(use-package evil-ghostel
  :after (ghostel evil)
  :hook (ghostel-mode . evil-ghostel-mode))

(provide '50shell)
