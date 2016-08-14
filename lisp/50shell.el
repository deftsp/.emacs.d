;;; file 50shell.el

;; To use ansi-term with ZShell on a Mac, make sure you run
;; tic -o ~/.terminfo /Applications/Emacs.app/Contents/Resources/etc/e/eterm-color.ti
;; to get rid of weird characters

(setq shell-command-switch "-cf")
(setq shell-command-completion-mode t)  ; Allow completion for some shell-command functions.

(defun paloryemacs/shell-mode-init  ()
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
                        #'paloryemacs/shell-mode-kill-buffer-on-exit))

(add-hook 'shell-mode-hook 'paloryemacs/shell-mode-init)

;; auto close shell buffer, after execute `exit' exit shell
(defun paloryemacs/shell-mode-kill-buffer-on-exit (process state)
  (message "%s" state)
  (if (or
       (string-match "exited abnormally with code.*" state)
       (string-match "finished" state))
      (kill-buffer (current-buffer))))

;;; ansi-term
(defun paloryemacs/ansi-term ()
  "Use bash for ansi term"
  (interactive)
  (ansi-term "/bin/zsh"))

(provide '50shell)
