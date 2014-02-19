;;; file 50shell.el

;; To use ansi-term with ZShell on a Mac, make sure you run
;; tic -o ~/.terminfo /Applications/Emacs.app/Contents/Resources/etc/e/eterm-color.ti
;; to get rid of weird characters

(setq shell-command-switch "-cf")
(setq shell-command-completion-mode t)  ; Allow completion for some shell-command functions.

(defun pl/shell-mode-hook-func  ()
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
  (set-process-sentinel (get-buffer-process (current-buffer))
                        #'pl/shell-mode-kill-buffer-on-exit))

(add-hook 'shell-mode-hook 'pl/shell-mode-hook-func)


;; auto close shell buffer, after execute `exit' exit shell
(defun pl/shell-mode-kill-buffer-on-exit (process state)
  (message "%s" state)
  (if (or
       (string-match "exited abnormally with code.*" state)
       (string-match "finished" state))
      (kill-buffer (current-buffer))))

;;; comint
;; set maximum-buffer size for shell-mode (useful if some program that you're debugging spews out large amounts of output).
(setq comint-buffer-maximum-size 10240
      comint-scroll-to-bottom-on-input t  ; always insert at the bottom
      comint-scroll-to-bottom-on-output t ; always add output at the bottom
      comint-scroll-show-maximum-output t ; scroll to show max possible output
      comint-completion-autolist t        ; show completion list when ambiguous
      ;; no duplicates in command history
      comint-input-ignoredups t)

;; will truncate shell buffer to comint-buffer-maximum-size.
(add-hook 'comint-output-filter-functions 'comint-truncate-buffer)
;; will disalllow passwords to be shown in clear text (this is useful, for example, if you use the shell and then, don't
;; echo passwords when communicating with interactive programs login/telnet/ftp/scp etc. to other machines).
(add-hook 'comint-output-filter-functions 'comint-watch-for-password-prompt)



;;; ansi-term
(defun pl/ansi-term ()
  "Use bash for ansi term"
  (interactive)
  (ansi-term "/bin/zsh"))

(provide '50shell)
