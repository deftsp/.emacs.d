;; Shell settings.

(make-variable-buffer-local 'comint-completion-addsuffix)
(setq comint-completion-addsuffix t)    ; insert space/slash after file completion
(setq comint-eol-on-send t)
(setq shell-command-switch "-cf")
(setq shell-command-completion-mode t)  ; Allow completion for some shell-command functions.
;; set maximum-buffer size for shell-mode (useful if some program that you're debugging spews out large amounts of output).
(setq comint-buffer-maximum-size 10240)

(add-hook 'shell-mode-hook '(lambda () (toggle-truncate-lines 1)))

;; will truncate shell buffer to comint-buffer-maximum-size.
(add-hook 'comint-output-filter-functions 'comint-truncate-buffer)
;; will disalllow passwords to be shown in clear text (this is useful, for example, if you use the shell and then, don't
;; echo passwords when communicating with interactive programs login/telnet/ftp/scp etc. to other machines).
(add-hook 'comint-output-filter-functions 'comint-watch-for-password-prompt)
;; will remove ctrl-m from shell output.
(add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m)

;; (add-to-list 'exec-path "~/bin") ; 加入的这些目录，可以在 shell 中用 tab 补全命令。

;;Here's what I use to make Emacs show colors in shell windows:
(setq ansi-color-names-vector           ; better contrast colors
      ["black" "red4" "green4" "yellow4"
               "blue3" "magenta4" "cyan4" "white"])
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(setq comint-scroll-to-bottom-on-input t  ; always insert at the bottom
      comint-scroll-to-bottom-on-output t ; always add output at the bottom
      comint-scroll-show-maximum-output t ; scroll to show max possible output
      comint-completion-autolist t        ; show completion list when ambiguous
      ;; no duplicates in command history
      comint-input-ignoredups t)

(add-hook 'shell-mode-hook
          '(lambda ()
            (local-set-key [home] 'comint-bol)      ; move to beginning of line, after prompt
            (local-set-key [up] '(lambda () (interactive) ; cycle backward through command history
                                  (if (comint-after-pmark-p)
                                      (comint-previous-input 1)
                                      (previous-line 1))))
            (local-set-key [down] '(lambda () (interactive) ; cycle forward through command history
                                    (if (comint-after-pmark-p)
                                        (comint-next-input 1)
                                        (forward-line 1))))))

;; term
(defun tsp-ansi-term ()
  "Use bash for ansi term"
  (interactive)
  (ansi-term "/bin/zsh"))

;; Bash settings.
;; (setq bash-shell-file-name "/bin/bash")
;; (setenv "SHELL" bash-shell-file-name)
;; (setq explicit-shell-file-name bash-shell-file-name)

;; Initialized from the SHELL environment variable, or to a system-dependent default if SHELL is not set.
;; shell-script-mode will use it as sh-feature, so do not set it.
;; (setq shell-file-name bash-shell-file-name)

;; run a few shells.
;; (shell "*shell5*")
;; (shell "*shell6*")
;; (shell "*shell7*")

;; C-5, 6, 7 to switch to shells
;; (global-set-key [(control 5)] (lambda () (interactive) (switch-to-buffer "*shell5*")))
;; (global-set-key [(control 6)] (lambda () (interactive) (switch-to-buffer "*shell6*")))
;; (global-set-key [(control 7)] (lambda () (interactive) (switch-to-buffer "*shell7*")))

;;; auto close shell buffer, after execute `exit' exit shell
(add-hook 'shell-mode-hook 'tsp-shell-mode-hook-func)
(defun tsp-shell-mode-hook-func  ()
  (set-process-sentinel (get-buffer-process (current-buffer))
                        #'tsp-shell-mode-kill-buffer-on-exit))
(defun tsp-shell-mode-kill-buffer-on-exit (process state)
  (message "%s" state)
  (if (or
       (string-match "exited abnormally with code.*" state)
       (string-match "finished" state))
      (kill-buffer (current-buffer))))

;; Better abbrev expansion in shell-mode, make abbrevs don’t get expanded when you press RET.
;; (defadvice comint-send-input (before expand-input activate)
;;   "Expand input before sending"
;;   (expand-abbrev))



;;Make completion buffers in a shell disappear after 6 seconds.
;;<http://snarfed.org/space/why+I+don't+run+shells+inside+Emacs>
;; (add-hook 'completion-setup-hook
;;           (lambda () (run-at-time 6 nil
;;                                   (lambda () (delete-windows-on "*Completions*")))))
