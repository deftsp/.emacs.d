;;; 50agent-shell.el ---                                    -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>
;; Keywords:

;;; Code:

(use-package agent-shell
  :straight t
  :commands (agent-shell-opencode-start-agent)
  :bind (:map agent-shell-mode-map
         ("RET" . newline)
         ("C-c C-c" . shell-maker-submit)
         ("C-c C-k" . agent-shell-interrupt))
  :config
  (setq agent-shell-opencode-authentication
        (agent-shell-opencode-make-authentication :none t))

  ;; disable creating transcript file
  ;; (setq agent-shell-transcript-file-path-function nil)

  ;; "opencode/claude-sonnet-4-6"
  ;; "deepseek/deepseek-reasoner"
  ;; using the default model in opencode.jsonc
  (setq agent-shell-opencode-default-model-id nil)

  (setq agent-shell-preferred-agent-config (agent-shell-opencode-make-agent-config))

  ;; (defun tl/agent-shell-start-gemini-agent ()
  ;;   "Start an interactive Gemini CLI agent shell."
  ;;   (interactive)
  ;;   (agent-shell--start
  ;;    :new-session t
  ;;    :mode-line-name "Gemini"
  ;;    :buffer-name "Gemini"
  ;;    :shell-prompt "Gemini> "
  ;;    :shell-prompt-regexp "Gemini> "
  ;;    :needs-authentication t
  ;;    :authenticate-request-maker (lambda ()
  ;;                                  (acp-make-authenticate-request :method-id "gemini-api-key"))
  ;;    :client-maker (lambda ()
  ;;                    (acp-make-client
  ;;                     :command "gemini"
  ;;                     :command-params '("--experimental-acp")
  ;;                     :environment-variables (list
  ;;                                             (format "GEMINI_API_KEY=%s"
  ;;                                                     (agent-shell-google-key)))))))

  (setq agent-shell-prefer-viewport-interaction t)

  (with-eval-after-load 'evil
    ;; Configure *agent-shell-diff* buffers to start in Emacs state
    (defun tl//agent-shell-diff-with-evil-emacs-state ()
      (when (string-match-p "\\*agent-shell-diff\\*" (buffer-name))
        (evil-emacs-state)))

    (add-hook 'diff-mode-hook 'tl//agent-shell-diff-with-evil-emacs-state)

    ;; (add-hook 'agent-shell-viewport-edit-mode-hook #'evil-emacs-state)

    ;; FIXME: bind "j" and "k" with general-define-key or evil-define-key is not work
    ;; (evil-define-key 'normal agent-shell-mode-map
    ;;   "j" #'agent-shell-next-item
    ;;   "k" #'agent-shell-previous-item)

    ;; agent-shell-mode keybindings
    (general-define-key
     :states 'normal
     :keymaps '(agent-shell-mode-map)
     "TAB" #'agent-shell-next-item
     "<backtab>" #'agent-shell-previous-item
     "M-j" #'agent-shell-next-item
     "M-k" #'agent-shell-previous-item

     "m" #'agent-shell-set-session-mode
     "C-c C-v" #'agent-shell-set-session-model
     "C-<tab>" #'agent-shell-cycle-session-mode
     "o" #'agent-shell-other-buffer

     "RET" #'newline
     "C-c C-k" #'agent-shell-interrupt
     "C-c C-c" #'shell-maker-submit
     "?" #'agent-shell-help-menu)

    (general-define-key
     :states '(insert)
     :keymaps '(agent-shell-mode-map)
     "RET" #'newline
     "M-j" #'agent-shell-next-item
     "M-k" #'agent-shell-previous-item
     "C-c C-c" #'shell-maker-submit)

    ;; agent-shell-viewport-view-mode keybindings
    (general-define-key
     :states '(normal)
     :keymaps '(agent-shell-viewport-view-mode-map)
     "TAB" #'agent-shell-viewport-next-item
     "<backtab>" #'agent-shell-viewport-previous-item


     "j" #'agent-shell-viewport-next-item
     "k" #'agent-shell-viewport-previous-item
     "n" #'agent-shell-viewport-next-item
     "p" #'agent-shell-viewport-previous-item

     "f" #'agent-shell-viewport-next-page
     "b" #'agent-shell-viewport-previous-page

     "r" #'agent-shell-viewport-reply
     "y" #'agent-shell-viewport-reply-yes
     "1" #'agent-shell-viewport-reply-1
     "2" #'agent-shell-viewport-reply-2
     "3" #'agent-shell-viewport-reply-3
     "4" #'agent-shell-viewport-reply-4
     "5" #'agent-shell-viewport-reply-5
     "6" #'agent-shell-viewport-reply-6
     "7" #'agent-shell-viewport-reply-7
     "8" #'agent-shell-viewport-reply-8
     "9" #'agent-shell-viewport-reply-9
     "v" nil

     "m" #'agent-shell-viewport-reply-more
     "a" #'agent-shell-viewport-reply-again
     "c" #'agent-shell-viewport-reply-continue

     "gr" #'agent-shell-viewport-refresh

     "C-c C-m" #'agent-shell-viewport-set-session-mode
     "C-c C-o" #'agent-shell-other-buffer
     "C-c C-v" #'agent-shell-viewport-set-session-model
     "C-<tab>" #'agent-shell-viewport-cycle-session-mode
     "C-c C-k" #'agent-shell-viewport-interrupt

     "o" #'agent-shell-other-buffer
     "q" #'bury-buffer
     "?" #'agent-shell-viewport-help-menu)

    (general-define-key
     :states '(normal)
     :keymaps '(agent-shell-viewport-edit-mode-map)
     "M-j" #'agent-shell-viewport-next-history
     "M-k" #'agent-shell-viewport-previous-history
     "M-r" #'agent-shell-viewport-search-history
     "?" #'agent-shell-viewport-compose-help-menu)))

;; https://github.com/jethrokuan/agent-shell-manager
(use-package agent-shell-manager
  :straight (agent-shell-manager :type git :host github :repo "jethrokuan/agent-shell-manager")
  :commands (agent-shell-manager-toggle)
  :config
  ;; Use dedicated window with user-controlled placement
  (setq agent-shell-manager-side nil)
  ;; Options: 'left, 'right, 'top, 'bottom, or nil
  (setq agent-shell-manager-side 'bottom)

  (with-eval-after-load 'evil
    (general-define-key
     :states '(normal)
     :keymaps 'agent-shell-manager-mode-map
     "RET" #'agent-shell-manager-goto
     "gr" #'agent-shell-manager-refresh
     "q"      #'quit-window
     "x"      #'agent-shell-manager-kill
     "c"      #'agent-shell-manager-new
     "r"      #'agent-shell-manager-restart
     "d"      #'agent-shell-manager-delete-killed
     "m"      #'agent-shell-manager-set-mode
     "M"      #'agent-shell-manager-set-model
     "C-c C-c" #'agent-shell-manager-interrupt
     "t"      #'agent-shell-manager-view-traffic
     "l"      #'agent-shell-manager-toggle-logging)))

(use-package agent-shell-knockknock
  :straight (agent-shell-knockknock type git :host github :repo "xenodium/agent-shell-knockknock")
  :after (agent-shell knockknock)
  :hook (agent-shell-mode . agent-shell-knockknock-mode))


(provide '50agent-shell)
;;; 50agent-shell.el ends here
