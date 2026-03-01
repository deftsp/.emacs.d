;;; 50llm.el ---                                    -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>
;; Keywords:

;;; Code:

(use-package agent-shell
  :commands (agent-shell-opencode-start-agent)
  :config
  (setq agent-shell-opencode-authentication
        (agent-shell-opencode-make-authentication :none t))

  ;; disable creating transcript file
  ;; (setq agent-shell-transcript-file-path-function nil)

  ;; "opencode/claude-sonnet-4-6"
  (setq agent-shell-opencode-default-model-id "deepseek/deepseek-reasoner")

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

    ;; agent-shell-viewport-edit-mode: use Emacs state so normal text editing works
    (add-hook 'agent-shell-viewport-edit-mode-hook #'evil-emacs-state)

    ;; agent-shell-mode keybindings
    (evil-define-key 'normal agent-shell-mode-map (kbd "TAB") #'agent-shell-next-item)
    (evil-define-key 'normal agent-shell-mode-map (kbd "j") #'agent-shell-next-item)
    (evil-define-key 'normal agent-shell-mode-map (kbd "k") #'agent-shell-previous-item)
    (evil-define-key 'normal agent-shell-mode-map (kbd "C-c C-c") #'agent-shell-interrupt)
    (evil-define-key 'normal agent-shell-mode-map (kbd "m") #'agent-shell-set-session-mode)
    (evil-define-key 'normal agent-shell-mode-map (kbd "v") #'agent-shell-set-session-model)
    (evil-define-key 'normal agent-shell-mode-map (kbd "C-<tab>") #'agent-shell-cycle-session-mode)
    (evil-define-key 'normal agent-shell-mode-map (kbd "o") #'agent-shell-other-buffer)

    ;; agent-shell-viewport-view-mode keybindings
    (evil-define-key 'normal agent-shell-viewport-view-mode-map (kbd "TAB") #'agent-shell-viewport-next-item)
    (evil-define-key 'normal agent-shell-viewport-view-mode-map (kbd "j") #'agent-shell-viewport-next-item)
    (evil-define-key 'normal agent-shell-viewport-view-mode-map (kbd "k") #'agent-shell-viewport-previous-item)
    (evil-define-key 'normal agent-shell-viewport-view-mode-map (kbd "f") #'agent-shell-viewport-next-page)
    (evil-define-key 'normal agent-shell-viewport-view-mode-map (kbd "b") #'agent-shell-viewport-previous-page)
    (evil-define-key 'normal agent-shell-viewport-view-mode-map (kbd "r") #'agent-shell-viewport-reply)
    (evil-define-key 'normal agent-shell-viewport-view-mode-map (kbd "R") #'agent-shell-viewport-refresh)
    (evil-define-key 'normal agent-shell-viewport-view-mode-map (kbd "m") #'agent-shell-viewport-set-session-mode)
    (evil-define-key 'normal agent-shell-viewport-view-mode-map (kbd "v") #'agent-shell-viewport-set-session-model)
    (evil-define-key 'normal agent-shell-viewport-view-mode-map (kbd "C-<tab>") #'agent-shell-viewport-cycle-session-mode)
    (evil-define-key 'normal agent-shell-viewport-view-mode-map (kbd "o") #'agent-shell-other-buffer)
    (evil-define-key 'normal agent-shell-viewport-view-mode-map (kbd "C-c C-c") #'agent-shell-viewport-interrupt)
    (evil-define-key 'normal agent-shell-viewport-view-mode-map (kbd "q") #'bury-buffer)

    (evil-define-key 'insert agent-shell-mode-map (kbd "RET") #'newline)
    (evil-define-key 'insert agent-shell-mode-map (kbd "S-<return>") #'comint-send-input)

    (evil-define-key 'normal agent-shell-mode-map (kbd "RET") #'newline)
    (evil-define-key 'normal agent-shell-mode-map (kbd "S-<return>") #'comint-send-input)))

;; https://github.com/jethrokuan/agent-shell-manager
(use-package agent-shell-manager
  :commands (agent-shell-manager-toggle)
  :config
  ;; Use dedicated window with user-controlled placement
  (setq agent-shell-manager-side nil)
  ;; Options: 'left, 'right, 'top, 'bottom, or nil
  (setq agent-shell-manager-side 'bottom))

(provide '50llm)
;;; 50llm.el ends here
