;;; 50ai-code.el ---                                    -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>
;; Keywords:

;;; Code:

(use-package ai-code
  :commands (ai-code-menu
             ai-code-cli-start ai-code-cli-resume
             ai-code-cli-switch-to-buffer ai-code-send-command
             ai-code-code-change ai-code-ask-question)
  :bind ("C-c ," . #'ai-code-menu)
  :init
  (setq ai-code-backends-infra-terminal-backend 'ghostel)
  :config
  (ai-code-set-backend 'opencode)

  ;; Enable @ file completion in comments and AI sessions
  (ai-code-prompt-filepath-completion-mode +1)

  (setq ai-code-menu-layout 'default)

  (setq ai-code-notifications-enabled t)
  (setq ai-code-backends-infra-use-side-window nil)
  (setq ai-code-backends-infra-window-side 'right)
  (setq ai-code-backends-infra-window-width 90)
  ;; (setq ai-code-backends-infra-idle-delay 2.0)

  ;; Ask AI to run test after code changes, for a tighter build-test loop
  (setq ai-code-auto-test-type 'ask-me)

  ;; In AI session buffers, SPC in Evil normal state triggers the prompt-enter UI
  ;; (with-eval-after-load 'evil
  ;;   (ai-code-backends-infra-evil-setup))

  ;; Optional: Set up Magit integration for AI commands in Magit popups
  (with-eval-after-load 'magit
    (ai-code-magit-setup-transients)))

(provide '50ai-code)
;;; 50ai-code.el ends here
