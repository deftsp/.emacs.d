;;; 50ai-code.el ---                                    -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>
;; Keywords:

;;; Code:

(use-package ai-code
  :commands (ai-code-menu)
  :init
  (bind-key "C-c ," #'ai-code-menu)
  :config
  ;; use codex as backend, other options are 'claude-code, 'gemini, 'github-copilot-cli, 'opencode, 'grok, 'cursor, 'kiro, 'codebuddy, 'aider, 'agent-shell, 'claude-code-ide, 'claude-code-el
  (ai-code-set-backend 'opencode)

  ;; Enable @ file completion in comments and AI sessions
  (ai-code-prompt-filepath-completion-mode 1)

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
