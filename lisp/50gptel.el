;;; 50gptel.el ---                                    -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>
;; Keywords:

;;; Code:

(use-package gptel
  :commands (gptel)
  :config
  (setq gptel-default-mode 'org-mode)

  (setq gptel-backend
        (gptel-make-openai "OpenCode-Zen"
          :host "opencode.ai"
          :endpoint "/zen/v1/chat/completions"
          :stream t
          :key #'gptel-api-key-from-auth-source
          ;; https://opencode.ai/zen/v1/models
          :models '(claude-sonnet-4-6
                    gpt-5.3-codex)))

  (setq gptel-log-level 'debug)

  ;; (setq tl//gptel-model 'gpt-5.3-codex)
  ;; (setopt gptel-model (or tl//gptel-model (car (gptel-backend-models gptel-backend))))

  (setq gptel-model 'gpt-5.3-codex))

(provide '50gptel)
;;; 50gptel.el ends here
