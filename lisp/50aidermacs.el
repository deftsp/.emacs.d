;;; 50aidermacs.el ---
;; Author: Shihpin Tseng <deftsp@gmail.com>

;; https://github.com/MatthewZMD/aidermacs

(use-package aidermacs
  ;; :bind (("s-x" . aidermacs-transient-menu))
  ;; :bind (("C-c C-a" . aidermacs-transient-menu))
  :custom
  ;; See the Configuration section below
  (aidermacs-default-model "gemini-2.5-pro-exp-03-25")
  ;; (aidermacs-default-model "openrouter/google/gemini-2.5-pro-exp-03-25:free")
  (aidermacs-weak-model "openrouter/deepseek/deepseek-chat")
  (aidermacs-use-architect-mode t)

  :config
  ;; config environment variable by setenv .zprofile is not work, use aidermacs-extra-args
  (add-to-list 'aidermacs-extra-args
               (format
                "--api-key openrouter=%s --api-key gemini=%s"
                tl/openrouter-api-key
                tl/gemini-api-key))
  ;; (setq aidermacs-extra-args nil)
  (setq aidermacs-backend 'vterm)
  (add-hook 'aidermacs-before-run-backend-hook 'tl/aidermacs-before-run-backend-hook))

(defun tl/aidermacs-before-run-backend-hook ()
  ;; (setenv "ANTHROPIC_API_KEY" "sk-...")
  ;; (setenv "DEEPSEEK_API_KEY" "")
  ;; (setenv "OPENAI_API_KEY" (password-store-get "code/openai_api_key"))
  ;; (setenv "GEMINI_API_KEY" tl/gemini-api-key)
  (setenv "OPENROUTER_API_KEY" tl/openrouter-api-key))

(provide '50aidermacs)
