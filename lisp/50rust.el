;;; 50rust.el ---                               -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>
;; Keywords: languages

(use-package rustic
  :mode ("\\.rs$" . rustic-mode)
  :commands (rustic-run-cargo-command rustic-cargo-outdated)
  :init
  (setq rustic-lsp-server 'rust-analyzer)
  (setq rustic-lsp-client 'lsp-mode)
  ;; (setq rustic-lsp-client nil)          ; setting to nil to disable LSP

  (setq rustic-ansi-faces ["black"
                           "#bb0000"
                           "green3"
                           "yellow3"
                           "#26bbf8"
                           "magenta3"
                           "cyan3"
                           "white"])
  (setq rustic-indent-method-chain t
        rustic-format-trigger nil)
  :config
  (define-key rustic-compilation-mode-map "q" 'tl/quit-rustic-compilation-window)
  (define-key rustic-cargo-outdated-mode-map"q" 'tl/quit-rustic-compilation-window)
  ;; (define-key rustic-cargo-test-mode-map "q" nil)
  ;; (define-key rustic-cargo-clippy-mode-map "q" nil)

  (defun tl/rustic-mode-init ()
    (smartparens-strict-mode +1)
    (rainbow-delimiters-mode -1)
    (when (fboundp 'org-link-minor-mode)
      (org-link-minor-mode +1)))

  (add-hook 'rustic-mode-hook 'tl/rustic-mode-init)

  ;; use lsp-mode with rust-analyzer instead of rustic-clippy
  ;; (push 'rustic-clippy flycheck-checkers)

  (general-define-key
   :states 'normal
   :keymaps 'rustic-compilation-mode-map
   "gg" 'evil-goto-first-line
   "gr" 'rustic-recompile
   "G" 'evil-goto-line)

  (general-define-key
   :states 'normal
   :keymaps 'rustic-mode-map
   :prefix ","
   "b" '(:ignore t :which-key "backend")
   "bI" 'lsp-rust-analyzer-status
   "b." 'lsp-rust-analyzer-reload-workspace

   "h" '(:ignore t :which-key "help")

   "t" '(:ignore t :which-key "toggle")
   "th" 'lsp-rust-analyzer-inlay-hints-mode
   "tr" 'lsp-rust-analyzer-related-tests

   "p"  'rustic-format-buffer
   "v"  '(:ignore t :which-key "variable")
   "vm" 'tl/toggle-mut))

(use-package rustic-cargo
  :after rustic
  :config

  (general-define-key
   :states 'normal
   :keymaps 'rustic-mode-map
   :prefix ","
   "c" '(:ignore t :which-key "cargo")
   "ca" 'tl/cargo-audit
   "cb" 'rustic-cargo-build
   "cB" 'rustic-cargo-bench
   "cc" 'rustic-cargo-check
   "cC" 'rustic-cargo-clippy
   "cd" 'rustic-cargo-doc
   "cf" 'rustic-cargo-fmt
   "cm" 'tl/maximize-rustic-compilation-window
   "cn" 'rustic-cargo-new
   "co" 'rustic-cargo-outdated
   "cr" 'rustic-cargo-run
   "cT" 'rustic-cargo-test
   "ct" 'rustic-cargo-current-test)


  (defun tl/cargo-process-quit ()
    (interactive)
    (if (and (= 1 (length (window-list)))
             (assoc ?_ register-alist))
        (jump-to-register ?_)
      (quit-window))))


(defun tl/toggle-mut ()
  "Toggles the mutability of the variable defined on the current line"
  (interactive)
  (save-excursion
    (back-to-indentation)
    (forward-word)
    (if (string= " mut" (buffer-substring (point) (+ (point) 4)))
        (delete-region (point) (+ (point) 4))
      (insert " mut"))))

(defun tl/cargo-audit ()
  "Run 'cargo audit' for the current project."
  (interactive)
  (rustic-run-cargo-command "cargo audit -f"))


(defun tl/quit-rustic-compilation-window ()
  (interactive)
  (let ((win (selected-window)))
    (if (assoc ?? register-alist)
        (progn
          (jump-to-register ??)
          (setq register-alist (assoc-delete-all ?? register-alist))
          (delete-window win))
      (call-interactively 'quit-window))))

(defun tl/maximize-rustic-compilation-window ()
  (interactive)
  (let* ((b-lst (-map (lambda (win) (buffer-name (window-buffer win)) )
                      (-mapcat 'window-list (frame-list))))
         (buf (car (or
                    (-filter (lambda (s) (string= "*rustic-compilation*" s))  b-lst)
                    (-filter (lambda (s) (string-prefix-p "*cargo-" s))
                             b-lst)))))
    (when (and buf
               (> (length (window-list)) 1))

      (window-configuration-to-register ??)
      (select-window (get-buffer-window buf))
      (delete-other-windows))))


(provide '50rust)
;;; 50rust.el ends here
