;;; 50rustic.el ---                               -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>
;; Keywords: languages

;;; Code:

(use-package rustic
  :init
  (setq rustic-lsp-server 'rust-analyzer)
  (setq rustic-cargo-check-arguments "--benches --tests")
  (setq rustic-treesitter-derive t)

  ;; lsp-mode use `rustic-indent-offset', but it is defined in rustic-rust-mode and for emacs 29 it will not be require
  (defvaralias 'rustic-indent-offset 'rust-indent-offset)

  (let ((client
         (pcase dottl-lsp-client
           ('lsp-mode  'lsp-mode)
           ('lsp-bridge nil)
           (_ nil))))
    (setq rustic-lsp-client client))

  (setq rustic-ansi-faces
        ["black"
         "#bb0000"
         "green3"
         "yellow3"
         "#26bbf8"
         "magenta3"
         "cyan3"
         "white"])
  (setq rustic-indent-method-chain t
        rustic-cargo-run-use-comint t
        rustic-format-trigger nil)
  :config
  (define-key rustic-compilation-mode-map "q" 'tl/quit-rustic-compilation-window)
  (define-key rustic-cargo-outdated-mode-map"q" 'tl/quit-rustic-compilation-window)
  ;; (define-key rustic-cargo-test-mode-map "q" nil)
  ;; (define-key rustic-cargo-clippy-mode-map "q" nil)

  (defun tl/rustic-mode-init ()
    ;; (prettify-symbols-mode +1)
    ;; (smartparens-strict-mode +1)
    (when (fboundp 'pangu-spacing-mode)
      (pangu-spacing-mode -1))
    ;; (when (fboundp 'org-link-minor-mode)
    ;;   (org-link-minor-mode +1))
    (rainbow-delimiters-mode -1))

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
   "ct" 'rustic-cargo-current-test
   "c." 'pl/rustic-cargo-current-test-nocapture

   "h" '(:ignore t :which-key "help")

   "t" '(:ignore t :which-key "toggle")
   "th" 'lsp-inlay-hints-mode
   "tr" 'lsp-rust-analyzer-related-tests

   "p"  'tl/rustic-format-buffer
   "v"  '(:ignore t :which-key "variable")
   "vm" 'tl/toggle-mut)

  ;; https://github.com/bbatsov/projectile/issues/234
  ;; see also: locate-dominating-stop-dir-regexp
  (defun rustic-buffer-crate (&optional nodefault)
    "Return the crate for the current buffer.
When called outside a Rust project, then return `default-directory',
or if NODEFAULT is non-nil, then fall back to returning nil."
    (let ((dir (unless (file-remote-p default-directory)
                 (locate-dominating-file default-directory "Cargo.toml"))))
      (when dir
        (setq dir (expand-file-name dir)))
      (or dir
          (and (not nodefault)
               default-directory))))

  ;; Bug Fix: if rename the directory using dired. it may lead to "Error in rustic-flycheck-setup: (file-missing
  ;; "Setting current directory" "No such file or directory" "/Users/test/foo/")"
  (defun tl/rustic-flycheck-setup ()
    "Setup Rust in Flycheck.

If the current file is part of a Cargo project, configure
Flycheck according to the Cargo project layout."
    (interactive)
    ;; We should avoid raising any error in this function, as in combination
    ;; with `global-flycheck-mode' it will render Emacs unusable (see
    ;; https://github.com/flycheck/flycheck-rust/issues/40#issuecomment-253760883).
    (with-demoted-errors "Error in rustic-flycheck-setup: %S"
      (-when-let* ((file-name (buffer-file-name))
                   (is-file-exists (file-exists-p file-name))
                   (target (rustic-flycheck-find-cargo-target file-name)))
        (let-alist target
          (setq-local flycheck-rust-features .required-features)
          (setq-local flycheck-rust-crate-type .kind)
          (setq-local flycheck-rust-binary-name .name)))))

  ;; (advice-remove 'rustic-flycheck-setup #'tl/rustic-flycheck-setup)
  (advice-add 'rustic-flycheck-setup :override #'tl/rustic-flycheck-setup))

(use-package rustic-cargo
  :after rustic
  :config
  (defun pl/rustic-cargo-current-test-nocapture ()
    "Run 'cargo test' for the test near point."
    (interactive)
    (rustic-compilation-process-live)
    (-if-let (test-to-run (rustic-cargo--get-test-target))
        (let* ((command (list rustic-cargo-bin "test" "--" "--nocapture" test-to-run))
               (c (append command (split-string rustic-test-arguments)))
               (buf rustic-test-buffer-name)
               (proc rustic-test-process-name)
               (mode 'rustic-cargo-test-mode))
          (rustic-compilation c (list :buffer buf :process proc :mode mode)))
      (message "Could not find test at point.")))

  (defun tl/cargo-process-quit ()
    (interactive)
    (if (and (= 1 (length (window-list)))
             (assoc ?_ register-alist))
        (jump-to-register ?_)
      (quit-window))))


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

(defun tl/rustic-format-buffer ()
  "Update diff-hl after format.

It's a dirty hack, `rustic-format-start-process' do not giving a finish hook"
  (interactive)
  (rustic-format-buffer)
  (with-current-buffer (current-buffer)
    (run-with-timer 2 nil #'diff-hl-update)))


(provide '50rustic)
;;; 50rustic.el ends here
