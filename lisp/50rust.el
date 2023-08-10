;;; 50rust.el ---                               -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>
;; Keywords: languages

;;; Code:

(use-package rust-ts-mode
  :defer t
  :config
  ;; remove rust-ts-mode from `auto-mode-alist'
  (let ((mode '("\\.rs\\'" . rust-ts-mode)))
    (when (member mode auto-mode-alist)
      (setq auto-mode-alist (remove mode auto-mode-alist)))))

(use-package rust-mode
  :defer t
  :init
  ;; (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))
  ;; (defface tl/rust-exclaim-face
  ;;   '((t :inherit font-lock-keyword-face))
  ;;   "Face for the exclaim reference mark."
  ;;   :group 'rust-mode)
  :config
  ;; the symbol in macro like "write!" will be affected.
  ;; (push '("!" . ?¬) rust-prettify-symbols-alist)
  ;; (push '("\\!" . 'tl/rust-exclaim-face) rust-font-lock-keywords)

  ;; use the rust-mode instead of rust-ts-mode
  (let ((mode '("\\.rs\\'" . rust-ts-mode)))
    (when (member mode auto-mode-alist)
      (setq auto-mqode-alist (remove mode auto-mode-alist))))

  (general-define-key
   :states 'normal
   :keymaps 'rust-mode-map
   :prefix ","
   "b" '(:ignore t :which-key "backend")
   "bI" 'lsp-rust-analyzer-status
   "b." 'lsp-rust-analyzer-reload-workspace

   "h" '(:ignore t :which-key "help")

   "t" '(:ignore t :which-key "toggle")
   "th" 'lsp-inlay-hints-mode
   "tr" 'lsp-rust-analyzer-related-tests

   "p"  'tl/rust-format-buffer-with-diff-hl-update
   "v"  '(:ignore t :which-key "variable")
   "vm" 'tl/toggle-mut))

(use-package rust-cargo
  :after rust-mode
  :config
  ;; (defun pl/rustic-cargo-current-test-nocapture ()
  ;;   "Run 'cargo test' for the test near point."
  ;;   (interactive)
  ;;   (rustic-compilation-process-live)
  ;;   (-if-let (test-to-run (rustic-cargo--get-test-target))
  ;;       (let* ((command (list rustic-cargo-bin "test" "--" "--nocapture" test-to-run))
  ;;              (c (append command (split-string rustic-test-arguments)))
  ;;              (buf rustic-test-buffer-name)
  ;;              (proc rustic-test-process-name)
  ;;              (mode 'rustic-cargo-test-mode))
  ;;         (rustic-compilation c (list :buffer buf :process proc :mode mode)))
  ;;     (message "Could not find test at point.")))

  (general-define-key
   :states 'normal
   :keymaps 'rust-mode-map
   :prefix ","
   "c" '(:ignore t :which-key "cargo")
   ;; "ca" 'tl/cargo-audit
   ;; "cb" 'rustic-cargo-build
   ;; "cB" 'rustic-cargo-bench
   "cc" 'tl/rust-check
   ;; "cC" 'rustic-cargo-clippy
   ;; "cd" 'rustic-cargo-doc
   ;; "cf" 'rustic-cargo-fmt
   ;; "cm" 'tl/maximize-rustic-compilation-window
   ;; "cn" 'rustic-cargo-new
   ;; "co" 'rustic-cargo-outdated
   ;; "cr" 'rustic-cargo-run
   ;; "cT" 'rustic-cargo-test
   ;; "ct" 'rustic-cargo-current-test
   ;; "c." 'pl/rustic-cargo-current-test-nocapture
   )


  (defun tl/cargo-process-quit ()
    (interactive)
    (if (and (= 1 (length (window-list)))
             (assoc ?_ register-alist))
        (jump-to-register ?_)
      (quit-window))))


(defun tl/rust-check ()
  "Compile using `cargo check`"
  (interactive)
  (rust--compile "%s check %s" rust-cargo-bin "--benches --tests"))


(defun tl/toggle-mut ()
  "Toggles the mutability of the variable defined on the current line"
  (interactive)
  (save-excursion
    (back-to-indentation)
    (forward-word)
    (if (string= " mut" (buffer-substring (point) (+ (point) 4)))
        (delete-region (point) (+ (point) 4))
      (insert " mut"))))

(defun tl/rust-format-buffer-with-diff-hl-update ()
  "Update diff-hl after format. "
  (interactive)
  (rust-format-buffer)
  (with-current-buffer (current-buffer)
    (run-with-timer 2 nil #'diff-hl-update)))


(provide '50rust)
;;; 50rust.el ends here
