;;; 50rust.el ---                               -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>
;; Keywords: languages

;;; Code:

(use-package rust-ts-mode
  :defer t
  :config

  ;; https://github.com/ZharMeny/cue-ts-mode/blob/a9874bb50503fd9e72a26aed20351bd84c65d325/cue-ts-mode.el#L87
  ;; https://www.masteringemacs.org/article/lets-write-a-treesitter-major-mode
  ;; https://github.com/nix-community/nix-ts-mode/blob/cea932fd683bfab84234205f5f1d161e29a5adf6/nix-ts-mode.el
  ;; https://github.com/c3lang/c3-ts-mode/blob/15eb262f4bc0e390ae6b78657f2bccb303651426/README.md?plain=1#L90
  ;; https://github.com/mickeynp/combobulate/blob/ca2545be196ec32945b5db109578582d7c7270b5/combobulate-query.el#L1156
  ;; https://github.com/nverno/nvp/blob/ea8793058c2c39ca5321c4a1241e1559c659eeb5/modes/rust/nvp-rust-ts.el#L11

  (add-to-list 'rust-ts-mode--font-lock-settings
               (car (treesit-font-lock-rules
                     :language 'rust
                     :override t
                     ;; Choose a feature name here (see docs for 'treesit-font-lock-rules')
                     :feature 'tl/negation-operator
                     ;; NOTE: my own custom face is not work
                     ;; See "37.5 Pattern Matching Tree-sitter Nodes" in the Emacs manual
                     ;; M-x treesit-explore-mode

                     ;; it works for "&&", "+", ...
                     ;; '((binary_expression operator: _ @font-lock-negation-char-face))
                     ;; Not works. Why?
                     ;; '((unary_expression operator: _ @font-lock-negation-char-face))
                     ;; FIXME: it will include the macro println!
                     '(["!"] @font-lock-negation-char-face)))
               t)

  (defun tl/rust-ts-mode-init ()
    ;; add the feature to feature level 4
    (cl-pushnew 'tl/negation-operator (nth 3 treesit-font-lock-feature-list))
    (treesit-font-lock-recompute-features)
    (font-lock-flush))

  (add-hook 'rust-ts-mode-hook 'tl/rust-ts-mode-init)

  ;; remove rust-ts-mode from `auto-mode-alist'
  (let ((mode '("\\.rs\\'" . rust-ts-mode)))
    (when (member mode auto-mode-alist)
      (setq auto-mode-alist (remove mode auto-mode-alist)))))

(use-package rust-mode
  :defer t
  :init
  (setq rust-rustfmt-switches '("--edition" "2021"))
  (setq rust-mode-treesitter-derive t)
  ;; (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))
  ;; (defface tl/rust-exclaim-face
  ;;   '((t :inherit font-lock-keyword-face))
  ;;   "Face for the exclaim reference mark."
  ;;   :group 'rust-mode)
  :config
  ;; the symbol in macro like "write!" will be affected.
  ;; (push '("!" . ?¬) rust-prettify-symbols-alist)
  ;; (push '("\\!" . 'tl/rust-exclaim-face) rust-font-lock-keywords)

  (general-define-key
   :states 'normal
   :keymaps 'rust-mode-map
   :prefix ","
   "h" '(:ignore t :which-key "help")

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
