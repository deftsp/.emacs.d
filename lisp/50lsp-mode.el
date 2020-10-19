;;; 50lsp-mode.el ---                                -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>

(use-package lsp-mode
  :commands lsp
  :init
  (setq lsp-log-io nil
        lsp-enable-folding nil
        lsp-diagnostics-provider :flycheck  ; :none, no real time syntax check
        lsp-enable-symbol-highlighting nil ; turn off for better performance
        lsp-modeline-diagnostics-enable nil
        lsp-enable-snippet nil          ; use yasnippet instead
        ;; use `company-ctags' only.
        ;; N.B. `company-lsp' is automatically enabled if installed
        ;; lsp-enable-completion-at-point nil
        lsp-restart 'auto-restart
        lsp-enable-links nil            ; use ffip instead
        lsp-headerline-breadcrumb-enable t
        lsp-modeline-code-actions-enable nil
        lsp-completion-show-kind nil
        lsp-enable-semantic-highlighting nil)

  (setq lsp-prefer-capf t)
  ;; This variable determines how often lsp-mode will refresh the highlights, lenses, links, etc while you type.
  (setq lsp-idle-delay 0.500)
  :hook (
         ;; (rust-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :config
  (use-package lsp-rust
    :init
    (setq lsp-rust-server 'rust-analyzer
          lsp-rust-analyzer-cargo-watch-command "clippy"
          lsp-rust-analyzer-server-command '("~/.cargo/bin/rust-analyzer")))

  (tl/set-leader-keys-for-minor-mode 'lsp-mode
    ;; format
    "=b" #'lsp-format-buffer
    "=r" #'lsp-format-region
    "=o" #'lsp-organize-imports
    ;; code actions
    "aa" #'lsp-execute-code-action
    ;; goto
    ;; N.B. implementation and references covered by xref bindings / lsp provider...
    "gt" #'lsp-find-type-definition
    "gk" #'tl/lsp-avy-goto-word
    "gK" #'tl/lsp-avy-goto-symbol
    "gM" #'lsp-ui-imenu
    ;; help
    "hh" #'lsp-describe-thing-at-point
    "hd" #'lsp-ui-doc-show
    ;; jump
    ;; backend
    "bd" #'lsp-describe-session
    "br" #'lsp-workspace-restart
    "bs" #'lsp-workspace-shutdown
    ;; refactor
    "rr" #'lsp-rename
    ;; toggles
    "Td" #'lsp-ui-doc-mode
    "Ts" #'lsp-ui-sideline-mode
    ;; "TF" #'spacemacs/lsp-ui-doc-func
    ;; "TS" #'spacemacs/lsp-ui-sideline-symb
    ;; "TI" #'spacemacs/lsp-ui-sideline-ignore-duplicate
    "Tl" #'lsp-lens-mode
    ;; folders
    "Fs" #'lsp-workspace-folders-switch
    "Fr" #'lsp-workspace-folders-remove
    "Fa" #'lsp-workspace-folders-add
    ;; text/code
    "xh" #'lsp-document-highlight
    "xl" #'lsp-lens-show
    "xL" #'lsp-lens-hide))

(use-package lsp-ivy
  :defer t
  :commands (lsp-ivy-workspace-symbol lsp-ivy-global-workspace-symbol))

(use-package lsp-ui
  :defer t
  :commands lsp-ui-mode
  :init
  (setq lsp-ui-sideline-delay 0.2
        lsp-ui-sideline-enable t
        lsp-ui-flycheck-list-position 'right
        lsp-ui-doc-enable nil)
  :config
  (progn
    (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
    (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)

    (defun tl/lsp-define-key (keymap key def &rest bindings)
      "Define multiple key bindings with KEYMAP KEY DEF BINDINGS."
      (interactive)
      (while key
        (define-key keymap (kbd key) def)
        (setq key (pop bindings)
              def (pop bindings))))

    (tl/lsp-define-key
     lsp-ui-peek-mode-map
     "h" #'lsp-ui-peek--select-prev-file
     "j" #'lsp-ui-peek--select-next
     "k" #'lsp-ui-peek--select-prev
     "l" #'lsp-ui-peek--select-next-file)))


;; ivy integration
(defun tl//lsp-avy-document-symbol (all)
  (interactive)
  (let ((line 0) (col 0) (w (selected-window))
        (ccls (and (memq major-mode '(c-mode c++-mode objc-mode)) (eq c-c++-backend 'lsp-ccls)))
        (start-line (1- (line-number-at-pos (window-start))))
        (end-line (1- (line-number-at-pos (window-end))))
        ranges point0 point1
        candidates)
    (save-excursion
      (goto-char 1)
      (cl-loop for loc in
               (lsp--send-request
                (lsp--make-request
                 "textDocument/documentSymbol"
                 `(:textDocument ,(lsp--text-document-identifier)
                   :all ,(if all t :json-false)
                   :startLine ,start-line :endLine ,end-line)))
               for range = (if ccls
                               loc
                             (->> loc (gethash "location") (gethash "range")))
               for range_start = (gethash "start" range)
               for range_end = (gethash "end" range)
               for l0 = (gethash "line" range_start)
               for c0 = (gethash "character" range_start)
               for l1 = (gethash "line" range_end)
               for c1 = (gethash "character" range_end)
               while (<= l0 end-line)
               when (>= l0 start-line)
               do
               (forward-line (- l0 line))
               (forward-char c0)
               (setq point0 (point))
               (forward-line (- l1 l0))
               (forward-char c1)
               (setq point1 (point))
               (setq line l1 col c1)
               (push `((,point0 . ,point1) . ,w) candidates)))
    (avy-with avy-document-symbol
      (avy--process candidates
                    (avy--style-fn avy-style)))))

(defun tl/lsp-avy-goto-word ()
  (interactive)
  (tl//lsp-avy-document-symbol t))

(defun tl/lsp-avy-goto-symbol ()
  (interactive)
  (tl//lsp-avy-document-symbol nil))



(provide '50lsp-mode)
