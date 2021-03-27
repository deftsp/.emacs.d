;;; 50lsp-mode.el ---                                -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>

(use-package lsp-mode
  :commands (lsp lsp-deferred)
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
        lsp-file-watch-threshold 5000
        lsp-restart 'auto-restart
        lsp-enable-links nil            ; use ffip instead
        ;; lsp-eldoc-render-all t
        lsp-eldoc-enable-hover nil
        lsp-headerline-breadcrumb-enable t
        lsp-headerline-breadcrumb-icons-enable nil
        lsp-modeline-code-actions-enable nil
        lsp-completion-show-kind nil
        lsp-enable-semantic-highlighting nil)

  (setq lsp-prefer-capf t)
  ;; This variable determines how often lsp-mode will refresh the highlights, lenses, links, etc while you type.
  (setq lsp-idle-delay 0.500)
  :hook ((lsp-mode . lsp-enable-which-key-integration)
         ;; (python-mode . lsp-deferred)
         ;; (rust-mode . lsp)
         )
  :config
  ;; don't scan 3rd party javascript libraries
  ;; (push "[/\\\\][^/\\\\]*\\.\\(json\\|html\\|jade\\)$" lsp-file-watch-ignored) ; json
  (push "[/\\\\]pgdata" lsp-file-watch-ignored)

  (use-package lsp-rust
    :init
    (setq lsp-rust-server 'rust-analyzer
          lsp-rust-analyzer-server-display-inlay-hints t
          lsp-rust-analyzer-proc-macro-enable t
          lsp-rust-analyzer-inlay-hints-mode t
          lsp-rust-analyzer-display-chaining-hints t
          lsp-rust-analyzer-display-parameter-hints t
          lsp-rust-analyzer-cargo-run-build-scripts t
          lsp-rust-analyzer-cargo-watch-enable t
          lsp-rust-analyzer-cargo-watch-command "clippy"
          lsp-rust-analyzer-server-command '("~/.cargo/bin/rust-analyzer")))

  (tl/set-leader-keys-for-mode 'lsp-mode
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
    "j." #'smart-jump-go
    "j," #'smart-jump-back
    "j?" #'smart-jump-references
    "jr" #'smart-jump-references
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
  :after lsp-mode
  :commands lsp-ui-mode
  :init
  (setq lsp-ui-peek-always-show t
        lsp-ui-sideline-delay 0.2
        lsp-ui-sideline-enable nil      ; annoying when window width is small
        lsp-ui-sideline-show-diagnostics t
        lsp-ui-sideline-show-hover t
        lsp-ui-sideline-show-code-actions t
        lsp-ui-flycheck-list-position 'right
        lsp-ui-imenu-window-width 32
        lsp-ui-imenu-auto-refresh 'after-save
        lsp-ui-doc-position 'bottom
        lsp-ui-doc-use-childframe t
        ;; https://github.com/emacs-lsp/lsp-ui/issues/310
        lsp-ui-doc-border "#43586d" ; FIXME: native not work, should swith to emacs-mac-port?
        lsp-ui-doc-enable t)
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


;; Integration with the debug server
(use-package dap-mode
  :defer t
  :after lsp-mode
  :hook (dap-mode . dap-tooltip-mode)
  :config
  (require 'dap-lldb)
  (require 'dap-gdb-lldb)
  (dap-gdb-lldb-setup)                  ; installs .extension/vscode if needed
  (dap-auto-configure-mode)

  (dap-register-debug-template "Rust::LLDB Run Configuration"
                               (list :type "lldb"
                                     :request "launch"
                                     :name "LLDB::Run"
                                     :gdbpath "rust-lldb"
                                     :target nil
                                     :cwd nil))
  (load "~/.emacs.d/private-dap-debug-template.el")

  (general-define-key
   :states 'normal
   :keymaps 'rustic-mode-map
   :prefix ","
   "d" '(:ignore t :which-key "debug")
   ;; repl
   "d'"  #'dap-ui-repl
   ;; abandon
   "da"  #'dap-disconnect
   "dA"  #'dap-delete-all-sessions
   ;; breakpoints
   "dbb" #'dap-breakpoint-toggle
   "dbc" #'dap-breakpoint-condition
   "dbl" #'dap-breakpoint-log-message
   "dbh" #'dap-breakpoint-hit-condition
   "dba" #'dap-breakpoint-add
   "dbd" #'dap-breakpoint-delete
   "dbD" #'dap-breakpoint-delete-all
   ;; debuging/running
   "ddd" #'dap-debug
   "dde" #'dap-debug-edit-template
   "ddl" #'dap-debug-last
   "ddr" #'dap-debug-recent
   ;; eval
   "dee" #'dap-eval
   "der" #'dap-eval-region
   "det" #'dap-eval-thing-at-point
   "det" #'dap-ui-expressions-add
   ;; inspect
   "dIi" #'dap-ui-inspect
   "dIr" #'dap-ui-inspect-region
   "dIt" #'dap-ui-inspect-thing-at-point
   ;; stepping
   "dc"  #'dap-continue
   "di"  #'dap-step-in
   "do"  #'dap-step-out
   "dr"  #'dap-restart-frame
   "ds"  #'dap-next
   "dv"  #'dap-ui-inspect-thing-at-point
   ;; switching
   "dSs" #'dap-switch-session
   "dSt" #'dap-switch-thread
   "dSf" #'dap-switch-frame
   ;; toggles
   "dTm" 'spacemacs/toggle-dap-mouse
   ;; windows
   "dwo" #'dap-go-to-output-buffer
   "dwl" #'dap-ui-locals
   "dws" #'dap-ui-sessions
   "dwb" #'dap-ui-breakpoints)

  (dap-mode +1))

(use-package dap-ui
  :after dap-mode
  :hook (dap-mode . dap-ui-mode)
  :hook (dap-ui-mode . dap-ui-controls-mode))

(provide '50lsp-mode)
