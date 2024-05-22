;;; 50lsp-bridge.el ---                                -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>

;; gpip install --upgrade epc orjson sexpdata six setuptools paramiko rapidfuzz

;; https://github.com/tjtrabue/dotfiles/blob/develop/link/emacs/plugin-notebook/my-lsp.org
(defun tl/define-hydra-lsp-bridge-peek ()
  "Define the hydgra for `lsp-bridge-peek'."
  (interactive "P")
  (defhydra hydra-lsp-bridge-peek (:color pink :hint nil)
    "
^Primary^      ^Movement^            ^Actions^              ^Tree^
^^^^^^^^------------------------------------------------------------------------
_P_:   peek   _j_: next line        _l_:        jump        _u_: next branch
_q_:   abort  _k_: prev line        _<return>_: jump        _i_: prev branch
_C-g_: abort  _J_: next file line   _h_:        jump back   _o_: next node
^ ^           _K_: prev file line   _t_:        through     _p_: prev node
"
    ("P"        lsp-bridge-peek)
    ("C-g"      lsp-bridge-peek-abort :exit t)
    ("j"        lsp-bridge-peek-list-next-line)
    ("k"        lsp-bridge-peek-list-prev-line)
    ("J"        lsp-bridge-peek-file-content-next-line)
    ("K"        lsp-bridge-peek-file-content-prev-line)
    ("l"        lsp-bridge-peek-jump)
    ("<return>" lsp-bridge-peek-jump)
    ("h"        lsp-bridge-peek-jump-back)
    ("t"        lsp-bridge-peek-through)
    ("u"        lsp-bridge-peek-tree-next-branch)
    ("i"        lsp-bridge-peek-tree-previous-branch)
    ("o"        lsp-bridge-peek-tree-next-node)
    ("p"        lsp-bridge-peek-tree-previous-node)
    ("q"        lsp-bridge-peek-abort :exit t)))

(use-package lsp-bridge
  :if (eq dottl-lsp-client 'lsp-bridge)
  :init
  (setq acm-enable-quick-access t
        ;; lsp-bridge-enable-log t
        lsp-bridge-enable-inlay-hint nil
        lsp-bridge-enable-diagnostics t
        lsp-bridge-enable-completion-in-minibuffer nil
        lsp-bridge-signature-show-function 'lsp-bridge-signature-show-with-frame
        lsp-bridge-enable-with-tramp t
        lsp-bridge-enable-org-babel t
        acm-backend-yas-match-by-trigger-keyword t
        acm-enable-tabnine nil
        acm-enable-codeium nil)

  :general-config
  (general-def lsp-bridge-mode-map
    ;; "M-." 'lsp-bridge-find-def
    ;; "M-," 'lsp-bridge-find-def-return
    "M-]" 'lsp-bridge-diagnostic-jump-next
    "M-[" 'lsp-bridge-diagnostic-jump-prev
    "M-r" 'lsp-bridge-find-references
    "M-R" 'lsp-bridge-diagnostic-list
    "M-v" 'acm-select-next-page)
  (general-def acm-mode-map
    "C-j" 'acm-select-next
    "C-k" 'acm-select-prev
    "C-l" 'acm-complete)
  (general-def 'normal lsp-bridge-mode-map
    ;; "ga" 'xref-find-apropos
    ;; "gd" 'lsp-bridge-find-def
    ;; "gD" 'lsp-bridge-find-def-other-window
    "gd" 'smart-jump-go
    "K"  'lsp-bridge-lookup-documentation
    "gj" 'lsp-bridge-diagnostic-jump-next
    "gk" 'lsp-bridge-diagnostic-jump-prev
    "gl" 'lsp-bridge-diagnostic-list
    ;; "gi" 'lsp-bridge-find-impl
    ;; "gI" 'lsp-bridge-find-impl-other-window
    "gr" 'lsp-bridge-find-references
    "gR" 'lsp-bridge-rename)
  (general-def lsp-bridge-peek-keymap
    "C-j" 'lsp-bridge-peek-list-next-line
    "C-k" 'lsp-bridge-peek-list-prev-line)
  (general-def 'normal lsp-bridge-ref-mode-map
    ;; Evil keybindings for interacting with the references buffer:
    "<return>" 'lsp-bridge-ref-open-file-and-stay
    "SPC"      'lsp-bridge-ref-open-file
    "D"        'lsp-bridge-ref-remove-line-from-results
    "F"        'lsp-bridge-ref-filter-mismatch-results
    "e"        'lsp-bridge-ref-switch-to-edit-mode
    "f"        'lsp-bridge-ref-filter-match-results
    "h"        'lsp-bridge-ref-jump-prev-file
    "i"        'lsp-bridge-ref-insert-current-line
    "j"        'lsp-bridge-ref-jump-next-keyword
    "k"        'lsp-bridge-ref-jump-prev-keyword
    "l"        'lsp-bridge-ref-jump-next-file
    "q"        'lsp-bridge-ref-quit
    "r"        'lsp-bridge-ref-replace-all-matches
    "u"        'lsp-bridge-ref-unfilter
    "x"        'lsp-bridge-ref-filter-match-files)
  :config
  (add-to-list 'lsp-bridge-multi-lang-server-extension-list '(("html") . "html_tailwindcss"))
  (add-to-list 'lsp-bridge-multi-lang-server-extension-list '(("css") . "css_tailwindcss"))

  ;; (evil-define-key 'motion 'lsp-bridge-mode (kbd "C-]") 'lsp-bridge-find-define)
  ;; (evil-define-key 'motion 'lsp-bridge-mode (kbd "g d") 'lsp-bridge-find-define)
  ;; (evil-define-key 'normal 'lsp-bridge-mode (kbd "C-t") 'lsp-bridge-return-from-def)
  ;; (evil-define-key 'normal 'lsp-bridge-mode (kbd "g r") 'lsp-bridge-find-references)
  ;; (evil-define-key 'normal 'lsp-bridge-mode (kbd "K") 'lsp-bridge-lookup-documentation)

  ;; (evil-define-key 'insert 'corfu-mode (kbd "C-n") 'corfu-next)
  ;; (evil-define-key 'insert 'corfu-mode (kbd "C-p") 'corfu-previous)

  ;; lsp-bridge 发现 lsp-bridge-get-project-path-by-filepath 返回 nil 的时候就会继续查找 git 命令的查询结果。
  (setq lsp-bridge-get-project-path-by-filepath 'tl/lsp-bridge-get-project-path-by-filepath)

  (load "~/.emacs.d/lsp-bridge-known-projects.el")
  (defun tl/lsp-bridge-get-project-path-by-filepath (filepath)
    (--first (string-prefix-p it filepath) tl/lsp-bridge-known-projects))

  (tl/set-leader-keys-for-mode 'lsp-bridge-mode
    ;; code actions
    "aa" #'lsp-bridge-code-action
    ;; ;; goto
    ;; ;; N.B. implementation and references covered by xref bindings / lsp provider...
    ;; "gt" #'lsp-find-type-definition
    ;; "gk" #'tl/lsp-avy-goto-word
    ;; "gK" #'tl/lsp-avy-goto-symbol
    ;; "gM" #'lsp-ui-imenu
    ;; ;; help
    ;; "hh" #'lsp-describe-thing-at-point
    ;; "hd" #'lsp-ui-doc-show
    ;; ;; jump
    ;; "j." #'smart-jump-go
    ;; "j," #'smart-jump-back
    ;; "j?" #'smart-jump-references
    ;; ;; "jr" #'smart-jump-references
    "jr" #'lsp-bridge-find-references
    "js" #'lsp-bridge-workspace-list-symbols
    ;; ;; backend
    ;; "bd" #'lsp-describe-session
    "br" #'lsp-bridge-restart-process
    ;; "bs" #'lsp-workspace-shutdown
    ;; ;; refactor
    "rr" #'lsp-bridge-rename
    ;; ;; toggles
    ;; "Td" #'lsp-ui-doc-mode
    ;; "Ts" #'lsp-ui-sideline-mode
    ;; ;; "TF" #'spacemacs/lsp-ui-doc-func
    ;; ;; "TS" #'spacemacs/lsp-ui-sideline-symb
    ;; ;; "TI" #'spacemacs/lsp-ui-sideline-ignore-duplicate
    ;; "Tl" #'lsp-lens-mode
    ;; ;; folders
    ;; "Fs" #'lsp-workspace-folders-switch
    ;; "Fr" #'lsp-workspace-folders-remove
    ;; "Fa" #'lsp-workspace-folders-add
    ;; ;; text/code
    ;; "xh" #'lsp-document-highlight
    ;; "xl" #'lsp-lens-show
    ;; "xL" #'lsp-lens-hide
    )

  (tl/define-hydra-lsp-bridge-peek)
  (global-lsp-bridge-mode))



(provide '50lsp-bridge)
