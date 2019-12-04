;;; 50javascript.el ---                         -*- lexical-binding: t; -*-

;; Copyright (C) 2013  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>

;;; js2-mode
(defun tl/js2-mode-hook ()
  (when (fboundp 'moz-minor-mode)
    (define-key js2-mode-map (kbd "C-c C-z") 'run-mozilla)
    (moz-minor-mode +1)))

;; (autoload 'js2-mode "js2-mode" nil t)
;; (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

(use-package js2-mode
  :defer t
  :init
  (progn
    (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
    (setq-default js-indent-level 2)

    ;; Required to make imenu functions work correctly
    (add-hook 'js2-mode-hook 'js2-imenu-extras-mode)
    (add-hook 'js2-mode-hook 'tl/js2-mode-hook))
  :config
  (progn
    ;; prefixes
    (tl/declare-prefix-for-mode 'js2-mode "mh" "documentation")
    (tl/declare-prefix-for-mode 'js2-mode "mg" "goto")
    (tl/declare-prefix-for-mode 'js2-mode "mr" "refactor")
    (tl/declare-prefix-for-mode 'js2-mode "mz" "folding")
    ;; key bindings
    (tl/set-leader-keys-for-major-mode 'js2-mode
      "w" 'js2-mode-toggle-warnings-and-errors
      "zc" 'js2-mode-hide-element
      "zo" 'js2-mode-show-element
      "zr" 'js2-mode-show-all
      "ze" 'js2-mode-toggle-element
      "zF" 'js2-mode-toggle-hide-functions
      "zC" 'js2-mode-toggle-hide-comments)))


;;; js-refactor
(defun tl/js2-refactor-require ()
  "Lazy load js2-refactor"
  (require 'js2-refactor))


(use-package js2-refactor
  :defer t
  :init
  (progn
    (add-hook 'js2-mode-hook 'tl/js2-refactor-require)
    ;; prefixes
    (tl/declare-prefix-for-mode 'js2-mode "mr3" "ternary")
    (tl/declare-prefix-for-mode 'js2-mode "mra" "add/args")
    (tl/declare-prefix-for-mode 'js2-mode "mrb" "barf")
    (tl/declare-prefix-for-mode 'js2-mode "mrc" "contract")
    (tl/declare-prefix-for-mode 'js2-mode "mre" "expand/extract")
    (tl/declare-prefix-for-mode 'js2-mode "mri" "inline/inject/introduct")
    (tl/declare-prefix-for-mode 'js2-mode "mrl" "localize/log")
    (tl/declare-prefix-for-mode 'js2-mode "mrr" "rename")
    (tl/declare-prefix-for-mode 'js2-mode "mrs" "split/slurp")
    (tl/declare-prefix-for-mode 'js2-mode "mrt" "toggle")
    (tl/declare-prefix-for-mode 'js2-mode "mru" "unwrap")
    (tl/declare-prefix-for-mode 'js2-mode "mrv" "var")
    (tl/declare-prefix-for-mode 'js2-mode "mrw" "wrap")
    (tl/declare-prefix-for-mode 'js2-mode "mx" "text")
    (tl/declare-prefix-for-mode 'js2-mode "mxm" "move")
    ;; key bindings
    (tl/set-leader-keys-for-major-mode 'js2-mode
      "r3i" 'js2r-ternary-to-if
      "rag" 'js2r-add-to-globals-annotation
      "rao" 'js2r-arguments-to-object
      "rba" 'js2r-forward-barf
      "rca" 'js2r-contract-array
      "rco" 'js2r-contract-object
      "rcu" 'js2r-contract-function
      "rea" 'js2r-expand-array
      "ref" 'js2r-extract-function
      "rem" 'js2r-extract-method
      "reo" 'js2r-expand-object
      "reu" 'js2r-expand-function
      "rev" 'js2r-extract-var
      "rig" 'js2r-inject-global-in-iife
      "rip" 'js2r-introduce-parameter
      "riv" 'js2r-inline-var
      "rlp" 'js2r-localize-parameter
      "rlt" 'js2r-log-this
      "rrv" 'js2r-rename-var
      "rsl" 'js2r-forward-slurp
      "rss" 'js2r-split-string
      "rsv" 'js2r-split-var-declaration
      "rtf" 'js2r-toggle-function-expression-and-declaration
      "ruw" 'js2r-unwrap
      "rvt" 'js2r-var-to-this
      "rwi" 'js2r-wrap-buffer-in-iife
      "rwl" 'js2r-wrap-in-for-loop
      "k" 'js2r-kill
      "xmj" 'js2r-move-line-down
      "xmk" 'js2r-move-line-up)))

(use-package eslintd-fix
  :after web-mode)

(use-package flycheck-flow
  :after web-mode
  :config
  (with-eval-after-load 'flycheck
    ;; flycheck-flow have already add
    ;; (flycheck-add-next-checker 'javascript-eslint 'javascript-flow)
    ;; (flycheck-add-next-checker 'javascript-flow 'javascript-flow-coverage)
    ))

(use-package flow-minor-mode
  :after web-mode
  :config
  (with-eval-after-load 'flycheck
    (flycheck-add-mode 'javascript-eslint 'flow-minor-mode)
    (flycheck-add-mode 'javascript-flow 'flow-minor-mode)))

;; instead by TabNine
;; (use-package company-flow
;;   :after web-mode
;;   :config
;;   (with-eval-after-load 'company
;;     (add-to-list 'company-backends 'company-flow)))


(provide '50javascript)
;;; 50javascript.el ends here
