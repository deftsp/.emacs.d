;;; 50javascript.el ---                         -*- lexical-binding: t; -*-

;; Copyright (C) 2013  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>

;;; js2-mode
(defun paloryemacs/js2-mode-hook ()
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
    (add-hook 'js2-mode-hook 'paloryemacs/js2-mode-hook))
  :config
  (progn
    ;; prefixes
    (paloryemacs/declare-prefix-for-mode 'js2-mode "mh" "documentation")
    (paloryemacs/declare-prefix-for-mode 'js2-mode "mg" "goto")
    (paloryemacs/declare-prefix-for-mode 'js2-mode "mr" "refactor")
    (paloryemacs/declare-prefix-for-mode 'js2-mode "mz" "folding")
    ;; key bindings
    (paloryemacs/set-leader-keys-for-major-mode 'js2-mode
      "w" 'js2-mode-toggle-warnings-and-errors
      "zc" 'js2-mode-hide-element
      "zo" 'js2-mode-show-element
      "zr" 'js2-mode-show-all
      "ze" 'js2-mode-toggle-element
      "zF" 'js2-mode-toggle-hide-functions
      "zC" 'js2-mode-toggle-hide-comments)))


;;; js-refactor
(defun paloryemacs/js2-refactor-require ()
  "Lazy load js2-refactor"
  (require 'js2-refactor))


(use-package js2-refactor
  :defer t
  :init
  (progn
    (add-hook 'js2-mode-hook 'paloryemacs/js2-refactor-require)
    ;; prefixes
    (paloryemacs/declare-prefix-for-mode 'js2-mode "mr3" "ternary")
    (paloryemacs/declare-prefix-for-mode 'js2-mode "mra" "add/args")
    (paloryemacs/declare-prefix-for-mode 'js2-mode "mrb" "barf")
    (paloryemacs/declare-prefix-for-mode 'js2-mode "mrc" "contract")
    (paloryemacs/declare-prefix-for-mode 'js2-mode "mre" "expand/extract")
    (paloryemacs/declare-prefix-for-mode 'js2-mode "mri" "inline/inject/introduct")
    (paloryemacs/declare-prefix-for-mode 'js2-mode "mrl" "localize/log")
    (paloryemacs/declare-prefix-for-mode 'js2-mode "mrr" "rename")
    (paloryemacs/declare-prefix-for-mode 'js2-mode "mrs" "split/slurp")
    (paloryemacs/declare-prefix-for-mode 'js2-mode "mrt" "toggle")
    (paloryemacs/declare-prefix-for-mode 'js2-mode "mru" "unwrap")
    (paloryemacs/declare-prefix-for-mode 'js2-mode "mrv" "var")
    (paloryemacs/declare-prefix-for-mode 'js2-mode "mrw" "wrap")
    (paloryemacs/declare-prefix-for-mode 'js2-mode "mx" "text")
    (paloryemacs/declare-prefix-for-mode 'js2-mode "mxm" "move")
    ;; key bindings
    (paloryemacs/set-leader-keys-for-major-mode 'js2-mode
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

(provide '50javascript)
;;; 50javascript.el ends here
