;;; 50tree-sitter.el ---                                -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>
;; Keywords:

;; FIXME: tree-sitter--after-change is buggy when replace-match
;; tsc-dyn-dir: The directory where ‘tsc-dyn’ module is resided
;; https://emacs-tree-sitter.github.io/
;; https://www.masteringemacs.org/article/tree-sitter-complications-of-parsing-languages
;; https://github.com/emacs-tree-sitter/elisp-tree-sitter/blob/master/core/Cargo.toml
(use-package tree-sitter
  :hook ((c-mode c++-mode css-mode html-mode js2-mode son-mode rust-mode) . tree-sitter-mode)
  :config
  (require 'tree-sitter-langs)
  (add-to-list 'tree-sitter-major-mode-language-alist '(mhtml-mode . html)))

(use-package tree-sitter-hl
  :hook ((css-mode python-mode rust-mode) . tree-sitter-hl-mode)
  :after tree-sitter
  :config
  ;; The unary logical negation operator ! in rust.
  (tree-sitter-hl-add-patterns 'rust
    ["!" @operator]))

;; https://emacs-china.org/t/tree-sitter/19014
;; (use-package grammatical-edit
;;   :after tree-sitter
;;   :hook ((css-mode python-mode rust-mode) . grammatical-edit-mode))
;;

;; to support the elisp
;; 1. git clone https://github.com/Wilfred/tree-sitter-elisp
;; 2. gcc ./src/parser.c -fPIC -I./ --shared -o elisp.so
;; 3. cp ./elisp.so ~/.tree-sitter-langs/bin

;; TODO: https://github.com/mickeynp/combobulate


(provide '50tree-sitter)

;; Local Variables:
;; coding: utf-8-unix
;; End:
