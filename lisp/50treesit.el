;;; 50tree-sitter.el ---                                -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>
;; Keywords:

;;; Require

;;; Code:

;; (treesit-language-available-p 'rust)

(use-package treesit
  :config
  ;; https://manateelazycat.github.io/2023/09/02/treesit/
  (defun pl//find-parent-node-match (node-types)
    (treesit-parent-until
     (treesit-node-at (point))
     (lambda (parent)
       (member (treesit-node-type parent) node-types))))

  (setq treesit-font-lock-level 4)

  ;; M-x `treesit-install-language-grammar` to install language grammar.
  (setq treesit-language-source-alist
        '((bash . ("https://github.com/tree-sitter/tree-sitter-bash"))
          (c . ("https://github.com/tree-sitter/tree-sitter-c"))
          (cpp . ("https://github.com/tree-sitter/tree-sitter-cpp"))
          (css . ("https://github.com/tree-sitter/tree-sitter-css"))
          (cmake . ("https://github.com/uyha/tree-sitter-cmake"))
          (csharp     . ("https://github.com/tree-sitter/tree-sitter-c-sharp.git"))
          (dockerfile . ("https://github.com/camdencheek/tree-sitter-dockerfile"))
          (elisp . ("https://github.com/Wilfred/tree-sitter-elisp"))
          (go . ("https://github.com/tree-sitter/tree-sitter-go"))
          (gomod      . ("https://github.com/camdencheek/tree-sitter-go-mod.git"))
          (html . ("https://github.com/tree-sitter/tree-sitter-html"))
          (java       . ("https://github.com/tree-sitter/tree-sitter-java.git"))
          (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript"))
          (json . ("https://github.com/tree-sitter/tree-sitter-json"))
          (lua . ("https://github.com/Azganoth/tree-sitter-lua"))
          (make . ("https://github.com/alemuller/tree-sitter-make"))
          (markdown . ("https://github.com/MDeiml/tree-sitter-markdown" nil "tree-sitter-markdown/src"))
          (ocaml . ("https://github.com/tree-sitter/tree-sitter-ocaml" nil "ocaml/src"))
          (org . ("https://github.com/milisims/tree-sitter-org"))
          (python . ("https://github.com/tree-sitter/tree-sitter-python"))
          (php . ("https://github.com/tree-sitter/tree-sitter-php"))
          (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" nil "typescript/src"))
          (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" nil "tsx/src"))
          (ruby . ("https://github.com/tree-sitter/tree-sitter-ruby"))
          (rust . ("https://github.com/tree-sitter/tree-sitter-rust"))
          (sql . ("https://github.com/m-novikov/tree-sitter-sql"))
          (vue . ("https://github.com/merico-dev/tree-sitter-vue"))
          (yaml . ("https://github.com/ikatyang/tree-sitter-yaml"))
          (toml . ("https://github.com/tree-sitter/tree-sitter-toml"))
          (zig . ("https://github.com/GrayJack/tree-sitter-zig"))))

  (setq major-mode-remap-alist
        '((c-mode          . c-ts-mode)
          (c++-mode        . c++-ts-mode)
          ;; rust-ts-mode is build-in mode
          ;; (rust-mode       . rust-ts-mode)
          (cmake-mode      . cmake-ts-mode)
          (conf-toml-mode  . toml-ts-mode)
          (css-mode        . css-ts-mode)
          (js-mode         . js-ts-mode)
          (js-json-mode    . json-ts-mode)
          (python-mode     . python-ts-mode)
          (sh-mode         . bash-ts-mode)
          (typescript-mode . typescript-ts-mode))))


;; FIXME: tree-sitter--after-change is buggy when replace-match
;; tsc-dyn-dir: The directory where ‘tsc-dyn’ module is resided
;; https://emacs-tree-sitter.github.io/
;; https://www.masteringemacs.org/article/tree-sitter-complications-of-parsing-languages
;; https://github.com/emacs-tree-sitter/elisp-tree-sitter/blob/master/core/Cargo.toml
;; (use-package tree-sitter
;;   :hook ((c-mode c++-mode css-mode html-mode js2-mode son-mode rust-mode) . tree-sitter-mode)
;;   :diminish tree-sitter-mode
;;   :config
;;   (require 'tree-sitter-langs)
;;   (add-to-list 'tree-sitter-major-mode-language-alist '(mhtml-mode . html)))

;; (use-package tree-sitter-hl
;;   :hook ((css-mode python-mode rust-mode) . tree-sitter-hl-mode)
;;   :after tree-sitter
;;   :config
;;   ;; The unary logical negation operator ! in rust.
;;   (tree-sitter-hl-add-patterns 'rust
;;     ["!" @operator]))


;; https://github.com/manateelazycat/fingertip
;; M-x `treesit-install-language-grammar' to update per language. check out ~/.emacs.d/tree-sitter
(use-package treesit-auto
  :config
  ;; NOTE: when global-treesit-auto-mode enabled, rust-mode will be rempap to the build-in rust-ts-mode.
  ;; (global-treesit-auto-mode)
  (setq treesit-auto-install 'prompt))


;; (setq treesit-language-source-alist
;;       '((bash "https://github.com/tree-sitter/tree-sitter-bash")
;;         (cmake "https://github.com/uyha/tree-sitter-cmake")
;;         (css "https://github.com/tree-sitter/tree-sitter-css")
;;         (elisp "https://github.com/Wilfred/tree-sitter-elisp")
;;         (go "https://github.com/tree-sitter/tree-sitter-go")
;;         (html "https://github.com/tree-sitter/tree-sitter-html")
;;         (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
;;         (json "https://github.com/tree-sitter/tree-sitter-json")
;;         (make "https://github.com/alemuller/tree-sitter-make")
;;         (markdown "https://github.com/ikatyang/tree-sitter-markdown")
;;         (python "https://github.com/tree-sitter/tree-sitter-python")
;;         (toml "https://github.com/tree-sitter/tree-sitter-toml")
;;         (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
;;         (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
;;         (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

;; (use-package fingertip
;;   :hook ((css-mode python-mode rustc-mode) . fingertip-mode))

;; to support the elisp
;; 1. git clone https://github.com/Wilfred/tree-sitter-elisp
;; 2. gcc ./src/parser.c -fPIC -I./ --shared -o elisp.so
;; 3. cp ./elisp.so ~/.tree-sitter-langs/bin

;; TODO: https://github.com/mickeynp/combobulate

;; (add-hook 'markdown-mode-hook #'(lambda () (treesit-parser-create 'markdown)))

;; (add-hook 'web-mode-hook #'(lambda ()
;;                              (let ((file-name (buffer-file-name)))
;;                                (when file-name
;;                                  (treesit-parser-create
;;                                   (pcase (file-name-extension file-name)
;;                                     ("vue" 'vue)
;;                                     ("html" 'html)
;;                                     ("php" 'php))))
;;                                )))

;; (add-hook 'emacs-lisp-mode-hook #'(lambda () (treesit-parser-create 'elisp)))
;; (add-hook 'ielm-mode-hook #'(lambda () (treesit-parser-create 'elisp)))
;; (add-hook 'json-mode-hook #'(lambda () (treesit-parser-create 'json)))
;; (add-hook 'go-mode-hook #'(lambda () (treesit-parser-create 'go)))
;; (add-hook 'java-mode-hook #'(lambda () (treesit-parser-create 'java)))
;; (add-hook 'java-ts-mode-hook #'(lambda () (treesit-parser-create 'java)))
;; (add-hook 'php-mode-hook #'(lambda () (treesit-parser-create 'php)))
;; (add-hook 'php-ts-mode-hook #'(lambda () (treesit-parser-create 'php)))

(defun tl/create-rust-treesit-parser ()
  (treesit-parser-create 'rust))

(add-hook 'rustic-mode-hook 'tl/create-rust-treesit-parser)
(add-hook 'rust-mode-hook 'tl/create-rust-treesit-parser)


(provide '50treesit)

;; Local Variables:
;; coding: utf-8-unix
;; End:
