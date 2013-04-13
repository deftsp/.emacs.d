;;; 50go.el ---

;; Copyright (C) 2013  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>
;; Keywords:

;; "C-c C-a" go-import-add
;; "C-c C-j" godef-jump
;; "C-c C-d" godef-describe

(add-to-list 'load-path "~/.emacs.d/lisp/go-errcheck.el")
(add-to-list 'load-path "~/opt/go-packages/src/github.com/nsf/gocode/emacs")
(add-to-list 'load-path "~/opt/go-packages/src/github.com/dougm/goflymake")

(when (eq system-type 'darwin)
 (setenv "GOPATH" (concat (expand-file-name "~/opt/go-packages") ":"
                          (expand-file-name "~/Lab/gocode"))))

(defun pl/go-mode-hook-func ()
  (setq imenu-generic-expression '(("type" "^type *\\([^ \t\n\r\f]*\\)" 1)
                                   ("func" "^func *\\(.*\\) {" 1)))
  (imenu-add-to-menubar "Index")
  ;; (setq show-trailing-whitespace t)
  (local-set-key (kbd "C-c C-r") 'go-remove-unused-imports)
  (local-set-key (kbd "C-c I") 'go-goto-imports))

(eval-after-load "go-mode"
  '(progn
     (require 'go-flymake) ; use go-errcheck instead
     ;; (require 'go-errcheck nil t)
     (require 'go-autocomplete nil t)
     (add-hook 'go-mode-hook 'pl/go-mode-hook-func)
     (add-hook 'before-save-hook 'gofmt-before-save)))

;; helper function
;; https://github.com/astaxie/build-web-application-with-golang/blob/master/ebook/01.4.md
(defun pl/go ()
  "run current buffer"
  (interactive)
  (compile (concat "go run " (buffer-file-name))))

(defun pl/go-fix-buffer ()
  "run gofix on current buffer"
  (interactive)
  (show-all)
  (shell-command-on-region (point-min) (point-max) "go tool fix -diff"))



(provide '50go)
;;; 50go.el ends here
