;;; 50go.el ---

;; Copyright (C) 2013  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>
;; Keywords:

;; "C-c C-a" go-import-add
;; "C-c C-j" godef-jump
;; "C-c C-d" godef-describe

(when (eq system-type 'darwin)
 (setenv "GOPATH" (concat (expand-file-name "~/opt/go-packages") ":"
                          (expand-file-name "~/Lab/gocode"))))

(defun paloryemacs/go-mode-hook-func ()
  (setq imenu-generic-expression '(("variable" "^var *\\(.*\\) *" 1)
                                   ("type" "^type *\\([^ \t\n\r\f]*\\)" 1)
                                   ("func" "^func *\\(.*\\)" 1)))
  (imenu-add-to-menubar "Index")
  ;; (setq show-trailing-whitespace t)
  (local-set-key (kbd "C-c C-r") 'go-remove-unused-imports)
  (local-set-key (kbd "C-c I") 'go-goto-imports))

(eval-after-load "go-mode"
  '(progn
     (add-hook 'go-mode-hook 'paloryemacs/go-mode-hook-func)
     (add-hook 'before-save-hook 'gofmt-before-save)))

;; helper function
;; https://github.com/astaxie/build-web-application-with-golang/blob/master/ebook/01.4.md
(defun paloryemacs/go ()
  "run current buffer"
  (interactive)
  (compile (concat "go run " (buffer-file-name))))

(defun paloryemacs/go-fix-buffer ()
  "run gofix on current buffer"
  (interactive)
  (show-all)
  (shell-command-on-region (point-min) (point-max) "go tool fix -diff"))

(provide '50go)
;;; 50go.el ends here
