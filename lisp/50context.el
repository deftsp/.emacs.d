;;; 50context.el ---                                 -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>
;; Keywords:

(defun tl/copy-opencode-file-reference ()
  "Copy current file reference for opencode."
  (interactive)
  (unless buffer-file-name
    (user-error "Current buffer is not visiting a file"))
  (let* ((root (project-root (project-current t)))
         (relative-file (file-relative-name buffer-file-name root))
         (text (format "@%s:%d" relative-file (line-number-at-pos))))
    (kill-new text)
    (message "%s" text)))


(provide '50context)
