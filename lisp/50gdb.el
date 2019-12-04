;; -*- mode: Emacs-Lisp -*-

(setq gdb-show-main t)
(autoload 'gdb-many-windows "gdb-mi" nil t)

(defun tl/gdb-mode-init ()
    ;; (tool-bar-mode t)
  (gdb-many-windows t)
  (gud-tooltip-mode 1))

(add-hook 'gud-mode-hook 'tl/gdb-mode-init)


(provide '50gdb)
