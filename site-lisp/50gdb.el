;; -*- mode: Emacs-Lisp -*-

(setq gdb-show-main t)

(defun pl/gdb-mode-init ()
  (gud-tooltip-mode 1)
    ;; (tool-bar-mode t)
  (gdb-many-windows t))

(add-hook 'gud-mode-hook 'pl/gdb-mode-init)


(provide '50gdb)
