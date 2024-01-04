;;; 50ansi-color.el ---

;; Copyright (C) 2024  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>
;; Keywords:

;; https://emacs.stackexchange.com/questions/69938/how-can-i-apply-ansi-color-to-complete-buffer
;; https://stackoverflow.com/questions/23378271/how-do-i-display-ansi-color-codes-in-emacs-for-any-mode
;; (defun tl/ansi-color-on-buffer ()
;;   "apply ansi color on region"
;;   (interactive)
;;   (let ((inhibit-read-only t))
;;     (ansi-color-apply-on-region (point-min) (point-max))))

;; (define-minor-mode ansi-color-mode
;;   "ansi color minor mode"
;;   nil nil nil
;;   (ansi-color-apply-on-region 1 (buffer-size)))


;; https://endlessparentheses.com/ansi-colors-in-the-compilation-buffer-output.html
;; (require 'ansi-color)
;; (defun endless/colorize-compilation ()
;;   "Colorize from `compilation-filter-start' to `point'."
;;   (let ((inhibit-read-only t))
;;     (ansi-color-apply-on-region
;;      compilation-filter-start (point))))

;; (add-hook 'compilation-filter-hook
;;           #'endless/colorize-compilation)

;;;;;;;;;;; do not modifying the contents of the file
(use-package tty-format)

;; M-x display-ansi-colors to explicitly decode ANSI color escape sequences
(defun pl/display-ansi-colors ()
  (interactive)
  (format-decode-buffer 'ansi-colors))

;; decode ANSI color escape sequences for *.txt or README files
;; (add-hook 'find-file-hooks 'tty-format-guess)

;; decode ANSI color escape sequences for .log files
;; (add-to-list 'auto-mode-alist '("\\.log\\'" . display-ansi-colors))

;;;;;;;;;;;;;;;;;;;;;
;; This minor mode runs `ansi-color-apply-on-region' lazily, i.e. only the visible part of the buffer. Hence, it does
;; NOT freeze Emacs even if the log file is huge.
(defun ansi-color-after-scroll (window start)
  "Used by ansi-color-mode minor mode"
  (ansi-color-apply-on-region start (window-end window t) t))

(define-minor-mode ansi-color-mode
  "A very primitive minor mode to view log files containing ANSI color codes.

Pros: this minor mode runs `ansi-color-apply-on-region' lazily,
i.e. only the visible part of the buffer. Hence, it does NOT
freeze Emacs even if the log file is huge.

Cons: a) when the minor code is toggled off, it does not undo
what has already been ansi colorized. b) assumes the buffer
content etc. does not change. c) jumping to random places within
the buffer may incur incorrect/incomplete colorization.

How to install: put this code into your init.el, then evaluate it or
restart Emacs for the code to take effect.

How to use: in the log buffer of need run `M-x ansi-color-mode'.
Alternatively, feel free to enable this minor mode via mode hooks
so that you needn't enable it manually.

-- lgfang
"
  :global nil
  :lighter ""
  (if ansi-color-mode
      (progn
        (ansi-color-apply-on-region (window-start) (window-end) t)
        (add-hook 'window-scroll-functions 'ansi-color-after-scroll 80 t))
    (remove-hook 'window-scroll-functions 'ansi-color-after-scroll t)))



(provide '50ansi-color)
