;;; 50eshell.el ---

;; Copyright (C) 2008  Shihpin Tseng
;; Author: Shihpin Tseng <deftsp@gmail.com>


;;; eshell-glob
(setq eshell-error-if-no-glob t
      eshell-history-size 500)

(defun pl/eshell-mode-hook-func ()
  (local-set-key "\C-d" 'eshell-delchar-or-exit)
  ;; (local-set-key "\C-u" 'eshell-kill-input)
  (local-set-key "\C-a" 'eshell-maybe-bol)
  (set (make-local-variable 'scroll-margin) 0))

(add-hook 'eshell-mode-hook 'pl/eshell-mode-hook-func)

(defun eshell-delchar-or-exit (arg)
  (interactive "P")
  (let ((cur-point (point))
        (delchar-flag t))
    (save-excursion
      (eshell-bol)
      (when (and (= cur-point (point))
                 (= cur-point (point-max)))
        (setq delchar-flag nil)))
    (if delchar-flag
        (delete-char (if (null arg) 1 arg) (if (null arg) nil t))
        (eshell-bol)
        (insert "exit")
        (eshell-send-input))))


;; C-a to beginning of command line or beginning of line?
;; I use the following code. It makes C-a go to the beginning of the command line, unless it is already there, in which
;; case it goes to the beginning of the line. So if you are at the end of the command line and want to go to the real
;; beginning of line, hit C-a twice:

(defun eshell-maybe-bol ()
  (interactive)
  (let ((p (point)))
    (eshell-bol)
    (if (= p (point))
        (beginning-of-line))))


(defun eshell/l (&rest args)
  "ls -ltr alias"
  (eshell/ls "-ltr" args))

(defun eshell/ll (&rest args)
  "ls -alh alias"
  (eshell/ls "-alh" args))

(defun eshell/lla (&rest args)
  "ls -lA alias"
  (eshell/ls "-lA" args))

(defalias 'eshell/emacs 'find-file)
(defun eshell/dired () (dired (eshell/pwd)))

(provide '50eshell)
