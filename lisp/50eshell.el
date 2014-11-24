;;; 50eshell.el ---

;; Copyright (C) 2008  Shihpin Tseng
;; Author: Shihpin Tseng <deftsp@gmail.com>


;;; eshell-glob
(setq eshell-error-if-no-glob t
      eshell-history-size 500)

(defun pl/eshell-mode-init ()
  (local-set-key "\C-d" 'pl/eshell-delchar-or-exit)
  (local-set-key "\C-u" 'eshell-kill-input)
  (local-set-key "\C-a" 'pl/eshell-maybe-bol)
  (set (make-local-variable 'scroll-margin) 0))

(add-hook 'eshell-mode-hook 'pl/eshell-mode-init)

(defun pl/eshell-delchar-or-exit (arg)
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
      (eshell-send-input)
      (delete-window))))


;; C-a to beginning of command line or beginning of line?
;; I use the following code. It makes C-a go to the beginning of the command line, unless it is already there, in which
;; case it goes to the beginning of the line. So if you are at the end of the command line and want to go to the real
;; beginning of line, hit C-a twice:
(defun pl/eshell-maybe-bol ()
  (interactive)
  (let ((p (point)))
    (eshell-bol)
    (if (= p (point))
        (beginning-of-line))))


;;; eshell here
;; http://www.howardism.org/Technical/Emacs/eshell-fun.html
(defun pl/eshell-here ()
  "Opens up a new shell in the directory associated with the
current buffer's file. The eshell is renamed to match that
directory to make multiple eshell windows easier."
  (interactive)
  (let* ((parent (if (buffer-file-name)
                     (file-name-directory (buffer-file-name))
                   default-directory))
         (height (/ (window-total-height) 3))
         (name   (car (last (split-string parent "/" t)))))
    (split-window-vertically (- height))
    (other-window 1)
    (eshell "new")
    (rename-buffer (concat "*eshell: " name "*"))

    (insert (concat "ls"))
    (eshell-send-input)))

(global-set-key (kbd "C-!") 'pl/eshell-here)

;;; alias
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
