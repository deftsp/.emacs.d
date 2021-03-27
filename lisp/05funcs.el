;;; 05funcs.el --- 
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; Originally based on http://stackoverflow.com/questions/2321904/elisp-how-to-save-data-in-a-file
(defun tl/dump-vars-to-file (varlist filename)
  "simplistic dumping of variables in VARLIST to a file FILENAME"
  (with-temp-file filename
    (tl/dump varlist (current-buffer))
    (make-directory (file-name-directory filename) t)))

;; From http://stackoverflow.com/questions/2321904/elisp-how-to-save-data-in-a-file
(defun tl/dump (varlist buffer)
  "insert into buffer the setq statement to recreate the variables in VARLIST"
  (cl-loop for var in varlist do
           (print (list 'setq var (list 'quote (symbol-value var)))
                  buffer)))

(defun tl/echo (msg &rest args)
  "Display MSG in echo-area without logging it in *Messages* buffer."
  (interactive)
  (let ((message-log-max nil))
    (apply 'message msg args)))

(defun tl/derived-mode-p (mode &rest modes)
  "Non-nil if MODE is derived from one of MODES."
  ;; We could have copied the built-in `derived-mode-p' and modified it a bit so
  ;; it works on arbitrary modes instead of only the current major-mode. We
  ;; don't do that because then we will need to modify the function if
  ;; `derived-mode-p' changes.
  (let ((major-mode mode))
    (apply #'derived-mode-p modes)))

(defun tl/alternate-buffer (&optional window)
  "Switch back and forth between current and last buffer in the
current window."
  (interactive)
  (let ((current-buffer (window-buffer window)))
    ;; if no window is found in the windows history, `switch-to-buffer' will
    ;; default to calling `other-buffer'.
    (switch-to-buffer
     (cl-find-if (lambda (buffer)
                   (not (eq buffer current-buffer)))
                 (mapcar #'car (window-prev-buffers window))))))

(defun tl/alternate-window ()
  "Switch back and forth between current and last window in the
current frame."
  (interactive)
  (let (;; switch to first window previously shown in this frame
        (prev-window (get-mru-window nil t t)))
    ;; Check window was not found successfully
    (unless prev-window (user-error "Last window not found."))
    (select-window prev-window)))

(defun tl/comint-clear-buffer ()
  (interactive)
  (let ((comint-buffer-maximum-size 0))
    (comint-truncate-buffer)))


;; Generalized next-error system ("gne")

(defun tl/error-delegate ()
  "Decide which error API to delegate to.

Delegates to flycheck if it is enabled and the next-error buffer
is not visible. Otherwise delegates to regular Emacs next-error."
  (if (and (bound-and-true-p flycheck-mode)
           (let ((buf (ignore-errors (next-error-find-buffer))))
             (not (and buf (get-buffer-window buf)))))
      'flycheck
    'emacs))

(defun tl/next-error (&optional n reset)
  "Dispatch to flycheck or standard emacs error."
  (interactive "P")
  (let ((sys (tl/error-delegate)))
    (cond
     ((eq 'flycheck sys) (call-interactively 'flycheck-next-error))
     ((eq 'emacs sys) (call-interactively 'next-error)))))

(defun tl/previous-error (&optional n reset)
  "Dispatch to flycheck or standard emacs error."
  (interactive "P")
  (let ((sys (tl/error-delegate)))
    (cond
     ((eq 'flycheck sys) (call-interactively 'flycheck-previous-error))
     ((eq 'emacs sys) (call-interactively 'previous-error)))))

(defvar-local tl--gne-min-line nil
  "The first line in the buffer that is a valid result.")
(defvar-local tl--gne-max-line nil
  "The last line in the buffer that is a valid result.")
(defvar-local tl--gne-cur-line 0
  "The current line in the buffer. (It is problematic to use
point for this.)")
(defvar-local tl--gne-line-func nil
  "The function to call to visit the result on a line.")

(defun tl/gne-next (num reset)
  "A generalized next-error function. This function can be used
as `next-error-function' in any buffer that conforms to the
TL generalized next-error API.

The variables `tl--gne-min-line',
`tl--gne-max-line', and `tl--line-func' must be
set."
  (when reset (setq tl--gne-cur-line
                    tl--gne-min-line))
  (setq tl--gne-cur-line
        (min tl--gne-max-line
             (max tl--gne-min-line
                  (+ num tl--gne-cur-line))))
  (goto-line tl--gne-cur-line)
  (funcall tl--gne-line-func
           (buffer-substring (point-at-bol) (point-at-eol))))

(provide '05funcs)
