;;; frame-misc.el ---                                -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>
;; Keywords:


(defun tl/on-emacs-activated ()
  "Switch to other frame, if selected window is org agenda and is the only window"
  (when (> (length (frame-list)) 1)
    (let* ((frame (selected-frame))
           (lst (window-list frame)))
      (when (and (= (length lst) 1)
                 (string= org-agenda-buffer-name
                          (buffer-name (window-buffer (car lst) ))))

        (call-interactively 'other-frame)))))
