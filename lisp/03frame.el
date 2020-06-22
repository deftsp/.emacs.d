;;; 03frame.el ---                                   -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>
;; Keywords:

;;; Code:
;; do not create new frame with `open' in Mac OS X
(when (memq window-system '(mac ns))
  (setq ns-pop-up-frames nil))

;;; set frame title
;; it should be set before org-clock loaded, which will use it to set
;; `org-frame-title-format-backup'
(defun tl//frame-title-format ()
  (concat "GNU Emacs "
          emacs-version
          "@" (or (file-remote-p default-directory 'host) system-name)
          ":"
          (let ((file buffer-file-name))
            (if file
                (concat
                 (when (and (bound-and-true-p projectile-mode)
                            (projectile-project-p))
                   (format " [%s]" (projectile-project-name)))
                 " "
                 (abbreviate-file-name file))
              "%b"))))

(when (display-graphic-p)
  (setq frame-title-format '((:eval (tl//frame-title-format))))
  (setq icon-title-format frame-title-format))


(defvar tl/emacs-deactivated-focus-frame nil
  "The frame when emacs lost focus.")

(defun tl/on-emacs-deactivated ()
  (setq tl/emacs-deactivated-focus-frame (selected-frame)))

(add-hook 'focus-out-hook 'tl/on-emacs-deactivated)

(defun tl/on-emacs-activated ()
  (when (and tl/emacs-deactivated-focus-frame
             (not (eq (selected-frame) tl/emacs-deactivated-focus-frame)))
    (select-frame-set-input-focus tl/emacs-deactivated-focus-frame)
    (setq tl/emacs-deactivated-focus-frame nil)))

(add-hook 'focus-in-hook 'tl/on-emacs-activated)

(provide '03frame)
;;; 03frame.el ends here
