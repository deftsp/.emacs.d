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

;;; Focus the last frame when Emacs re-activated
(defvar tl/emacs-deactivated-focus-frame nil
  "The frame when emacs lost focus.")

;; N.B. It can not be triggered by org-protocol, because Emacs will get focus
;; Even call the org-protocol with "open -g ...."
(defun tl/on-emacs-deactivated ()
  (setq tl/emacs-deactivated-focus-frame (selected-frame)))


;;; N.B. The `frame-focus-state' in `after-focus-change-function' can not get
;;; `t' when using `hs.application.open("/Applications/Emacs.app")' in
;;; Hammerspoon to make Emacs activate. Only mouse click the Emacs frame or
;;; Alt-Tab can. So, triggering it with the help of Hammerspoon.
(defun tl/on-emacs-activated ()
  (when (and tl/emacs-deactivated-focus-frame
             (not (eq (selected-frame) tl/emacs-deactivated-focus-frame)))
    (select-frame-set-input-focus tl/emacs-deactivated-focus-frame)
    (setq tl/emacs-deactivated-focus-frame nil)))

;;; N.B. focus-in/out-hook is obsoleted since Emacs27.1
;; (add-hook 'focus-in-hook 'tl/on-emacs-activated)
;; (add-hook 'focus-out-hook 'tl/on-emacs-deactivated)

;; (defun tl/fix-focus-frame-when-activated ()
;;   (message (format "frame-focus-state: %S>>> %S|| %S"
;;                    (frame-focus-state)
;;                    (selected-frame)
;;                    tl/emacs-deactivated-focus-frame)))

(when (and (boundp 'after-focus-change-function)
           (member window-system '(ns mac)))
  ;; (add-function :after after-focus-change-function
  ;;   'tl/fix-focus-frame-when-activated)
  (add-function :after after-focus-change-function
    'tl/on-emacs-deactivated))

;; (remove-function  after-focus-change-function 'tl/fix-focus-frame-when-activated)


(provide '03frame)
;;; 03frame.el ends here
