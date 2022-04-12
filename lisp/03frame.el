;;; 03frame.el ---                                   -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>
;; Keywords:

;;; Code:
(require 'dash)

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

;;;
(defvar tl//after-focus-events-timer nil)

(defvar tl/emacs-reactivate-hook nil)
(defvar tl/emacs-deactivate-hook nil)

;; The focus frame of when emacs deactivated
(defvar tl//emacs-deactivated-saved-frame nil "The frame when emacs lost focus.")

(defun tl//after-focus-change-handler ()
  (setq tl//after-focus-events-timer nil)
  (let ((flst (frame-list)))
    (if (and tl//emacs-deactivated-saved-frame
             (frame-live-p tl//emacs-deactivated-saved-frame)
             (-any? (lambda (f) (eq (frame-focus-state f) t)) flst))
        (progn
          ;; (message "emacs reactivated")
          (when (not (eq (selected-frame) tl//emacs-deactivated-saved-frame))
            (select-frame-set-input-focus tl//emacs-deactivated-saved-frame))
          (run-hooks 'tl/emacs-reactivate-hook)
          (setq tl//emacs-deactivated-saved-frame nil))
      (unless (frame-focus-state (selected-frame))
        ;; if selected-frame lost focus, it can be treat as emacs de-activated
        ;; (message "emacs deactivated")
        (setq tl//emacs-deactivated-saved-frame (selected-frame))
        (run-hooks 'tl/emacs-deactivate-hook)))))

(defun tl//run-after-focus-change-handler-with-timer ()
  (unless (timerp tl//after-focus-events-timer)
    (setq tl//after-focus-events-timer
          (run-at-time "0.2 sec" nil #'tl//after-focus-change-handler))))


(when (and (boundp 'after-focus-change-function)
           (member window-system '(ns mac)))
  ;; focus events is delivered asynchronously with a timer to make sure it
  ;; (remove-function after-focus-change-function 'tl//run-after-focus-change-handler-with-timer)
  (add-function :after after-focus-change-function #'tl//run-after-focus-change-handler-with-timer))

(provide '03frame)
;;; 03frame.el ends here
