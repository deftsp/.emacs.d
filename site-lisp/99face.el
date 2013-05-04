;;; 50face.el ---

;; Copyright (C) 2008  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>

;;  M-x list-colors-display RET
;;  M-x list-faces-display RET


;;; frame parameters
(when (eq window-system 'ns)
  (setq initial-frame-alist `((tool-bar-lines . 0)
                              (menu-bar-lines . 0)
                              (width . 268)
                              (height . 65)
                              (top . 22) ; (frame-parameter nil 'top)
                              (left . 0)
                              (alpha . (98 98)) ; first number is for the active window and the second for the inactive
                                        ; window
                              (mouse-color . "gray80")
                              ;; (right-fringe)
                              ;; (left-fringe)
                              ;; (scroll-bar-width . 12)
                              (vertical-scroll-bars . right))

        ;; default-frame-alist is defined in terms of initial-frame-alist.  Don't
        ;; use copy-sequence here -- it doesn't copy the list elements, just the
        ;; list's cons cells.  Use copy-alist instead.
        default-frame-alist (copy-alist initial-frame-alist))
  (setq tooltip-frame-parameters  '((name . "tooltip")
                                    (internal-border-width . 1)
                                    (border-width . 0))))



;;; font lock
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t
      font-lock-global-modes '(not text-mode)
      font-lock-verbose nil
      font-lock-maximum-size '((t . 1048576) (vm-mode . 5250000)))


;;; cursor
(setq cursor-type t) ; use the cursor specified for the frame
(setq x-stretch-cursor t) ; *Non-nil means draw block cursor as wide as the glyph under it.

(when (fboundp 'blink-cursor-mode)
  (setq blink-cursor-delay 0.2
        blink-cursor-interval 0.3)
  (blink-cursor-mode -1))

;;; update cursor color according to mode
(defvar pl/lastest-cursor-color "")
(defvar pl/lastest-update-cursor-color-buffer "")
(defun pl/update-cursor-color-according-to-mode ()
  "change cursor color according to some minor modes."
  ;; set-cursor-color is somewhat costly, so we only call it when needed:
  (let ((color
         (if buffer-read-only "DodgerBlue"
           (if overwrite-mode
               "yellow" "#cd0000"))))
    (unless (and
             (string= color pl/lastest-cursor-color)
             (string= (buffer-name) pl/lastest-update-cursor-color-buffer))
      (set-cursor-color (setq pl/lastest-cursor-color color))
      (setq pl/lastest-update-cursor-color-buffer (buffer-name)))))
(add-hook 'post-command-hook 'pl/update-cursor-color-according-to-mode)


(provide '99face)
