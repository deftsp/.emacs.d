;;; 50face.el ---

;; Copyright (C) 2008  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>

;;  M-x list-colors-display RET
;;  M-x list-faces-display RET

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
;; alternative http://www.emacswiki.org/emacs/cursor-chg.el
;; (defvar paloryemacs/lastest-cursor-color "")
;; (defvar paloryemacs/lastest-update-cursor-color-buffer "")
;; (defun paloryemacs/update-cursor-color-according-to-mode ()
;;   "change cursor color according to some minor modes."
;;   ;; set-cursor-color is somewhat costly, so we only call it when needed:
;;   (let ((color
;;          (if buffer-read-only "DodgerBlue"
;;            (if overwrite-mode
;;                "yellow" "#cd0000"))))
;;     (unless (and
;;              (string= color paloryemacs/lastest-cursor-color)
;;              (string= (buffer-name) paloryemacs/lastest-update-cursor-color-buffer))
;;       (set-cursor-color (setq paloryemacs/lastest-cursor-color color))
;;       (setq paloryemacs/lastest-update-cursor-color-buffer (buffer-name)))))
;; (add-hook 'post-command-hook 'paloryemacs/update-cursor-color-according-to-mode)

(provide '99face)
