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
(defun paloryemacs//frame-title-format ()
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
  (setq frame-title-format '((:eval (paloryemacs//frame-title-format))))
  (setq icon-title-format frame-title-format))


(provide '03frame)
;;; 03frame.el ends here
