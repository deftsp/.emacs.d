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
(setq frame-title-format
      (list "GNU Emacs "
            emacs-version
            "@" system-name ": "
            '(:eval
              (if buffer-file-name
                  (replace-regexp-in-string
                   (getenv "HOME") "~"
                   (file-name-directory buffer-file-name))
                (buffer-name)))))
(setq icon-title-format frame-title-format)


(provide '03frame)
;;; 03frame.el ends here
