;;; 50hammerspoon.el ---                             -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>

(require 'dash)

(defun pl/open-hammerspoon-url (event &rest params)
  (let ((len (length params))
        (url (concat "hammerspoon://" event)))
    (when (> len 0)
      (if (zerop (% len 2))
          (let ((querys (--reduce (format "%s&%s" acc it)
                                  (-map (lambda (l)
                                          (format "%s=%s" (car l) (cadr l)))
                                        (-partition-all 2 params)))))
            (setq url (concat url "?" querys)))

        (error "illegal hammerspoon params")))
    (shell-command (concat "open -g" " " url))))

(defun pl/notify-hammerspoon-did-initialzie ()
  (pl/open-hammerspoon-url "emacs_did_load"))

(add-hook 'after-init-hook #'pl/notify-hammerspoon-did-initialzie t)


(provide '50hammerspoon)
;;; 50hammerspoon.el ends here
