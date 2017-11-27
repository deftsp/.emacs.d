;;; 50hammerspoon.el ---                             -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>

(require 'dash)

(defun paloryemacs/open-hammerspoon-url (event &rest params)
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
    (paloryemacs/with-suppress-message "Shell command succeeded with"
      (shell-command (format "open -g \"%s\"" url)))))

(defun paloryemacs/notify-hammerspoon-did-init ()
  (paloryemacs/open-hammerspoon-url "emacs_did_init"))

(add-hook 'after-init-hook #'paloryemacs/notify-hammerspoon-did-init t)

(defun paloryemacs/notify-hammerspoon-did-kill ()
  (paloryemacs/open-hammerspoon-url "emacs_did_kill"))

(add-hook 'kill-emacs-hook #'paloryemacs/notify-hammerspoon-did-kill t)


(provide '50hammerspoon)
;;; 50hammerspoon.el ends here
