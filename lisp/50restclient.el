;;; 50restclient.el ---                              -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>

;;; Code:

;; https://github.com/zweifisch/ob-http
(use-package ob-http
  :defer t
  :init
  (progn
    ;; (add-to-list 'auto-mode-alist '("\\.http\\'" . org-mode))
    ;; (add-to-list 'org-babel-load-languages '(http . t))
    (setq ob-http:remove-cr t)
    (setq ob-http:curl-custom-arguments nil)
    (setq ob-http:max-time 10)))

;; https://github.com/alf/ob-restclient.el
(use-package restclient
  :defer t
  :init
  (progn
    (add-to-list 'auto-mode-alist '("\\.http\\'" . restclient-mode)))
  :config
  (progn
    (defun tl/restclient-http-send-current-raw-stay-in-window ()
      (interactive)
      (restclient-http-send-current t t))

    (tl/set-leader-keys-for-major-mode 'restclient-mode
      "n" 'restclient-jump-next
      "p" 'restclient-jump-prev
      "s" 'restclient-http-send-current-stay-in-window
      "S" 'restclient-http-send-current
      "r" 'tl/restclient-http-send-current-raw-stay-in-window
      "R" 'restclient-http-send-current-raw
      "y" 'restclient-copy-curl-command)))



(provide '50restclient)
;;; 50restclient.el ends here
