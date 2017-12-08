;;; 50restclient.el ---                              -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>

;;; Code:
(use-package restclient
  :defer t
  :init
  (progn
    (add-to-list 'auto-mode-alist '("\\.http\\'" . restclient-mode)))
  :config
  (progn
    (defun paloryemacs/restclient-http-send-current-raw-stay-in-window ()
      (interactive)
      (restclient-http-send-current t t))

    (paloryemacs/set-leader-keys-for-major-mode 'restclient-mode
      "n" 'restclient-jump-next
      "p" 'restclient-jump-prev
      "s" 'restclient-http-send-current-stay-in-window
      "S" 'restclient-http-send-current
      "r" 'paloryemacs/restclient-http-send-current-raw-stay-in-window
      "R" 'restclient-http-send-current-raw
      "y" 'restclient-copy-curl-command)))



(provide '50restclient)
;;; 50restclient.el ends here
