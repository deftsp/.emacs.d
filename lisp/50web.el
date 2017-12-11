;;; 50web.el ---                                -*- lexical-binding: t; -*-

;; Copyright (C) 2014  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>
;; Keywords:


;;; Commentary:

;;; Code:

;;; web-mode.el is an autonomous emacs major-mode for editing web templates:
;;; HTML documents embedding parts (CSS / JavaScript) and blocks (client / server side)
(use-package web-mode
  :defer t
  :init
  (progn
    (defun paloryemacs/init-web-mode ()
      "Hooks for Web mode."
      (setq web-mode-markup-indent-offset 2
            web-mode-css-indent-offset 2
            ;; script offset indentation (for JavaScript, Java, PHP, etc.)
            web-mode-code-indent-offset 2))

    (setq web-mode-script-padding 4
          web-mode-style-padding 1
          web-mode-block-padding 0))
  (add-hook 'web-mode-hook 'paloryemacs/init-web-mode))


(provide '50web)
;;; 50web.el ends here
