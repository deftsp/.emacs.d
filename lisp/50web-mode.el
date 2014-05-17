;;; 50web-mode.el ---                                -*- lexical-binding: t; -*-

;; Copyright (C) 2014  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>
;; Keywords:


;;; Commentary:

;;; Code:

;;; web-mode.el is an autonomous emacs major-mode for editing web templates:
;f;; HTML documents embedding parts (CSS / JavaScript) and blocks (client / server side)

(defun pl/init-web-mode ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
  web-mode-css-indent-offset 2
  ;; script offset indentation (for JavaScript, Java, PHP, etc.)
  web-mode-code-indent-offset 2)

(add-hook 'web-mode-hook 'pl/init-web-mode)


(provide '50web-mode)
;;; 50web-mode.el ends here
