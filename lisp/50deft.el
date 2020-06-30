;;; 50deft.el ---                                    -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>

;;; Code:

(use-package deft
  :defer t
  :init
  (setq deft-extensions '("txt" "tex" "org")
        deft-directory "~/org"
        deft-recursive t
        deft-use-filename-as-title t))


(provide '50deft)
;;; 50deft.el ends here
