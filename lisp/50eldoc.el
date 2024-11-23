;;; 50eldoc.el ---

;; Copyright (C) 2024  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>

;; https://github.com/casouri/eldoc-box
(use-package eldoc-box
  :after eldoc
  :config
  'eldoc-box-help-at-point
  (tl/set-leader-keys
    "dd"  'eldoc-box-help-at-point))

(provide '50eldoc)
;;; 50eldoc ends here
