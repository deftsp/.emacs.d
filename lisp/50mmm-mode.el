;;; 50mmm-mode.el ---                                -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>
;; Keywords: mmm-mode

;;; Commentary:

;;; Code:
(require 'mmm-mode nil t)

(with-eval-after-load "mmm-mode"
  (setq mmm-global-mode 'maybe)
  (mmm-add-mode-ext-class 'html-mode "\\.php\\'" 'html-php))

(provide '50mmm-mode)

;;; 50mmm-mode.el ends here
