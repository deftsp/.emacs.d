;;; 50prescient.el ---                                     -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>

(use-package prescient
  :defer t)

(use-package ivy-prescient
  :after ivy
  :config
  (ivy-prescient-mode +1))

(use-package company-prescient
  :after company
  :config
  (company-prescient-mode +1))

;; (use-package prescient-persist-mode
;;   :after selectrum
;;   :config
;;   (prescient-persist-mode +1))

(provide '50prescient)

;; Local Variables: **
;; outline-regexp: ";;; " **
;; byte-compile-warnings: (not noruntime free-vars) **
;; End: **
