;;; 52prescient.el ---                                     -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>

(use-package prescient
  :defer t
  :init
  (setq prescient-history-length 200)
  (setq prescient-filter-method '(literal regexp initialism fuzzy)))

;; must load Counsel before ivy-prescient.el
(use-package ivy-prescient
  :after (ivy counsel)
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

(provide '52prescient)

;; Local Variables: **
;; outline-regexp: ";;; " **
;; byte-compile-warnings: (not noruntime free-vars) **
;; End: **
