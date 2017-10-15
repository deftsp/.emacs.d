;;; 50mmm-mode.el ---                                -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>
;; Keywords: mmm-mode

;;; Commentary:

;;; Code:
(use-package mmm-mode
  :defer t
  :init
  (setq mmm-global-mode 'maybe)
  :config
  (mmm-add-mode-ext-class 'html-mode "\\.php\\'" 'html-php))

;;example use of the universal mmm mode
;;{%vhdl%} signal a : in bit; {%/vhdl%}

;; (mmm-add-classes
;;  '((embedded-vhdl
;;     :submode vhdl-mode
;;     :front "begin{vhdlcode}.*$"
;;     :back "\\end{vhdlcode}")
;;    (embedded-python
;;     :submode python-mode
;;     :front "begin{pythoncode}.*$"
;;     :back "\\end{pythoncode}")
;;    (embedded-c
;;     :submode c-mode
;;     :front "begin{ccode}.*$"
;;     :back "\\end{ccode}")))
;; (mmm-add-mode-ext-class 'latex-mode "\.tex$" 'embedded-vhdl)
;; (mmm-add-mode-ext-class 'latex-mode "\.tex$" 'embedded-python)
;;(mmm-add-mode-ext-class 'latex-mode "\.tex$" 'embedded-c) ;does not work well

;;mmm-mode example:
;;(mmm-add-classes
;; '((embedded-sql
;;    :submode sql-mode
;;    :front "EXEC SQL"
;;    :back ";")))
;;(setq-default mmm-global-mode t)
;;(mmm-add-mode-ext-class 'c-mode "\.pc$" 'embedded-sql)
;;(mmm-add-mode-ext-class 'c-mode "\.sqc$" 'embedded-sql)
;;(setq-default mmm-never-modes
;;              (append '(ediff-mode) '(text-mode) mmm-never-modes))
;;)



(provide '50mmm-mode)

;;; 50mmm-mode.el ends here
