;;; 50perl.el ---

;; (add-to-list 'load-path "~/.emacs.d/packages/Emacs-PDE-0.2.16/lisp")
;; (load "pde-load")

;; cperl-mode is preferred to perl-mode
(defalias 'perl-mode 'cperl-mode)

(add-to-list 'auto-mode-alist '("\\.\\([pP][Llm]\\|al\\)\\'" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("perl" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("perl5" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("miniperl" . cperl-mode))


(provide '50perl)