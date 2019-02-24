;;; 50jump.el ---                                    -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>
;; Keywords:


;;; dumb-jump
(use-package dumb-jump
  :defer t
  :init
  (progn
    (setq dumb-jump-selector 'ivy)
    (setq dumb-jump-prefer-searcher 'rg)
    (define-key global-map (kbd "M-g o") 'dumb-jump-go-other-window)
    (define-key global-map (kbd "M-g j") 'dumb-jump-go)
    (define-key global-map (kbd "M-g b") 'dumb-jump-back)
    (define-key global-map (kbd "M-g x") 'dumb-jump-go-prefer-external)
    (define-key global-map (kbd "M-g o") 'dumb-jump-go-prefer-external-other-window)))

(use-package smart-jump
  :config
  (setq smart-jump-default-mode-list
        '(lisp-mode
          ;; cc-mode ;; `java-mode', `c-mode', `c++-mode', `objc-mode'
          clojure-mode
          elisp-mode
          elm-mode
          erlang-mode
          go-mode
          haskell-mode
          (js2-mode rjsx-mode)
          lispy
          lua-mode
          lsp-mode
          python
          ruby-mode
          rust-mode
          scheme
          typescript-mode
          web-mode))
  (smart-jump-setup-default-registers)

  ;; C/C++
  (smart-jump-register :modes '(c-mode c++-mode)
                       :jump-fn 'ggtags-find-tag-dwim
                       :pop-fn 'ggtags-prev-mark
                       :refs-fn 'ggtags-find-reference
                       :should-jump  (lambda ()
                                       (bound-and-true-p ggtags-mode))
                       :heuristic 'point
                       :async 3000
                       :order 1)
  )


(provide '50jump)
