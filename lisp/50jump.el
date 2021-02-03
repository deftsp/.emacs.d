;;; 50jump.el ---                                    -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>

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


;; FIXME: remove it after smart-jump can be required
;; (require 'xref)
;; (load "~/.emacs.d/straight/build/smart-jump/smart-jump.el")

(use-package smart-jump
  :commands (smart-jump-go smart-jump-back smart-jump-references)
  :init
  (general-define-key
   :states '(normal visual motion)
   "M-." 'smart-jump-go
   "M-," 'smart-jump-back
   "M-?" 'smart-jump-references)
  :config
  (smart-jump-register :modes '(c-mode c++-mode)
                       :jump-fn 'ggtags-find-tag-dwim
                       :pop-fn 'ggtags-prev-mark
                       :refs-fn 'ggtags-find-reference
                       :should-jump t
                       :heuristic 'point
                       :async 500
                       :order 2))

(provide '50jump)
