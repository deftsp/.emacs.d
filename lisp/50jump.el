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
                       :order 2)
  (when (eq dottl-lsp-client 'lsp-bridge)
    (smart-jump-register :modes '(rust-mode rustic-mode)
                         :jump-fn 'lsp-bridge-find-def
                         :pop-fn 'lsp-bridge-find-def-return
                         :refs-fn 'lsp-bridge-find-references
                         :should-jump t
                         :heuristic 'point
                         :async 500
                         :order 2)))

;; [[https://github.com/gilbertw1/better-jumper][gilbertw1/better-jumper: A configurable jump list implementation for Emacs]]
(use-package better-jumper
  :hook (after-init . better-jumper-mode)
  :diminish better-jumper-local-mode
  :init
  (global-set-key [remap evil-jump-forward]  #'better-jumper-jump-forward)
  (global-set-key [remap evil-jump-backward] #'better-jumper-jump-backward)
  (global-set-key [remap xref-pop-marker-stack] #'better-jumper-jump-backward))

(provide '50jump)
