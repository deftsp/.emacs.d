;;; 50haskell.el ---

;; Copyright (C) 2012  Shihpin Tseng

;; Keywords:

(require 'haskell-mode nil t)

(setq haskell-font-lock-symbols t)

(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)

(eval-after-load "haskell-mode"
  '(progn
     (require 'haskell-process)
     ;; (define-key haskell-mode-map (kbd "C-j") 'haskell-newline-and-indent)
     (define-key haskell-mode-map (kbd "C-j") 'newline)
     (define-key haskell-mode-map (kbd "C-c h") 'haskell-hoogle)))


;;; indent
;; turn-on-haskell-indentation
;; turn-on-haskell-indent
;; turn-on-haskell-simple-indent
;; Note that the three indentation modules are mutually exclusive - add at most one. In preference for the more advanced.
(remove-hook 'haskell-mode-hook 'turn-on-haskell-indentation) ; el-get default install `turn-on-haskell-indentation'
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;; (setq haskell-indentation-layout-offset 4
;;       haskell-indentation-left-offset 4
;;       haskell-indentation-ifte-offset 4)


;;; ghc-mod
;; install ghc-mod
;; % cabal update
;; % cabal install ghc-mod


(autoload 'ghc-init "ghc" nil t)
(defun haskell-mode-hook-funs ()
  (define-key haskell-mode-map (kbd "C-c |") 'haskell-indent-insert-guard)
  ;; (define-key haskell-mode-map (kbd "C-M-d") 'anything-ghc-browse-document)
  (flymake-mode)
  (ghc-init))

(add-hook 'haskell-mode-hook #'haskell-mode-hook-funs)

;;; navigation
;; $ cabal install hasktags
;; Run M-x haskell-process-generate-tags to generate the TAGS file of the current session directory.
;; You can now use M-. on a name in a Haskell buffer which will jump directly to its definition.
(setq haskell-tags-on-save nil)

(provide '50haskell)
