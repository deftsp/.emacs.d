;;; 50haskell.el ---

;; Copyright (C) 2012  Shihpin Tseng

;; Keywords:

(require 'haskell-mode nil t)

(setq haskell-font-lock-symbols t)

(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)

(eval-after-load "haskell-mode"
  '(progn
     (require 'haskell-process)
     (define-key haskell-mode-map (kbd "C-c h") 'haskell-hoogle)
     (define-key haskell-mode-map (kbd "C-j") 'haskell-newline-and-indent)))


;;; indent
;; turn-on-haskell-indentation
;; turn-on-haskell-indent
;; turn-on-haskell-simple-indent
;; Note that the three indentation modules are mutually exclusive - add at most one. In preference for the more advanced.
;; (remove-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-indent)


;;"Here comes a short Emacs helper for Haskell coders. It allows you to hoogle
;;lookup the symbol currently under your cursor." Hoogle is a Haskell search engine.
;;<http://clemens.endorphin.org/weblog/archives/2007-01.shtml#e2007-01-09T09_57_26.txt>
;;(require 'hoogle nil t)
;;(define-key haskell-mode-map [?\C-c ?h] 'hoogle-lookup)


;;; ghc-mod
;; install ghc-mod
;; % cabal update
;; % cabal install ghc-mod


(autoload 'ghc-init "ghc" nil t)
(defun haskell-mode-hook-funs ()
  ;; (define-key haskell-mode-map (kbd "C-M-d") 'anything-ghc-browse-document)
  (flymake-mode)
  (ghc-init))

(add-hook 'haskell-mode-hook #'haskell-mode-hook-funs)

;;; navigation
;; $ cabal install hasktags
;; Run M-x haskell-process-generate-tags to generate the TAGS file of the current session directory.
;; You can now use M-. on a name in a Haskell buffer which will jump directly to its definition.
(setq haskell-tags-on-save t)

(provide '50haskell)
