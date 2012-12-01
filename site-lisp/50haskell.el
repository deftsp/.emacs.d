;;; 50haskell.el ---

;; Copyright (C) 2007  Shihpin Tseng

;; Keywords:

(require 'haskell-mode nil t)
(setq haskell-font-lock-symbols t)

(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)

;;; indent
;; turn-on-haskell-indentation
;; turn-on-haskell-indent
;; turn-on-haskell-simple-indent
;; Note that the three indentation modules are mutually exclusive - add at most one. In preference for the more advanced.
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)


;;"Here comes a short Emacs helper for Haskell coders. It allows you to hoogle
;;lookup the symbol currently under your cursor." Hoogle is a Haskell search engine.
;;<http://clemens.endorphin.org/weblog/archives/2007-01.shtml#e2007-01-09T09_57_26.txt>
;;(require 'hoogle nil t)
;;(define-key haskell-mode-map [?\C-c ?h] 'hoogle-lookup)





(provide '50haskell)
