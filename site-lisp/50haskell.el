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
     ;; (define-key haskell-mode-map (kbd "C-M-d") 'anything-ghc-browse-document)
     (define-key haskell-mode-map (kbd "C-c |") 'haskell-indent-insert-guard)
     (define-key haskell-mode-map (kbd "C-j") 'newline)
     (define-key haskell-mode-map (kbd "C-c h") 'haskell-hoogle)))

;;; ghc-mod
;; install ghc-mod
;; % cabal update
;; % cabal install ghc-mod
(autoload 'ghc-init "ghc" nil t)


(add-hook 'haskell-mode-hook 'pl/haskell-mode-setup)
(defun pl/haskell-mode-setup ()
  ;; enable our level computation
  (setq outline-level 'pl/outline-level)
  (outline-minor-mode t)
  ;; initially hide all but the headers
  ;;(hide-body)

  (flymake-mode)
  (ghc-init))

;; this gets called by outline to determine the level. Just use the length of the whitespace
(defun pl/outline-level ()
  (let (buffer-invisibility-spec)
    (save-excursion
      (skip-chars-forward "\t ")
      (current-column))))


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


;;; navigation
;; $ cabal install hasktags
;; Run M-x haskell-process-generate-tags to generate the TAGS file of the current session directory.
;; You can now use M-. on a name in a Haskell buffer which will jump directly to its definition.
(setq haskell-tags-on-save nil)

(provide '50haskell)
