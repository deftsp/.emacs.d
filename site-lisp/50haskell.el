;;; 50haskell.el ---

;; Copyright (C) 2012  Shihpin Tseng

;; Keywords:

(require 'haskell-mode nil t)

(setq haskell-font-lock-symbols t)

(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)

(defvar pl/haskell-mode-doc-map nil
  "Keymap for documentation commands. Bound to a prefix key.")

(eval-after-load "haskell-mode"
  '(progn
     (require 'haskell-process)
     (setq haskell-stylish-on-save nil) ; or use M-x haskell-mode-stylish-buffer to call `stylish-haskell'
     ;; keymap for documentation
     (setq pl/haskell-mode-doc-map (make-sparse-keymap))
     (define-key pl/haskell-mode-doc-map (kbd "i") 'inferior-haskell-info)
     (define-key pl/haskell-mode-doc-map (kbd "t") 'inferior-haskell-type)
     (define-key pl/haskell-mode-doc-map (kbd "a") 'helm-ghc-browse-document)
     (define-key pl/haskell-mode-doc-map (kbd "C-a") 'helm-ghc-browse-document)
     (define-key pl/haskell-mode-doc-map (kbd "h") 'haskell-hoogle)
     (define-key pl/haskell-mode-doc-map (kbd "d") 'inferior-haskell-find-haddock)
     (define-key pl/haskell-mode-doc-map (kbd "C-d") 'inferior-haskell-find-haddock)

     (define-key haskell-mode-map (kbd "C-c C-d") pl/haskell-mode-doc-map)
     (define-key haskell-mode-map (kbd "C-M-x") 'inferior-haskell-send-decl)
     (define-key haskell-mode-map (kbd "C-x C-e") 'inferior-haskell-send-decl)
     (define-key haskell-mode-map (kbd "C-c |") 'haskell-indent-insert-guard)
     (define-key haskell-mode-map (kbd "C-j") 'newline)
     ;; (define-key haskell-mode-map (kbd "C-j") 'haskell-newline-and-indent)
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

  (if (buffer-file-name (current-buffer))
      (flymake-mode))
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

;;; hamlet-mode
(require 'hamlet-mode nil t)

(provide '50haskell)
