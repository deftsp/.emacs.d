;;; 50haskell.el ---

;; Copyright (C) 2012  Shihpin Tseng

;; Keywords:

;; (add-to-list 'load-path "~/.emacs.d/lisp/haskell-mode/")
;; (require 'haskell-mode-autoloads)

(defvar pl/haskell-mode-doc-map nil
  "Keymap for documentation commands. Bound to a prefix key.")

(defvar pl/haskell-mode-key-chord-map nil
  "Keymap for key chord prefix commands in haskell mode.")

(eval-after-load "haskell-mode"
  '(progn
     (setq haskell-font-lock-symbols t
           haskell-process-type 'cabal-dev  ; 'cabal-dev
           haskell-notify-p t)
     (require 'haskell-process)
     (setq haskell-stylish-on-save nil) ; or use M-x haskell-mode-stylish-buffer to call `stylish-haskell'
     (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
     (add-hook 'haskell-mode-hook 'imenu-add-menubar-index)

     (setq pl/haskell-mode-key-chord-map (make-sparse-keymap))
     (define-key pl/haskell-mode-key-chord-map (kbd "e") 'haskell-indent-insert-equal)
     (define-key pl/haskell-mode-key-chord-map (kbd "=") 'haskell-indent-insert-equal)
     (define-key pl/haskell-mode-key-chord-map (kbd "g") 'haskell-indent-insert-guard)
     (define-key pl/haskell-mode-key-chord-map (kbd "|") 'haskell-indent-insert-guard)
     (define-key pl/haskell-mode-key-chord-map (kbd "o") 'haskell-indent-insert-otherwise)
     (define-key pl/haskell-mode-key-chord-map (kbd "w") 'haskell-indent-insert-where)
     (define-key pl/haskell-mode-key-chord-map (kbd ".") 'haskell-indent-align-guards-and-rhs)
     (define-key pl/haskell-mode-key-chord-map (kbd ">") 'haskell-indent-put-region-in-literate)
     (define-key pl/haskell-mode-key-chord-map (kbd "l") 'pl/pop-haskell-process-log-buffer)

     ;; keymap for documentation
     (setq pl/haskell-mode-doc-map (make-sparse-keymap))
     (define-key pl/haskell-mode-doc-map (kbd "i") 'haskell-process-do-info) ; inferior-haskell-info
     (define-key pl/haskell-mode-doc-map (kbd "t") 'haskell-process-do-type) ; inferior-haskell-type
     (define-key pl/haskell-mode-doc-map (kbd "a") 'helm-ghc-browse-document)
     (define-key pl/haskell-mode-doc-map (kbd "C-a") 'helm-ghc-browse-document)
     (define-key pl/haskell-mode-doc-map (kbd "h") 'haskell-hoogle)
     (define-key pl/haskell-mode-doc-map (kbd "d") 'inferior-haskell-find-haddock)
     (define-key pl/haskell-mode-doc-map (kbd "C-d") 'inferior-haskell-find-haddock)))




;;; ghc-mod
;; install ghc-mod
;; % cabal update
;; % cabal install ghc-mod
(autoload 'ghc-init "ghc" nil t)

;;; haskell mode hook
(add-hook 'haskell-mode-hook 'pl/haskell-mode-setup)
(defun pl/haskell-mode-setup ()
  ;; (if (buffer-file-name (current-buffer))
  ;;     (flymake-mode))
  (ghc-init)
  ;; enable our level computation
  (setq outline-level 'pl/outline-level)
  (outline-minor-mode t)
  ;; initially hide all but the headers
  ;;(hide-body)
  (subword-mode +1)
  (when (fboundp 'key-chord-define)
    (key-chord-define haskell-mode-map ".x" pl/haskell-mode-key-chord-map))
  (define-key haskell-mode-map (kbd "C-c C-d") pl/haskell-mode-doc-map)
  (define-key haskell-mode-map (kbd "C-M-x") 'inferior-haskell-send-decl)
  (define-key haskell-mode-map (kbd "C-x C-e") 'inferior-haskell-send-decl)
  (define-key haskell-mode-map (kbd "C-c |") 'haskell-indent-insert-guard)
  (define-key haskell-mode-map (kbd "C-j") 'newline)
  ;; (define-key haskell-mode-map (kbd "C-j") 'haskell-newline-and-indent)
  (define-key haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
  ;; Load the current file (and make a session if not already made).
  (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-file)
  ;; “Bring” the REPL, hiding all other windows apart from the source and the REPL.
  (define-key haskell-mode-map (kbd "C-`") 'haskell-interactive-bring)
  ;; Build the Cabal project.
  (define-key haskell-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
  ;; Interactively choose the Cabal command to run.
  (define-key haskell-mode-map (kbd "C-c c") 'haskell-process-cabal)

  ;; Contextually do clever things on the space key, in particular:
  ;;   1. Complete imports, letting you choose the module name.
  ;;   2. Show the type of the symbol after the space.
  (define-key haskell-mode-map (kbd "SPC") 'haskell-mode-contextual-space)
  ;; Jump to the imports. Keep tapping to jump between import groups. C-u C-c I to jump back again.
  (define-key haskell-mode-map (kbd "C-c I") 'haskell-navigate-imports)

  ;; Jump to the definition of the current symbol.
  (define-key haskell-mode-map (kbd "M-.") 'haskell-mode-tag-find)

  ;; Interactive block indentation
  (define-key haskell-mode-map (kbd "C-,") 'haskell-move-nested-left)
  (define-key haskell-mode-map (kbd "C-.") 'haskell-move-nested-right)

  (define-key haskell-mode-map (kbd "C-c h") 'haskell-hoogle))

;; this gets called by outline to determine the level. Just use the length of the whitespace
(defun pl/outline-level ()
  (let (buffer-invisibility-spec)
    (save-excursion
      (skip-chars-forward "\t ")
      (current-column))))


;;;
(defun pl/pop-haskell-process-log-buffer ()
  (interactive)
  (let ((buf (get-buffer "*haskell-process-log*")))
    (if buf
        (pop-to-buffer buf)
      (message "Can not find haskell process log buffer. Have you start inferior?"))))


;;; indent
;; turn-on-haskell-indentation
;; turn-on-haskell-indent
;; turn-on-haskell-simple-indent
;; Note that the three indentation modules are mutually exclusive - add at most one. In preference for the more advanced.
(remove-hook 'haskell-mode-hook 'turn-on-haskell-indentation) ; el-get default install `turn-on-haskell-indentation'
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
(setq haskell-indent-thenelse 2)
(eval-after-load "haskell-indent"
  '(setq haskell-indent-after-keywords '(("where" 2 0)
                                         ("of" 2)
                                         ("do" 4 0)
                                         ("mdo" 2)
                                         ("rec" 2)
                                         ("in" 2 0)
                                         ("{" 2)
                                         ("defaultLayout" 4)
                                         "if"
                                         "then"
                                         "else"
                                         "let")))


;; (setq haskell-indentation-layout-offset 4
;;       haskell-indentation-left-offset 4
;;       haskell-indentation-ifte-offset 4)

;;; navigation
;; $ cabal install hasktags
;; Run M-x haskell-process-generate-tags to generate the TAGS file of the current session directory.
;; You can now use M-. on a name in a Haskell buffer which will jump directly to its definition.
(setq haskell-tags-on-save t)

;;; cabal
;; Useful to have these keybindings for .cabal files, too.
(defun pl/haskell-cabal-hook ()
  (define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
  (define-key haskell-cabal-mode-map (kbd "C-c c") 'haskell-process-cabal)
  (define-key haskell-cabal-mode-map (kbd "C-`") 'haskell-interactive-bring)
  (define-key haskell-cabal-mode-map [?\C-c ?\C-z] 'haskell-interactive-switch))

(add-hook 'haskell-cabal-mode-hook 'pl/haskell-cabal-hook)


;;; hamlet-mode
(require 'hamlet-mode nil t)


;;; align regexp
(add-to-list 'align-rules-list
             '(haskell-types
               (regexp . "\\(\\s-+\\)\\(::\\|∷\\)\\s-+")
               (modes quote (haskell-mode literate-haskell-mode))))
(add-to-list 'align-rules-list
             '(haskell-assignment
               (regexp . "\\(\\s-+\\)=\\s-+")
               (modes quote (haskell-mode literate-haskell-mode))))
(add-to-list 'align-rules-list
             '(haskell-arrows
               (regexp . "\\(\\s-+\\)\\(->\\|→\\)\\s-+")
               (modes quote (haskell-mode literate-haskell-mode))))
(add-to-list 'align-rules-list
             '(haskell-left-arrows
               (regexp . "\\(\\s-+\\)\\(<-\\|←\\)\\s-+")
               (modes quote (haskell-mode literate-haskell-mode))))


(provide '50haskell)
