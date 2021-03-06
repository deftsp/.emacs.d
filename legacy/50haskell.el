;;; 50haskell.el ---

;; Copyright (C) 2012  Shihpin Tseng

;; Keywords:

;;; Code:
;; (add-to-list 'load-path "~/.emacs.d/site-lisp/haskell-mode/")
;; (add-to-list 'Info-default-directory-list "~/.emacs.d/site-lisp/haskell-mode/")
;; (require 'haskell-mode-autoloads)
;; dante: https://github.com/jyp/dante, a fork of Intero mode.

;; TODO: ghcid
;; http://www.parsonsmatt.org/2018/05/19/ghcid_for_the_win.html
;; https://github.com/ndmitchell/ghcid

(setq haskell-modes '(haskell-mode literate-haskell-mode))

(defvar tl/haskell-completion-backend 'ghc-mod
  "Completion backend used by company.
Available options are `ghci', `intero', `dante', and `ghc-mod'")

;; do not use it, since it doesn't play with well with evil.
(defvar tl/use-structured-haskell-mode nil)

(defun tl-haskell//setup-completion-backend ()
  "Conditionally setup haskell completion backend."
  (pcase haskell-completion-backend
    (`ghc-mod (spacemacs-haskell//setup-ghc-mod))
    (`intero (spacemacs-haskell//setup-intero))))

(setq haskell-modes '(haskell-mode literate-haskell-mode))


(add-hook 'haskell-mode-hook 'tl/company-haskell-mode-setup)
(defun tl/company-haskell-mode-setup ()
  (let ((backends tl/company-prog-common-backends))
    (case tl/haskell-completion-backend
      ('intero (push 'company-intero backends))
      ('ghc-mod (push 'company-ghc backends)))
    (set (make-local-variable 'company-backends) backends)))

(add-hook 'haskell-cabal-mode-hook 'tl/company-haskell-cabal-mode-setup)
(defun tl/company-haskell-cabal-mode-setup ()
  (let ((backends tl/company-prog-common-backends))
    (push 'company-cabal backends)
    (set (make-local-variable 'company-backends) backends)))



(defvar tl/haskell-mode-doc-map nil
  "Keymap for documentation commands. Bound to a prefix key.")

(defvar tl/haskell-mode-key-chord-map nil
  "Keymap for key chord prefix commands in haskell mode.")

(use-package haskell-mode
  :defer t
  :init
  (progn

    (setq haskell-process-log t
          haskell-font-lock-symbols nil ; disabled because it will casue alignment problem
          haskell-process-path-cabal (expand-file-name "~/.cabal/bin/cabal")
          haskell-stylish-on-save nil ; or use M-x haskell-mode-stylish-buffer to call `stylish-haskell'
          ;; Better import handling
          haskell-process-suggest-remove-import-lines t
          haskell-process-auto-import-loaded-modules t
          haskell-process-args-stack-ghci '("--ghci-options=-ferror-spans")
          haskell-interactive-popup-errors  nil
          haskell-notify-p t)

    (add-hook 'haskell-mode-hook 'imenu-add-menubar-index)
    (add-hook 'haskell-mode-hook 'turn-on-haskell-decl-scan)

    ;; use stack instead cabal
    (setq haskell-compile-cabal-build-command "stack build")
    ;; alternative: cabal-repl or cabal-dev
    (setq haskell-process-type 'auto))
  :config
  (progn
    (dolist (mode haskell-modes)
      (tl/declare-prefix-for-mode 'haskell-mode "mg" "haskell/navigation")
      (tl/declare-prefix-for-mode 'haskell-mode "ms" "haskell/repl")
      (tl/declare-prefix-for-mode 'haskell-mode "mc" "haskell/cabal")
      (tl/declare-prefix-for-mode 'haskell-mode "mh" "haskell/documentation")
      (tl/declare-prefix-for-mode 'haskell-mode "md" "haskell/debug")
      (tl/declare-prefix-for-mode 'haskell-mode "mr" "haskell/refactor"))

    (tl/declare-prefix-for-mode 'haskell-interactive-mode "ms" "haskell/repl")
    (tl/declare-prefix-for-mode 'haskell-cabal-mode "ms" "haskell/repl")
    (tl/declare-prefix-for-mode 'intero-repl-mode "ms" "haskell/repl")

    (defun tl/haskell-process-do-type-on-prev-line ()
      (interactive)
      (haskell-process-do-type 1))

    (tl/set-leader-keys-for-mode 'haskell-mode
      "gg"  'haskell-mode-jump-to-def-or-tag

      "gi"  'haskell-navigate-imports
      "gl"  'haskell-mode-goto-loc

      "F"   'haskell-mode-stylish-buffer

      "sb"  'haskell-process-load-file
      "sc"  'haskell-interactive-mode-clear
      "ss"  'tl/haskell-interactive-bring
      "sS"  'haskell-interactive-switch

      "se"  'hasky-stack-execute

      "ca"  'haskell-process-cabal
      "cb"  'haskell-process-cabal-build
      "cc"  'haskell-compile
      "cv"  'haskell-cabal-visit-file

      "hd"  'inferior-haskell-find-haddock
      "hh"  'hoogle
      "hH"  'haskell-hoogle-lookup-from-local
      "hi"  'haskell-process-do-info
      "ht"  'haskell-process-do-type
      "hT"  'haskell-intero/insert-type
      "hT"  'tl/haskell-process-do-type-on-prev-line
      "hy"  'hayoo

      "da"  'haskell-debug/abandon
      "db"  'haskell-debug/break-on-function
      "dB"  'haskell-debug/delete
      "dc"  'haskell-debug/continue
      "dd"  'haskell-debug
      "dn"  'haskell-debug/next
      "dN"  'haskell-debug/previous
      ;; "dp"  'haskell-debug/previous
      "dr"  'haskell-debug/refresh
      "ds"  'haskell-debug/step
      "dt"  'haskell-debug/trace

      "E"    #'hasky-extensions
      "U"   'haskell-mode-find-uses
      "rb" 'hlint-refactor-refactor-buffer
      "rr" 'hlint-refactor-refactor-at-point)

    (general-define-key
     :states 'normal
     :keymaps 'haskell-debug-mode-map
     "RET" 'haskell-debug/select
     "a" 'haskell-debug/abandon
     "b" 'haskell-debug/break-on-function
     "c" 'haskell-debug/continue
     "d" 'haskell-debug/delete
     "n" 'haskell-debug/next
     "N" 'haskell-debug/previous
     "p" 'haskell-debug/previous
     "r" 'haskell-debug/refresh
     "s" 'haskell-debug/step
     "t" 'haskell-debug/trace)

    ;; Switch back to editor from REPL
    (tl/set-leader-keys-for-mode 'haskell-interactive-mode
      "ss"  'haskell-interactive-switch-back)
    (tl/set-leader-keys-for-mode 'intero-repl-mode
      "ss"  'intero-repl-switch-back)

    (tl/set-leader-keys-for-mode 'haskell-cabal
      "C"  'haskell-compile)

    ;; Cabal-file bindings
    (tl/set-leader-keys-for-mode 'haskell-cabal-mode
      ;; "="   'haskell-cabal-subsection-arrange-lines ;; Does a bad job, 'gg=G' works better
      "d"   'haskell-cabal-add-dependency
      "b"   'haskell-cabal-goto-benchmark-section
      "e"   'haskell-cabal-goto-executable-section
      "t"   'haskell-cabal-goto-test-suite-section
      "m"   'haskell-cabal-goto-exposed-modules
      "l"   'haskell-cabal-goto-library-section
      "n"   'haskell-cabal-next-subsection
      "p"   'haskell-cabal-previous-subsection
      "sc"  'haskell-interactive-mode-clear
      "ss"  'tl/haskell-interactive-bring
      "sS"  'haskell-interactive-switch
      "N"   'haskell-cabal-next-section
      "P"   'haskell-cabal-previous-section
      "f"   'haskell-cabal-find-or-create-source-file)

    (with-eval-after-load "evil"
      ;; Make "RET" behaviour in REPL saner
      (evil-define-key 'insert haskell-interactive-mode-map
        (kbd "RET") 'haskell-interactive-mode-return)

      (evil-define-key 'normal haskell-interactive-mode-map
        (kbd "RET") 'haskell-interactive-mode-return))


    (define-key haskell-mode-map (kbd "C-c v c") 'haskell-cabal-visit-file)

    (setq tl/haskell-mode-key-chord-map (make-sparse-keymap))
    (define-key tl/haskell-mode-key-chord-map (kbd "e") 'haskell-indent-insert-equal)
    (define-key tl/haskell-mode-key-chord-map (kbd "=") 'haskell-indent-insert-equal)
    (define-key tl/haskell-mode-key-chord-map (kbd "g") 'haskell-indent-insert-guard)
    (define-key tl/haskell-mode-key-chord-map (kbd "|") 'haskell-indent-insert-guard)
    (define-key tl/haskell-mode-key-chord-map (kbd "o") 'haskell-indent-insert-otherwise)
    (define-key tl/haskell-mode-key-chord-map (kbd "w") 'haskell-indent-insert-where)
    (define-key tl/haskell-mode-key-chord-map (kbd ".") 'haskell-indent-align-guards-and-rhs)
    (define-key tl/haskell-mode-key-chord-map (kbd ">") 'haskell-indent-put-region-in-literate)
    (define-key tl/haskell-mode-key-chord-map (kbd "l") 'tl/pop-haskell-process-log-buffer)
    (define-key tl/haskell-mode-key-chord-map (kbd "y") 'tl/pop-yesod-devel-buffer)
    (define-key tl/haskell-mode-key-chord-map (kbd "u") (lambda () (interactive) (insert "undefined")))

    ;; keymap for documentation
    (setq tl/haskell-mode-doc-map (make-sparse-keymap))
    (define-key tl/haskell-mode-doc-map (kbd "i") 'haskell-process-do-info) ; inferior-haskell-info
    (define-key tl/haskell-mode-doc-map (kbd "C-i") 'haskell-process-do-info)
    (define-key tl/haskell-mode-doc-map (kbd "t") 'haskell-process-do-type) ; inferior-haskell-type
    (define-key tl/haskell-mode-doc-map (kbd "C-t") 'haskell-process-do-type)
    (define-key tl/haskell-mode-doc-map (kbd "a") 'helm-ghc-browse-document)
    (define-key tl/haskell-mode-doc-map (kbd "C-a") 'helm-ghc-browse-document)
    (define-key tl/haskell-mode-doc-map (kbd "h") 'haskell-hoogle)
    (define-key tl/haskell-mode-doc-map (kbd "d") 'inferior-haskell-find-haddock)
    (define-key tl/haskell-mode-doc-map (kbd "C-d") 'inferior-haskell-find-haddock))

  (with-eval-after-load 'flycheck
    (setq flycheck-ghc-language-extensions '("DeriveFunctor"
                                             "DeriveDataTypeable"
                                             "DeriveFoldable"
                                             "DeriveTraversable"
                                             "TemplateHaskell"))
    ;; (require 'flycheck-hdevtools nil t) ; not works with cabal sandbox for now
    ;; flycheck-haskell: Improved Haskell support for Flycheck
    (add-hook 'flycheck-mode-hook #'flycheck-haskell-setup)))

;;; ghc-mod
;; install ghc-mod
;; % cabal update
;; % cabal install ghc-mod
(autoload 'ghc-init "ghc" nil t)
(autoload 'ghc-debug "ghc" nil t)

;; initial syntax check with hlint not ghc use `ghc-toggle-check-command' switch them.
(setq ghc-check-command t)

;; http://www.haskell.org/ghc/docs/latest/html/users_guide/options-sanity.html
;; note: it will not take effect until ghc-modi process restart
(setq ghc-ghc-options '("-fno-warn-unused-binds"
                        "-fno-warn-unused-do-bind"
                        "-fno-warn-unused-matches"
                        "-fno-warn-warnings-deprecations"
                        "-fno-warn-missing-signatures"
                        "-fno-warn-unused-imports"))
;; TODO: which will cause ghc-check-syntax fail
;; (setq ghc-hlint-options '("--ignore=Use camelCase"
;;                           "--ignore=Unused LANGUAGE pragma"))


(defun tl-haskell//setup-dante ()
  (dante-mode +1)
  (dolist (mode haskell-modes)
    (tl/set-leader-keys-for-mode mode
      "ht" 'dante-type-at
      "hT" 'spacemacs-haskell//dante-insert-type
      "hi" 'dante-info
      "rs" 'dante-auto-fix
      "se" 'dante-eval-block
      "sr" 'dante-restart)))

(defun tl-haskell//setup-intero ()
  (intero-mode +1)
  (dolist (mode haskell-modes)
    (tl/set-leader-keys-for-mode mode
      "hi" 'intero-info
      "ht" 'intero-type-at
      "hT" 'haskell-intero/insert-type
      "rs" 'intero-apply-suggestions
      "sb" 'intero-repl-load))

  (dolist (mode (cons 'haskell-cabal-mode haskell-modes))
    (tl/set-leader-keys-for-mode mode
      "sc"  nil
      "ss"  'haskell-intero/display-repl
      "sS"  'haskell-intero/pop-to-repl))

  (dolist (mode (append haskell-modes '(haskell-cabal-mode intero-repl-mode)))
    (tl/declare-prefix-for-mode mode "mi" "haskell/intero")
    (tl/set-leader-keys-for-mode mode
      "ic"  'intero-cd
      "id"  'intero-devel-reload
      "ik"  'intero-destroy
      "il"  'intero-list-buffers
      "ir"  'intero-restart
      "it"  'intero-targets))

  (evil-define-key '(insert normal) intero-mode-map
    ;; TODO: replace it with smart-jump
    (kbd "M-.") 'intero-goto-definition))


(defun tl-haskell//setup-ghc-mod ()
  (ghc-init)
  (dolist (mode haskell-modes)
    (tl/declare-prefix-for-mode mode "mm" "haskell/ghc-mod")
    (tl/set-leader-keys-for-mode mode
      "mt" 'ghc-insert-template-or-signature
      "mu" 'ghc-initial-code-from-signature
      "ma" 'ghc-auto
      "mf" 'ghc-refine
      "me" 'ghc-expand-th
      "mn" 'ghc-goto-next-hole
      "mp" 'ghc-goto-prev-hole
      "m>" 'ghc-make-indent-deeper
      "m<" 'ghc-make-indent-shallower
      "hi" 'ghc-show-info
      "ht" 'ghc-show-type)))

;;; haskell mode hook
(add-hook 'haskell-mode-local-vars-hook #'#'tl-haskell//setup-completion-backend)
(add-hook 'haskell-mode-hook 'tl/haskell-mode-setup)
(defun tl/haskell-mode-setup ()
  ;; (let ((checkers '(haskell-ghc  haskell-stack-ghc)))
  ;;   (if (boundp 'flycheck-disabled-checkers)
  ;;       (dolist (checker checkers)
  ;;         (add-to-list 'flycheck-disabled-checkers checker))
  ;;     (setq flycheck-disabled-checkers checkers)))

  ;; use only internal indentation system from haskell
  (if (fboundp 'electric-indent-local-mode)
      (electric-indent-local-mode -1))

  (haskell-doc-mode +1)
  (haskell-indentation-mode +1)

  ;; enable our level computation
  (setq outline-level 'tl/outline-level)
  (outline-minor-mode t)
  ;; initially hide all but the headers
  ;; (hide-body)
  (subword-mode +1)
  ;; (capitalized-words-mode +1)

  ;; it doesn’t play very well with evil
  ;; (when (fboundp 'structured-haskell-mode)
  ;;   (structured-haskell-mode t))

  (smartparens-mode -1)
  (smartparens-strict-mode -1)

  ;; https://github.com/commercialhaskell/hindent
  (when (fboundp 'hindent-mode)
    (hindent-mode +1))

  ;; lambda symbol can safely replace '\' because they are the same length and it wont screw up indentation
  (and (fboundp 'decode-char) ; prefer single-width Unicode font for lambda
       (push (cons "\\" (decode-char 'ucs 955)) prettify-symbols-alist))
  (prettify-symbols-mode +1)

  (when (fboundp 'key-chord-define)
    (key-chord-define haskell-mode-map ".x" tl/haskell-mode-key-chord-map))
  ;; (flyspell-prog-mode) ; can not work with key-chord
  (setq evil-auto-indent nil)
  ;; smartparens-mode
  ;; (smartparens-mode 1)
  ;; (show-smartparens-mode 1)
  ;; (setq sp-pair-list
  ;;       '(("\\\"" . "\\\"")
  ;;         ("{-" . "-}")
  ;;         ("\"" . "\"")
  ;;         ("(" . ")")
  ;;         ("[" . "]")
  ;;         ("{" . "}")
  ;;         ("`" . "`")))

  (define-key haskell-mode-map (kbd "C-c C-d") tl/haskell-mode-doc-map)
  (define-key haskell-mode-map (kbd "C-M-x") 'inferior-haskell-send-decl)
  (define-key haskell-mode-map (kbd "C-x C-e") 'inferior-haskell-send-decl)
  (define-key haskell-mode-map (kbd "C-c |") 'haskell-indent-insert-guard)
  (define-key haskell-mode-map (kbd "C-j") 'newline)
  ;; (define-key haskell-mode-map (kbd "C-j") 'haskell-newline-and-indent)
  (define-key haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
  ;; Load the current file (and make a session if not already made).
  (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-file)
  (define-key haskell-mode-map [f5] 'haskell-process-load-file)
  ;; “Bring” the REPL, hiding all other windows apart from the source and the REPL.
  (define-key haskell-mode-map (kbd "C-`") 'haskell-interactive-bring)
  (define-key haskell-mode-map (kbd "C-c C-c") 'haskell-compile)
  ;; Interactively choose the Cabal command to run.
  ;; (define-key haskell-mode-map (kbd "C-c c") 'haskell-process-cabal) ; M-x hpc

  (define-key haskell-mode-map (kbd "C-c C-v") 'tl/haskell-check)

  ;; Contextually do clever things on the space key, in particular:
  ;;   1. Complete imports, letting you choose the module name.
  ;;   2. Show the type of the symbol after the space.
  ;; Jump to the imports. Keep tapping to jump between import groups. C-u C-c I to jump back again.
  (define-key haskell-mode-map (kbd "C-c I") 'haskell-navigate-imports)

  ;; Jump to the definition of the current symbol.
  ;; (define-key haskell-mode-map (kbd "M-.") 'haskell-mode-tag-find) ; the `find-tag' + `ido' is better enough

  ;; Interactive block indentation
  (define-key haskell-mode-map (kbd "C-,") 'haskell-move-nested-left)
  (define-key haskell-mode-map (kbd "C-.") 'haskell-move-nested-right)
  (define-key haskell-mode-map (kbd "M-.") 'haskell-mode-jump-to-def-or-tag)
  (define-key haskell-mode-map (kbd "C-c h") 'haskell-hoogle))

(add-hook 'haskell-interactive-mode-hook 'tl/haskell-interactive-mode-setup)
(defun tl/haskell-interactive-mode-setup ()
  ;; (modify-syntax-entry ?_ "w")
  (define-key haskell-interactive-mode-map (kbd "C-u") 'haskell-interactive-mode-kill-whole-line)
  ;; (define-key haskell-interactive-mode-map (kbd "C-w") 'backward-kill-word)
  ;; (define-key haskell-interactive-mode-map (kbd "TAB") 'haskell-interactive-mode-tab)
  ;; (define-key haskell-interactive-mode-map (kbd "C-j") nil)
  (define-key haskell-interactive-mode-map (kbd "C-c C-l") 'haskell-interactive-mode-clear))

;;; completion backend
;; TODO: ghc-mode have switch to [[https://github.com/haskell/haskell-ide-engine][haskell/haskell-ide-engine]]
(defun tl-haskell//setup-ghc-mod ()
  (ghc-init)
  (tl/declare-prefix-for-mode 'haskell-mode "mm" "haskell/ghc-mod")
  (tl/set-leader-keys-for-mode 'haskell-mode
    "mt" 'ghc-insert-template-or-signature
    "mu" 'ghc-initial-code-from-signature
    "ma" 'ghc-auto
    "mf" 'ghc-refine
    "me" 'ghc-expand-th
    "mn" 'ghc-goto-next-hole
    "mp" 'ghc-goto-prev-hole
    "m>" 'ghc-make-indent-deeper
    "m<" 'ghc-make-indent-shallower
    "hi" 'ghc-show-info
    "ht" 'ghc-show-type))



;; this gets called by outline to determine the level. Just use the length of the whitespace
(defun tl/outline-level ()
  (let (buffer-invisibility-spec)
    (save-excursion
      (skip-chars-forward "\t ")
      (current-column))))


;;;
(defun tl/pop-haskell-process-log-buffer ()
  (interactive)
  (let ((buf (get-buffer "*haskell-process-log*")))
    (if buf
        (pop-to-buffer buf)
      (message "Can not find haskell process log buffer. Have you started inferior?"))))


;;; indent
;; turn-on-haskell-indent
;; turn-on-haskell-simple-indent
;; turn-on-haskell-indentation   ; Improved variation of turn-on-haskell-indent indentation mode.
;; Note that the three indentation modules are mutually exclusive - add at most one. In preference for the more advanced.
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation) ; el-get default install `turn-on-haskell-indentation'

;; Turn off haskell-indentation-modes. They are incompatible with structured-haskell-mode, which  has its own
;; indentation functionality.
(defun tl/haskell-indent-init ()
  (setq haskell-indent-thenelse 2)
  (setq haskell-indent-after-keywords '(("where" 2 0)
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
(defun tl/haskell-indent-init ()
  (setq haskell-indentation-layout-offset 2
        haskell-indentation-left-offset 2
        haskell-indentation-where-pre-offset 2
        haskell-indentation-where-post-offset 2))

(tl/haskell-indent-init)

;;; hindent
(with-eval-after-load 'hindent
  (tl/set-leader-keys-for-mode 'haskell-mode "f" 'hindent-reformat-decl))


;;; Check
(defun tl/haskell-check (arg)
  "Check a Haskell file (default current buffer's file).
  if arg is not equal to 1, ignore `haskell-saved-check-command'
  See also`haskell-check'."
  (interactive "p")
  (if (= arg 1)
      (call-interactively 'haskell-check)
    (let ((haskell-saved-check-command nil))
      (call-interactively 'haskell-check))))

;;; navigation
;; $ cabal install hasktags
;; Run M-x haskell-process-generate-tags to generate the TAGS file of the current session directory.
;; You can now use M-. on a name in a Haskell buffer which will jump directly to its definition.
(setq haskell-tags-on-save t)

;;; cabal
;; Useful to have these keybindings for .cabal files, too.
(defun tl/haskell-cabal-hook ()
  (define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-compile)
  ;; (define-key haskell-cabal-mode-map (kbd "C-c c") 'haskell-process-cabal) ; ; M-x hpc
  (define-key haskell-cabal-mode-map (kbd "C-`") 'haskell-interactive-bring)
  (define-key haskell-cabal-mode-map [?\C-c ?\C-z] 'haskell-interactive-switch))

(add-hook 'haskell-cabal-mode-hook 'tl/haskell-cabal-hook)


;;; hamlet-mode
;; installed with el-get
;; (require 'hamlet-mode nil t)

;;; structured-haskell-mode
;; https://github.com/chrisdone/structured-haskell-mode
;; git clone git@github.com:chrisdone/structured-haskell-mode.git
;; $ cd structured-haskell-mode
;; $ cabal install
;; (add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/structured-haskell-mode/elisp"))

;; el-get install structured-haskell-mode
;; (require 'shm nil t)
(autoload 'shm/case-split "shm-case-split" "Prompt for a type then do a case split based on it" t)
(with-eval-after-load "shm"
  (define-key shm-map (kbd "C-c S") 'shm/case-split) ; "C-c C-s"
  (define-key shm-map (kbd "M-a") 'shm/goto-parent)
  (define-key shm-map (kbd "M-e") 'shm/goto-parent-end))

(defun tl/shm-evil-join ()
  "send the node of the next line up one line."
  (interactive)
  (evil-next-line 1)
  (evil-first-non-blank)
  (evil-backward-char 1 nil t)
  (shm/delete-indentation))

;; see also https://github.com/phenguin/dotfiles/blob/master/emacs.d.linkme/config/lang/haskell-config.el
(with-eval-after-load 'shm
  (with-eval-after-load 'evil
    (define-key shm-map (kbd "C-k") nil)
    (define-key shm-map (kbd "C-j") nil)

    (evil-define-key 'normal shm-map
      (kbd "TAB") 'shm/tab
      (kbd "<backtab>") 'shm/backtab
      (kbd "D") 'shm/kill-line
      (kbd "R") 'shm/raise
      (kbd "P") 'shm/yank
      (kbd "RET") 'shm/newline-indent ; 'shm/ret-proxy
      (kbd "M-RET") 'evil-ret
      (kbd "M-k") 'sp-splice-sexp-killing-backward
      (kbd "M-j") 'sp-splice-sexp-killing-forward
      (kbd "M-l") 'sp-forward-slurp-sexp
      (kbd "M-h") 'sp-forward-barf-sexp
      (kbd "M-H") 'sp-backward-slurp-sexp
      (kbd "M-L") 'sp-backward-barf-sexp
      (kbd "s") 'sp-splice-sexp
      (kbd "S") 'shm/split-list
      (kbd "M-R") 'sp-raise-sexp
      (kbd "J") 'tl/shm-evil-join ; 'sp-join-sexp
      (kbd ")") 'shm/forward-node
      (kbd "(") 'shm/backward-node
      (kbd "M-(") 'sp-backward-up-sexp
      (kbd "M-)") 'sp-down-sexp
      (kbd "C-(") 'sp-backward-down-sexp
      (kbd "C-)") 'sp-up-sexp
      (kbd "DEL") 'shm/del)

    (evil-define-key 'insert shm-map
      (kbd "M-RET") 'evil-ret
      (kbd "RET") 'shm/newline-indent ; 'shm/ret-proxy
      (kbd "M-k") 'sp-splice-sexp-killing-backward
      (kbd "M-j") 'sp-splice-sexp-killing-forward
      (kbd "M-l") 'sp-forward-slurp-sexp
      (kbd "M-h") 'sp-forward-barf-sexp
      (kbd "M-H") 'sp-backward-slurp-sexp
      (kbd "M-L") 'sp-backward-barf-sexp)

    (evil-define-key 'operator shm-map
      (kbd ")") 'shm/forward-node
      (kbd "(") 'shm/backward-node
      (kbd "M-(") 'sp-backward-up-sexp
      (kbd "M-)") 'sp-down-sexp
      (kbd "C-(") 'sp-backward-down-sexp
      (kbd "C-)") 'sp-up-sexp)

    (evil-define-key 'motion shm-map
      (kbd ")") 'shm/forward-node
      (kbd "(") 'shm/backward-node
      (kbd "M-(") 'sp-backward-up-sexp
      (kbd "M-)") 'sp-down-sexp
      (kbd "C-(") 'sp-backward-down-sexp
      (kbd "C-)") 'sp-up-sexp)

    (evil-define-key 'emacs shm-map
      (kbd "M-k") 'sp-splice-sexp-killing-backward
      (kbd "M-j") 'sp-splice-sexp-killing-forward
      (kbd "M-l") 'sp-forward-slurp-sexp
      (kbd "M-h") 'sp-forward-barf-sexp
      (kbd "M-H") 'sp-backward-slurp-sexp
      (kbd "M-L") 'sp-backward-barf-sexp)))

;;;
;; https://github.com/chrisdone/chrisdone-emacs/blob/master/config/haskell.el
(defun tl/haskell-who-calls (&optional prompt)
  "Grep the codebase to see who uses the symbol at point."
  (interactive "P")
  (let ((sym (if prompt
                 (read-from-minibuffer "Look for: ")
               (haskell-ident-at-point))))
    (let ((existing (get-buffer "*who-calls*")))
      (when existing
        (kill-buffer existing)))
    (cond
     ;; Use grep
     (nil (let ((buffer
                 (grep-find (format "cd %s && find . -name '*.hs' -exec grep -inH -e %s {} +"
                                    (haskell-session-current-dir (haskell-session))
                                    sym))))
            (with-current-buffer buffer
              (rename-buffer "*who-calls*")
              (switch-to-buffer-other-window buffer))))
     (t (ag sym (haskell-session-current-dir (haskell-session)))))))

;;; align regexp
;; (require 'align nil t)
(with-eval-after-load 'align
  (add-to-list 'align-rules-list
               '(haskell-types
                 (regexp . "\\(\\s-+\\)\\(::\\|∷\\|=\\)\\s-+")
                 (modes quote (haskell-mode haskell-c-mode literate-haskell-mode))))
  (add-to-list 'align-rules-list
               '(haskell-assignment
                 (regexp . "\\(\\s-+\\)=\\s-+")
                 (modes quote (haskell-mode haskell-c-mode literate-haskell-mode))))
  (add-to-list 'align-rules-list
               '(haskell-arrows
                 (regexp . "\\(\\s-+\\)\\(->\\|→\\)\\s-+")
                 (modes quote (haskell-mode haskell-c-mode literate-haskell-mode))))
  (add-to-list 'align-rules-list
               '(haskell-left-arrows
                 (regexp . "\\(\\s-+\\)\\(<-\\|←\\)\\s-+")
                 (modes quote (haskell-mode haskell-c-mode literate-haskell-mode)))))

;;;
;; http://hub.darcs.net/ivanm/emacs-d/raw-file/site-lisp/haskell-settings.el
;; Based upon http://www.serpentine.com/blog/2007/10/09/using-emacs-to-insert-scc-annotations-in-haskell-code/
;; SCC: set cost centre
(defun toggle-scc-at-point (&optional arg)
  "Insert or kill (with universal-argument) an SCC annotation at
point."
  (interactive "P")
  (if (equal arg nil)
      (insert-scc-at-point)
    (kill-scc-at-point)))

(defun insert-scc-at-point ()
  "Insert an SCC annotation at point."
  (interactive)
  (if (or (looking-at "\\b\\|[ \t]\\|$") (and (not (bolp))
                                              (save-excursion
                                                (forward-char -1)
                                                (looking-at "\\b\\|[ \t]"))))
      (let ((space-at-point (looking-at "[ \t]")))
        (unless (and (not (bolp)) (save-excursion
                                    (forward-char -1)
                                    (looking-at "[ \t]")))
          (insert " "))
        (insert "{-# SCC \"\" #-}")
        (unless space-at-point
          (insert " "))
        (forward-char (if space-at-point -5 -6)))
    (error "Not over an area of whitespace")))

(defun kill-scc-at-point ()
  "Kill the SCC annotation at point."
  (interactive)
  (save-excursion
    (let ((old-point (point))
          (scc "\\({-#[ \t]*SCC \"[^\"]*\"[ \t]*#-}\\)[ \t]*"))
      (while (not (or (looking-at scc) (bolp)))
        (forward-char -1))
      (if (and (looking-at scc)
               (<= (match-beginning 1) old-point)
               (> (match-end 1) old-point))
          (kill-region (match-beginning 0) (match-end 0))
        (error "No SCC at point")))))


(defun tl/haskell-interactive-bring ()
  "Bring up the interactive mode for this session without
         switching to it."
  (interactive)
  (let* ((session (haskell-session))
         (buffer (haskell-session-interactive-buffer session)))
    (display-buffer buffer)))


;; Intero functions
(defun haskell-intero/insert-type ()
  (interactive)
  (intero-type-at :insert))

(defun haskell-intero/display-repl (&optional prompt-options)
  (interactive "P")
  (let ((buffer (intero-repl-buffer prompt-options)))
    (unless (get-buffer-window buffer 'visible)
      (display-buffer buffer))))

(defun haskell-intero/pop-to-repl (&optional prompt-options)
  (interactive "P")
  (pop-to-buffer (intero-repl-buffer prompt-options)))

(defun haskell-intero//preserve-focus (f &rest args)
  (let ((buffer (current-buffer)))
    (apply f args)
    (pop-to-buffer buffer)))

;; (with-eval-after-load "intero"
;;   (advice-add 'intero-repl-load :around #'haskell-intero//preserve-focus))

;; (with-eval-after-load 'intero
;;   (flycheck-add-next-checker 'intero '(warning . haskell-hlint))
;;   (flycheck-add-next-checker 'intero '(warning . haskell-stack-ghc)))

;;; misc
;; (require 'yesod-devel-mode nil t)
(autoload 'yesod-devel-mode "yesod-devel-mode" "Yesod devel mode." t)

(defun tl/pop-yesod-devel-buffer ()
  (interactive)
  (let ((buf (get-buffer "*yesod-devel*")))
    (if buf
        (pop-to-buffer buf)
      (message "Can not find yesod devel buffer. Have you started it?"))))


(provide '50haskell)
;;; 50haskell.el ends here
