;;; 50haskell.el ---

;; Copyright (C) 2012  Shihpin Tseng

;; Keywords:

;;; Code:
;; (add-to-list 'load-path "~/.emacs.d/site-lisp/haskell-mode/")
;; (add-to-list 'Info-default-directory-list "~/.emacs.d/site-lisp/haskell-mode/")
;; (require 'haskell-mode-autoloads)

(defvar pl/haskell-mode-doc-map nil
  "Keymap for documentation commands. Bound to a prefix key.")

(defvar pl/haskell-mode-key-chord-map nil
  "Keymap for key chord prefix commands in haskell mode.")

(eval-after-load "haskell-mode"
  '(progn
     (setq haskell-process-log t
           haskell-font-lock-symbols t
           haskell-process-path-cabal (expand-file-name "~/.cabal/bin/cabal")
           haskell-process-type 'cabal-repl  ; 'cabal-dev
           haskell-notify-p t)
     (setq haskell-stylish-on-save nil) ; or use M-x haskell-mode-stylish-buffer to call `stylish-haskell'
     (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
     (add-hook 'haskell-mode-hook 'imenu-add-menubar-index)
     (add-hook 'haskell-mode-hook 'turn-on-haskell-decl-scan)

     (define-key haskell-mode-map (kbd "C-c v c") 'haskell-cabal-visit-file)

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
     (define-key pl/haskell-mode-key-chord-map (kbd "y") 'pl/pop-yesod-devel-buffer)
     (define-key pl/haskell-mode-key-chord-map (kbd "u") (lambda () (interactive) (insert "undefined")))

     ;; keymap for documentation
     (setq pl/haskell-mode-doc-map (make-sparse-keymap))
     (define-key pl/haskell-mode-doc-map (kbd "i") 'haskell-process-do-info) ; inferior-haskell-info
     (define-key pl/haskell-mode-doc-map (kbd "t") 'haskell-process-do-type) ; inferior-haskell-type
     (define-key pl/haskell-mode-doc-map (kbd "a") 'helm-ghc-browse-document)
     (define-key pl/haskell-mode-doc-map (kbd "C-a") 'helm-ghc-browse-document)
     (define-key pl/haskell-mode-doc-map (kbd "h") 'haskell-hoogle)
     (define-key pl/haskell-mode-doc-map (kbd "d") 'inferior-haskell-find-haddock)
     (define-key pl/haskell-mode-doc-map (kbd "C-d") 'inferior-haskell-find-haddock)))

(eval-after-load 'flycheck
  '(progn
     ;; (require 'flycheck-hdevtools nil t) ; not works with cabal sandbox for now
     (add-hook 'flycheck-mode-hook #'flycheck-haskell-setup)))

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
  ;; (capitalized-words-mode +1)
  (when (fboundp 'key-chord-define)
    (key-chord-define haskell-mode-map ".x" pl/haskell-mode-key-chord-map))
  ;; (flyspell-prog-mode) ; can not work with key-chord
  (setq evil-auto-indent nil)
  ;; smartparens-mode
  (smartparens-mode 1)
  (show-smartparens-mode 1)
  (setq sp-pair-list
        '(("\\\"" . "\\\"")
          ("{-" . "-}")
          ("\"" . "\"")
          ("(" . ")")
          ("[" . "]")
          ("{" . "}")
          ("`" . "`")))

  (define-key haskell-mode-map (kbd "C-c C-d") pl/haskell-mode-doc-map)
  (define-key haskell-mode-map (kbd "C-M-x") 'inferior-haskell-send-decl)
  (define-key haskell-mode-map (kbd "C-x C-e") 'inferior-haskell-send-decl)
  (define-key haskell-mode-map (kbd "C-c |") 'haskell-indent-insert-guard)
  (define-key haskell-mode-map (kbd "C-j") 'newline)
  ;; (define-key haskell-mode-map (kbd "C-j") 'haskell-newline-and-indent)
  (define-key haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
  ;; Load the current file (and make a session if not already made).
  (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-file)
  (define-key haskell-mode-map (kbd "C-c C-r") 'haskell-process-reload-file)
  (define-key haskell-mode-map [f5] 'haskell-process-load-or-reload)
  ;; “Bring” the REPL, hiding all other windows apart from the source and the REPL.
  (define-key haskell-mode-map (kbd "C-`") 'haskell-interactive-bring)
  (define-key haskell-mode-map (kbd "C-c C-c") 'haskell-compile)
  ;; Interactively choose the Cabal command to run.
  (define-key haskell-mode-map (kbd "C-c c") 'haskell-process-cabal)

  (define-key haskell-mode-map (kbd "C-c C-v") 'pl/haskell-check)

  ;; Contextually do clever things on the space key, in particular:
  ;;   1. Complete imports, letting you choose the module name.
  ;;   2. Show the type of the symbol after the space.
  (define-key haskell-mode-map (kbd "SPC") 'haskell-mode-contextual-space)
  ;; Jump to the imports. Keep tapping to jump between import groups. C-u C-c I to jump back again.
  (define-key haskell-mode-map (kbd "C-c I") 'haskell-navigate-imports)

  ;; Jump to the definition of the current symbol.
  ;; (define-key haskell-mode-map (kbd "M-.") 'haskell-mode-tag-find) ; the `find-tag' + `ido' is better enough

  ;; Interactive block indentation
  (define-key haskell-mode-map (kbd "C-,") 'haskell-move-nested-left)
  (define-key haskell-mode-map (kbd "C-.") 'haskell-move-nested-right)
  (define-key haskell-mode-map (kbd "M-.") 'haskell-mode-jump-to-def-or-tag)
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
      (message "Can not find haskell process log buffer. Have you started inferior?"))))


;;; indent
;; turn-on-haskell-indent
;; turn-on-haskell-simple-indent
;; turn-on-haskell-indentation   ; Improved variation of turn-on-haskell-indent indentation mode.
;; Note that the three indentation modules are mutually exclusive - add at most one. In preference for the more advanced.
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation) ; el-get default install `turn-on-haskell-indentation'
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

;;; Check
(defun pl/haskell-check (arg)
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
(defun pl/haskell-cabal-hook ()
  (define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-compile)
  (define-key haskell-cabal-mode-map (kbd "C-c c") 'haskell-process-cabal)
  (define-key haskell-cabal-mode-map (kbd "C-`") 'haskell-interactive-bring)
  (define-key haskell-cabal-mode-map [?\C-c ?\C-z] 'haskell-interactive-switch))

(add-hook 'haskell-cabal-mode-hook 'pl/haskell-cabal-hook)


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
(if (fboundp 'structured-haskell-mode)
    (add-hook 'haskell-mode-hook 'structured-haskell-mode))

;;;
;; https://github.com/chrisdone/chrisdone-emacs/blob/master/config/haskell.el
(defun pl/haskell-who-calls (&optional prompt)
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
(eval-after-load "align"
  '(progn (add-to-list 'align-rules-list
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
                         (modes quote (haskell-mode haskell-c-mode literate-haskell-mode))))))

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

;;; misc
;; (require 'yesod-devel-mode nil t)
(autoload 'yesod-devel-mode "yesod-devel-mode" "Yesod devel mode." t)

(defun pl/pop-yesod-devel-buffer ()
  (interactive)
  (let ((buf (get-buffer "*yesod-devel*")))
    (if buf
        (pop-to-buffer buf)
      (message "Can not find yesod devel buffer. Have you started it?"))))


(provide '50haskell)
;;; 50haskell.el ends here
