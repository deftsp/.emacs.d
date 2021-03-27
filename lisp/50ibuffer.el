;;; 50ibuffer.el ---                                 -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>
(defvar tl//ibuffer-gnus-styled-groups
  '(("default"
     ("Dired" (or (mode . dired-mode)))
     ("erc" (mode . erc-mode))
     ("svn" (or (mode . svn-status-mode)
                (mode . svn-log-edit-mode)
                (name . "^\\*svn-")
                (name . "^\\*vc\\*$")
                (name . "^\\*Annotate")
                (name . "^\\*vc-")))
     ("Haskell" (or (mode . haskell-mode )
                    (mode . haskell-interactive-mode)
                    (mode . hamlet-mode)
                    (mode . haskell-cabal-mode)
                    (name . "*haskell-process-log*")))
     ("Scheme" (or (mode . scheme-mode )
                   (mode . inferior-scheme-mode)))
     ("CC Mode" (or (mode . c-mode)
                    (mode . c++-mode)
                    (mode . objc-mode)))
     ("Lua" (or (mode . lua-mode)
                (name . "^\\*lua\\*$")))
     ("Emacs" (or (name . "^\\*scratch\\*$")
                  (name . "^\\*Messages\\*$")
                  (name . "^\\*Help\\*$")
                  (name . "^\\*info\\*$")
                  (name . "^\\*Occur\\*$")
                  (name . "^\\*grep\\*$")
                  (name . "^\\*Compile-Log\\*$")
                  (name . "^\\*Backtrace\\*$")
                  (name . "^\\*Process List\\*$")
                  (name . "^\\*gud\\*$")
                  (name . "^\\*Man")
                  (name . "^\\*WoMan")
                  (name . "^\\*Kill Ring\\*$")
                  (name . "^\\*Completions\\*$")
                  (name . "^\\*tramp")
                  (name . "^\\*shell\\*$")
                  (name . "^\\*compilation\\*$")))
     ("Elisp" (or (mode . emacs-lisp-mode)
                  (filename . "/Applications/Emacs.app")
                  (filename . "/bin/emacs")))
     ("Lisp source" (mode . lisp-mode))
     ("Ivy-Occur" (mode . ivy-occur-grep-mode))
     ("Agenda" (or (name . "^\\*Calendar\\*$")
                   (name . "^diary$")
                   (name . "^\\*Agenda")
                   (name . "^\\*org-")
                   (name . "^\\*Org")
                   (mode . org-mode)))
     ("Muse" (mode . muse-mode))
     ("Sawfish" (mode . sawfish-mode))
     ("Gnus" (or (mode . message-mode)
                 (mode . bbdb-mode)
                 (mode . mail-mode)
                 (mode . gnus-group-mode)
                 (mode . gnus-summary-mode)
                 (mode . gnus-article-mode)
                 (name . "^\\.bbdb$")
                 (name . "^\\.newsrc-dribble")))
     ("Latex" (or (mode . latex-mode)
                  (mode . LaTeX-mode)
                  (mode . bibtex-mode)
                  (mode . reftex-mode))))))

(defun tl//evilified-binding ()
  (general-define-key
   :states 'normal
   :keymaps 'ibuffer-mode-map
   "=" 'ibuffer-diff-with-file
   "J" 'ibuffer-jump-to-buffer
   "M-g" 'ibuffer-jump-to-buffer
   "M-s a C-s" 'ibuffer-do-isearch
   "M-s a M-C-s" 'ibuffer-do-isearch-regexp
   "M-s a C-o" 'ibuffer-do-occur

   ;; mark
   "m" 'ibuffer-mark-forward
   "~" 'ibuffer-toggle-marks
   "u" 'ibuffer-unmark-forward
   "DEL" 'ibuffer-unmark-backward
   "M-DEL" 'ibuffer-unmark-all
   "* *" 'ibuffer-unmark-all
   "* c" 'ibuffer-change-marks
   "U" 'ibuffer-unmark-all-marks
   "* M" 'ibuffer-mark-by-mode
   "* m" 'ibuffer-mark-modified-buffers
   "* u" 'ibuffer-mark-unsaved-buffers
   "* s" 'ibuffer-mark-special-buffers
   "* r" 'ibuffer-mark-read-only-buffers
   "* /" 'ibuffer-mark-dired-buffers
   "* e" 'ibuffer-mark-dissociated-buffers
   "* h" 'ibuffer-mark-help-buffers
   "* z" 'ibuffer-mark-compressed-file-buffers
   "." 'ibuffer-mark-old-buffers

   "d" 'ibuffer-mark-for-delete
   "x" 'ibuffer-do-kill-on-deletion-marks

   ;; immediate operations
   "gj" 'ibuffer-forward-line
   "gk" 'ibuffer-backward-line
   "gr" 'ibuffer-update

   "}" 'ibuffer-forward-next-marked
   "{" 'ibuffer-backwards-next-marked
   "M-}" 'ibuffer-forward-next-marked
   "M-{" 'ibuffer-backwards-next-marked

   "gR" 'ibuffer-redisplay
   "gr" 'ibuffer-update

   "`" 'ibuffer-switch-format
   "-" 'ibuffer-add-to-tmp-hide
   "+" 'ibuffer-add-to-tmp-show
   "X" 'ibuffer-bury-buffer
   "," 'ibuffer-toggle-sorting-mode
   "oi" 'ibuffer-invert-sorting
   "oa" 'ibuffer-do-sort-by-alphabetic
   "ov" 'ibuffer-do-sort-by-recency
   "os" 'ibuffer-do-sort-by-size
   "of" 'ibuffer-do-sort-by-filename/process
   "om" 'ibuffer-do-sort-by-major-mode

   "s" '(:ignore t :which-key "filter")
   "s <RET>" 'ibuffer-filter-by-mode
   "sm" 'ibuffer-filter-by-used-mode
   "sM" 'ibuffer-filter-by-derived-mode
   "sn" 'ibuffer-filter-by-name
   "s*" 'ibuffer-filter-by-starred-name
   "sf" 'ibuffer-filter-by-filename
   "sb" 'ibuffer-filter-by-basename
   "s." 'ibuffer-filter-by-file-extension
   "s<" 'ibuffer-filter-by-size-lt
   "s>" 'ibuffer-filter-by-size-gt
   "si" 'ibuffer-filter-by-modified
   "sv" 'ibuffer-filter-by-visiting-file
   "sc" 'ibuffer-filter-by-content
   "se" 'ibuffer-filter-by-predicate

   "sr" 'ibuffer-switch-to-saved-filters
   "sa" 'ibuffer-add-saved-filters
   "sx" 'ibuffer-delete-saved-filters
   "sd" 'ibuffer-decompose-filter
   "ss" 'ibuffer-save-filters
   "sp" 'ibuffer-pop-filter
   "s!" 'ibuffer-negate-filter
   "st" 'ibuffer-exchange-filters
   "s TAB" 'ibuffer-exchange-filters
   "so" 'ibuffer-or-filter
   "s|" 'ibuffer-or-filter
   "s&" 'ibuffer-and-filter
   "sg" 'ibuffer-filters-to-filter-group
   "sP" 'ibuffer-pop-filter-group
   "sD" 'ibuffer-decompose-filter-group
   "s/" 'ibuffer-filter-disable

   "C-j" 'ibuffer-forward-filter-group
   "M-n" 'ibuffer-forward-filter-group
   "]]" 'ibuffer-forward-filter-group
   "\t" 'ibuffer-forward-filter-group
   "M-p" 'ibuffer-backward-filter-group
   "C-k" 'ibuffer-backward-filter-group
   "[[" 'ibuffer-backward-filter-group
   [backtab] 'ibuffer-backward-filter-group
   "M-j" 'ibuffer-jump-to-filter-group
   "gx" 'ibuffer-kill-line
   "C-y" 'ibuffer-yank
   "sS" 'ibuffer-save-filter-groups
   "sR" 'ibuffer-switch-to-saved-filter-groups
   "sX" 'ibuffer-delete-saved-filter-groups
   "s\\" 'ibuffer-clear-filter-groups

   "%n" 'ibuffer-mark-by-name-regexp
   "%m" 'ibuffer-mark-by-mode-regexp
   "%f" 'ibuffer-mark-by-file-name-regexp
   "%g" 'ibuffer-mark-by-content-regexp
   "%L" 'ibuffer-mark-by-locked

   "C-t" 'ibuffer-visit-tags-table

   "|" 'ibuffer-do-shell-command-pipe
   "!" 'ibuffer-do-shell-command-file
   "t" 'ibuffer-do-toggle-modified
   ;; marked operations
   "A" 'ibuffer-do-view
   "D" 'ibuffer-do-delete
   "E" 'ibuffer-do-eval
   "F" 'ibuffer-do-shell-command-file
   "I" 'ibuffer-do-query-replace-regexp
   "J"  'ibuffer-jump-to-buffer
   "K"  'ibuffer-do-kill-lines
   "H" 'ibuffer-do-view-other-frame
   "N" 'ibuffer-do-shell-command-pipe-replace
   "M" 'ibuffer-do-toggle-modified
   "O" 'ibuffer-do-occur
   "P" 'ibuffer-do-print
   "Q" 'ibuffer-do-query-replace
   "R" 'ibuffer-do-rename-uniquely
   "S" 'ibuffer-do-save
   "T" 'ibuffer-do-toggle-read-only
   "r" 'ibuffer-do-replace-regexp
   "V" 'ibuffer-do-revert
   "W" 'ibuffer-do-view-and-eval

   "K" 'ibuffer-do-kill-lines
   "yf" 'ibuffer-copy-filename-as-kill
   "yb" 'ibuffer-copy-buffername-as-kill

   "RET" 'ibuffer-visit-buffer
   "go" 'ibuffer-visit-buffer-other-window
   "C-o" 'ibuffer-visit-buffer-other-window-noselect
   ;; "M-o" 'ibuffer-visit-buffer-1-window
   "M-r" 'ibuffer-redisplay
   "gv" 'ibuffer-do-view
   "gV" 'ibuffer-do-view-horizontally

   ;; Quit
   "q" 'quit-window
   "ZZ" 'quit-window
   "ZQ" 'quit-window))

(use-package ibuffer
  :defer t
  :bind (("C-x C-b" . ibuffer))
  :init
  (setq ibuffer-shrink-to-minimum-size t
        ibuffer-always-show-last-buffer nil
        ibuffer-sorting-mode 'recency
        ibuffer-use-header-line t
        ibuffer-elide-long-columns t
        ibuffer-eliding-string "...")
  (setq ibuffer-formats '((mark modified read-only " "
                                (name 18 18 :left :elide) " "
                                (size 9 -1 :left) " "
                                (mode 16 16 :left :elide)
                                " " filename-and-process)
                          (mark " " (name 16 -1) " " filename)))
  (setq ibuffer-show-empty-filter-groups nil)
  ;; (setq ibuffer-saved-filter-groups tl//ibuffer-gnus-styled-groups)
  :config
  ;; "M-o" already for ace-window
  (define-key ibuffer-mode-map (kbd "M-o") nil)

  (tl//evilified-binding)

  (defface tl//ibuffer-filter-group-name-face
    '((((class color))
       :foreground "#a9d9d9" :background "#43586d" :weight bold)
      (t (:weight bold)))
    "face to ibuffer filter group name")

  (setq ibuffer-filter-group-name-face 'tl//ibuffer-filter-group-name-face)


  (use-package ibuffer-vc)

  ;; reverse the order of groups:
  ;; FIXME: SLOW!!!
  ;; (defadvice ibuffer-generate-filter-groups (after reverse-ibuffer-groups ()
  ;;                                                  activate)
  ;;   (setq ad-return-value (nreverse ad-return-value)))

  (defun tl//ibuffer-init-h ()
    (ibuffer-vc-set-filter-groups-by-vc-root)

    ;; (ibuffer-switch-to-saved-filter-groups
    ;;  "default")
    ;; (ibuffer-auto-mode +1)
    (unless (eq ibuffer-sorting-mode 'alphabetic)
      (ibuffer-do-sort-by-alphabetic)))

  (add-hook 'ibuffer-mode-hook 'tl//ibuffer-init-h))


;; (use-package ibuffer-projectile
;;   :after (projectile)
;;   :init
;;   (defun tl//ibufer-projectile-h ()
;;     (ibuffer-projectile-set-filter-groups)
;;     (unless (eq ibuffer-sorting-mode 'alphabetic)
;;       (ibuffer-do-sort-by-alphabetic)))

;;   (add-hook 'ibuffer-hook 'tl//ibufer-projectile-h))


(provide '50ibuffer)
