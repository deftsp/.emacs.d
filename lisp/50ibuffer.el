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
  (with-eval-after-load "evil-evilified-state"
    (evilified-state-evilify-map ibuffer-mode-map
      :mode ibuffe-mode
      :bindings
      (kbd "C-h") nil

      (kbd "=") 'ibuffer-diff-with-file
      (kbd "J") 'ibuffer-jump-to-buffer
      (kbd "M-g") 'ibuffer-jump-to-buffer
      (kbd "M-s a C-s") 'ibuffer-do-isearch
      (kbd "M-s a M-C-s") 'ibuffer-do-isearch-regexp
      (kbd "M-s a C-o") 'ibuffer-do-occur

      ;; mark
      (kbd "m") 'ibuffer-mark-forward
      (kbd "~") 'ibuffer-toggle-marks
      (kbd "u") 'ibuffer-unmark-forward
      (kbd "DEL") 'ibuffer-unmark-backward
      (kbd "M-DEL") 'ibuffer-unmark-all
      (kbd "* *") 'ibuffer-unmark-all
      (kbd "* c") 'ibuffer-change-marks
      (kbd "U") 'ibuffer-unmark-all-marks
      (kbd "* M") 'ibuffer-mark-by-mode
      (kbd "* m") 'ibuffer-mark-modified-buffers
      (kbd "* u") 'ibuffer-mark-unsaved-buffers
      (kbd "* s") 'ibuffer-mark-special-buffers
      (kbd "* r") 'ibuffer-mark-read-only-buffers
      (kbd "* /") 'ibuffer-mark-dired-buffers
      (kbd "* e") 'ibuffer-mark-dissociated-buffers
      (kbd "* h") 'ibuffer-mark-help-buffers
      (kbd "* z") 'ibuffer-mark-compressed-file-buffers
      (kbd ".") 'ibuffer-mark-old-buffers

      (kbd "d") 'ibuffer-mark-for-delete
      (kbd "x") 'ibuffer-do-kill-on-deletion-marks

      ;; immediate operations
      (kbd "gj") 'ibuffer-forward-line
      (kbd "gk") 'ibuffer-backward-line
      (kbd "gr") 'ibuffer-update

      (kbd "}") 'ibuffer-forward-next-marked
      (kbd "{") 'ibuffer-backwards-next-marked
      (kbd "M-}") 'ibuffer-forward-next-marked
      (kbd "M-{") 'ibuffer-backwards-next-marked

      (kbd "gR") 'ibuffer-redisplay
      (kbd "gr") 'ibuffer-update

      "`" 'ibuffer-switch-format
      "-" 'ibuffer-add-to-tmp-hide
      "+" 'ibuffer-add-to-tmp-show
      "X" 'ibuffer-bury-buffer
      (kbd ",") 'ibuffer-toggle-sorting-mode
      (kbd "o i") 'ibuffer-invert-sorting
      (kbd "o a") 'ibuffer-do-sort-by-alphabetic
      (kbd "o v") 'ibuffer-do-sort-by-recency
      (kbd "o s") 'ibuffer-do-sort-by-size
      (kbd "o f") 'ibuffer-do-sort-by-filename/process
      (kbd "o m") 'ibuffer-do-sort-by-major-mode

      (kbd "s RET") 'ibuffer-filter-by-mode
      (kbd "s m") 'ibuffer-filter-by-used-mode
      (kbd "s M") 'ibuffer-filter-by-derived-mode
      (kbd "s n") 'ibuffer-filter-by-name
      (kbd "s *") 'ibuffer-filter-by-starred-name
      (kbd "s f") 'ibuffer-filter-by-filename
      (kbd "s b") 'ibuffer-filter-by-basename
      (kbd "s .") 'ibuffer-filter-by-file-extension
      (kbd "s <") 'ibuffer-filter-by-size-lt
      (kbd "s >") 'ibuffer-filter-by-size-gt
      (kbd "s i") 'ibuffer-filter-by-modified
      (kbd "s v") 'ibuffer-filter-by-visiting-file
      (kbd "s c") 'ibuffer-filter-by-content
      (kbd "s e") 'ibuffer-filter-by-predicate

      (kbd "s r") 'ibuffer-switch-to-saved-filters
      (kbd "s a") 'ibuffer-add-saved-filters
      (kbd "s x") 'ibuffer-delete-saved-filters
      (kbd "s d") 'ibuffer-decompose-filter
      (kbd "s s") 'ibuffer-save-filters
      (kbd "s p") 'ibuffer-pop-filter
      (kbd "s <up>") 'ibuffer-pop-filter
      (kbd "s !") 'ibuffer-negate-filter
      (kbd "s t") 'ibuffer-exchange-filters
      (kbd "s TAB") 'ibuffer-exchange-filters
      (kbd "s o") 'ibuffer-or-filter
      (kbd "s |") 'ibuffer-or-filter
      (kbd "s &") 'ibuffer-and-filter
      (kbd "s g") 'ibuffer-filters-to-filter-group
      (kbd "s P") 'ibuffer-pop-filter-group
      (kbd "s S-<up>") 'ibuffer-pop-filter-group
      (kbd "s D") 'ibuffer-decompose-filter-group
      (kbd "s /") 'ibuffer-filter-disable

      (kbd "C-j") 'ibuffer-forward-filter-group
      (kbd "M-n") 'ibuffer-forward-filter-group
      (kbd "]]") 'ibuffer-forward-filter-group
      "\t" 'ibuffer-forward-filter-group
      (kbd "M-p") 'ibuffer-backward-filter-group
      (kbd "C-k") 'ibuffer-backward-filter-group
      (kbd "[[") 'ibuffer-backward-filter-group
      [backtab] 'ibuffer-backward-filter-group
      (kbd "M-j") 'ibuffer-jump-to-filter-group
      (kbd "gx") 'ibuffer-kill-line
      (kbd "C-y") 'ibuffer-yank
      (kbd "s S") 'ibuffer-save-filter-groups
      (kbd "s R") 'ibuffer-switch-to-saved-filter-groups
      (kbd "s X") 'ibuffer-delete-saved-filter-groups
      (kbd "s \\") 'ibuffer-clear-filter-groups

      (kbd "% n") 'ibuffer-mark-by-name-regexp
      (kbd "% m") 'ibuffer-mark-by-mode-regexp
      (kbd "% f") 'ibuffer-mark-by-file-name-regexp
      (kbd "% g") 'ibuffer-mark-by-content-regexp
      (kbd "% L") 'ibuffer-mark-by-locked

      (kbd "C-t") 'ibuffer-visit-tags-table

      (kbd "|") 'ibuffer-do-shell-command-pipe
      (kbd "!") 'ibuffer-do-shell-command-file
      (kbd "t") 'ibuffer-do-toggle-modified
      ;; marked operations
      (kbd "A") 'ibuffer-do-view
      (kbd "D") 'ibuffer-do-delete
      (kbd "E") 'ibuffer-do-eval
      (kbd "F") 'ibuffer-do-shell-command-file
      (kbd "I") 'ibuffer-do-query-replace-regexp
      (kbd "J")  'ibuffer-jump-to-buffer
      (kbd "K")  'ibuffer-do-kill-lines
      (kbd "H") 'ibuffer-do-view-other-frame
      (kbd "N") 'ibuffer-do-shell-command-pipe-replace
      (kbd "M") 'ibuffer-do-toggle-modified
      (kbd "O") 'ibuffer-do-occur
      (kbd "P") 'ibuffer-do-print
      (kbd "Q") 'ibuffer-do-query-replace
      (kbd "R") 'ibuffer-do-rename-uniquely
      (kbd "S") 'ibuffer-do-save
      (kbd "T") 'ibuffer-do-toggle-read-only
      (kbd "r") 'ibuffer-do-replace-regexp
      (kbd "V") 'ibuffer-do-revert
      (kbd "W") 'ibuffer-do-view-and-eval

      (kbd "K") 'ibuffer-do-kill-lines
      (kbd "yf") 'ibuffer-copy-filename-as-kill
      (kbd "yb") 'ibuffer-copy-buffername-as-kill

      (kbd "RET") 'ibuffer-visit-buffer
      (kbd "go") 'ibuffer-visit-buffer-other-window
      (kbd "C-o") 'ibuffer-visit-buffer-other-window-noselect
      ;; (kbd "M-o") 'ibuffer-visit-buffer-1-window
      (kbd "M-r") 'ibuffer-redisplay
      (kbd "gv") 'ibuffer-do-view
      (kbd "gV") 'ibuffer-do-view-horizontally

      ;; Quit
      "q" 'quit-window
      "ZZ" 'quit-window
      "ZQ" 'quit-window)))

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
