;;; 52evil-mode.el ---                               -*- lexical-binding: t; -*-

;; Copyright (C) 2013  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>
;; Keywords:

(require 'evil nil t)

;; `C-M-x' on a defface expression reinitializes the face according to the defface specification.

(defface pl/evil-normal-tag
  `((t (:weight bold :foreground "orchid")))
  "Evil normal mode indicator face")

(defface pl/evil-insert-tag
  `((t (:weight bold :foreground "OliveDrab1")))
  "Evil insert mode indicator face")

(defface pl/evil-replace-tag
  `((t (:weight bold :foreground "orchid")))
  "Evil replace mode indicator face")

(defface pl/evil-emacs-tag
  `((t (:weight bold :foreground "#7cfa42")))
  "Evil emacs mode indicator face")

(defface pl/evil-visual-tag
  `((t (:weight bold :foreground "Purple")))
  "Evil visual mode indicator face")

(defface pl/evil-motion-tag
  `((t (:weight bold :foreground "Cyan")))
  "Evil motion mode indicator face")

(defface pl/evil-operator-tag
  `((t (:weight bold :foreground "maroon")))
  "Evil operator mode indicator face")


;;; Visual indicators
(setq evil-mode-line-format 'before

      evil-emacs-state-tag    (propertize "« E »" 'face 'pl/evil-emacs-tag)
      evil-normal-state-tag   (propertize "« ☢ »" 'face 'pl/evil-normal-tag)
      evil-insert-state-tag   (propertize "« I »" 'face 'pl/evil-insert-tag)
      evil-motion-state-tag   (propertize "« M »" 'face 'pl/evil-motion-tag)
      evil-visual-state-tag   (propertize "« ∞ »" 'face 'pl/evil-visual-tag)
      evil-operator-state-tag (propertize "« O »" 'face 'pl/evil-operator-tag)
      evil-replace-state-tag  (propertize "« R »" 'face 'pl/evil-replace-tag)

      evil-emacs-state-cursor    `(box    ,(face-attribute 'pl/evil-emacs-tag    :foreground))
      evil-normal-state-cursor   `(box    ,(face-attribute 'pl/evil-normal-tag   :foreground))
      evil-insert-state-cursor   `(box    ,(face-attribute 'pl/evil-insert-tag   :foreground))
      evil-motion-state-cursor   `(box    ,(face-attribute 'pl/evil-motion-tag   :foreground))
      evil-visual-state-cursor   `(hollow ,(face-attribute 'pl/evil-visual-tag   :foreground))
      evil-replace-state-cursor  `(hbar   ,(face-attribute 'pl/evil-replace-tag  :foreground))
      evil-operator-state-cursor `(hollow ,(face-attribute 'pl/evil-operator-tag :foreground)))

;; Getting :n[ew] to work
;; As of this writing, Evil does not allow you to shorten ':new' to ':n', but you can define a command that does.
;; (evil-ex-define-cmd "n[ew]" 'evil-window-new)

;; Modes that should be insert state by default
;; (dolist (mode '(sql-interactive-mode
;;                 magit-log-edit-mode erlang-shell-mode
;;                 inferior-moz-mode inferior-octave-mode
;;                 inferior-ess-mode
;;                 grep-mode pylookup-mode))
;;   (add-to-list 'evil-insert-state-modes mode))

;;; esc quits
(defun pl/minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))

(define-key minibuffer-local-map [escape] 'pl/minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'pl/minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'pl/minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'pl/minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'pl/minibuffer-keyboard-quit)

;;; 'imap fd <ESC>' equivalent
;; (key-chord-define-global "fd" [escape])
;; (define-key evil-insert-state-map (kbd "f d") 'evil-normal-state)
(key-chord-define evil-normal-state-map           "fd" 'evil-force-normal-state)
(key-chord-define evil-insert-state-map           "fd" 'evil-normal-state)
(key-chord-define evil-visual-state-map           "fd" 'evil-change-to-previous-state)
(key-chord-define evil-emacs-state-map            "fd" 'evil-normal-state)
(key-chord-define evil-motion-state-map           "fd" 'evil-normal-state)
(key-chord-define evil-replace-state-map          "fd" 'evil-normal-state)

(key-chord-define minibuffer-local-map            "fd" 'pl/minibuffer-keyboard-quit)
(key-chord-define minibuffer-local-ns-map         "fd" 'pl/minibuffer-keyboard-quit)
(key-chord-define minibuffer-local-completion-map "fd" 'pl/minibuffer-keyboard-quit)
(key-chord-define minibuffer-local-must-match-map "fd" 'pl/minibuffer-keyboard-quit)
(key-chord-define minibuffer-local-isearch-map    "fd" 'pl/minibuffer-keyboard-quit)

;;;
;; (setcdr evil-insert-state-map nil) ;; make insert state like emacs state

;;; ace jump mode
(define-key evil-normal-state-map (kbd "SPC") 'ace-jump-mode)
(define-key evil-normal-state-map (kbd "S-SPC") 'ace-jump-char-mode)
(define-key evil-visual-state-map (kbd "SPC") 'ace-jump-mode)
(define-key evil-visual-state-map (kbd "S-SPC") 'ace-jump-char-mode)

;;;
(define-key evil-normal-state-map (kbd "C-o") 'pl/open-line-with-indent) ; default evil-jump-backward
(define-key evil-normal-state-map (kbd "TAB") 'indent-for-tab-command)
;; (define-key evil-normal-state-map "b" 'backward-word)
;; (define-key evil-normal-state-map "w" 'forward-word)
(define-key evil-visual-state-map "Q" "gq")
(define-key evil-normal-state-map "Q" "gqap")
;; (define-key evil-normal-state-map "S" "vabsba")
;; (define-key evil-normal-state-map "s" "gv")

;; (evil-define-key 'visual surround-mode-map "S" "sba")
(define-key evil-normal-state-map "+" 'evil-numbers/inc-at-pt) ; default `evil-next-line-first-non-blank'
(define-key evil-normal-state-map "-" 'evil-numbers/dec-at-pt) ; default `evil-previous-line-first-non-blank'

;; (define-key evil-motion-state-map "v" 'evil-visual-block)
;; make it easy to switch to visual-char mode from visual-block mode
;; (define-key evil-visual-state-map "v" 'evil-visual-char)

(define-key evil-insert-state-map (kbd "C-n") 'next-line)
(define-key evil-insert-state-map (kbd "C-p") 'previous-line)
(define-key evil-insert-state-map (kbd "C-d") 'delete-char)
(define-key evil-normal-state-map (kbd "C-e") 'evil-end-of-line)
(define-key evil-insert-state-map (kbd "C-e") 'evil-end-of-line)

;;; evil-leader
;; Note: You should enable global-evil-leader-mode before you enable evil-mode, otherwise evil-leader won't be enabled
;; in initial buffers (*scratch*, *Messages*, ...).
(setq evil-leader/leader ","
      evil-leader/in-all-states t)

(if (fboundp 'global-evil-leader-mode)
    (global-evil-leader-mode))

(evil-leader/set-key
  ","   'evilnc-comment-operator
  "a"   'org-agenda
  "b"   'bookmark-map
  "ci"  'evilnc-comment-or-uncomment-lines
  "cl"  'evilnc-comment-or-uncomment-to-the-line
  "cc"  'evilnc-copy-and-comment-lines
  "cp"  'evilnc-comment-or-uncomment-paragraphs
  "cr"  'comment-or-uncomment-region
  "cs"  'pl/evil-change-symbol-in-defun
  "ch"  'crosshairs-mode
  "D"   'dired-jump
  "dp"  'dash-at-point
  "dP"  'dash-at-point-with-docset
  "ff"  'pl/toggle-full-window
  "fc"  'flycheck-buffer
  "fC"  'flycheck-clear
  "fi"  'flycheck-info
  "fl"  'flycheck-list-errors
  "fn"  'flycheck-next-error
  "fp"  'flycheck-previous-error
  "fs"  'flycheck-select-checker
  "f/"  'flycheck-google-messages
  "fy"  'flycheck-copy-messages-as-kill
  "fb"  'flycheck-compile
  "fV"  'flycheck-version
  "f?"  'flycheck-describe-checker
  "ft"  'flycheck-mode
  "g="  'git-gutter:popup-hunk
  "gj"  'git-gutter:next-hunk
  "gk"  'git-gutter:previous-hunk
  "gs"  'git-gutter:stage-hunk
  "gr"  'git-gutter:revert-hunk
  "gt"  'git-gutter:toggle
  "hs"  'helm-swoop
  "hS"  'helm-swoop-back-to-last-point
  "j"   'helm-etags-select
  "k"   'kill-this-buffer
  "lr"  'pl/linum-relative-toggle
  "ms"  'magit-status
  "p"  'projectile-commander
  "ut"  'undo-tree-visualize
  "w"   'save-buffer
  "W"   'save-some-buffers
  "xb"  'switch-to-buffer
  "xc"  'save-buffers-kill-terminal
  "xf"  'ido-find-file
  "xk"  'kill-buffer
  "xz"  'suspend-frame
  "xvv" 'vc-next-action
  "xv=" 'vc-diff
  "xvl" 'vc-print-log)

(eval-after-load "helm-config"
  '(progn
     (evil-leader/set-key "4" helm-command-map)))

;; (evil-leader/set-key-for-mode 'emacs-lisp-mode "b" 'byte-compile-file)

;;; expand-region
(key-chord-define evil-normal-state-map "er" 'er/expand-region)
(eval-after-load "evil"
  '(setq expand-region-contract-fast-key "r")) ; default "-"

;;; enable evil mode
(if (fboundp 'evil-mode)
    (evil-mode 1))

(setq evil-move-cursor-back nil
      evil-magic 'very-magic)


;;; evil-surround
(if (fboundp 'global-surround-mode)
    (global-surround-mode 1))

;;; default mode
(setq evil-default-state 'normal)
(loop for (mode . state) in
      '((comint-mode               . insert)
        (compilation-mode          . emacs)
        (diff-mode                 . emacs)
        (eshell-mode               . emacs)
        (eww-mode                  . emacs)
        (help-mode                 . emacs)
        (helm-grep-mode            . emacs)
        (ibuffer-mode              . normal)
        (inferior-emacs-lisp-mode  . emacs)
        (inf-ruby-mode             . emacs)
        (Info-mode                 . emacs)
        (log-edit-mode             . emacs)
        (fundamental-mode          . emacs)
        (erc-mode                  . emacs)
        (git-commit-mode           . insert)
        (git-rebase-mode           . emacs)
        (git-rebase-mode           . emacs)
        (gtags-select-mode         . emacs)
        (gud-mode                  . emacs)
        (grep-mode                 . emacs)
        (haskell-interactive-mode  . insert)
        (message-mode              . emacs)
        (magit-commit-mode         . normal)
        (magit-log-edit-mode       . emacs)
        (magit-branch-manager-mode . emacs)
        (nrepl-mode                . insert)
        (weibo-timeline-mode       . emacs)
        (weibo-post-mode           . emacs)
        (org-mode                  . normal)
        (sr-mode                   . emacs)
        (shell-mode                . emacs)
        (term-mode                 . emacs)
        (speedbar-mode             . emacs)
        (yari-mode                 . emacs))
      do (evil-set-initial-state mode state))

;; (define-key evil-motion-state-map "f" 'jump-char-forward)
;; (define-key evil-motion-state-map "F" 'jump-char-backward)

;;; haskell mode
(defun pl/newline-and-indent-relative ()
  (interactive)
  (newline)
  (indent-to-column
   (save-excursion
     (forward-line -1)
     (back-to-indentation)
     (current-column))))

(defun pl/haskell-evil-open-below ()
  (interactive)
  (evil-append-line 1)
  (pl/newline-and-indent-relative))

(defun pl/haskell-evil-open-above ()
  (interactive)
  (previous-line)
  (evil-append-line 1)
  (pl/newline-and-indent-relative))

(evil-declare-key 'insert haskell-mode-map (kbd "RET") 'pl/newline-and-indent-relative)
(evil-declare-key 'normal haskell-mode-map "o" 'pl/haskell-evil-open-below)
(evil-declare-key 'normal haskell-mode-map "O" 'pl/haskell-evil-open-above)

;;; evil-nerd-commenter
(add-to-list 'load-path "~/.emacs.d/site-lisp/evil-nerd-commenter")
(require 'evil-nerd-commenter nil t)
(setq evilnc-hotkey-comment-operator ",,")
(global-set-key (kbd "M-;") 'evilnc-comment-or-uncomment-lines)
;; (global-set-key (kbd "C-c l") 'evilnc-comment-or-uncomment-to-the-line)
;; (global-set-key (kbd "C-c c") 'evilnc-copy-and-comment-lines)
;; (global-set-key (kbd "C-c p") 'evilnc-comment-or-uncomment-paragraphs)

;;;
(require 'evil-indent-textobject nil t)
;; ii - Inner Indentation: the surrounding textblock with the same indentation
;; ai - Above and Indentation: ii + the line above with a different indentation
;; aI - Above and Indentation+: ai + the line below with a different indentation

;;; evil-matchit
(require 'evil-matchit nil t)
(when (fboundp 'global-evil-matchit-mode)
  (global-evil-matchit-mode +1))

;;; evil-little-word.el
(require 'evil-little-word nil t)

;;; evil-textobj-between.el
(require 'evil-textobj-between nil t)

;;; evil-numbers
(define-key evil-normal-state-map (kbd "C-c +") 'evil-numbers/inc-at-pt)
(define-key evil-normal-state-map (kbd "C-c -") 'evil-numbers/dec-at-pt)

;;; refactor/rename a variable name in a function efficiently
;; http://blog.binchen.org/?p=583
(defun pl/evil-change-symbol-in-defun ()
  "mark the region in defun (definition of function) and use string replacing UI in evil-mode
to replace the symbol under cursor"
  (interactive)
  (let ((old (thing-at-point 'symbol)))
    (mark-defun)
    (unless (evil-visual-state-p)
      (evil-visual-state))
    (evil-ex (concat "'<,'>s/" (if (= 0 (length old)) "" "\\<\\(") old (if (= 0 (length old)) "" "\\)\\>/")))))

;;; Lua mode
(evil-declare-key 'normal lua-mode-map
  ",md" 'mobdebug-minor-mode)

;;; magit mode
;; (evil-add-hjkl-bindings magit-mode-map 'emacs)
;; (evil-add-hjkl-bindings magit-diff-mode-map 'emacs)
;; (evil-add-hjkl-bindings git-rebase-mode-map 'emacs
;;   "K" 'git-rebase-mode-kill-line
;;   "h" 'describe-mode)
;; (evil-add-hjkl-bindings magit-log-mode-map 'emacs
;;   "l" 'magit-key-mode-popup-logging)
(evil-add-hjkl-bindings magit-status-mode-map 'emacs
  "K" 'magit-discard-item
  "l" 'magit-key-mode-popup-logging
  "h" 'magit-toggle-diff-refine-hunk
  "J" 'magit-section-jump-map ; `J  default bind to `magit-key-mode-popup-apply-mailbox'
  "j" 'magit-goto-next-section ; default bind to `magit-section-jump-map'
  "k" 'magit-goto-previous-section)

;;;
;; (evil-add-hjkl-bindings grep-mode-map 'emacs)
;; (evil-add-hjkl-bindings helm-grep-mode-map 'emacs)
(evil-add-hjkl-bindings help-mode-map 'emacs) ; both `h' and `? default binding to describe-mode

;;; eww
(evil-add-hjkl-bindings eww-mode-map 'emacs
  "L" 'eww-back-url) ; defaut "l"

;;; dired
(eval-after-load "dired"
  '(progn
     (evil-define-key 'normal dired-mode-map "M-r" 'dired-do-redisplay)
     (evil-define-key 'normal dired-mode-map "r" 'wdired-change-to-wdired-mode)))

;;; ibuffer
(eval-after-load "ibuffer"
  '(progn
     (evil-define-key 'normal ibuffer-mode-map "J" 'ibuffer-jump-to-buffer)
     (evil-define-key 'normal ibuffer-mode-map "M-r" 'ibuffer-redisplay) ; default `l'
     (evil-define-key 'normal ibuffer-mode-map "K" 'ibuffer-do-kill-lines)))

;;; org agenda -- leave in emacs mode but add j & k
(eval-after-load "org-agenda"
  '(progn
     (define-key org-agenda-mode-map "j" 'evil-next-line) ; overide org-agenda-goto-date
     (define-key org-agenda-mode-map "k" 'evil-previous-line))) ; org-agenda-capture

;;; smartparens
(define-key evil-normal-state-map "M" 'evil-set-marker)
;; my own evil movements. Some default bindings were moved here.
(defvar pl/evil-move-map (make-sparse-keymap))
(define-prefix-command 'pl/evil-move-map)
(define-key evil-normal-state-map "m" pl/evil-move-map)
(define-key pl/evil-move-map "d" 'sp-down-sexp)
(define-key pl/evil-move-map "a" 'sp-backward-down-sexp)

;;; evil-exchange
;; `gx': evil-exchange-key, `gX': evil-exchange-cancel-key
(add-to-list 'load-path "~/.emacs.d/site-lisp/evil-exchange")
(require 'evil-exchange nil t)
(if (fboundp 'evil-exchange-install)
    (evil-exchange-install))


(provide '52evil-mode)
;;; 50evil-mode.el ends here
