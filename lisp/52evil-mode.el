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


;;; visual indicators
(setq evil-mode-line-format 'before
      evil-emacs-state-tag    (propertize "« E »" 'face 'pl/evil-emacs-tag)
      evil-normal-state-tag   (propertize "« ☢ »" 'face 'pl/evil-normal-tag)
      evil-insert-state-tag   (propertize "« I »" 'face 'pl/evil-insert-tag)
      evil-motion-state-tag   (propertize "« M »" 'face 'pl/evil-motion-tag)
      evil-visual-state-tag   (propertize "« ∞ »" 'face 'pl/evil-visual-tag)
      evil-operator-state-tag (propertize "« O »" 'face 'pl/evil-operator-tag)
      evil-replace-state-tag  (propertize "« R »" 'face 'pl/evil-replace-tag))

;; FIXME: as Official Emacs 24.4, if set color color,  when multile
;; windows (> 6), C-h evil-mode `q' then quit the help window will be very slow
;; see more https://bitbucket.org/lyro/evil/issue/487/after-set-colors-for-different-state-by
(if (eq window-system 'mac) ; emacs-mac-port (https://github.com/railwaycat/emacs-mac-port)
    (setq evil-emacs-state-cursor    `(box    ,(face-attribute 'pl/evil-emacs-tag    :foreground))
          evil-normal-state-cursor   `(box    ,(face-attribute 'pl/evil-normal-tag   :foreground))
          evil-insert-state-cursor   `(box    ,(face-attribute 'pl/evil-insert-tag   :foreground))
          evil-motion-state-cursor   `(box    ,(face-attribute 'pl/evil-motion-tag   :foreground))
          evil-visual-state-cursor   `(hollow ,(face-attribute 'pl/evil-visual-tag   :foreground))
          evil-replace-state-cursor  `(hbar   ,(face-attribute 'pl/evil-replace-tag  :foreground))
          evil-operator-state-cursor `(hollow ,(face-attribute 'pl/evil-operator-tag :foreground)))
  (setq evil-default-cursor '(box "#cd0000") ; emacs official
        evil-emacs-state-cursor    'box
        evil-normal-state-cursor   'box
        evil-insert-state-cursor   'box
        evil-motion-state-cursor   'box
        evil-visual-state-cursor   'hollow
        evil-replace-state-cursor  'hbar
        evil-operator-state-cursor 'hollow))

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

;;; escape
;;; cycle emacs state
(defvar pl/evil-saved-state nil "saved evil state for later use.")
(make-variable-buffer-local 'pl/evil-saved-state)

(defun pl/evil-emacs-state-cycle ()
  "When this function be called, it will change evil-state back
to previous saved state, or simply change evil-state to emacs."
  (when evil-mode
    (cond (pl/evil-saved-state
           (unless (eq evil-state pl/evil-saved-state)
             (evil-change-state pl/evil-saved-state))
           (setq pl/evil-saved-state nil))
          (t
           (unless (eq evil-state 'emacs)
             (setq pl/evil-saved-state evil-state)
             (evil-change-state 'emacs))))))

;;; escape dwim
(defun pl/escape-dwim ()
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark t))
  (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
  (when multiple-cursors-mode
    (multiple-cursors-mode -1))
  (cond ((minibuffer-window-active-p (selected-window))
         (abort-recursive-edit))
        ((or (memq major-mode '(Info-mode org-agenda-mode help-mode))
             pl/evil-saved-state)
         (keyboard-quit))
        ((and (fboundp 'evil-mode) evil-mode)
         (cond ((eq evil-state 'emacs) (evil-exit-emacs-state))
               (t (let ((binding (key-binding [escape])))
                    (if binding (call-interactively binding)
                      (keyboard-quit))))))
        (t (keyboard-quit))))

(key-chord-define-global "df" 'pl/escape-dwim)

(defun pl/company-abort-then-escape-dwim ()
  (interactive)
  (company-abort)
  (pl/escape-dwim))

(with-eval-after-load 'company
  (key-chord-define company-active-map "df" 'pl/company-abort-then-escape-dwim))

;;; 'imap fd <ESC>' equivalent
;; (key-chord-define-global "df" [escape])
;; (key-chord-define evil-normal-state-map       "df" 'pl/escape-dwim)
;; (key-chord-define evil-insert-state-map       "df" 'pl/escape-dwim)
;; (key-chord-define evil-visual-state-map       "df" 'pl/escape-dwim)
;; (key-chord-define evil-emacs-state-map        "df" 'pl/escape-dwim)
;; (key-chord-define evil-motion-state-map       "df" 'pl/escape-dwim)
;; (key-chord-define evil-replace-state-map      "df" 'pl/escape-dwim)
;; (key-chord-define evil-operator-state-map     "df" 'pl/escape-dwim)
;; (key-chord-define evil-operator-shortcut-map  "df" 'keyboard-quit)
;; (key-chord-define evil-read-key-map           "df" 'pl/escape-dwim)
;; (key-chord-define evil-outer-text-objects-map "df" 'pl/escape-dwim)
;; (key-chord-define evil-inner-text-objects-map "df" 'pl/escape-dwim)
;; (key-chord-define evil-ex-completion-map      "df" 'pl/escape-dwim)

;; (key-chord-define minibuffer-local-map            "df" 'pl/minibuffer-keyboard-quit)
;; (key-chord-define minibuffer-local-ns-map         "df" 'pl/minibuffer-keyboard-quit)
;; (key-chord-define minibuffer-local-completion-map "df" 'pl/minibuffer-keyboard-quit)
;; (key-chord-define minibuffer-local-must-match-map "df" 'pl/minibuffer-keyboard-quit)
;; (key-chord-define minibuffer-local-isearch-map    "df" 'pl/minibuffer-keyboard-quit)

;; (define-key minibuffer-local-map [escape] 'pl/minibuffer-keyboard-quit)
;; (define-key minibuffer-local-ns-map [escape] 'pl/minibuffer-keyboard-quit)
;; (define-key minibuffer-local-completion-map [escape] 'pl/minibuffer-keyboard-quit)
;; (define-key minibuffer-local-must-match-map [escape] 'pl/minibuffer-keyboard-quit)
;; (define-key minibuffer-local-isearch-map [escape] 'pl/minibuffer-keyboard-quit)

;;; ace jump mode
(define-key evil-normal-state-map (kbd "SPC") 'ace-jump-mode)
(define-key evil-normal-state-map (kbd "S-SPC") 'ace-jump-char-mode)
(define-key evil-visual-state-map (kbd "SPC") 'ace-jump-mode)
(define-key evil-visual-state-map (kbd "S-SPC") 'ace-jump-char-mode)

;;;
(define-key evil-normal-state-map (kbd "C-o") 'pl/open-line-with-indent) ; default evil-jump-backward

;; (defun pl/evil-undefine ()
;;   (interactive)
;;   (let (evil-mode-map-alist)
;;     (call-interactively (key-binding (this-command-keys)))))
;; make sure that Evil's normal state never touches TAB, just wire this fall-through binding
;; (define-key evil-normal-state-map (kbd "TAB") 'pl/evil-undefine)
(define-key evil-normal-state-map (kbd "TAB") 'indent-for-tab-command)
(define-key evil-motion-state-map (kbd "TAB") 'indent-for-tab-command)

(define-key evil-normal-state-map "gL" 'org-mac-grab-link)
(define-key evil-normal-state-map "gb" 'switch-to-buffer)
(define-key evil-normal-state-map "gt" 'hydra-toggle/body)

;; (define-key evil-normal-state-map "b" 'backward-word)
;; (define-key evil-normal-state-map "w" 'forward-word)
(define-key evil-visual-state-map "Q" "gq")
(define-key evil-normal-state-map "Q" "gqap")
;; (define-key evil-normal-state-map "S" "vabsba")
;; (define-key evil-normal-state-map "s" "gv")

;; (evil-define-key 'visual surround-mode-map "S" "sba")
(define-key evil-normal-state-map "+" 'evil-numbers/inc-at-pt) ; default `evil-next-line-first-non-blank'
(define-key evil-normal-state-map "-" 'evil-numbers/dec-at-pt) ; default `evil-previous-line-first-non-blank'

(define-key evil-normal-state-map (kbd "C-e") 'evil-end-of-line)

;; (define-key evil-motion-state-map "v" 'evil-visual-block)
;; make it easy to switch to visual-char mode from visual-block mode
;; (define-key evil-visual-state-map "v" 'evil-visual-char)

;; q is being used at many places to close things, and sometimes it so happens that evil mode is turned on in that
;; window at the same time, which results in recording a macro instead of closing the window.
(define-key evil-normal-state-map (kbd "q") nil) ; `q' is binded to `evil-record-macro'

;;;
;; (setcdr evil-insert-state-map nil) ;; make insert state like emacs state
(define-key evil-insert-state-map "\C-k" nil)
(define-key evil-insert-state-map "\C-o" nil)
(define-key evil-insert-state-map "\C-r" nil)
(define-key evil-insert-state-map "\C-y" nil)
(define-key evil-insert-state-map "\C-e" nil)
(define-key evil-insert-state-map "\C-n" nil)
(define-key evil-insert-state-map "\C-p" nil)
(define-key evil-insert-state-map "\C-x\C-n" nil)
(define-key evil-insert-state-map "\C-x\C-p" nil)
(define-key evil-insert-state-map "\C-t" nil)
(define-key evil-insert-state-map "\C-d" nil)
(define-key evil-insert-state-map "\C-w" nil)
(define-key evil-insert-state-map [remap newline] nil)
(define-key evil-insert-state-map [remap newline-and-indent] nil)

;;; evil-leader
;; Note: You should enable global-evil-leader-mode before you enable evil-mode, otherwise evil-leader won't be enabled
;; in initial buffers (*scratch*, *Messages*, ...).
(setq evil-leader/leader ","
      evil-leader/in-all-states t)

(if (fboundp 'global-evil-leader-mode)
    (global-evil-leader-mode))

(evil-leader/set-key
  ","  'evilnc-comment-operator
  "1"   'delete-other-windows
  "2"   (kbd "C-x 2")
  "3"   "\C-x3"
  "a"   'org-agenda
  "b"   'bookmark-map
  ;; "ci", "cl", "cc", "cp", cr" and "cv" are used by evil-nerd-commenter
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
  "Hs"  'helm-multi-swoop
  "HS"  'helm-multi-swoop-all
  "j"   'helm-etags-select
  "k"   'kill-this-buffer
  ;; "ll" are used by evil-nerd-commenter
  "lr"  'pl/linum-relative-toggle
  "ms"  'magit-status
  "p"   'projectile-commander
  "ut"  'undo-tree-visualize
  "vr"  'vr/replace
  "vq"  'vr/query-replace
  "vm"  'vr/mc-mark
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
(with-eval-after-load 'key-chord
  (setq expand-region-contract-fast-key "r") ; default "-"
  (key-chord-define evil-normal-state-map "er" 'er/expand-region))

;;; enable evil mode
(if (fboundp 'evil-mode)
    (evil-mode 1))

(setq evil-move-cursor-back nil
      evil-want-visual-char-semi-exclusive t
      evil-want-C-i-jump nil
      evil-cross-lines t
      ;; evil-highlight-closing-paren-at-point-states nil
      evil-magic 'very-magic)


;;; evil-surround
(when (fboundp 'global-evil-surround-mode)
    (global-evil-surround-mode 1)
    ;; evil-surround-pairs-alist is a buffer local variable
    (setq-default evil-surround-pairs-alist (cl-adjoin
                                             '(?~ . ("``" . "``"))
                                             evil-surround-pairs-alist)))
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
        (flycheck-error-list-mode  . emacs)
        (fundamental-mode          . emacs)
        (erc-mode                  . emacs)
        (git-commit-mode           . insert)
        (git-rebase-mode           . emacs)
        (git-rebase-mode           . emacs)
        (gtags-select-mode         . emacs)
        (gud-mode                  . emacs)
        (grep-mode                 . emacs)
        (haskell-error-mode        . emacs)
        (haskell-interactive-mode  . insert)
        (message-mode              . emacs)
        (magit-commit-mode         . normal)
        (magit-log-edit-mode       . emacs)
        (magit-branch-manager-mode . emacs)
        (nrepl-mode                . insert)
        (weibo-timeline-mode       . emacs)
        (weibo-post-mode           . emacs)
        (org-mode                  . insert)
        (sr-mode                   . emacs)
        (shell-mode                . emacs)
        (term-mode                 . emacs)
        (tuareg-interactive-mode   . insert)
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
(evil-declare-key 'normal haskell-mode-map ",gn" 'ghc-goto-next-error)
(evil-declare-key 'normal haskell-mode-map ",gp" 'ghc-goto-prev-error)

(evil-define-key 'normal haskell-mode-map (kbd "gT") 'haskell-process-do-type)
(evil-define-key 'normal haskell-mode-map (kbd "gI") 'haskell-process-do-info)
(evil-define-key 'normal haskell-mode-map (kbd "gz") 'haskell-interactive-switch)
(evil-define-key 'normal haskell-interactive-mode (kbd "gz") 'haskell-interactive-switch-back)
(evil-define-key 'normal haskell-cabal-mode-map (kbd "gz") 'haskell-interactive-switch)

(evil-define-key 'normal haskell-mode-map (kbd "gl") 'haskell-process-load-or-reload)

(evil-define-key 'normal haskell-interactive-mode-map (kbd "RET") #'haskell-interactive-mode-return)
(evil-define-key 'insert haskell-interactive-mode-map (kbd "C-u") 'haskell-interactive-mode-kill-whole-line)
(evil-define-key 'normal haskell-interactive-mode-map (kbd "G") 'end-of-buffer)

(evil-define-key 'normal shm-map (kbd "D") 'shm/kill-line)
(evil-define-key 'normal shm-map (kbd "R") 'shm/raise)
(evil-define-key 'normal shm-map (kbd "P") 'shm/yank)
(evil-define-key 'normal shm-map (kbd "TAB") 'shm/tab)
(evil-define-key 'normal shm-map (kbd "<backtab>") 'shm/backtab)

(evil-define-key 'insert shm-map (kbd "RET") 'shm/ret-proxy)
(evil-define-key 'normal shm-map (kbd "RET") 'shm/ret-proxy)
(evil-define-key 'insert shm-map (kbd "M-RET") 'evil-ret)

(evil-define-key 'normal shm-map
  (kbd "M-k") 'sp-splice-sexp-killing-backward
  (kbd "M-j") 'sp-splice-sexp-killing-forward
  (kbd "M-l") 'sp-forward-slurp-sexp
  (kbd "M-h") 'sp-forward-barf-sexp
  (kbd "M-H") 'sp-backward-slurp-sexp
  (kbd "M-L") 'sp-backward-barf-sexp
  (kbd "s") 'sp-splice-sexp
  (kbd "S") 'shm/split-list
  (kbd "M-R") 'sp-raise-sexp
  (kbd "J") 'sp-join-sexp
  (kbd ")") 'shm/forward-node
  (kbd "(") 'shm/backward-node
  (kbd "M-(") 'sp-backward-up-sexp
  (kbd "M-)") 'sp-down-sexp
  (kbd "C-(") 'sp-backward-down-sexp
  (kbd "C-)") 'sp-up-sexp)

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

(evil-define-key 'insert shm-map
  (kbd "M-k") 'sp-splice-sexp-killing-backward
  (kbd "M-j") 'sp-splice-sexp-killing-forward
  (kbd "M-l") 'sp-forward-slurp-sexp
  (kbd "M-h") 'sp-forward-barf-sexp
  (kbd "M-H") 'sp-backward-slurp-sexp
  (kbd "M-L") 'sp-backward-barf-sexp)

(evil-define-key 'emacs shm-map
  (kbd "M-k") 'sp-splice-sexp-killing-backward
  (kbd "M-j") 'sp-splice-sexp-killing-forward
  (kbd "M-l") 'sp-forward-slurp-sexp
  (kbd "M-h") 'sp-forward-barf-sexp
  (kbd "M-H") 'sp-backward-slurp-sexp
  (kbd "M-L") 'sp-backward-barf-sexp)

;;; evil-nerd-commenter
;; installed with el-get
(setq evilnc-hotkey-comment-operator ",,")
(require 'evil-nerd-commenter nil t)

(global-set-key (kbd "M-;") 'evilnc-comment-or-uncomment-lines)
;; (global-set-key (kbd "C-c l") 'evilnc-comment-or-uncomment-to-the-line)
;; (global-set-key (kbd "C-c c") 'evilnc-copy-and-comment-lines)
;; (global-set-key (kbd "C-c p") 'evilnc-comment-or-uncomment-paragraphs)

(with-eval-after-load 'evil
  (define-key evil-normal-state-map ",ci" 'evilnc-comment-or-uncomment-lines)
  (define-key evil-normal-state-map ",cl" 'evilnc-comment-or-uncomment-to-the-line)
  (define-key evil-normal-state-map ",cc" 'evilnc-copy-and-comment-lines)
  (define-key evil-normal-state-map ",cp" 'evilnc-comment-or-uncomment-paragraphs)
  (define-key evil-normal-state-map ",cr" 'comment-or-uncomment-region)
  (define-key evil-normal-state-map ",cv" 'evilnc-toggle-invert-comment-line-by-line)
  (define-key evil-normal-state-map ",ll" 'evilnc-quick-comment-or-uncomment-to-the-line))

;;;
(require 'evil-indent-textobject nil t)
;; ii - Inner Indentation: the surrounding textblock with the same indentation
;; ai - Above and Indentation: ii + the line above with a different indentation
;; aI - Above and Indentation+: ai + the line below with a different indentation

;;; evil-matchit
(require 'evil-matchit nil t)
(when (fboundp 'global-evil-matchit-mode)
  (global-evil-matchit-mode +1))

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

(evil-define-key 'normal magit-log-mode-map
  "j" 'magit-goto-next-section
  "k" 'magit-goto-previous-section)

(evil-define-key 'normal magit-diff-mode-map
  "j" 'magit-goto-next-section
  "k" 'magit-goto-previous-section)

;;;
;; (evil-add-hjkl-bindings grep-mode-map 'emacs)
;; (evil-add-hjkl-bindings helm-grep-mode-map 'emacs)
(evil-add-hjkl-bindings help-mode-map 'emacs) ; both `h' and `? default binding to describe-mode

;;; flycheck
(eval-after-load "flycheck"
  '(progn
     (define-key flycheck-error-list-mode-map "j" 'evil-next-line)
     (define-key flycheck-error-list-mode-map "k" 'evil-previous-line)))

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
;; (add-to-list 'load-path "~/.emacs.d/site-lisp/evil-exchange")
;; installed with el-get
(require 'evil-exchange nil t)
(if (fboundp 'evil-exchange-install)
    (evil-exchange-install))

;;; work with mutiple cursors
(add-hook 'multiple-cursors-mode-enabled-hook
          'pl/evil-switch-to-insert-maybe)
;;; evil-args
(require 'evil-args nil t)
(eval-after-load "evil-args"
  '(progn
     ;; evil-args-openers
     ;; evil-args-closers
     ;; evil-args-delimiters
     ;; bind evil-args text objects
     (define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
     (define-key evil-outer-text-objects-map "a" 'evil-outer-arg)

     ;; bind evil-forward/backward-args
     (define-key evil-normal-state-map "L" 'evil-forward-arg)
     (define-key evil-normal-state-map "H" 'evil-backward-arg)
     (define-key evil-motion-state-map "L" 'evil-forward-arg)
     (define-key evil-motion-state-map "H" 'evil-backward-arg)

     ;; bind evil-jump-out-args
     ;; (define-key evil-normal-state-map "K" 'evil-jump-out-args)
     ;; (key-chord-define evil-normal-state-map "hl" 'evil-jump-out-args)
     (define-key evil-normal-state-map "gk" 'evil-jump-out-args)))

;;; git-timemachine
(with-eval-after-load "git-timemachine"
  ;; (add-hook 'git-timemachine-mode-hook 'pl/evil-emacs-state-cycle)

  ;; @see https://bitbucket.org/lyro/evil/issue/511/let-certain-minor-modes-key-bindings
  (evil-make-overriding-map git-timemachine-mode-map 'normal)
  ;; force update evil keymaps after git-timemachine-mode loaded
  (add-hook 'git-timemachine-mode-hook #'evil-normalize-keymaps))

;;; company-mode
(when (fboundp 'evil-declare-change-repeat)
  (mapc #'evil-declare-change-repeat
        '(company-complete-common
          company-select-next
          company-select-previous
          company-complete-selection
          company-complete-number)))

(eval-after-load "helm-swoop"
  '(progn
     ;; When doing evil-search, hand the word over to helm-swoop
     (define-key evil-motion-state-map (kbd "H-i") 'helm-swoop-from-evil-search)))

;;; visual-line-mode
;; from https://github.com/tarleb/evil-rebellion
;; switch bindings for visual line and logical line movements.
(evil-define-key 'normal visual-line-mode-map
  "$" 'evil-end-of-visual-line
  "^" 'evil-beginning-of-visual-line
  "g$" 'evil-end-of-line
  "g^" 'evil-beginning-of-line
  "gj" 'evil-next-line
  "gk" 'evil-previous-line
  "j" 'evil-next-visual-line
  "k" 'evil-previous-visual-line)

;;; elisp-slime-nav
(evil-define-key 'normal elisp-slime-nav-mode-map
  (kbd "M-.") 'elisp-slime-nav-find-elisp-thing-at-point
  (kbd "M-,") 'pop-tag-mark)

;;; misc
;; using both the RET and <return> forms to make sure the key works both in terminal and under X.
(evil-define-key 'motion completion-list-mode-map (kbd "<return>") 'choose-completion)
(evil-define-key 'motion completion-list-mode-map (kbd "RET") 'choose-completion)
(evil-define-key 'motion browse-kill-ring-mode-map (kbd "<return>") 'browse-kill-ring-insert-and-quit)
(evil-define-key 'motion browse-kill-ring-mode-map (kbd "RET") 'browse-kill-ring-insert-and-quit)
(evil-define-key 'motion occur-mode-map (kbd "<return>") 'occur-mode-goto-occurrence)
(evil-define-key 'motion occur-mode-map (kbd "RET") 'occur-mode-goto-occurrence)

;;; evil-escape
;; (setq-default evil-escape-key-sequence "fd") 'must be set before requiring evil-escape.
;; (setq-default evil-escape-delay 0.1)
;; (evil-escape-mode +1)

;;; evil-snipe
;; https://github.com/hlissner/evil-snipe
(require 'evil-snipe nil t)
(with-eval-after-load "evil-snipe"
  (global-evil-snipe-mode +1)
  ;; (evil-snipe-enable-nN)
  ;; replaces evil-mode's f/F/t/T/;/, with snipe
  (evil-snipe-replace-evil))


;;; bugfix
;; https://bitbucket.org/lyro/evil/issue/432/edebug-mode-map-cant-take-effect-for-the
;; (add-hook 'edebug-mode-hook 'evil-normalize-keymaps) ; recreate `evil-mode-map-alist'
(with-eval-after-load 'edebug
  (add-hook 'edebug-mode-hook 'pl/evil-emacs-state-cycle))

(provide '52evil-mode)
;;; 50evil-mode.el ends here
