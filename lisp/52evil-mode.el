;;; 52evil-mode.el ---                               -*- lexical-binding: t; -*-

;; Copyright (C) 2013  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>
;; Keywords:

(require 'evil nil t)

;; Copy from Spacemacs
(with-eval-after-load "evil"
  (require 'evil-evilified-state))

;; `C-M-x' on a defface expression reinitializes the face according to the
;; defface specification.

(defface paloryemacs/evil-normal-tag
  `((t (:weight bold :foreground "orchid")))
  "Evil normal mode indicator face")

(defface paloryemacs/evil-insert-tag
  `((t (:weight bold :foreground "OliveDrab1")))
  "Evil insert mode indicator face")

(defface paloryemacs/evil-replace-tag
  `((t (:weight bold :foreground "orchid")))
  "Evil replace mode indicator face")

(defface paloryemacs/evil-emacs-tag
  `((t (:weight bold :foreground "#7cfa42")))
  "Evil emacs mode indicator face")

(defface paloryemacs/evil-visual-tag
  `((t (:weight bold :foreground "Purple")))
  "Evil visual mode indicator face")

(defface paloryemacs/evil-motion-tag
  `((t (:weight bold :foreground "Cyan")))
  "Evil motion mode indicator face")

(defface paloryemacs/evil-operator-tag
  `((t (:weight bold :foreground "maroon")))
  "Evil operator mode indicator face")

(defface paloryemacs/evil-lispy-tag
  `((t (:weight bold :foreground "blue")))
  "Evil lispy mode indicator face")

(defface paloryemacs/evil-iedit-tag
  `((t (:weight bold :foreground "yellow")))
  "Evil iedit mode indicator face")

;;; visual indicators
(setq evil-mode-line-format 'before
      evil-emacs-state-tag    (propertize "« E »" 'face 'paloryemacs/evil-emacs-tag)
      evil-normal-state-tag   (propertize "« ☢ »" 'face 'paloryemacs/evil-normal-tag)
      evil-insert-state-tag   (propertize "« I »" 'face 'paloryemacs/evil-insert-tag)
      evil-motion-state-tag   (propertize "« M »" 'face 'paloryemacs/evil-motion-tag)
      evil-visual-state-tag   (propertize "« ∞ »" 'face 'paloryemacs/evil-visual-tag)
      evil-operator-state-tag (propertize "« O »" 'face 'paloryemacs/evil-operator-tag)
      evil-replace-state-tag  (propertize "« R »" 'face 'paloryemacs/evil-replace-tag)
      evil-iedit-state-tag    (propertize "« E »" 'face 'paloryemacs/evil-iedit-tag))

;; FIXME: as Official Emacs 24.4, if set color color,  when multile
;; windows (> 6), C-h evil-mode `q' then quit the help window will be very slow
;; see more https://bitbucket.org/lyro/evil/issue/487/after-set-colors-for-different-state-by
(if (eq window-system 'mac) ; emacs-mac-port (https://github.com/railwaycat/emacs-mac-port)
    (setq evil-emacs-state-cursor    `(box    ,(face-attribute 'paloryemacs/evil-emacs-tag    :foreground))
          evil-normal-state-cursor   `(box    ,(face-attribute 'paloryemacs/evil-normal-tag   :foreground))
          evil-insert-state-cursor   `(box    ,(face-attribute 'paloryemacs/evil-insert-tag   :foreground))
          evil-motion-state-cursor   `(box    ,(face-attribute 'paloryemacs/evil-motion-tag   :foreground))
          evil-visual-state-cursor   `(hollow ,(face-attribute 'paloryemacs/evil-visual-tag   :foreground))
          evil-replace-state-cursor  `(hbar   ,(face-attribute 'paloryemacs/evil-replace-tag  :foreground))
          evil-operator-state-cursor `(hollow ,(face-attribute 'paloryemacs/evil-operator-tag :foreground))
          evil-iedit-state-cursor    `(box   ,(face-attribute 'paloryemacs/evil-iedit-tag  :foreground)))
  (setq evil-default-cursor '(box "#cd0000") ; emacs official
        evil-emacs-state-cursor    'box
        evil-normal-state-cursor   'box
        evil-insert-state-cursor   'box
        evil-motion-state-cursor   'box
        evil-visual-state-cursor   'hollow
        evil-replace-state-cursor  'hbar
        evil-replace-state-cursor  'hbar
        evil-iedit-state-cursor    'box))

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
;;; Cycle emacs state
(defvar paloryemacs--saved-evil-state nil "Saved evil state for recover later.")
(make-variable-buffer-local 'paloryemacs--saved-evil-state)

(defun paloryemacs/evil-state-cycle (&optional state)
  "When STATE is non-nil, change evil state to it.
When STATE is nil, if `paloryemacs--saved-evil-state' is non-nil,
recover evil state to it, otherwiser change to evil-emacs-state."
  (when (and (boundp 'evil-mode) evil-mode)
    (if state
        (progn
          (setq paloryemacs--saved-evil-state evil-state)
          (evil-change-state state))
      (cond (paloryemacs--saved-evil-state
             (unless (eq evil-state paloryemacs--saved-evil-state)
               (evil-change-state paloryemacs--saved-evil-state))
             (setq paloryemacs--saved-evil-state nil))
            (t
             (unless (eq evil-state 'emacs)
               (setq paloryemacs--saved-evil-state evil-state)
               (evil-change-state 'emacs)))))))

;; Note: the defadvice to keyboard-quit will not work when execute read-key-sequence

;;; escape dwim
(defun paloryemacs/escape-dwim ()
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark t))
  (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
  (when multiple-cursors-mode
    (multiple-cursors-mode -1))
  (cond ((minibuffer-window-active-p (selected-window))
         (abort-recursive-edit))
        ((or (memq major-mode '(Info-mode org-agenda-mode help-mode))
             paloryemacs--saved-evil-state)
         (keyboard-quit))
        ((and (fboundp 'evil-mode) evil-mode)
         (cond ((eq evil-state 'emacs) (evil-exit-emacs-state))
               ((eq evil-state 'lispy)
                (let ((binding (key-binding (kbd "C-g"))))
                  (if binding (call-interactively binding)
                    (keyboard-quit))))
               (t (let ((binding (key-binding [escape])))
                    (if binding (call-interactively binding)
                      (keyboard-quit))))))
        (t (keyboard-quit))))

(defun paloryemacs/company-abort-then-escape-dwim ()
  (interactive)
  (company-abort)
  (paloryemacs/escape-dwim))


;;; use Karabiner to remap simultaneous key presses [D+F] to Escape
;; (key-chord-define-global "df" 'paloryemacs/escape-dwim)
;; (with-eval-after-load 'company
;;   (key-chord-define company-active-map "df" 'paloryemacs/company-abort-then-escape-dwim))

;;; 'imap fd <ESC>' equivalent
;; (key-chord-define-global "df" [escape])
;; (key-chord-define evil-normal-state-map       "df" 'paloryemacs/escape-dwim)
;; (key-chord-define evil-insert-state-map       "df" 'paloryemacs/escape-dwim)
;; (key-chord-define evil-visual-state-map       "df" 'paloryemacs/escape-dwim)
;; (key-chord-define evil-emacs-state-map        "df" 'paloryemacs/escape-dwim)
;; (key-chord-define evil-motion-state-map       "df" 'paloryemacs/escape-dwim)
;; (key-chord-define evil-replace-state-map      "df" 'paloryemacs/escape-dwim)
;; (key-chord-define evil-operator-state-map     "df" 'paloryemacs/escape-dwim)
;; (key-chord-define evil-operator-shortcut-map  "df" 'keyboard-quit)
;; (key-chord-define evil-read-key-map           "df" 'paloryemacs/escape-dwim)
;; (key-chord-define evil-outer-text-objects-map "df" 'paloryemacs/escape-dwim)
;; (key-chord-define evil-inner-text-objects-map "df" 'paloryemacs/escape-dwim)
;; (key-chord-define evil-ex-completion-map      "df" 'paloryemacs/escape-dwim)

;; (key-chord-define minibuffer-local-map            "df" 'paloryemacs/minibuffer-keyboard-quit)
;; (key-chord-define minibuffer-local-ns-map         "df" 'paloryemacs/minibuffer-keyboard-quit)
;; (key-chord-define minibuffer-local-completion-map "df" 'paloryemacs/minibuffer-keyboard-quit)
;; (key-chord-define minibuffer-local-must-match-map "df" 'paloryemacs/minibuffer-keyboard-quit)
;; (key-chord-define minibuffer-local-isearch-map    "df" 'paloryemacs/minibuffer-keyboard-quit)

(define-key minibuffer-local-map [escape] 'paloryemacs/escape-dwim)
;; (define-key minibuffer-local-ns-map [escape] 'paloryemacs/minibuffer-keyboard-quit)
;; (define-key minibuffer-local-completion-map [escape] 'paloryemacs/minibuffer-keyboard-quit)
;; (define-key minibuffer-local-must-match-map [escape] 'paloryemacs/minibuffer-keyboard-quit)
;; (define-key minibuffer-local-isearch-map [escape] 'paloryemacs/minibuffer-keyboard-quit)

;;;
(define-key evil-normal-state-map (kbd "C-o") 'paloryemacs/open-line-with-indent) ; default evil-jump-backward

;; (defun paloryemacs/evil-undefine ()
;;   (interactive)
;;   (let (evil-mode-map-alist)
;;     (call-interactively (key-binding (this-command-keys)))))
;; make sure that Evil's normal state never touches TAB, just wire this fall-through binding
;; (define-key evil-normal-state-map (kbd "TAB") 'paloryemacs/evil-undefine)
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
(define-key evil-insert-state-map "\C-v" nil)
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
(define-key evil-insert-state-map "\C-a" nil)
(define-key evil-insert-state-map "\C-w" nil)
(define-key evil-insert-state-map [remap newline] nil)
(define-key evil-insert-state-map [remap newline-and-indent] nil)


;; (with-eval-after-load "workgroups2"
;;   (defun paloryemacs/activate-all-major-mode-leader ()
;;     (dolist (buf (buffer-list))
;;       (with-current-buffer buf
;;         (paloryemacs/activate-major-mode-leader))))
;;   (add-hook 'wg-after-switch-to-workgroup-hook
;;             'paloryemacs/activate-all-major-mode-leader))


(paloryemacs/set-leader-keys
  "1"   'delete-other-windows
  "2"   (kbd "C-x 2")
  "3"   "\C-x3"
  "a"   'org-agenda
  ;; "b"   'bookmark-map
  ;; ";" "cc" "ci", "cl", "cp", "cr", "ct", "cy" and "cv" are used by evil-nerd-commenter
  "cs"  'paloryemacs/evil-change-symbol-in-defun
  "ch"  'crosshairs-mode
  "D"   'dired-jump
  "dp"  'dash-at-point
  "dP"  'dash-at-point-with-docset
  "ff"  'paloryemacs/toggle-full-window
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
  "gb"  'magit-blame-mode
  "gl"  'magit-log
  "gs"  'magit-status
  "gC"  'magit-commit
  "gg"  'counsel-git-grep
  "j"   'helm-etags-select
  "k"   'kill-this-buffer
  ;; "ll" are used by evil-nerd-commenter
  ;; "lr"  'paloryemacs/linum-relative-toggle
  "n"   'evil-narrow-indirect
  "p"   'projectile-commander
  "u"   'universal-argument
  "se"  'evil-iedit-state/iedit-mode
  "ss"  'helm-swoop
  "sS"  'helm-multi-swoop
  "s C-s" 'helm-multi-swoop-all
  ;; "ut"  'undo-tree-visualize
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

(paloryemacs/set-leader-keys
  ;; "TAB" 'spacemacs/alternate-buffer
  "bb"  'ivy-switch-buffer
  "bd"  'kill-this-buffer
  ;; "be"  'spacemacs/safe-erase-buffer
  ;; "bh"  'spacemacs/home
  "bk"  'paloryemacs/kill-matching-buffers-rudely
  "bn"  'next-buffer
  ;; "em"  'spacemacs/kill-other-buffers
  ;; "bN"  'spacemacs/new-empty-buffer
  ;; "bP"  'spacemacs/copy-clipboard-to-whole-buffer
  "bp"  'previous-buffer
  ;; "bR"  'spacemacs/safe-revert-buffer
  "bs"  'paloryemacs/switch-to-scratch-buffer
  ;; "bY"  'spacemacs/copy-whole-buffer-to-clipboard
  "bw"  'read-only-mode)

(paloryemacs/declare-prefix "b" "buffers")

;; from https://github.com/gempesaw/dotemacs/blob/emacs/dg-defun.el
(defun paloryemacs/kill-matching-buffers-rudely (regexp &optional internal-too)
  "Kill buffers whose name matches the specified REGEXP. This
function, unlike the built-in `kill-matching-buffers` does so
WITHOUT ASKING. The optional second argument indicates whether to
kill internal buffers too."
  (interactive "sKill buffers matching this regular expression: \nP")
  (dolist (buffer (buffer-list))
    (let ((name (buffer-name buffer)))
      (when (and name (not (string-equal name ""))
                 (or internal-too (/= (aref name 0) ?\s))
                 (string-match regexp name))
        (kill-buffer buffer)))))


(defun paloryemacs/switch-to-scratch-buffer ()
  "Switch to the `*scratch*' buffer. Create it first if needed."
  (interactive)
  (let ((exists (get-buffer "*scratch*")))
    (switch-to-buffer (get-buffer-create "*scratch*"))
    (when (and (not exists)
               (not (eq major-mode dotpaloryemacs-scratch-mode))
               (fboundp dotpaloryemacs-scratch-mode))
      (funcall dotpaloryemacs-scratch-mode))))

;; only active in certain evil states which defined in bind-map-default-evil-states
(defun paloryemacs-bootstrap/init-bind-map ()
  (require 'bind-map)
  (bind-map paloryemacs-default-map
    :prefix-cmd paloryemacs-cmds
    :keys (dotpaloryemacs-emacs-leader-key)
    :evil-keys (dotpaloryemacs-leader-key)
    :override-minor-modes t
    :override-mode-name paloryemacs-leader-override-mode))

(paloryemacs-bootstrap/init-bind-map)

;;; emacs-lisp
(paloryemacs/set-leader-keys-for-major-mode 'emacs-lisp-mode
  "cc" 'emacs-lisp-byte-compile
  "e$" 'lisp-state-eval-sexp-end-of-line
  "eb" 'eval-buffer
  "ee" 'eval-last-sexp
  "er" 'eval-region
  "ef" 'eval-defun
  "el" 'lisp-state-eval-sexp-end-of-line
  ","  'lisp-state-toggle-lisp-state
  "tb" 'paloryemacs/ert-run-tests-buffer
  "tq" 'ert
  "f" 'describe-function/with-ido
  "k" 'describe-key
  "hh" 'elisp-slime-nav-describe-elisp-thing-at-point
  "gg" 'elisp-slime-nav-find-elisp-thing-at-point
  "v" 'describe-variable/with-ido)

(require 'evil-lisp-state nil t)
(with-eval-after-load "evil-lisp-state"
  (setq evil-lisp-state-global t)
  (paloryemacs/set-leader-keys "k" evil-lisp-state-map))

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
(loop for (mode . state) in
      '((comint-mode               . insert)
        (compilation-mode          . emacs)
        (diff-mode                 . emacs)
        (eshell-mode               . emacs)
        (eww-mode                  . emacs)
        (helm-grep-mode            . emacs)
        (ibuffer-mode              . normal)
        (inferior-emacs-lisp-mode  . emacs)
        (inf-ruby-mode             . emacs)
        (log-edit-mode             . emacs)
        (flycheck-error-list-mode  . emacs)
        (fundamental-mode          . normal)
        (erc-mode                  . emacs)
        (git-commit-mode           . insert)
        (git-rebase-mode           . emacs)
        (gtags-select-mode         . emacs)
        (gud-mode                  . emacs)
        (grep-mode                 . emacs)
        (haskell-error-mode        . emacs)
        (haskell-interactive-mode  . insert)
        (Info-mode                 . emacs)
        (message-mode              . emacs)
        (nrepl-mode                . insert)
        (weibo-timeline-mode       . emacs)
        (weibo-post-mode           . emacs)
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
(defun paloryemacs/newline-and-indent-relative ()
  (interactive)
  (newline)
  (indent-to-column
   (save-excursion
     (forward-line -1)
     (back-to-indentation)
     (current-column))))

(defun paloryemacs/haskell-evil-open-below ()
  (interactive)
  (evil-append-line 1)
  (paloryemacs/newline-and-indent-relative))

(defun paloryemacs/haskell-evil-open-above ()
  (interactive)
  (previous-line)
  (evil-append-line 1)
  (paloryemacs/newline-and-indent-relative))

(evil-declare-key 'insert haskell-mode-map (kbd "RET") 'paloryemacs/newline-and-indent-relative)
(evil-declare-key 'normal haskell-mode-map "o" 'paloryemacs/haskell-evil-open-below)
(evil-declare-key 'normal haskell-mode-map "O" 'paloryemacs/haskell-evil-open-above)
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


;;; evil-nerd-commenter
(with-eval-after-load "evil"
  (setq evilnc-hotkey-comment-operator (kbd "SPC SPC"))
  (require 'evil-nerd-commenter nil t))

(global-set-key (kbd "M-;") 'evilnc-comment-or-uncomment-lines)

(paloryemacs/set-leader-keys
  "SPC" 'evilnc-comment-operator ; alternative ";"
  "cc" 'evilnc-copy-and-comment-lines
  "ci" 'evilnc-toggle-invert-comment-line-by-line
  "cl" 'evilnc-comment-or-uncomment-lines
  "cp" 'evilnc-comment-or-uncomment-paragraphs
  "cr" 'comment-or-uncomment-region
  "ct" 'evilnc-quick-comment-or-uncomment-to-the-line
  "cy" 'evilnc-copy-and-comment-lines
  "cv" 'evilnc-toggle-invert-comment-line-by-line)

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
(defun paloryemacs/evil-change-symbol-in-defun ()
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


;; (with-eval-after-load "magit"
;;   (define-key magit-status-mode-map "j" 'magit-section-forward)
;;   (define-key magit-status-mode-map "k" 'magit-section-backward))

;;; org agenda -- leave in emacs mode but add j & k
(eval-after-load "org-agenda"
  '(progn
     (define-key org-agenda-mode-map "j" 'evil-next-line) ; overide org-agenda-goto-date
     (define-key org-agenda-mode-map "k" 'evil-previous-line))) ; org-agenda-capture

;;; evil-exchange
;; `gx': evil-exchange-key, `gX': evil-exchange-cancel-key
;; (add-to-list 'load-path "~/.emacs.d/site-lisp/evil-exchange")
;; installed with el-get
(require 'evil-exchange nil t)
(if (fboundp 'evil-exchange-install)
    (evil-exchange-install))

;;; work with mutiple cursors
(add-hook 'multiple-cursors-mode-enabled-hook
          'paloryemacs/evil-switch-to-insert-maybe)
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
  ;; (add-hook 'git-timemachine-mode-hook 'paloryemacs/evil-state-cycle)

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

;; Abort company-mode when exiting insert mode
(defun paloryemacs/abort-company-on-insert-state-exit ()
  (when (fboundp 'company-abort)
    (company-abort)))

(add-hook 'evil-insert-state-exit-hook 'paloryemacs/abort-company-on-insert-state-exit)

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
  (setq evil-snipe-scope 'line)

  (evil-snipe-mode +1)
  ;; replaces evil-mode's f/F/t/T/;/, with snipe
  (evil-snipe-override-mode +1))


;; evil-iedit-state
(require 'iedit nil t)
(with-eval-after-load "iedit"
  (require 'evil-iedit-state nil t)
  (with-eval-after-load "evil-iedit-state"
    ))

(require 'evil-visualstar nil t)
(with-eval-after-load 'evil-visualstar
  (global-evil-visualstar-mode +1))

;;; shift
;; (setq evil-shift-width 2)
(defun paloryemacs/shift-left-visual ()
  "Shift left and restore visual selection."
  (interactive)
  (evil-shift-left (region-beginning) (region-end))
  (evil-normal-state)
  (evil-visual-restore))

(defun paloryemacs/shift-right-visual ()
  "Shift right and restore visual selection."
  (interactive)
  (evil-shift-right (region-beginning) (region-end))
  (evil-normal-state)
  (evil-visual-restore))

(define-key evil-visual-state-map (kbd ">") 'paloryemacs/shift-right-visual)
(define-key evil-visual-state-map (kbd "<") 'paloryemacs/shift-left-visual)

;;; auto insert blank in between Chinese and English
(define-key evil-visual-state-map "g " 'evil/add-blank-between-chinese-and-english)

(defun evil/add-blank-between-chinese-and-english ()
  (interactive)
  (paloryemacs/add-blank-between-chinese-and-english (region-beginning) (region-end))
  (evil-normal-state))

;;; bugfix
;; https://bitbucket.org/lyro/evil/issue/432/edebug-mode-map-cant-take-effect-for-the
;; (add-hook 'edebug-mode-hook 'evil-normalize-keymaps) ; recreate `evil-mode-map-alist'
(with-eval-after-load 'edebug
  (add-hook 'edebug-mode-hook 'paloryemacs/evil-state-cycle))

(provide '52evil-mode)
;;; 50evil-mode.el ends here
