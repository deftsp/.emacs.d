;;; 09evil-mode.el ---                               -*- lexical-binding: t; -*-

;; Copyright (C) 2013  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>
;; Keywords:

;;; evil special map
;; evil-normal-state-map
;; evil-insert-state-map
;; evil-visual-state-map
;; evil-emacs-state-map
;; evil-motion-state-map
;; evil-replace-state-map
;; evil-operator-state-map
;; evil-operator-shortcut-map
;; evil-read-key-map
;; evil-outer-text-objects-map
;; evil-inner-text-objects-map
;; evil-ex-completion-map
;; minibuffer-local-map
;; minibuffer-local-ns-map
;; minibuffer-local-completion-map
;; minibuffer-local-must-match-map
;; minibuffer-local-isearch-map
;; minibuffer-local-completion-map
;; minibuffer-local-must-match-map
;; minibuffer-local-isearch-map


;; `C-M-x' on a defface expression reinitializes the face according to the
;; defface specification.

(defface tl/evil-normal-tag
  `((t (:weight bold :foreground "orchid")))
  "Evil normal mode indicator face")

(defface tl/evil-insert-tag
  `((t (:weight bold :foreground "OliveDrab1")))
  "Evil insert mode indicator face")

(defface tl/evil-replace-tag
  `((t (:weight bold :foreground "orchid")))
  "Evil replace mode indicator face")

(defface tl/evil-emacs-tag
  `((t (:weight bold :foreground "#7cfa42")))
  "Evil emacs mode indicator face")

(defface tl/evil-visual-tag
  `((t (:weight bold :foreground "Purple")))
  "Evil visual mode indicator face")

(defface tl/evil-motion-tag
  `((t (:weight bold :foreground "Cyan")))
  "Evil motion mode indicator face")

(defface tl/evil-operator-tag
  `((t (:weight bold :foreground "maroon")))
  "Evil operator mode indicator face")

(defface tl/evil-treemacs-tag
  `((t (:weight bold :foreground "#00cbcb")))
  "Evil treemacs mode indicator face")

(defface tl/evil-lispy-tag
  `((t (:weight bold :foreground "orange")))
  "Evil lispy mode indicator face")

;;; visual indicators
(setq evil-mode-line-format 'before
      evil-emacs-state-tag    (propertize "« E »" 'face 'tl/evil-emacs-tag)
      evil-normal-state-tag   (propertize "« ☢ »" 'face 'tl/evil-normal-tag)
      evil-insert-state-tag   (propertize "« I »" 'face 'tl/evil-insert-tag)
      evil-motion-state-tag   (propertize "« M »" 'face 'tl/evil-motion-tag)
      evil-visual-state-tag   (propertize "« V »" 'face 'tl/evil-visual-tag)
      evil-operator-state-tag (propertize "« O »" 'face 'tl/evil-operator-tag)
      evil-replace-state-tag  (propertize "« R »" 'face 'tl/evil-replace-tag))

;; FIXME: as Official Emacs 24.4, if set color color,  when multile
;; windows (> 6), C-h evil-mode `q' then quit the help window will be very slow
;; see more https://bitbucket.org/lyro/evil/issue/487/after-set-colors-for-different-state-by
(if (member window-system '(ns mac))   ; emacs-mac-port (https://github.com/railwaycat/emacs-mac-port)
    (setq evil-emacs-state-cursor    `(box    ,(face-attribute 'tl/evil-emacs-tag    :foreground))
          evil-normal-state-cursor   `(box    ,(face-attribute 'tl/evil-normal-tag   :foreground))
          evil-insert-state-cursor   `(box    ,(face-attribute 'tl/evil-insert-tag   :foreground))
          evil-motion-state-cursor   `(box    ,(face-attribute 'tl/evil-motion-tag   :foreground))
          evil-visual-state-cursor   `(hollow ,(face-attribute 'tl/evil-visual-tag   :foreground))
          evil-replace-state-cursor  `(hbar   ,(face-attribute 'tl/evil-replace-tag  :foreground))
          evil-operator-state-cursor `(hollow ,(face-attribute 'tl/evil-operator-tag :foreground))
          ;; FIXME: not works after recreate treemacs window
          evil-treemacs-state-cursor `(box    ,(face-attribute 'tl/evil-treemacs-tag :foreground))
          evil-lispy-state-cursor    `(box    ,(face-attribute 'tl/evil-lispy-tag    :foreground)))
  (setq evil-default-cursor '(box "#cd0000") ; emacs official
        evil-emacs-state-cursor    'box
        evil-normal-state-cursor   'box
        evil-insert-state-cursor   'box
        evil-motion-state-cursor   'box
        evil-visual-state-cursor   'hollow
        evil-replace-state-cursor  'hbar
        evil-operator-state-cursor 'hbar
        evil-treemacs-state-cursor 'box
        evil-lisp-state-cursor     'hbar))

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
(defvar tl--saved-evil-state nil "Saved evil state for recover later.")
(make-variable-buffer-local 'tl--saved-evil-state)

(defun tl/evil-state-cycle (&optional state)
  "When STATE is non-nil, change evil state to it.
When STATE is nil, if `tl--saved-evil-state' is non-nil,
recover evil state to it, otherwiser change to evil-emacs-state."
  (when (and (boundp 'evil-mode) evil-mode)
    (if state
        (progn
          (setq tl--saved-evil-state evil-state)
          (evil-change-state state))
      (cond (tl--saved-evil-state
             (unless (eq evil-state tl--saved-evil-state)
               (evil-change-state tl--saved-evil-state))
             (setq tl--saved-evil-state nil))
            (t
             (unless (eq evil-state 'emacs)
               (setq tl--saved-evil-state evil-state)
               (evil-change-state 'emacs)))))))

;; Note: the defadvice to keyboard-quit will not work when execute read-key-sequence

;;; escape dwim
(defun tl/keyboard-escape-quit ()
  "Enhanced Edition of the build-in function `keyboard-escape-quit`."
  (interactive)
  (when (and delete-selection-mode transient-mark-mode mark-active)
    (setq deactivate-mark t))
  (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
  (cond ((eq last-command 'mode-exited) nil)
	    ((region-active-p)
	     (deactivate-mark))
        ((and delete-selection-mode transient-mark-mode mark-active)
         (setq deactivate-mark t))
	    ((> (minibuffer-depth) 0) ; (minibuffer-window-active-p (selected-window))
	     (abort-recursive-edit))
	    (current-prefix-arg nil)
	    ((> (recursion-depth) 0)
	     (exit-recursive-edit))
	    (buffer-quit-function
	     (funcall buffer-quit-function))
	    ((string-match "^ \\*" (buffer-name (current-buffer)))
	     (bury-buffer))
        ((get-buffer "*Completions*")
         (delete-windows-on "*Completions*"))
        ((or (memq major-mode '(Info-mode org-agenda-mode help-mode))
             tl--saved-evil-state)
         (keyboard-quit))
        ((and (fboundp 'evil-mode) evil-mode (eq evil-state 'emacs))
         (evil-exit-emacs-state))
        ((and (fboundp 'evil-mode) evil-mode (eq evil-state 'lispy))
         (let ((binding (key-binding (kbd "C-g"))))
           (if binding (call-interactively binding)
             (keyboard-quit))))
        ((and (fboundp 'evil-mode) evil-mode)
         (let ((binding (key-binding [escape])))
           (if binding (call-interactively binding)
             (keyboard-quit))))
        ((not (one-window-p t))
         (delete-other-windows))
        (t (keyboard-quit))))

(defun tl/company-abort-then-escape-dwim ()
  (interactive)
  (company-abort)
  (tl/keyboard-escape-quit))

;;; use Karabiner to remap simultaneous key presses [D+F] to Escape
;; (key-chord-define-global "df" 'tl/keyboard-escape-quit)
;; (key-chord-define-global "df" [escape])
;; (with-eval-after-load 'company
;;   (key-chord-define company-active-map "df" 'tl/company-abort-then-escape-dwim))


;;;
(defun tl/evil-undefine ()
  (interactive)
  (let (evil-mode-map-alist)
    (call-interactively (key-binding (this-command-keys)))))

;; (use-package evil-easymotion
;;   :after (evil)
;;   :config
;;   (evilem-default-keybindings "g SPC")
;;   (evilem-default-keybindings "gs"))

;; (with-eval-after-load "workgroups2"
;;   (defun tl/activate-all-major-mode-leader ()
;;     (dolist (buf (buffer-list))
;;       (with-current-buffer buf
;;         (tl/activate-major-mode-leader))))
;;   (add-hook 'wg-after-switch-to-workgroup-hook
;;             'tl/activate-all-major-mode-leader))



;; from https://github.com/gempesaw/dotemacs/blob/emacs/dg-defun.el
(defun tl/kill-matching-buffers-rudely (regexp &optional internal-too)
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


(defun tl/switch-to-scratch-buffer ()
  "Switch to the `*scratch*' buffer. Create it first if needed."
  (interactive)
  (let ((exists (get-buffer "*scratch*")))
    (switch-to-buffer (get-buffer-create "*scratch*"))
    (when (and (not exists)
               (not (eq major-mode dottl-scratch-mode))
               (fboundp dottl-scratch-mode))
      (funcall dottl-scratch-mode))))

;;; default mode
(defun tl/evil-set-initial-state ()
  (cl-loop
   for (mode . state) in
   '((comint-mode               . insert)
     (compilation-mode          . normal)
     (color-rg-mode             . normal)
     (diff-mode                 . emacs)
     (docker-image-mode         . normal)
     (docker-container-mode     . normal)
     (docker-network-mode       . normal)
     (docker-volume-mode        . normal)
     (docker-machine-mode       . normal)
     (eshell-mode               . insert)
     (eww-mode                  . emacs)
     (helm-grep-mode            . emacs)
     (ibuffer-mode              . normal)
     (bookmark-bmenu-mode       . normal)
     (dired-mode                . normal)
     (dired-sidebar-mode        . normal)
     (easy-hugo-mode            . emacs)
     (emms-playlist-mode        . normal)
     (emms-metaplaylist-mode    . normal)
     (explain-pause-top-mode    . emacs)
     (irfc-mode                 . normal)
     (inferior-emacs-lisp-mode  . emacs)
     (inf-ruby-mode             . emacs)
     (log-edit-mode             . emacs)
     (flycheck-error-list-mode  . normal)
     (flutter-mode  . normal)
     (fundamental-mode          . normal)
     (erc-mode                  . emacs)
     (git-commit-mode           . insert)
     (git-rebase-mode           . emacs)
     (gtags-select-mode         . emacs)
     (gud-mode                  . emacs)
     (grep-mode                 . normal)
     (haskell-error-mode        . emacs)
     (haskell-interactive-mode  . insert)
     (helpful-mode              . motion)
     (message-mode              . emacs)
     (org-agenda-mode           . normal)
     (nrepl-mode                . insert)
     (prodigy-mode              . normal)
     (prodigy-view-mode         . normal)
     (profiler-report-mode      . emacs)
     (weibo-timeline-mode       . emacs)
     (weibo-post-mode           . emacs)
     (rg-mode                   . normal)
     (sr-mode                   . emacs)
     (shell-mode                . emacs)
     (skewer-clients-mode       . normal)
     (term-mode                 . emacs)
     (tuareg-interactive-mode   . insert)
     (speedbar-mode             . emacs)
     (vterm-mode                . insert)
     (yari-mode                 . emacs))
   do (evil-set-initial-state mode state)))

(defun tl/evil-init ()
  (setq evil-disable-insert-state-bindings nil
        evil-move-cursor-back nil
        evil-move-beyond-eol t
        evil-want-integration t
        evil-want-keybinding nil ;; evil-collection instead
        evil-want-visual-char-semi-exclusive t
        evil-want-C-i-jump t
        evil-cross-lines t
        ;; evil-want-fine-undo t
        ;; evil-highlight-closing-paren-at-point-states nil
        evil-magic 'very-magic))


(use-package evil
  :init
  (tl/evil-init)
  :config
  (evil-set-undo-system 'undo-fu)
  ;; (evil-set-undo-system 'undo-tree)
  ;; (with-eval-after-load 'undo-tree
  ;;   (add-hook 'evil-local-mode-hook 'turn-on-undo-tree-mode))

  ;; (setcdr evil-insert-state-map nil) ;; make insert state like emacs state
  (define-key evil-insert-state-map [remap evil-complete-previous] 'hippie-expand)
  (tl/evil-set-initial-state)
  (dolist (m (list minibuffer-local-map
                   minibuffer-local-ns-map
                   minibuffer-local-completion-map
                   minibuffer-local-must-match-map
                   minibuffer-local-isearch-map))
    ;; 'tl/keyboard-escape-quit
    (define-key m (kbd "<escape>") 'keyboard-escape-quit))

  ;; (define-key evil-normal-state-map "p" 'evil-paste-after)
  ;; (define-key evil-normal-state-map "P" 'evil-paste-before)

  ;; alternate binding to search next occurrence with isearch without
  ;; exiting isearch
  (define-key isearch-mode-map (kbd "S-<return>") 'isearch-repeat-forward)
  (define-key isearch-mode-map (kbd "M-S-<return>") 'isearch-repeat-backward)
  ;; Escape from isearch-mode("/" and "?" in evil-mode) like vim
  (define-key isearch-mode-map (kbd "<escape>") 'isearch-cancel)

  (general-evil-setup t)

  (nmap
    "H"    "^"
    ;; make sure that Evil's normal state never touches TAB, just wire this fall-through binding
    ;; "TAB" 'tl/evil-undefine
    "gL"  'org-mac-link-get-link
    "gd"  'smart-jump-go
    ;; "gD"  'smart-jump-go-other-window
    "gO"  'ff-find-other-file
    "Q" "gqap"
    ;; "s" "gvfd"
    ;; "S" "vabsba"
    "+" 'evil-numbers/inc-at-pt         ; default `evil-next-line-first-non-blank'
    "-" 'evil-numbers/dec-at-pt         ; default `evil-previous-line-first-non-blank'
    ;; "C-e" 'evil-end-of-line

    ;; q is being used at many places to close things, and sometimes it so
    ;; happens that evil mode is turned on in that window at the same time,
    ;; which results in recording a macro instead of closing the window.
    "q" nil                    ; `q' is binded to `evil-record-macro'
    ;; "s" nil ; default `evil-substitute' remove default binding so I can override it
    ;; "S" nil ; default `evil-change-whole-line' remove default binding so I can override it
    )

  (mmap
    "TAB" 'indent-for-tab-command
    ;; reserved for evil-snipe
    ;; "s" 'evil-avy-goto-char-timer
    ;; "S" 'evil-avy-goto-word-or-subword-1

    [C-i] 'evil-jump-forward            ; bind evil-jump-forward for GUI only.
    "C-v" nil)

  (vmap
    ")"  "S)"
    "v" 'evil-visual-block              ; make it easy to switch to visual-char-block mode from visual-char
    "Q" "gq")

  (imap
    "C-v" nil
    "C-k" nil
    "C-o" nil
    "C-r" nil
    "C-y" nil
    "C-e" nil
    "C-n" nil
    "C-p" nil
    "C-x C-n" nil
    "C-x C-p" nil
    "C-t" nil
    "C-d" nil
    "C-a" nil
    "C-w" nil
    "C-o" 'tl/open-line-with-indent
    [remap newline] nil
    [remap newline-and-indent] nil)

  (omap
    "." 'evil-avy-goto-word-or-subword-1
    "l" 'evil-avy-goto-line
    "c" 'evil-avy-goto-char
    "SPC" 'evil-avy-goto-char-timer
    ",w" 'evil-avy-goto-subword-0)

  ;; the `d' and `f' behavior is strange with following setting
  ;; (general-define-key
  ;;  :keymaps '(evil-ex-completion-map evil-ex-search-keymap)
  ;;  "C-a" #'evil-beginning-of-line
  ;;  "C-e" #'evil-end-of-line
  ;;  "C-b" #'evil-backward-char
  ;;  "C-f" #'evil-forward-char
  ;;  "M-j" #'next-complete-history-element
  ;;  "M-k" #'previous-complete-history-element)

  (evil-mode +1))

(use-package evil-collection
  :after evil
  :init
  ;; (setq evil-collection-mode-list '(calendar info magit magit-todos vterm (pdf pdf-view)))
  (setq evil-collection-setup-minibuffer t)
  (setq evil-collection-key-whitelist '())
  (setq evil-collection-key-blacklist '("M-o" "gz" "gr"))
  :config
  (evil-collection-init)
  (general-define-key
   :states '(normal)
   :keymaps 'magit-status-mode-map
   "gr" 'magit-refresh)

  (evil-collection-define-key 'normal 'Info-mode-map "o" 'ace-link-info)
  (evil-collection-define-key 'normal 'xref--xref-buffer-mode-map "o" 'ace-link-xref)
  (evil-collection-define-key 'normal 'compilation-mode-map "o" 'ace-link-compilation)
  (evil-collection-define-key 'normal 'woman-mode-map "o" 'ace-link-woman)
  (evil-collection-define-key 'normal 'helpful-mode-map "o" 'ace-link-help)
  (evil-collection-define-key 'normal 'help-mode-map "o" 'ace-link-help))

(use-package evil-collection-unimpaired
  :defer t
  :diminish evil-collection-unimpaired-mode)

(use-package evil-escape
  :commands (evil-escape)
  :diminish evil-escape-mode
  :defer t
  :init
  ;; use key-chord instead evil-escape's key sequence
  ;; (setq-default evil-escape-key-sequence "jk")
  ;; (setq-default evil-escape-delay 0.08)
  (setq evil-escape-unordered-key-sequence t)
  (global-set-key (kbd "C-c C-g") 'evil-escape)
  :chords (("jk" . evil-escape))
  :config
  (progn
    (evil-escape-mode +1)))

;;; evil-surround
(use-package evil-surround
  :after evil
  :config
  ;; evil-surround-pairs-alist is a buffer local variable
  ;; (setq-default evil-surround-pairs-alist (cl-adjoin
  ;;                                          '(?~ . ("``" . "``"))
  ;;                                          evil-surround-pairs-alist))
  (with-eval-after-load 'org
    (defun tl/add-org-surrounds ()
      (push '(?: . tl//surround-drawer) evil-surround-pairs-alist)
      (push '(?# . tl//surround-code) evil-surround-pairs-alist))

    (add-hook 'org-mode-hook 'tl/add-org-surrounds))
  (global-evil-surround-mode +1))

;;; evil-embrace
;; (with-eval-after-load 'org
;; (when  (fboundp 'evil-embrace-enable-evil-surround-integration)
;;   (setq embrace-show-help-p nil
;;         evil-embrace-show-help-p nil)
;;   (evil-embrace-enable-evil-surround-integration)))

;; (define-key evil-motion-state-map "f" 'jump-char-forward)
;; (define-key evil-motion-state-map "F" 'jump-char-backward)

;;; haskell mode
(with-eval-after-load 'evil
  (defun tl/newline-and-indent-relative ()
    (interactive)
    (newline)
    (indent-to-column
     (save-excursion
       (forward-line -1)
       (back-to-indentation)
       (current-column))))

  (defun tl/haskell-evil-open-below ()
    (interactive)
    (evil-append-line 1)
    (tl/newline-and-indent-relative))

  (defun tl/haskell-evil-open-above ()
    (interactive)
    (previous-line)
    (evil-append-line 1)
    (tl/newline-and-indent-relative))

  ;; (evil-declare-key 'normal haskell-mode-map ",gn" 'ghc-goto-next-error)
  ;; (evil-declare-key 'normal haskell-mode-map ",gp" 'ghc-goto-prev-error)

  ;; (evil-define-key 'normal haskell-mode-map (kbd "gT") 'haskell-process-do-type)
  ;; (evil-define-key 'normal haskell-mode-map (kbd "gI") 'haskell-process-do-info)
  ;; (evil-define-key 'normal haskell-mode-map (kbd "gz") 'haskell-interactive-switch)
  ;; (evil-define-key 'normal haskell-interactive-mode (kbd "gz") 'haskell-interactive-switch-back)
  ;; (evil-define-key 'normal haskell-cabal-mode-map (kbd "gz") 'haskell-interactive-switch)

  ;; (evil-define-key 'normal haskell-mode-map (kbd "gl") 'haskell-process-load-or-reload)

  ;; (evil-define-key 'normal haskell-interactive-mode-map (kbd "RET") #'haskell-interactive-mode-return)
  ;; (evil-define-key 'insert haskell-interactive-mode-map (kbd "C-u") 'haskell-interactive-mode-kill-whole-line)
  ;; (evil-define-key 'normal haskell-interactive-mode-map (kbd "G") 'end-of-buffer)
  (evil-declare-key 'insert haskell-mode-map (kbd "RET") 'tl/newline-and-indent-relative)
  (evil-declare-key 'normal haskell-mode-map "o" 'tl/haskell-evil-open-below)
  (evil-declare-key 'normal haskell-mode-map "O" 'tl/haskell-evil-open-above))


;;; evil-nerd-commenter
(global-set-key (kbd "M-;") 'evilnc-comment-or-uncomment-lines)
(use-package evil-nerd-commenter
  :defer t
  :commands (evilnc-comment-or-uncomment-lines evilnc-comment-operator)
  :init
  (progn
    (define-key evil-normal-state-map "gc" 'evilnc-comment-operator)
    (define-key evil-normal-state-map "gy" 'evilnc-copy-and-comment-lines)
    (tl/set-leader-keys
      "ci" 'evilnc-comment-or-uncomment-lines
      "cl" 'evilnc-quick-comment-or-uncomment-to-the-line
      "cc" 'evilnc-copy-and-comment-lines
      "cp" 'evilnc-comment-or-uncomment-paragraphs
      "cr" 'comment-or-uncomment-region
      "cv" 'evilnc-toggle-invert-comment-line-by-line
      "c." 'evilnc-copy-and-comment-operator
      ;; SPC ; SPC: comment out current line
      ;; SPC ; 9 j: comment out the next9  lines
      ;; SPC ; a o: comment out the current symbol
      ";"  'evilnc-comment-operator)))

;; ;;; evil-indent-plus which replace evil-indent-textobject
(use-package evil-indent-plus
  :after (evil)
  :init
  (evil-indent-plus-default-bindings))

;;; evil-matchit
(use-package evil-matchit
  :after (evil)
  :commands (evilmi-inner-text-object evilmi-outer-text-object evilmi-jump-items)
  :init
  (general-define-key
   :states '(normal visual motion operator)
   "M" 'evilmi-jump-items)
  ;; (general-define-key
  ;;  :keymaps '(evil-inner-text-objects-map)
  ;;  "m" 'evilmi-inner-text-object)
  ;; (general-define-key
  ;;  :keymaps '(evil-outer-text-objects-map)
  ;;  "m" 'evilmi-outer-text-object)
  :config
  (global-evil-matchit-mode +1))


(evil-define-text-object tl:evil-textobj-whole-buffer (count &optional _beg _end type)
  "Text object to select the whole buffer."
  (evil-range (point-min) (point-max) type))



;; (save-excursion
;;   (cons
;;    (progn
;;      (rust-beginning-of-defun 1)
;;      (point))
;;    (progn
;;      (rustic-end-of-defun)
;;      (point))))


(evil-define-text-object tl:evil-textobj-defun-outer (count &optional _beg _end type)
  "Text object to select the whole buffer."
  (cl-destructuring-bind (beg . end)
      (if (and (eq major-mode 'rustic-mode) (and (boundp 'tree-sitter-mode) tree-sitter-mode) )
          (let ((range (tl/textbobj-function-outer)))
            (cons (car range) (cadr range)))
        (bounds-of-thing-at-point 'defun))
    (evil-range beg end type)))

(evil-define-text-object tl:evil-textobj-defun-inner (count &optional _beg _end type)
  "Text object to select the whole buffer."
  (cl-destructuring-bind (beg . end)
      (if (and (eq major-mode 'rustic-mode) (and (boundp 'tree-sitter-mode) tree-sitter-mode))
          (let ((range (tl/textbobj-function-inner)))
            (cons (car range) (cadr range)))
        (bounds-of-thing-at-point 'defun))
    (evil-range beg end type)))

(define-key evil-outer-text-objects-map "m" 'tl:evil-textobj-defun-outer)
(define-key evil-inner-text-objects-map "m" 'tl:evil-textobj-defun-inner)


;; https://github.com/meain/evil-textobj-tree-sitter
(use-package evil-textobj-tree-sitter
  :after tree-sitter
  :config
  ;; bind `function.outer`(entire function block) to `m` for use in things like `vaf`, `yaf`
  (fset 'tl/textbobj-function-outer (evil-textobj-tree-sitter-get-textobj "function.outer"))
  ;; bind `function.inner`(function block without name and args) to `m` for use in things like `vif`, `yif`
  (fset 'tl/textbobj-function-inner (evil-textobj-tree-sitter-get-textobj "function.inner")))


;; (evil-define-text-object tl/evil-textobj-a-defun (count &optional beg end type)
;;   (evil-select-an-object 'evil-defun beg end type count))

;; (evil-define-text-object tl/evil-textobj-inner-defun (count &optional beg end type)
;;   (evil-select-inner-object 'evil-defun beg end type count))

;; (general-define-key
;;  :keymaps '(evil-inner-text-objects-map)
;;  "m" 'tl/evil-textobj-inner-defun)

;; (general-define-key
;;  :keymaps '(evil-outer-text-objects-map)
;;  "m" 'tl/evil-textobj-a-defun)

;;; evil-textobj-between.el
(use-package evil-textobj-between
  :defer 3)

;;; evil-numbers
(use-package evil-numbers
  :defer t
  :init
  (define-key evil-normal-state-map (kbd "C-c +") 'evil-numbers/inc-at-pt)
  (define-key evil-normal-state-map (kbd "C-c -") 'evil-numbers/dec-at-pt))

;;; refactor/rename a variable name in a function efficiently
;; http://blog.binchen.org/?p=583
(defun tl/evil-change-symbol-in-defun ()
  "mark the region in defun (definition of function) and use string replacing UI in evil-mode
to replace the symbol under cursor"
  (interactive)
  (let ((old (thing-at-point 'symbol)))
    (mark-defun)
    (unless (evil-visual-state-p)
      (evil-visual-state))
    (evil-ex (concat "'<,'>s/" (if (= 0 (length old)) "" "\\<\\(") old (if (= 0 (length old)) "" "\\)\\>/")))))

;;; Lua mode
(use-package mobdebug-mode
  :defer t
  :init
  (evil-declare-key 'normal lua-mode-map
    ",md" 'mobdebug-minor-mode))

(with-eval-after-load 'evil
  ;; (evil-add-hjkl-bindings grep-mode-map 'emacs)
  ;; (evil-add-hjkl-bindings helm-grep-mode-map 'emacs)
  ;; both `h' and `? default binding to describe-mode
  (evil-add-hjkl-bindings help-mode-map 'emacs))

;;; flycheck
(with-eval-after-load "flycheck"
  (define-key flycheck-error-list-mode-map "j" 'evil-next-line)
  (define-key flycheck-error-list-mode-map "k" 'evil-previous-line))

;;; eww
(with-eval-after-load 'evil
  (evil-add-hjkl-bindings eww-mode-map 'emacs
    ;; defaut "l"
    "L" 'eww-back-url))

;;; evil-exchange
;; `gx': evil-exchange-key, `gX': evil-exchange-cancel-key
;; (add-to-list 'load-path "~/.emacs.d/site-lisp/evil-exchange")
;; installed with el-get
(use-package evil-exchange
  :defer 3
  :config
  (evil-exchange-install))

;;; evil-args
(use-package evil-args
  :defer t
  :init
  ;; evil-args-openers
  ;; evil-args-closers
  ;; evil-args-delimiters
  (progn
    (with-eval-after-load 'evil
      ;; bind evil-args text objects
      (define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
      (define-key evil-outer-text-objects-map "a" 'evil-outer-arg)

      ;; bind evil-forward/backward-args
      ;; (define-key evil-normal-state-map "L" 'evil-forward-arg)
      ;; (define-key evil-normal-state-map "H" 'evil-backward-arg)
      ;; (define-key evil-motion-state-map "L" 'evil-forward-arg)
      ;; (define-key evil-motion-state-map "H" 'evil-backward-arg)

      ;; bind evil-jump-out-args
      ;; (define-key evil-normal-state-map "K" 'evil-jump-out-args)
      ;; (key-chord-define evil-normal-state-map "hl" 'evil-jump-out-args)
      ;; (define-key evil-normal-state-map "gk" 'evil-jump-out-args)
      )))

;;; git-timemachine
(with-eval-after-load 'evil
  (with-eval-after-load "git-timemachine"
    ;; (add-hook 'git-timemachine-mode-hook 'tl/evil-state-cycle)

    ;; @see https://bitbucket.org/lyro/evil/issue/511/let-certain-minor-modes-key-bindings
    (evil-make-overriding-map git-timemachine-mode-map 'normal)
    ;; force update evil keymaps after git-timemachine-mode loaded
    (add-hook 'git-timemachine-mode-hook #'evil-normalize-keymaps)))

;;; company-mode
(with-eval-after-load 'evil
  (when (fboundp 'evil-declare-change-repeat)
    (mapc #'evil-declare-change-repeat
          '(company-complete-common
            company-select-next
            company-select-previous
            company-complete-selection
            company-complete-number))))

;; Abort company-mode when exiting insert mode
(defun tl/abort-company-on-insert-state-exit ()
  (when (fboundp 'company-abort)
    (company-abort)))

(add-hook 'evil-insert-state-exit-hook 'tl/abort-company-on-insert-state-exit)

;;; visual-line-mode
;; from https://github.com/tarleb/evil-rebellion
;; switch bindings for visual line and logical line movements.
(with-eval-after-load 'evil
  (evil-define-key 'normal visual-line-mode-map
    "$" 'evil-end-of-visual-line
    "^" 'evil-beginning-of-visual-line
    "g$" 'evil-end-of-line
    "g^" 'evil-beginning-of-line
    "gj" 'evil-next-line
    "gk" 'evil-previous-line
    "j" 'evil-next-visual-line
    "k" 'evil-previous-visual-line))

;;; misc
(with-eval-after-load 'evil
  ;; using both the RET and <return> forms to make sure the key works both in terminal and under X.
  (evil-define-key 'motion completion-list-mode-map (kbd "<return>") 'choose-completion)
  (evil-define-key 'motion completion-list-mode-map (kbd "RET") 'choose-completion)
  (evil-define-key 'motion browse-kill-ring-mode-map (kbd "<return>") 'browse-kill-ring-insert-and-quit)
  (evil-define-key 'motion browse-kill-ring-mode-map (kbd "RET") 'browse-kill-ring-insert-and-quit)
  (evil-define-key 'motion occur-mode-map (kbd "<return>") 'occur-mode-goto-occurrence)
  (evil-define-key 'motion occur-mode-map (kbd "RET") 'occur-mode-goto-occurrence))

;; evil-snipe
;; https://github.com/hlissner/evil-snipe

;; if not override mode, only binds s (forward)/S (backward) to evil-snipe-s and
;; evil-snipe-S, respectively. In operator mode, snipe is bound to z/Z and x/X
;; (exclusive). The last snipe can be repeated with s/S after a successful snipe
;; (or with s+RET).
(defvar tl/evil-snipe-enable-alternate-f-and-t-behaviors t)
(use-package evil-snipe
  :after evil
  :init
  (setq evil-snipe-override-evil-repeat-keys nil
        evil-snipe-scope 'whole-buffer
        evil-snipe-enable-highlight t
        evil-snipe-enable-incremental-highlight t
        evil-snipe-auto-disable-substitute t
        evil-snipe-show-prompt t
        evil-snipe-smart-case t)
  :config
  (push '(?\[ "[[{(]") evil-snipe-aliases)
  (evil-snipe-mode +1)

  (when tl/evil-snipe-enable-alternate-f-and-t-behaviors
    (setq evil-snipe-repeat-scope 'whole-buffer)
    (evil-snipe-override-mode +1) ; replaces evil-mode's f/F/t/T/;/, with snipe
    (add-hook 'magit-mode-hook 'turn-off-evil-snipe-override-mode)
    (add-hook 'magit-mode-hook 'turn-off-evil-snipe-mode)
    (add-hook 'dired-mode-hook 'turn-off-evil-snipe-mode)
    (add-hook 'prodigy-mode-hook 'turn-off-evil-snipe-mode)
    (add-hook 'git-rebase-mode-hook 'turn-off-evil-snipe-override-mode)
    (add-hook 'git-rebase-mode-hook 'turn-off-evil-snipe-mode)))


(use-package evil-find-char-pinyin
  :defer 3
  :diminish evil-find-char-pinyin-mode
  :init
  (setq evil-find-char-pinyin-only-simplified t)
  :config
  (evil-find-char-pinyin-toggle-snipe-integration t)
  (evil-find-char-pinyin-mode +1))

(use-package evil-visualstar
  :defer 3
  :config
  (global-evil-visualstar-mode +1))

;;; evil-replace-with-register
(use-package evil-replace-with-register
  :defer 3
  :init
  (setq evil-replace-with-register-key (kbd "gR"))
  :config
  (evil-replace-with-register-install))

;;; shift
;; (setq evil-shift-width 2)
(defun tl/shift-left-visual ()
  "Shift left and restore visual selection."
  (interactive)
  (evil-shift-left (region-beginning) (region-end))
  (evil-normal-state)
  (evil-visual-restore))

(defun tl/shift-right-visual ()
  "Shift right and restore visual selection."
  (interactive)
  (evil-shift-right (region-beginning) (region-end))
  (evil-normal-state)
  (evil-visual-restore))

(with-eval-after-load 'evil
  (define-key evil-visual-state-map (kbd ">") 'tl/shift-right-visual)
  (define-key evil-visual-state-map (kbd "<") 'tl/shift-left-visual))

;;; auto insert blank in between Chinese and English
(with-eval-after-load 'evil
  (define-key evil-visual-state-map "g " 'evil/add-blank-between-chinese-and-english)

  (defun evil/add-blank-between-chinese-and-english ()
    (interactive)
    (tl/add-blank-between-chinese-and-english (region-beginning) (region-end))
    (evil-normal-state)))

;;; evil-lion
;; use `g a` (mnemonic `align`)
;; these variables should be changed before (evil-lion-mode) is called
(use-package evil-lion
  :defer 3
  :init
  (setq evil-lion-left-align-key (kbd "g a"))
  (setq evil-lion-right-align-key (kbd "g A"))
  :config
  (evil-lion-mode +1))

;;; evil-textobj-column
(use-package evil-textobj-column
  :defer t
  :init
  (with-eval-after-load 'evil
    (define-key evil-inner-text-objects-map "c" 'evil-textobj-column-word)
    (define-key evil-inner-text-objects-map "C" 'evil-textobj-column-WORD)))

(use-package evil-textobj-anyblock
  :defer 3
  :init
  (with-eval-after-load 'evil
    (define-key evil-inner-text-objects-map "b" 'evil-textobj-anyblock-inner-block)
    ;; (define-key evil-outer-text-objects-map "b" 'evil-textobj-anyblock-a-block)
    (define-key evil-inner-text-objects-map "B" 'evil-textobj-anyblock-a-block))
  :config
  (defun tl/evil-textobj-anyblock--init ()
    (setq-local evil-textobj-anyblock-blocks
                '(("(" . ")")
                  ("{" . "}")
                  ("\\[" . "\\]")
                  ("\"" . "\""))))
  (add-hook 'lisp-mode-hook 'tl/evil-textobj-anyblock--init)

  (evil-define-text-object tl/evil-textobj-anyblock-inner-quote
    (count &optional beg end type)
    "Select the closest outer quote."
    (let ((evil-textobj-anyblock-blocks
           '(("'" . "'")
             ("\"" . "\"")
             ("`" . "'")
             ("“" . "”"))))
      (evil-textobj-anyblock--make-textobj beg end type count nil)))

  (evil-define-text-object tl/evil-textobj-anyblock-a-quote
    (count &optional beg end type)
    "Select the closest outer quote."
    (let ((evil-textobj-anyblock-blocks
           '(("'" . "'")
             ("\"" . "\"")
             ("`" . "'")
             ("“" . "”"))))
      (evil-textobj-anyblock--make-textobj beg end type count t)))

  (define-key evil-inner-text-objects-map "q" 'tl/evil-textobj-anyblock-inner-quote)
  (define-key evil-outer-text-objects-map "q" 'tl/evil-textobj-anyblock-a-quote))


;;; ivy
(defun tl/ivy-evil-registers ()
  "Show evil registers"
  (interactive)
  (let ((ivy-height 24))
    (ivy-read "Evil Registers:"
              (cl-loop for (key . val) in (evil-register-list)
                       collect (eval `(format "%s : %s" (propertize ,(char-to-string key) 'face 'font-lock-builtin-face)
                                              ,(or (and val
                                                        (stringp val)
                                                        (replace-regexp-in-string "\n" "^J" val))
                                                   ""))))
              :action #'tl/ivy-insert-evil-register)))

(defun tl/ivy-insert-evil-register (candidate)
  (insert (replace-regexp-in-string "\\^J" "\n"
                                    (substring-no-properties candidate 4))))


;;; lispy [[https://github.com/abo-abo/lispy][abo-abo/lispy: Short and sweet LISP editing]]
(use-package lispy
  :hook ((common-lisp-mode . lispy-mode)
         (emacs-lisp-mode . lispy-mode)
         (scheme-mode . lispy-mode)
         (racket-mode . lispy-mode)
         (clojure-mode . lispy-mode))
  :config
  (setq lispy-close-quotes-at-end-p t))

;; auto-complete word in Emacs mini-buffer when using Evil
;; http://blog.binchen.org/posts/auto-complete-word-in-emacs-mini-buffer-when-using-evil.html
(defun tl/minibuffer-inactive-mode-hook-setup ()
  ;; make `try-expand-dabbrev' from `hippie-expand' work in mini-buffer
  ;; @see `he-dabbrev-beg', so we need re-define syntax for '/'
  (set-syntax-table (let* ((table (make-syntax-table)))
                      (modify-syntax-entry ?/ "." table)
                      table)))
(add-hook 'minibuffer-inactive-mode-hook 'tl/minibuffer-inactive-mode-hook-setup)


(use-package evil-vimish-fold
  :defer 3
  :config
  (defun tl/evil-vimish-fold-create-dwim ()
    "Create a fold from the current region or with avy."
    (interactive)
    (if mark-active
        (call-interactively 'evil-vimish-fold/create)
      (vimish-fold-avy)))

  (evil-define-key 'motion evil-vimish-fold-mode-map "zf" 'tl/evil-vimish-fold-create-dwim)

  (evil-vimish-fold-mode +1))

;;; evil-goggles
;; it might caused freeze when error typing? disable for test
;; After disable it, the typing response is quicker
;; (use-package evil-goggles
;;   :defer 3
;;   :init
;;   ;; default is 'region, you can try `isearch-fail
;;   (setq evil-goggles-default-face '((t (:inherit 'highlight)))) ; 'highlight
;;   (setq evil-goggles-duration 0.200) ;; default is 0.200
;;   (setq evil-goggles-pulse t)
;;   ;; to disable the hint when yanking or deleting
;;   (setq evil-goggles-blacklist `(evil-yank evil-delete))

;;   :config
;;   (evil-goggles-use-diff-faces)
;;   (evil-goggles-mode +1))

(use-package evil-visual-mark-mode
  :defer t
  :commands (evil-visual-mark-mode)
  :init
  (setq evil-visual-mark-exclude-marks '("^" "[" "]")))

(with-eval-after-load 'wgrep
  (evil-define-key 'normal wgrep-mode-map ",," 'wgrep-finish-edit)
  (evil-define-key 'normal wgrep-mode-map ",c" 'wgrep-finish-edit)
  (evil-define-key 'normal wgrep-mode-map ",a" 'wgrep-abort-changes)
  (evil-define-key 'normal wgrep-mode-map ",k" 'wgrep-abort-changes))

(with-eval-after-load 'ediff
  (use-package evil-ediff))

;; (use-package evil-fringe-mark
;;   :after (evil)
;;   :init
;;   (progn
;;     (setq-default evil-fringe-mark-show-special nil)
;;     (setq-default evil-fringe-mark-side 'right-fringe))
;;   :config
;;   (progn
;;     (global-evil-fringe-mark-mode +1)))

;;; sexp text object
(with-eval-after-load 'evil
  ;; think at point
  ;; To make ‘forward-thing’ work with a particular kind of thing, say, a new
  ;; type ‘foobar’ that you define, you can define a function
  ;; ‘forward-foobar’ that moves forward one or more ‘foobar’s. (With a
  ;; negative argument, ‘forward-foobar’ should move backward.)
  (defun forward-evil-thingatpt-sexp (&optional count)
    "Move forward COUNT sexps.
Moves point COUNT sexps forward or (- COUNT) sexps backward
if COUNT is negative. "
    (evil-motion-loop (dir (or count 1))
      (if (> dir 0)
          (thing-at-point--end-of-sexp)
        (thing-at-point--beginning-of-sexp))))

  (evil-define-motion evil-thingatpt-forward-sexp-end (count)
    :type inclusive
    (let ((thing 'evil-thingatpt-sexp)
          (count (or count 1)))
      (evil-signal-at-bob-or-eob count)
      (evil-forward-end thing count)))

  (evil-define-motion evil-thingatpt-backward-sexp-begin (count)
    :type exclusive
    (let ((thing 'evil-thingatpt-sexp)
          (count (or count 1)))
      (evil-signal-at-bob-or-eob count)
      (evil-backward-beginning thing count)))

  (define-key evil-motion-state-map "e" 'evil-thingatpt-forward-sexp-end)
  (define-key evil-motion-state-map "E" 'evil-thingatpt-backward-sexp-begin)

  (evil-define-text-object evil-a-sexp (count &optional beg end type)
    (evil-select-an-object 'evil-thingatpt-sexp beg end type count))

  (evil-define-text-object evil-inner-sexp (count &optional beg end type)
    (evil-select-inner-object 'evil-thingatpt-sexp beg end type count))

  (define-key evil-outer-text-objects-map "e" 'evil-a-sexp)
  (define-key evil-inner-text-objects-map "e" 'evil-inner-sexp))

;;; column and defun text object
(with-eval-after-load 'evil
  (evil-define-text-object evil-sp-a-comment (count &optional beg end type)
    "An outer comment text object as defined by `sp-get-comment-bounds'."
    (let ((bounds (sp-get-comment-bounds)))
      (if (not bounds)
          (error "Not inside a comment.")
        (let ((beg (car bounds))
              (end (cdr bounds)))
          (evil-range beg end 'line :expanded t)))))

  (evil-define-text-object evil-sp-inner-comment (count &optional beg end type)
    "An inner comment text object as defined by `sp-get-comment-bounds'."
    (let ((bounds (sp-get-comment-bounds)))
      (if (not bounds)
          (error "Not inside a comment.")
        (let ((beg (save-excursion
                     (goto-char (car bounds))
                     (forward-word 1)
                     (forward-word -1)
                     (point)))
              (end (save-excursion
                     (goto-char (cdr bounds))
                     (evil-end-of-line)
                     (point))))
          (evil-range beg end 'block :expanded t)))))

  (define-key evil-outer-text-objects-map (kbd "s-c") #'evil-sp-a-comment)
  (define-key evil-inner-text-objects-map (kbd "s-c") #'evil-sp-inner-comment))


(use-package evil-textobj-syntax
  :after evil
  :init
  (progn
    (setq evil-textobj-syntax-i-key "h")
    (setq evil-textobj-syntax-a-key "h")))

;; [[https://github.com/akicho8/string-inflection][akicho8/string-inflection: underscore -> UPCASE -> CamelCase conversion of names]]
;; [[https://github.com/ninrod/evil-string-inflection][;; ninrod/evil-string-inflection: evil operator to cycle *-case in text objects]]

(use-package string-inflection
  :diminish
  :config
  (defun pl/string-inflection-rust-style-cycle-function (str)
    "foo_bar => FOO_BAR => FooBar => foo_bar"
    (string-inflection-ruby-style-cycle-function str))

  (defun pl/string-inflection-all-cycle-function (str)
    "switching by major-mode"
    (interactive)
    (cond
     ;; for rust
     ((eq major-mode 'rustic-mode)
      (pl/string-inflection-rust-style-cycle-function str))
     (t
      ;; default
      (string-inflection-all-cycle-function str))))

  (with-eval-after-load 'evil
    (define-key evil-normal-state-map (kbd "g~") 'pl/evil-operator-string-inflection)
    (evil-define-operator pl/evil-operator-string-inflection (beg end _type)
      "Define a new evil operator that cicles underscore -> UPCASE -> CamelCase."
      :move-point nil
      (interactive "<R>")
      (let ((str (buffer-substring-no-properties beg end)))
        (save-excursion
          (delete-region beg end)
          (insert (pl/string-inflection-all-cycle-function str)))))))

;; (use-package evil-owl
;;   :diminish
;;   :after evil
;;   :config
;;   (setq evil-owl-display-method 'posframe)

;;   (if (eq evil-owl-display-method 'window)
;;       (progn
;;         (setq evil-owl-max-string-length 500)
;;         (add-to-list 'display-buffer-alist
;;                      '("*evil-owl*"
;;                        (display-buffer-in-side-window)
;;                        (side . bottom)
;;                        (window-height . 0.3))))
;;     (setq evil-owl-extra-posframe-args '(:width 60 :height 30)
;;           evil-owl-max-string-length 50))

;;   (setq evil-owl-idle-delay 0.3)
;;   (setq evil-owl-header-format      "# %s"
;;         evil-owl-register-format    "  %r: %s"
;;         evil-owl-local-mark-format  "  %m: [l: %-5l, c: %-5c]"
;;         evil-owl-global-mark-format "  %m: [l: %-5l, c: %-5c] %b"
;;         evil-owl-separator          "\n")

;;   (evil-owl-mode +1))

;;; defun text-object
(with-eval-after-load 'evil
  ;; https://github.com/Ambrevar/dotfiles/blob/master/.emacs.d/lisp/init-evil.el
  ;; https://github.com/emacs-evil/evil/issues/874
  ;; See Evil-cleverparens?
  (evil-define-text-object evil-inner-defun (count &optional beg end type)
    "Select inner defun."
    (evil-select-inner-object 'evil-defun beg end type count))
  (define-key evil-inner-text-objects-map "d" 'evil-inner-defun))

;;; bugfix
;; https://bitbucket.org/lyro/evil/issue/432/edebug-mode-map-cant-take-effect-for-the
;; (add-hook 'edebug-mode-hook 'evil-normalize-keymaps) ; recreate `evil-mode-map-alist'
(with-eval-after-load 'edebug
  (add-hook 'edebug-mode-hook 'tl/evil-state-cycle))

;; [[https://philjackson.github.io//evil/emacs/2021/07/11/start-evil-substitution-on-selection/][;; Start evil substitution on selection | Snippets and other bits]]
;; (with-eval-after-load 'evil
;;   (evil-global-set-key 'normal (kbd ",s") 'start-ex-sub-on-region)
;;   (evil-define-operator tl/start-ex-sub-on-region (beg end)
;;     (let ((region (buffer-substring beg end)))
;;       (evil-ex (concat "%s/"
;;                        (replace-regexp-in-string
;;                         "\/"
;;                         "\\\\/"
;;                         (regexp-quote region))
;;                        "/")))))

(provide '09evil-mode)
;;; 09evil-mode.el ends here
