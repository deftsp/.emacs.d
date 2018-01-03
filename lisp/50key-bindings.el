;;; 50key-bindings.el ---

;;; Code:
;; We define prefix commands only for the sake of which-key
(setq paloryemacs/key-binding-prefixes '(("a"   "applications")
                                         ("A"   "other applications")
                                         ("ai"  "irc")
                                         ("as"  "shells")
                                         ("ay"  "ipython notebook")
                                         ("b"   "buffers")
                                         ("bN"  "new empty buffer")
                                         ("c"   "compile/comments")
                                         ("C"   "capture/colors")
                                         ("e"   "errors")
                                         ("f"   "files")
                                         ("fC"  "files/convert")
                                         ("fe"  "emacs(spacemacs)")
                                         ("fv"  "variables")
                                         ("F"   "frame")
                                         ("g"   "git/versions-control")
                                         ("h"   "help")
                                         ("hd"  "help-describe")
                                         ("i"   "insertion")
                                         ("j"   "jump/join/split")
                                         ("k"   "lisp")
                                         ("kd"  "delete")
                                         ("kD"  "delete-backward")
                                         ("k`"  "hybrid")
                                         ("n"   "narrow/numbers")
                                         ("N"   "navigation")
                                         ("p"   "projects")
                                         ("p$"  "projects/shell")
                                         ("q"   "quit")
                                         ("r"   "registers/rings/resume")
                                         ("Re"  "elisp")
                                         ("Rp"  "pcre")
                                         ("s"   "search/symbol")
                                         ("sa"  "ag")
                                         ("sg"  "grep")
                                         ("sk"  "ack")
                                         ("sr"  "ripgrep")
                                         ("st"  "pt")
                                         ("sw"  "web")
                                         ("t"   "toggles")
                                         ("tC"  "colors")
                                         ("tE"  "editing-styles")
                                         ("th"  "highlight")
                                         ("tm"  "modeline")
                                         ("T"   "UI toggles/themes")
                                         ("C-t" "other toggles")
                                         ("w"   "windows")
                                         ("wp"  "popup")
                                         ("x"   "text")
                                         ("xa"  "align")
                                         ("xd"  "delete")
                                         ("xg"  "google-translate")
                                         ("xj"  "justification")
                                         ("xl"  "lines")
                                         ("xm"  "move")
                                         ("xt"  "transpose")
                                         ("xw"  "words")
                                         ("z"   "zoom")))

(mapc (lambda (x) (apply #'paloryemacs/declare-prefix x))
      paloryemacs/key-binding-prefixes)

;; Universal argument ---------------------------------------------------------
(paloryemacs/set-leader-keys "u" 'universal-argument)
(define-key universal-argument-map
  (kbd (concat dotpaloryemacs-leader-key " u"))
  'universal-argument-more)


(defun paloryemacs/org-clock-in ()
  "Start the clock on the current item.

If necessary, clock-out of the currently active clock.

offer a list of recently clocked tasks to clock into. "
  (interactive)
  (let ((current-prefix-arg '(4))) ; C-u
    (call-interactively 'org-clock-in))  )

(paloryemacs/set-leader-keys "!" 'shell-command)

;; applications
(paloryemacs/set-leader-keys
  "ac" 'calc-dispatch
  "au"  'undo-tree-visualize)


;; SPC o and SPC m o are reserved for the user
;; SPC m is reserved for the current major mode. Three keys bindings are not an
;; issue (ie. SPC m h d) since SPC m can be accessed via ​,​.
(paloryemacs/set-leader-keys
  "oa" 'org-agenda
  ;; "oc" 'org-capture
  "occ"   'org-clock-cancel
  "oce"   'org-clock-modify-effort-estimate
  "oci"   'org-clock-in-last
  "ocI"   'paloryemacs/org-clock-in
  "ocj"   'org-clock-jump-to-current-clock
  "ocg"   'org-clock-goto
  "oco"   'org-clock-out
  "ocr"   'org-resolve-clocks
  "ocx"   'org-clock-in-last
  "ocz"   'org-resolve-clocks

  "ops"   'profiler-start
  "opS"   'profiler-stop
  "opr"   'profiler-report)



(paloryemacs/set-leader-keys
  ;; ";" "cc" "ci", "cl", "cp", "cr", "ct", "cy" and "cv" are used by evil-nerd-commenter
  "cs"  'paloryemacs/evil-change-symbol-in-defun
  "ch"  'crosshairs-mode
  "fj"  'dired-jump
  "dp"  'dash-at-point
  "dP"  'dash-at-point-with-docset
  "ff"  'counsel-find-file       ; find-file-at-point
  "fel" 'counsel-find-library
  "fL"  'counsel-locate
  "fb"  'counsel-bookmark
  "fT"  'hydra-transparency/body
  "gl"  'magit-log
  "gs"  'magit-status
  "gC"  'magit-commit
  "gg"  'counsel-git-grep
  "jd"  'dired-jump
  "jD"  'dired-jump-other-window
  "jb" 'avy-pop-mark
  "jj" 'evil-avy-goto-char
  "jJ" 'evil-avy-goto-char-2
  "jl" 'evil-avy-goto-line
  "jw" 'evil-avy-goto-word-or-subword-1
  "n"   'evil-narrow-indirect
  "p"   'hydra-projectile/body   ; 'projectile-commander
  "P"   'hydra-projectile-other-window/body
  "re"  'paloryemacs/ivy-evil-registers
  "ry"  'counsel-yank-pop
  "se"  'evil-iedit-state/iedit-mode
  "ss"  'helm-swoop
  "sS"  'helm-multi-swoop
  "s C-s" 'helm-multi-swoop-all
  ;; "vr"  'vr/replace
  ;; "vq"  'vr/query-replace
  ;; "vm"  'vr/mc-mark
  "t"   'hydra-toggle/body
  "xb"  'switch-to-buffer
  "xc"  'save-buffers-kill-terminal
  "xf"  'ido-find-file
  "xk"  'kill-buffer
  "xz"  'suspend-frame
  "xdw" 'delete-trailing-whitespace
  "xvv" 'vc-next-action
  "xv=" 'vc-diff
  "xvl" 'vc-print-log
  "y"   'counsel-yank-pop)

;; errors ---------------------------------------------------------------------
(paloryemacs/set-leader-keys
  "ec" 'flycheck-clear
  "eh" 'flycheck-describe-checker
  "el" 'flycheck-list-errors
  "eL" 'paloryemacs/goto-flycheck-error-list
  "es" 'flycheck-select-checker
  "eS" 'flycheck-set-checker-executable
  "ev" 'flycheck-verify-setup

  "ef"  'flycheck-mode

  "en" 'paloryemacs/next-error
  "eN" 'paloryemacs/previous-error
  "ep" 'paloryemacs/previous-error)

(paloryemacs/set-leader-keys
  ;; "b<1-9>" 'buffer-to-window-<1-9>
  "TAB"   'paloryemacs/alternate-buffer
  "bb"    'ivy-switch-buffer
  "bB"    'ibuffer
  "bd"    'paloryemacs/kill-this-buffer
  "be"    'paloryemacs/safe-erase-buffer
  ;; "bh"    'paloryemacs/home
  "b C-d" 'paloryemacs/kill-other-buffers
  "b C-S-d" 'paloryemacs/kill-matching-buffers-rudely
  "bk"  'paloryemacs/kill-matching-buffers-rudely
  "bn"    'next-buffer
  "bm"    'paloryemacs/switch-to-messages-buffer
  "b N h" 'paloryemacs/new-empty-buffer-left
  "b N j" 'paloryemacs/new-empty-buffer-below
  "b N k" 'paloryemacs/new-empty-buffer-above
  "b N l" 'paloryemacs/new-empty-buffer-right
  "b N n" 'paloryemacs/new-empty-buffer
  "bP"    'paloryemacs/copy-clipboard-to-whole-buffer
  "bp"    'previous-buffer
  "bR"    'paloryemacs/safe-revert-buffer
  "bs"    'paloryemacs/switch-to-scratch-buffer
  "bu"    'paloryemacs/reopen-killed-buffer
  "bY"    'paloryemacs/copy-whole-buffer-to-clipboard
  "bw"    'read-only-mode)


;; text -----------------------------------------------------------------------
(defalias 'count-region 'count-words-region)

(paloryemacs/set-leader-keys
  "xa%" 'paloryemacs/align-repeat-percent
  "xa&" 'paloryemacs/align-repeat-ampersand
  "xa(" 'paloryemacs/align-repeat-left-paren
  "xa)" 'paloryemacs/align-repeat-right-paren
  "xa{" 'paloryemacs/align-repeat-left-curly-brace
  "xa}" 'paloryemacs/align-repeat-right-curly-brace
  "xa[" 'paloryemacs/align-repeat-left-square-brace
  "xa]" 'paloryemacs/align-repeat-right-square-brace
  "xa," 'paloryemacs/align-repeat-comma
  "xa." 'paloryemacs/align-repeat-decimal
  "xa:" 'paloryemacs/align-repeat-colon
  "xa;" 'paloryemacs/align-repeat-semicolon
  "xa=" 'paloryemacs/align-repeat-equal
  "xa'" 'paloryemacs/align-repeat-quote
  "xa\\" 'paloryemacs/align-repeat-backslash
  "xaa" 'align
  "xac" 'align-current
  "xam" 'paloryemacs/align-repeat-math-oper
  "xar" 'paloryemacs/align-repeat
  "xa|" 'paloryemacs/align-repeat-bar
  "xc"  'count-region
  "xd SPC" 'just-one-space
  "xdw" 'delete-trailing-whitespace
  "xjc" 'set-justification-center
  "xjf" 'set-justification-full
  "xjl" 'set-justification-left
  "xjn" 'set-justification-none
  "xjr" 'set-justification-right
  "xlc" 'paloryemacs/sort-lines-by-column
  "xlC" 'paloryemacs/sort-lines-by-column-reverse
  "xld" 'paloryemacs/duplicate-line-or-region
  "xls" 'paloryemacs/sort-lines
  "xlS" 'paloryemacs/sort-lines-reverse
  "xlu" 'paloryemacs/uniquify-lines
  "xtc" 'transpose-chars
  "xtl" 'transpose-lines
  "xtp" 'transpose-paragraphs
  "xts" 'transpose-sentences
  "xtw" 'transpose-words
  "xU"  'upcase-region
  "xu"  'downcase-region
  "xwc" 'paloryemacs/count-words-analysis
  "x TAB" 'indent-rigidly)

(define-key indent-rigidly-map "h" 'indent-rigidly-left)
(define-key indent-rigidly-map "l" 'indent-rigidly-right)
(define-key indent-rigidly-map "H" 'indent-rigidly-left-to-tab-stop)
(define-key indent-rigidly-map "L" 'indent-rigidly-right-to-tab-stop)


(defun paloryemacs/move-buffer-to-window (windownum follow-focus-p)
  "Moves a buffer to a window, using the paloryemacs numbering. follow-focus-p
   controls whether focus moves to new window (with buffer), or stays on
   current"
  (interactive)
  (let ((b (current-buffer))
        (w1 (selected-window))
        (w2 (winum-get-window-by-number windownum)))
    (unless (eq w1 w2)
      (set-window-buffer w2 b)
      (switch-to-prev-buffer)
      (unrecord-window-buffer w1 b)))
  (when follow-focus-p (select-window (winum-get-window-by-number windownum))))


(defun paloryemacs/swap-buffers-to-window (windownum follow-focus-p)
  "Swaps visible buffers between active window and selected window.
   follow-focus-p controls whether focus moves to new window (with buffer), or
   stays on current"
  (interactive)
  (let* ((b1 (current-buffer))
         (w1 (selected-window))
         (w2 (winum-get-window-by-number windownum))
         (b2 (window-buffer w2)))
    (unless (eq w1 w2)
      (set-window-buffer w1 b2)
      (set-window-buffer w2 b1)
      (unrecord-window-buffer w1 b1)
      (unrecord-window-buffer w2 b2)))
  (when follow-focus-p (winum-select-window-by-number windownum)))


(dotimes (i 9)
  (let ((n (+ i 1)))
    (eval `(defun ,(intern (format "buffer-to-window-%s" n)) (&optional arg)
             ,(format "Move buffer to the window with number %i." n)
             (interactive "P")
             (if arg
                 (paloryemacs/swap-buffers-to-window ,n t)
               (paloryemacs/move-buffer-to-window ,n t))))
    (eval `(defun ,(intern (format "move-buffer-window-no-follow-%s" n)) ()
             (interactive)
             (paloryemacs/move-buffer-to-window ,n nil)))
    (eval `(defun ,(intern (format "swap-buffer-window-no-follow-%s" n)) ()
             (interactive)
             (paloryemacs/swap-buffers-to-window ,n nil)))))

(dotimes (i 9)
  (let ((n (+ i 1)))
    (paloryemacs/set-leader-keys
      (format "b%i" n)
      (intern (format "buffer-to-window-%s" n)))))

(defun paloryemacs/switch-to-messages-buffer (&optional arg)
  "Switch to the `*Messages*' buffer.
if prefix argument ARG is given, switch to it in an other, possibly new window."
  (interactive "P")
  (with-current-buffer (messages-buffer)
    (goto-char (point-max))
    (if arg
        (switch-to-buffer-other-window (current-buffer))
      (switch-to-buffer (current-buffer)))))

;; file -----------------------------------------------------------------------
(paloryemacs/set-leader-keys
  ;; "fc" 'paloryemacs/copy-file
  ;; "fD" 'paloryemacs/delete-current-buffer-file
  ;; "fei" 'paloryemacs/find-user-init-file
  ;; "fed" 'paloryemacs/find-dotfile
  ;; "feD" 'paloryemacs/ediff-dotfile-and-template
  ;; "feR" 'dotpaloryemacs/sync-configuration-layers
  ;; "fev" 'paloryemacs/display-and-copy-version
  ;; "fCd" 'paloryemacs/unix2dos
  ;; "fCu" 'paloryemacs/dos2unix
  "fg" 'rgrep
  "fl" 'find-file-literally
  ;; "fE" 'paloryemacs/sudo-edit
  ;; "fo" 'paloryemacs/open-in-external-app
  ;; "fR" 'paloryemacs/rename-current-buffer-file
  "fS" 'evil-write-all
  "fs" 'save-buffer
  "fvd" 'add-dir-local-variable
  "fvf" 'add-file-local-variable
  "fvp" 'add-file-local-variable-prop-line
  ;; "fy" 'paloryemacs/show-and-copy-buffer-filename
  )

;; help -----------------------------------------------------------------------
(paloryemacs/set-leader-keys
  "?"   'counsel-descbinds
  "hdb" 'describe-bindings
  "hdc" 'describe-char
  "hdf" 'counsel-describe-function
  "hdF" 'counsel-describe-face
  "hi"  'counsel-info-lookup-symbol
  "hdm" 'describe-mode
  "hdk" 'describe-key
  ;; "hdl" 'paloryemacs/describe-last-keys
  "hdp" 'describe-package
  "hdP" 'configuration-layer/describe-package
  ;; "hds" 'paloryemacs/describe-system-info
  "hdt" 'describe-theme
  "hdv" 'counsel-describe-variable
  ;; "hI"  'paloryemacs/report-issue
  "hn"  'view-emacs-news)

(paloryemacs/set-leader-keys
  "w TAB"        'paloryemacs/alternate-window
  ;; "w1"           'delete-other-windows
  ;; "w2"           (kbd "C-x 2")
  ;; "w3"           "\C-x3"
  "wb"           'paloryemacs/switch-to-minibuffer-window
  "wd"           'paloryemacs/delete-window
  "wt"           'paloryemacs/toggle-current-window-dedication
  "wf"           'follow-mode
  "wF"           'make-frame
  "wH"           'evil-window-move-far-left
  "w <S-left>"   'evil-window-move-far-left
  "wh"           'evil-window-left
  "w <left>"     'evil-window-left
  "wJ"           'evil-window-move-very-bottom
  "w <S-down>"   'evil-window-move-very-bottom
  "wj"           'evil-window-down
  "w <down>"     'evil-window-down
  "wK"           'evil-window-move-very-top
  "w <S-up>"     'evil-window-move-very-top
  "wk"           'evil-window-up
  "w <up>"       'evil-window-up
  "wL"           'evil-window-move-far-right
  "w <S-right>"  'evil-window-move-far-right
  "wl"           'evil-window-right
  "w <right>"    'evil-window-right
  "wm"           'paloryemacs/toggle-maximize-buffer
  "wc"           'paloryemacs/toggle-centered-buffer-mode
  "wC"           'paloryemacs/toggle-centered-buffer-mode-frame
  "wo"           'other-frame
  "wr"           'paloryemacs/rotate-windows-forward
  "wR"           'paloryemacs/rotate-windows-backward
  "ws"           'split-window-below
  "wS"           'split-window-below-and-focus
  "w-"           'split-window-below
  "wU"           'winner-redo
  "wu"           'winner-undo
  "wv"           'split-window-right
  "wV"           'split-window-right-and-focus
  "ww"           'other-window
  "w/"           'split-window-right
  "w="           'balance-windows-area
  "w+"           'paloryemacs/window-layout-toggle
  "w_"           'paloryemacs/maximize-horizontally)

(defun paloryemacs/global-set-keys (&rest keycommands)
  "Register keys to commands."
  (while keycommands
    (let ((key (car keycommands))
          (command (cadr keycommands)))
      (eval `(global-set-key (kbd ,key) (quote ,command))))
    (setq keycommands (cdr (cdr keycommands)))))


;; register my preferred keybindings
(paloryemacs/global-set-keys
 "<f1>"    'anything-man ; 'woman-word-at-point
 "<C-f1>"  '(lambda () (interactive) (manual-entry (current-word))) ;;; load man pages when on a word and F1 is pressed
 "<S-f1>"  'iman
 ;; "<M-f1>"  'apropos                     ;the ultimate tool for hackers
 "<f2>"    'save-buffer
 "<f3>"    'find-file
 ;; "<M-f3>"  'grep-find
 "<C-f3>"  'hexl-find-file
 "<S-f3>"  'muse-project-find-file
 "<f4>"    'kill-this-buffer
 "<S-f4>"  'kill-buffer
 "<C-f4>"  'delete-window

 ;; Compile and debug
 ;; "<f5>"    'buffer-action-compile
 ;; "<C-f5>"  'buffer-action-run
 ;; "<S-f5>"  'compile
 "<f6>"    'first-error
 "<S-f6>"  'last-error

 ;; "<f9>"    'door-gnus
 "<S-f9>"  'ascii-table-show
 "<C-f9>"  'shell
 "<M-f9>"  'paloryemacs/ansi-term
 "<f10>"   'paloryemacs/w3m-switch-to-buffer
 ;; "<f11>"
 "<S-f11>" 'appt-add
 "<S-f12>" 'recentf-open-files
 "<f12>"   'list-bookmarks
 "<M-f12>" 'recentf-open-files)

;; (global-set-key "\C-cw" 'compare-windows)


;; (define-prefix-command 'menu-map)
;; (global-set-key (kbd "<menu>") 'menu-map)
;; (global-set-key (kbd "<menu> <menu>") (lookup-key global-map (kbd "<menu>")))
;; (global-unset-key (kbd "<menu>"))
;; (global-unset-key (kbd "<menu>") (lambda () (interactive) nil))

(global-set-key (kbd "<Scroll_Lock>") (lambda () (interactive) nil))

;;-------------------------------------------------------------------------------------

(global-set-key "\C-m" 'newline-and-indent) ; 'newline-and-indent 'reindent-then-newline-and-indent
;; (global-set-key "\C-j" 'reindent-then-newline-and-indent)

;;; indent the whole buffer
(global-set-key (kbd "C-c i w") 'paloryemacs/indent-whole-buffer)
(defun paloryemacs/indent-whole-buffer ()
  "indent whole buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))
;;----------------------------------------------------------------------
;; suspend-frame `C-x C-z'
;; (when window-system
;;   (global-unset-key "\C-z")
;;   ;; define a prefix command, make it possible to define key sequence like`C-z c c' 'C-z n`
;;   (define-prefix-command 'ctl-z-map)
;;   (global-set-key (kbd "C-z") 'ctl-z-map))

;;; view mode
;; rebind "C-x C-q" to `view-mode' instead of `read-only-mode'
(define-key ctl-x-map "\C-q" 'view-mode)
(add-hook 'view-mode-hook 'paloryemacs/view-mode-hook)
(defun paloryemacs/view-mode-hook ()
  (define-key view-mode-map "b" 'View-scroll-page-backward)
  (define-key view-mode-map "f" 'View-scroll-page-forward)
  (define-key view-mode-map "h" 'backward-char)
  (define-key view-mode-map "l" 'forward-char)
  (define-key view-mode-map "j" 'next-line)
  (define-key view-mode-map "k" 'previous-line))


;;; HOME & END
;;"Redefine the Home/End keys to (nearly) the same as visual studio behavior... special home and end by Shan-leung
;;Maverick WOO <sw77@cornell.edu>" This is complex. In short, the first invocation of Home/End moves to the beginning of
;;the *text* line. A second invocation moves the cursor to the beginning of the *absolute* line. Most of the time this
;;won't matter or even be noticeable, but when it does (in comments, for example) it will be quite convenient.
(global-set-key [home] 'paloryemacs/smart-home)
(global-set-key [end] 'paloryemacs/smart-end)

(defun paloryemacs/smart-home ()
  "Odd home to beginning of line, even home to beginning of
text/code."
  (interactive)
  (if (and (eq last-command 'paloryemacs/smart-home)
           (/= (line-beginning-position) (point)))
      (beginning-of-line)
    (beginning-of-line-text)))

(defun paloryemacs/smart-end ()
  "Odd end to end of line, even end to begin of text/code."
  (interactive)
  (if (and (eq last-command 'paloryemacs/smart-end)
           (= (line-end-position) (point)))
      (paloryemacs//end-of-line-text)
    (end-of-line)))

(defun paloryemacs//end-of-line-text ()
  "Move to end of current line and skip comments and trailing space.
Require `font-lock'."
  (end-of-line)
  (let ((bol (line-beginning-position)))
    (unless (eq font-lock-comment-face (get-text-property bol 'face))
      (while (and (/= bol (point))
                  (eq font-lock-comment-face
                      (get-text-property (point) 'face)))
        (backward-char 1))
      (unless (= (point) bol)
        (forward-char 1) (skip-chars-backward " \t\n"))))) ; Done with home and end keys.

;;; repeat to mark multi-line
;; (global-set-key (kbd "C-z") 'paloryemacs/mark-line)
(defun paloryemacs/mark-line (&optional arg allow-extend)
  "Put point at beginning of this line, mark at end.
The line marked is the one that contains point or follows point.

With argument ARG, puts mark at end of a following line, so that
the number of lines marked equals ARG.

If ARG is negative, point is put at end of this line, mark is put
at beginning of this or a previous line.

Interactively, if this command is repeated
or (in Transient Mark mode) if the mark is active,
it marks the next ARG lines after the ones already marked."
  (interactive "p\np")
  (unless arg (setq arg 1))
  (when (zerop arg)
    (error "Cannot mark zero lines"))
  (cond ((and allow-extend
              (or (and (eq last-command this-command) (mark t))
                  (and transient-mark-mode mark-active)))
         (set-mark
          (save-excursion
            (goto-char (mark))
            (forward-line arg)
            (point))))
        (t
         (forward-line arg)
         (push-mark nil t t)
         (forward-line (- arg)))))


;;-------------------------------------------------------------------------------------------------------

(defun paloryemacs/return-current-point ()
  (interactive)
  (message "Current point is: %d" (point)))
;;----------------------------------------------------------------------------------------------------

;;; zap to char
;; Emacs provides a zap-to-char command that kills from the current point to a character. It is
;; bound to `M-z'. Examples of its usage, include:

;; 'M-z e'
;;     deletes all characters to the next occurence of "e".
;; Typing 'C-u 2 M-z e'
;;     deletes all character to the second occurence of "e".
;; Typing 'C-- M-z e'
;;     deletes all characters to the previous occurence of "e".
;; Typing 'C-u -4 M-z e'
;;     deletes all character to the fourth previous occurence of "e".

;; delete sentences (`M-z .'). You can also use 'M-k' ('kill-sentence').
;; delete an XML tag (`M-z >').In nXML mode, 'C-M-k' ('kill-sexp') does the same thing.

;; killing a quote-delimited string. It's handy when you're coding, imagine editing a
;; System.out.println("blah blah blah"); With the point positioned at the first " you could `M-2 M-z
;; "' and kill upto and including the final ", allowing you to change the output. In this case you'd
;; better use 'C-M-k' at the starting "

;; (global-set-key (kbd "M-z") 'paloryemacs/zap-up-to-char)
;; (defun paloryemacs/zap-up-to-char (arg char)
;;   "Kill up to and including ARGth occurrence of CHAR. "
;;   (interactive (list (prefix-numeric-value current-prefix-arg)
;;                      (read-char "Zap to char: " t)))
;;   ;; Avoid "obsolete" warnings for translation-table-for-input.
;;   (with-no-warnings
;;     (if (char-table-p translation-table-for-input)
;;         (setq char (or (aref translation-table-for-input char) char))))
;;   (kill-region (point) (1- (progn
;;                              (let ((case-fold-search nil))
;;                                (search-forward (char-to-string char) nil nil arg))
;;                              (point))))
;;   (backward-char))

;; (global-set-key (kbd "M-Z") 'paloryemacs/zap-to-char-save)
;; (defun paloryemacs/zap-to-char-save (arg char)
;;   "Zap to a character, but save instead of kill."
;;   (interactive "p\ncZap to char: ")
;;   (save-excursion
;;     (zap-to-char arg char)
;;     (yank)))

(global-set-key (kbd "M-z") 'ace-jump-zap-up-to-char-dwim)
(global-set-key (kbd "M-Z") 'ace-jump-zap-to-char-dwim)

;;; extral key map
;; (setq paloryemacs/extra-key-map (make-keymap))
;; (global-set-key [(super z)] paloryemacs/extra-key-map)
;; (define-key paloryemacs/extra-key-map "b" 'bbdb)
;; (define-key paloryemacs/extra-key-map "m" 'bbdb-and-mail-with-default-mailer)


;;; M-^: delete-indentation
;; join-line is an alias for `delete-indentation'


;;; the point doesn’t move, the window does.
(global-set-key (kbd "<H-M-up>")
                (lambda ()
                  (interactive)
                  (let ((top (line-number-at-pos (window-start)))
                        (cur (line-number-at-pos (point))))
                    (when (/= top cur)
                      (scroll-up 1)))))

(global-set-key (kbd "<H-M-down>")
                (lambda ()
                  (interactive)
                  (save-excursion
                    (let ((cur (line-number-at-pos (point)))
                          (bot (- (line-number-at-pos (window-end nil t)) 1)))
                      (when (/= bot cur)
                        (scroll-down 1))))))

;;; _/-/SPACE
(global-set-key (kbd "H--") 'paloryemacs/_-SPC)
(defun paloryemacs/_-SPC ()
  (interactive)
  (let ((char (following-char)))
    (cl-case char
      ((?\_) (progn (delete-char 1)
                    (insert ?\-)
                    (backward-char 1)))
      ((?\-) (progn (delete-char 1)
                    (insert ?\ )
                    (backward-char 1)))
      ((?\ ) (progn (delete-char 1)
                    (insert ?\_)
                    (backward-char 1)))
      (t (message "Current is not '_ '- or SPC!")))))

;;; cycle-spacing, since 24.4
;; both `M-SPC' and `M-S-SPC' are binded to `just-one-space'
;; "M-\" is used to switch input source on OS X. The behavior of
;; double call `cycle-spacing' as same as `delete-horizontal-space'
(global-set-key (kbd "M-S-SPC") 'cycle-spacing)

;;; Quickly Find Emacs Lisp Sources
(define-key 'help-command (kbd "C-l") 'find-library)
(define-key 'help-command (kbd "C-k") 'find-function-on-key)
(define-key 'help-command (kbd "C-v") 'find-variable)

;; Find the definition of the FUNCTION near point. That's very useful!
(use-package find-func
  :defer 7
  :config
  ;; Define some key bindings for the find-function family of functions.
  (find-function-setup-keys))
;; C-x F   find-function
;; C-x 4 F find-function-other-window
;; C-x 5 F find-function-other-frame
;; C-x K   find-function-on-key
;; C-x V   find-variable
;; C-x 4 V find-variable-other-window
;; C-x 5 V find-variable-other-frame


;;;
(global-set-key (kbd "C-x \\") 'align)
(global-set-key (kbd "C-x |") 'align-regexp)

;;; miscellaneous toggle-map
(autoload 'dired-toggle-read-only "dired" nil t)



;;; hydra
;; hydra-lv is buggy, see https://github.com/abo-abo/hydra/issues/114
(setq hydra-lv t)

(with-eval-after-load "hydra"
  (defhydra hydra-toggle (:color blue)
    "
_`_ evil-visual-mark-mode:             %`evil-visual-mark-mode
_a_ abbrev-mode:                       %`abbrev-mode
_d_ debug-on-error:                    %`debug-on-error
_f_ auto-fill-mode:                    %`auto-fill-function
_n_ narrow-or-widen-dwim:              %(buffer-narrowed-p)
_g_ golden-ratio-mode:                 %`golden-ratio-mode
_i_ aggressive-indent-mode:            %`aggressive-indent-mode
_G_ debug-on-quit:                     %`debug-on-quit
_r_ read-only-mode:                    %`buffer-read-only
_s_ rainbow-delimiters-string-color    %`--paloryemacs/rainbow-delimiters-strong-color
_p_ smartparens:                       %`smartparens-mode
_t_ truncate-lines:                    %`truncate-lines
_w_ whitespace-mode:                   %(and (boundp 'whitespace-mode) whitespace-mode)

"
    ("`" evil-visual-mark-mode "abbrev")
    ("a" abbrev-mode "abbrev")
    ("d" toggle-debug-on-error "debug")
    ("f" auto-fill-mode "fill")
    ("n" paloryemacs/narrow-or-widen-dwim "")
    ("g" golden-ratio-mode "golden-ratio")
    ("hi" highlight-indent-guides-mode "hl-indent")
    ;; ("hi" highlight-indentation-mode "hl-indent")
    ;; ("hc" highlight-indentation-current-column-mode "hl-indent-column")
    ("i" aggressive-indent-mode "")
    ("G" toggle-debug-on-quit "debug-quit")
    ("p" smartparens-mode "")
    ("C-p" smartparens-global-mode "global smartparens")
    ("r" dired-toggle-read-only "read only") ; generalized version of `read-only-mode'.
    ("t" toggle-truncate-lines "truncate")
    ("w" whitespace-mode "whitespace")
    ("s" paloryemacs/toggle-saturate-rainbow-delimiters-color "strong color")
    ("<escape>" nil "cancel")
    ("q"        nil "cancel")))

;; Launcher Keymap
;; (define-prefix-command 'launcher-map)
;; C-x l is `count-lines-page' by default. If you
;; use that, you can try s-l or <C-return>.
;; (define-key ctl-x-map "l" 'launcher-map) ; `C-x l ' default bind to count-lines-page
;; (global-set-key (kbd "s-l") 'launcher-map)
;; (define-key launcher-map "c" #'calc)

;; Launching External Applications and Websites
;; http://endlessparentheses.com/keymap-for-launching-external-applications-and-websites.html
;; (defmacro paloryemacs/def-run (exec)
;;   "Return a function that runs the executable EXEC."
;;   (let ((func-name (intern (concat "paloryemacs/run-" exec))))
;;     `(progn
;;        (defun ,func-name ()
;;          ,(format "Run the %s executable." exec)
;;          (interactive)
;;          (start-process "" nil ,exec))
;;        ',func-name)))

;; (define-key launcher-map "m" (paloryemacs/def-run "Mathematica"))

;; (defmacro paloryemacs/def-browse (url)
;;   "Return a function that calls `browse-url' on URL."
;;   (let ((func-name (intern (concat "paloryemacs/browse-" url))))
;;     `(progn
;;        (defun ,func-name ()
;;          ,(format "Browse to the url %s." url)
;;          (interactive)
;;          (browse-url ,url))
;;        ',func-name)))

;; (define-key launcher-map "r" (paloryemacs/def-browse "http://www.reddit.com/r/emacs/"))
;; (define-key launcher-map "w" (paloryemacs/def-browse "http://www.emacswiki.org/"))

;;; Launcher
;; C-x l is `count-lines-page' by default. If you use that, you can try s-l or <C-return>.
(global-set-key
 (kbd "C-x l")
 (defhydra hydra-launcher (:color blue)
   "Launch"
   ("c" calc "calc")
   ("d" ediff-buffers "ediff buffers")
   ("f" find-dired "find dired")
   ("g" lgrep "lgrep")
   ("G" rgrep "rgrep")
   ("h" man "man")
   ("r" (browse-url "http://www.reddit.com/r/emacs/") "reddit")
   ("w" (browse-url "http://www.emacswiki.org/") "emacswiki")
   ("s" shell "shell")
   ("<escape>" nil "cancel")
   ("q"        nil "cancel")))

;;; high frequency key
(key-chord-define-global
 "jf"
 (defhydra hydra-high-frequency (:color teal)
   "
org-agend_a_ org-_c_apture org-clock-_g_oto
_p_rojectile _s_cratch
undo-tree-_u_ndo undo-tree-_r_edo
"
   ("SPC" mark-sexp "mark sexp" :color red)
   ("+" cnfonts-increase-fontsize :color red)
   ("-" cnfonts-decrease-fontsize :color red)
   ("=" paloryemacs/cnfonts-reset-profile-and-fontsize "reset font")
   ("a" org-agenda nil)
   ("c" org-capture nil)
   ("g" org-clock-goto nil)
   ("G" hydra-git-gutter/body "git gutter")
   ("h" hydra-apropos/body "apropos")
   ("j" dired-jump "dired jump")
   ("o" paloryemacs/replace-charset-to-oem "char->oem")
   ("p" hydra-projectile/body nil)
   ("s" paloryemacs/switch-to-scratch nil)
   ("t" hydra-toggle/body "toggle")
   ("u" undo-tree-undo nil :color red)
   ("r" undo-tree-redo nil :color red)
   ("<escape>" nil "cancel")
   ("q"        nil "cancel")))

(defun paloryemacs/switch-to-scratch ()
  (interactive)
  (switch-to-buffer "*scratch*"))

;;; Rectangle-mode Hydra
;; http://oremacs.com/2015/02/25/rectangle-hydra/
;; d deletes rectangle; it's similar to C-d.
;; n copies rectangle; it's similar to M-w.
;; q exits; it's very easy to press.
;; e exchanges the point and mark; it's also quite useful to re-activate the region if you disabled it with n or r.
;; s fills the selected rectangle with a string.
;; y yanks the rectangle that you saved before with n.
;; r deactivates or activates the rectangle at point.
;; u calls undo.
;; p kills the rectangle; it's similar to C-w.


(defvar rectangle-mark-mode)
(defun hydra-ex-point-mark ()
  "Exchange point and mark."
  (interactive)
  (if rectangle-mark-mode
      (exchange-point-and-mark)
    (let ((mk (mark)))
      (rectangle-mark-mode 1)
      (goto-char mk))))

(defhydra hydra-rectangle (:body-pre (rectangle-mark-mode 1)
                                     :color pink
                                     :post (deactivate-mark))
  "
  ^_k_^     _d_elete    _s_tring
_h_   _l_   _o_k        _y_ank
  ^_j_^     _n_ew-copy  _r_eset
^^^^        _e_xchange  _u_ndo
^^^^        ^ ^         _p_aste
"
  ("h" backward-char nil)
  ("l" forward-char nil)
  ("k" previous-line nil)
  ("j" next-line nil)
  ("e" hydra-ex-point-mark nil)
  ("n" copy-rectangle-as-kill nil)
  ("d" delete-rectangle nil)
  ("r" (if (region-active-p)
           (deactivate-mark)
         (rectangle-mark-mode 1)) nil)
  ("y" yank-rectangle nil)
  ("u" undo nil)
  ("s" string-rectangle nil)
  ("p" kill-rectangle nil)
  ("o" nil nil)
  ("<escape>" nil nil)
  ("q"        nil nil))

(global-set-key (kbd "C-x SPC") 'hydra-rectangle/body)

;;; hydra apropos family
(defhydra hydra-apropos (:color blue :hint nil)
  "
_a_propos _c_ommand
_d_ocumentation _l_ibrary
_v_ariable _u_ser-option
^ ^ valu_e_"
  ("a" apropos)
  ("d" apropos-documentation)
  ("v" apropos-variable)
  ("c" apropos-command)
  ("l" apropos-library)
  ("u" apropos-user-option)
  ("e" apropos-value)
  ("<escape>" nil nil)
  ("q"        nil nil))

(defhydra hydra-dumb-jump (:color pink)
  "Dumb Jump"
  ("g" dumb-jump-go "Go")
  ("b" dumb-jump-back "Back")
  ("l" dumb-jump-quick-look "Look")
  ("e" dumb-jump-go-prefer-external "External")
  ("<escape>" nil "Quit" :color blue)
  ("q"        nil "Quit" :color blue))

(provide '50key-bindings)
