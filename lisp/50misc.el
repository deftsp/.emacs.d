;;; 05misc.el ---                          -*- lexical-binding: t; -*-
;; Author: Shihpin Tsing <deftsp@gmail.com>

;;; Code:

;;; personal data
(setq user-full-name "Shihpin Tseng"
      user-mail-address "deftsp@gmail.com"
      change-log-default-name "ChangeLog"
      vc-user-login-name "author")

;;; init
(setq inhibit-startup-message t
      initial-scratch-message nil ;; no message in the scratch buff
      inhibit-startup-echo-area-message t)

(setq-default default-directory "~/")

(setq read-quoted-char-radix 10 ; accept decimal input when using ^q, e.g.: ^q 13 [RET] -> ^M
      ;; echo unfinished commands after this many seconds of pause.
      echo-keystrokes 0.02              ; 0.1
      tooltip-hide-delay 20             ; default 10
      confirm-kill-emacs nil            ; alternative `yes-or-no-p' ; `y-or-n-p'
      confirm-kill-processes nil
      use-dialog-box nil ; mouse commands don't use dialog boxes to ask quesary "english"
      mark-even-if-inactive t
      set-mark-command-repeat-pop t
      select-enable-clipboard t        ; cutting and pasting uses the clipboard.
      select-enable-primary t
      highlight-nonselected-windows nil
      next-screen-context-lines 2
      mail-user-agent 'gnus-user-agent
      ;; default-enable-multibyte-characters t
      enable-local-variables :safe)

(add-to-list 'safe-local-eval-forms '(auto-fill-mode -1))
;; (add-to-list 'safe-local-eval-forms '(org-link-beautify-disable))

;; https://unix.stackexchange.com/a/108308
;; https://emacsredux.com/blog/2014/05/16/opening-large-files/
;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

;;; mode line
(setq size-indication-mode t            ; show file size (emacs 22+)
      ;; slow, especially when two windows show the same buffer
      column-number-mode nil)

;; (if (eq system-type 'gnu/linux)
;;     (display-battery-mode t))


;; display world time
;; For other cities, please refer to this http://en.wikipedia.org/wiki/List_of_tz_database_time_zones
;; M-x display-time-world or helm-world-time
(setq display-time-world-list
      '(("America/Los_Angeles" "Seattle")
        ("America/New_York" "New York")
        ("Asia/Chongqing" "China")
        ("Asia/Tokyo" "Tokyo")
        ("Europe/London" "London")
        ("Europe/Paris" "Paris")))

;;; comint
(setq-default comint-process-echoes t) ; comint do not echo input
;; set maximum-buffer size for shell-mode (useful if some program that you're debugging spews out large amounts of output).
(setq comint-completion-addsuffix t       ; Insert space/slash after completion
      comint-buffer-maximum-size 10240
      comint-scroll-to-bottom-on-input t  ; always insert at the bottom
      comint-scroll-to-bottom-on-output t ; always add output at the bottom
      comint-scroll-show-maximum-output t ; scroll to show max possible output
      comint-completion-autolist t        ; show completion list when ambiguous
      ;; no duplicates in command history
      comint-input-ignoredups t)

;; clean comint buffer
;; http://emacsredux.com/blog/2015/01/18/clear-comint-buffers/
(defun tl/comint-clear-buffer ()
  (interactive)
  (let ((comint-buffer-maximum-size 0))
    (comint-truncate-buffer)))

(with-eval-after-load "comint"
  (define-key comint-mode-map "\C-c\M-o" #'tl/comint-clear-buffer))

;;; minibuffer
(setq minibuffer-electric-default-mode 1
      resize-mini-windows t
      enable-recursive-minibuffers t)
(minibuffer-depth-indicate-mode +1)
;; don't let the cursor go into minibuffer prompt
;; http://ergoemacs.org/emacs/emacs_stop_cursor_enter_prompt.html
(setq minibuffer-prompt-properties
      '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt))

(setq visible-bell nil                    ;no f* beep on error.
      ring-bell-function 'ignore)

;; Emacs does not beep when you hit `C-g' in the minibuffer or during
;; an `isearch' (http://www.emacswiki.org/cgi-bin/wiki.pl?AlarmBell)
;; (setq ring-bell-function
;;       (lambda ()
;;         (unless (memq this-command
;;                       '(isearch-abort abort-recursive-edit find-file
;;                         exit-minibuffer keyboard-quit))
;;           (ding))))

;;;
(when (eq system-type 'darwin)
  ;; (setq find-function-C-source-directory "")

  (defun tl//set-source-directory ()
    ;; https://github.com/krismolendyke/.emacs.d/blob/master/init.el
    (unless tl/brew-cache-directory
      (setq tl/brew-cache-directory
            (string-trim (shell-command-to-string
                          (string-join `(,(executable-find "brew") "--cache") " ")))))
    (dolist (dir '("emacs--git" "emacs-mac--git"))
      (let ((p (string-join `(,tl/brew-cache-directory ,dir) "/")))
        (when (file-exists-p p)
          (setq source-directory p)))))
  ;; brew --cache is slow for emacs bootup
  (run-with-idle-timer 13 nil #'tl//set-source-directory))

;;; Move to trash when deleting stuff
(when (eq system-type 'darwin)
  (setq delete-by-moving-to-trash t
        trash-directory "~/.Trash/__emacs_trash"))

;;; which func mode
(use-package which-func
  :init
  (setq which-func-modes
        '(c-mode
          c++-mode
          org-mode
          lua-mode
          rust-mode
          rustic-mode
          web-mode
          sql-mode
          org-mode
          emacs-lisp-mode))
  ;; N.B. enable all in some mode will get warning: "which-func-ff-hook error: (wrong-type-argument (or eieio-object class) nil obj)"
  ;; (setq which-func-modes t) ; enabled in any major mode that supports it.
  ;; We remove Which Function Mode from the mode line, because it's mostly invisible here anyway.
  ;; (setq mode-line-misc-info (assq-delete-all 'which-func-mode mode-line-misc-info))
  (setq which-func-unknown "⊤") ; "n/a" "N/A"
  :config
  (add-to-list 'which-func-non-auto-modes 'magit-status-mode)
  (add-to-list 'which-func-non-auto-modes 'treemacs-mode)

;;; header-line-format
  ;; setq-default header-line-format will cause hydra message window can only show one line
  ;; (setq-default header-line-format '((which-func-mode ("" which-func-format " "))))
  (defun tl/set-header-line-format ()
    (setq header-line-format
          '((which-func-mode ("" which-func-format " ")))))
  (add-hook 'prog-mode-hook 'tl/set-header-line-format)
  (add-hook 'org-mode-hook 'tl/set-header-line-format)

  ;; which function mode is a global minor mode
  (which-function-mode -1))


;;; fringe
(setq-default indicate-buffer-boundaries 'left)

;;; refresh
;;; refresh the buffer when the file changes on disk.
(global-auto-revert-mode 1)
;; also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)

;;; mailcap
(use-package mailcap
  :defer 7
  :config
  (mailcap-parse-mailcaps "~/.mailcap" t))

;;; syntax table
;; (modify-syntax-entry ?_ "w")            ; now '_' is not considered a word-delimiter
;; (modify-syntax-entry ?- "w")            ; now '-' is not considered a word-delimiter
;; (modify-syntax-entry ?_ "_")
;; (modify-syntax-entry ?' "." text-mode-syntax-table)

;;; turncate
(set-default 'truncate-lines t)
(setq truncate-partial-width-windows 50) ;; truncate lines in all windows less than full frame wide.

;; visual-line-mode is a new addition that is set on by default. It is something of a replacement
;; for longlines-mode. It doesn't insert soft line breaks, so it works better on larger files, and
;; it wraps to whatever your screen is, so if you are using longlines-mode you should switch to
;; using visual-line-mode. C-n and C-p will move by visual lines, which may screw up some macros,
;; but mostly it seems to work quite well.

;; in evil mode, use gj gk to move respecting to visual line
(setq visual-line-fringe-indicators (quote (left-curly-arrow right-curly-arrow)))
;; (global-visual-line-mode +1)

;; Active Region Highlighting
(transient-mark-mode t)

;;; display time
(setq display-time-24hr-format t
      display-time-day-and-date t
      display-time-interval 10          ; Seconds between updates of time in the mode line
      display-time-use-mail-icon t)
(display-time-mode +1)

;; check that the file specified by `display-time-mail-file' is nonempty or that the
;; directory `display-time-mail-directory' contains nonempty files.

(let ((dir "~/Mail/inbox/new/"))
  (when (file-exists-p dir)
    (setq display-time-mail-directory dir)))

(setq display-time-string-forms
      '((format-time-string " %y/%m/%d " now)
        " " 24-hours ":" minutes
        (if time-zone " ") time-zone (if time-zone " ")
        load
        (if mail
            (concat " "
                    (propertize
                     display-time-mail-string
                     'display (and display-time-use-mail-icon (display-graphic-p)
                                   display-time-mail-icon)
                     'help-echo "You have new mail; mouse-2: Read mail"
                     'mouse-face 'mode-line-highlight
                     'local-map (make-mode-line-mouse-map 'mouse-2 read-mail-command)))
          "")))

;;; use y-or-n-p instead of yes-or-no-p
(fset 'yes-or-no-p 'y-or-n-p)

;;; man
(use-package man
  :defer t
  :config
  (use-package man-completion)
  (setq Man-notify-method 'bully) ; make the manpage the current buffer in the current window
  ;;click on URLs in manual pages
  (add-hook 'Man-mode-hook 'goto-address))

(use-package woman
  :defer t
  :init
  (progn
    (setq woman-manpath-man-regexp "[Mm][Aa][Nn][1-9][^p]*$" ; "[Mm][Aa][Nn]"
          woman-show-log nil
          woman-cache-filename "~/.emacs.d/.wmncach.el"
          woman-use-own-frame nil
          woman-fill-column 100)
    (defun woman-word-at-point ()
      (interactive)
      (let ((woman-use-topic-at-point t))
        (woman))))
  :config
  ;; Not to lookup POSFIX man page
  ;; `woman-manpath-man-regexp' should be set before requrie woman.el. So the it should be changed
  (add-to-list 'woman-manpath "/usr/local/share/man"))

;;; compile
;; compilation-mode has a nice feature so that you can skip over your info and
;; warning level messages and jump right to those nasty errors. Set the variable
;; compilation-skip-threshold to 2 so that M-n and M-p will jump to the next or
;; previous error respectively. The other possible settings for this variable
;; are:

;; 2 — skip anything less than error
;; 1 — skip anything less than warning, or
;; 0 — don ’ t skip any messages.

(use-package compile
  :defer t
  :config
  (setq compilation-window-height nil      ; Some windows's height
        compilation-ask-about-save nil
        ;; compilation-context-lines 3
        ;; compilation-skip-threshold 1
        ;; compilation-skip-visited t
        ;; keep scrolling in compilation result buffer
        compilation-scroll-output t)
  (dolist (str '("*Help*" "*Completions*"))
    (add-to-list 'completion-ignored-extensions str)))

;;; help window
(setq help-window-select t)

;;; error
;; (setq next-error-highlight 0.5
;;       next-error-highlight-no-select t)

;;; image
(setq auto-image-file-mode nil)

;;; compress
;;Automatically uncompress .gz files
(auto-compression-mode +1)

;;; custom
;; kill customize buffers on exit
(setq custom-buffer-done-kill t)

;;; crosshair
;; dependencies hl-line+ col-highlight vline
(use-package crosshair
  :defer t
  :commands (crosshairs-mode))

;;; register
;; Initial register values
;; `C-x r j i' to open init.el,
(defun tl/register-init ()
  (mapc #'(lambda (r)
            (let ((file-path (cdr r)))
              (when (file-exists-p file-path)
                (set-register (car r) (cons 'file file-path)))))
        '((?i . "~/.emacs.d/init.el")
          (?g . "~/org/agenda/GTD.org"))))
(tl/register-init)

;;; file-name-shadow-mode
;; be smart about filenames understand ~/ etc
(when (fboundp 'file-name-shadow-mode)
  (file-name-shadow-mode t))

;;; paren
;; instead of by smartparens
;; (use-package mic-paren
;;   :defer 7
;;   :init
;;   (setq paren-sexp-mode 'mismatch)
;;   :config
;;   (paren-activate))

;; use mic-paren instead
;; (show-paren-mode t)
;; (setq show-paren-delay 0
;;       show-paren-style 'parenthesis)    ;expression

;; rainbow-delimiters
(use-package rainbow-delimiters
  :defer t
  :init
  (progn
    (defun tl/turn-on-rainbow-delimiters-mode ()
      (rainbow-delimiters-mode +1))

    (when (fboundp 'rainbow-delimiters-mode)
      (add-hook 'prog-mode-hook 'tl/turn-on-rainbow-delimiters-mode)))
  :config
  (progn
    (defvar --tl/rainbow-delimiters-strong-color nil)
    (defun tl/toggle-saturate-rainbow-delimiters-color (arg)
      (interactive "P")
      (let ((fun nil))
        (if --tl/rainbow-delimiters-strong-color
            (if (or (null arg) (< arg 0))
                (progn
                  (fset 'fun (symbol-function 'color-desaturate-name))
                  (setq --tl/rainbow-delimiters-strong-color nil)
                  (message "Turn off rainbow delimiters strong color."))
              (when arg
                (message "rainbow delimiters strong color is already on.")))
          (if (or (null arg) (> arg 0))
              (progn (fset 'fun (symbol-function 'color-saturate-name))
                     (setq --tl/rainbow-delimiters-strong-color t)
                     (message "Turn on rainbow delimiters strong color."))
            (when arg
              (message "rainbow delimiters strong color is already off."))))

        (when (fboundp 'fun)
          (cl-loop
           for index from 1 to rainbow-delimiters-max-face-count
           do
           (let ((face (intern (format "rainbow-delimiters-depth-%d-face" index))))
             (cl-callf fun (face-foreground face) 60))))))
    (tl/toggle-saturate-rainbow-delimiters-color +1)))

(defun tl/goto-match-paren (arg)
  "Go to the matching parenthesis if on parenthesis,
C-u delte pair if on parenthesis
 otherwise insert %.
vi style of % jumping to matching brace."
  (interactive "p")
  ;; first, check for "outside of bracket" positions expected by forward-sexp, etc.
  (cond ((looking-at "[\[\(\{]")
         (if (>= arg 4) (delete-pair) (forward-sexp) (backward-char 1)))
        ((looking-at "[\]\)\}]")
         (forward-char)
         (backward-sexp)
         (if (>= arg 4) (delete-pair)))
        (t (self-insert-command (or arg 1)))))

(global-set-key (kbd "C-x %") 'tl/kill-outside-paren-with-elt)
(defun tl/kill-outside-paren-with-elt (arg)
  (interactive "p")
  (if (not (looking-at "[([{]"))
      (up-list -1))
  (copy-region-as-kill (point) (scan-sexps (point) 1))
  (up-list -1)
  (kill-sexp 1)
  (yank 2))

;;; mouse
;; (setq mouse-drag-copy-region nil)
;; (mouse-avoidance-mode 'animate) ;; disable it until fix the bug in `org-capture' buffer 'C-x C-q' error
(when window-system
  (setq mouse-yank-at-point t)
  (mouse-wheel-mode t)
  ;; scroll one line at a time (less "jumpy" than defaults).
  ;; It can also be a floating point number, specifying the fraction of a full screen to scroll.
  (setq mouse-wheel-scroll-amount '(1  ; one line at a time
                                    ((shift) . 1)
                                    ((control))))
  (setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
  ;; scroll window under mouse
  (setq mouse-wheel-follow-mouse t)

  ;; (global-set-key (kbd "<mouse-1>") 'mouse-set-point)
  ;; (global-set-key (kbd "<down-mouse-1>") 'mouse-drag-region)
  ;; (global-set-key (kbd "<C-down-mouse-1>") 'mouse-buffer-menu)

  ;; (global-set-key [M-down-mouse-1] 'mouse-drag-secondary-pasting)
  ;; (global-set-key [M-S-down-mouse-1] 'mouse-drag-secondary-moving)
  (global-set-key (kbd "<H-mouse-1>") 'browse-url-at-mouse)

  ;; ;; setup scroll mouse settings
  ;; (defun up-slightly () (interactive) (scroll-up 5))
  ;; (defun down-slightly () (interactive) (scroll-down 5))
  ;; (global-set-key [mouse-4] 'down-slightly)
  ;; (global-set-key [mouse-5] 'up-slightly)

  ;; (defun up-one () (interactive) (scroll-up 1))
  ;; (defun down-one () (interactive) (scroll-down 1))
  ;; (global-set-key [S-mouse-4] 'down-one)
  ;; (global-set-key [S-mouse-5] 'up-one)

  ;; (defun up-a-lot () (interactive) (scroll-up))
  ;; (defun down-a-lot () (interactive) (scroll-down))
  ;; (global-set-key [C-mouse-4] 'down-a-lot)
  ;; (global-set-key [C-mouse-5] 'up-a-lot)
  ;; mouse button one drags the scroll bar
  (global-set-key [vertical-scroll-bar down-mouse-1] 'scroll-bar-drag))

;;; scroll
(setq scroll-step 1                     ; default 0, keyboard scroll one line at a time
      scroll-margin 3
      scroll-conservatively 10000)     ; Fix the whole huge-jumps-scrolling-between-windows nastiness
(setq next-screen-context-lines 3)  ; how many from page up/down
(setq scroll-preserve-screen-position nil) ; notice: set this to `t' will casue scrool jump badly
(setq hscroll-margin 1)

;;; better page up/down
;; https://www.emacswiki.org/emacs/Scrolling
(defun sfp-page-down (&optional arg)
  (interactive "^P")
  (setq this-command 'next-line)
  (next-line
   (- (window-text-height)
      next-screen-context-lines)))
(put 'sfp-page-down 'isearch-scroll t)
(put 'sfp-page-down 'CUA 'move)

(defun sfp-page-up (&optional arg)
  (interactive "^P")
  (setq this-command 'previous-line)
  (previous-line
   (- (window-text-height)
      next-screen-context-lines)))
(put 'sfp-page-up 'isearch-scroll t)
(put 'sfp-page-up 'CUA 'move)

(global-set-key (kbd "C-v") 'sfp-page-down)
(global-set-key (kbd "M-v") 'sfp-page-up)

;; Might as well limit how many messages fill up in the message buffer.
;; Not that we'll ever get that many, but just in case!
(setq message-log-max 3000)

;; kills an entire line if the cursor is at the beginning of line. And try M-0 C-k.
(setq-default kill-whole-line t)
(setq kill-ring-max 200) ; default 60

;; Use cperl-mode instead of perl-mode
;; (defalias 'perl-mode 'cperl-mode)

(use-package so-long
  :defer 5
  :config
  (setq so-long-threshold 400)      ; reduce false positives w/ larger threshold
  ;; Don't disable syntax highlighting and line numbers, or make the buffer
  ;; read-only, in `so-long-minor-mode', so we can have a basic editing
  ;; experience in them, at least. It will remain off in `so-long-mode',
  ;; however, because long files have a far bigger impact on Emacs performance.
  ;; ...and insist that save-place not operate in large/long files
  (add-to-list 'so-long-variable-overrides '(save-place-alist . nil))
  ;; Text files could possibly be too long too
  (add-to-list 'so-long-target-modes 'text-mode)
  ;; But disable everything else that may be unnecessary/expensive for large
  ;; or wide buffers.
  (setq so-long-minor-modes
        (append so-long-minor-modes
                '(flycheck-mode
                  flyspell-mode
                  spell-fu-mode
                  eldoc-mode
                  smartparens-mode
                  highlight-numbers-mode
                  better-jumper-local-mode
                  ws-butler-mode
                  auto-composition-mode
                  undo-tree-mode
                  highlight-indent-guides-mode
                  hl-fill-column-mode)))
  (global-so-long-mode +1))

;;; whitespace mode
;; If you don't like having lines of code/text with whitespace at the ends, Emacs highlight the offending whitespace.
;; When set, the variable's value becomes buffer local, so set it to true in the mode-hooks for your preferred modes.
;; Or, if you want it on all the time, change the default value with:

;; (if (>= emacs-major-version 21)
;;     (setq-default show-trailing-whitespace t))
;; make whitespace-mode use just basic coloring
(use-package whitespace
  :defer t
  :init
  (progn
    (setq whitespace-style (quote (face spaces lines-tail tabs trailing newline space-mark tab-mark newline-mark)))
    (setq whitespace-line-column nil)   ; use `fill-column' variable value
    (setq whitespace-display-mappings
          ;; all numbers are unicode codepoint in decimal. e.g. (insert-char 182 1)
          '((space-mark 32 [183] [46]) ; 32   SPACE「 」, 183 MIDDLE DOT「·」, 46 FULL STOP「.」
            (space-mark 160 [164] [95]) ; 160  NO-BREAK SPACE「 」, 164 MIDDLE DOT「¤」, 95 FULL STOP「_」
            (newline-mark 10 [182 10])  ; 10   LINE FEED, 182 PILCROW SIGN「¶」
            (tab-mark 9 [9655 9] [92 9]) ; 9 TAB, 9655 WHITE RIGHT-POINTING TRIANGLE「▷」
            ))))

;; auto delete trailing whitespace
;; (add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Unobtrusively trim extraneous white-space *ONLY* in lines edited.
;; https://github.com/lewang/ws-butler
(use-package ws-butler
  :defer 3
  :init
  :diminish ws-butler-mode
  :config
  (ws-butler-global-mode +1))

(setq line-number-display-limit 10000000)

;; M-x apropos  apropos works better but slower
(setq apropos-do-all t)
(setq apropos-do-all t) ; search more extensively
(setq apropos-sort-by-scores t) ; sort matches by scores; best match is shown first.

;;; Enable emacs functionality that is disabled by default

;; overwrite mode is not disabled
;;(put 'overwrite-mode 'disabled nil)
(put 'set-goal-column 'disabled nil)

(put 'upcase-region    'disabled nil)
(put 'downcase-region  'disabled nil)
(put 'LaTeX-hide-environment 'disabled nil)
;;Allow a command to erase an entire buffer
(put 'erase-buffer 'disabled nil)
(put 'eval-expression 'disabled nil)
;; C-x < & C-x >
(put 'scroll-left 'disabled nil)
(put 'rmail 'disabled t)

;;; fvwm
(use-package fvwm-mode
  :defer t
  :init
  (progn
    (setq fvwm-fvwmcommand-path "/usr/bin/FvwmCommand")
    ;; before modify it, use C-h v auto-mode-alist to check it.
    (autoload 'fvwm-mode "fvwm-mode"
      "Major mode for editing FVWM configuration files." t)))

(autoload 'xrdb-mode "xrdb-mode" "Mode for editing X resource files" t)

;;; auto mode alist
(defun tl/mode-alist-init ()
  (mapcar #'(lambda (lst) (add-to-list 'auto-mode-alist lst))
          '(("\\.\\(xml\\|rdf\\)\\'"              . nxml-mode) ; sgml-mode
            ("\\.css\\'"                          . css-mode)
            ("\\.\\(emacs\\|session\\|gnus\\)\\'" . emacs-lisp-mode)
            ("\\.\\(jl\\|sawfishrc\\)\\'"         . sawfish-mode)
            ("\\.scm\\'"                          . scheme-mode)
            ("\\.py\\'"                           . python-mode)
            ("\\.\\(ba\\)?sh\\'"                  . sh-mode)
            ("\\.l\\'"                            . c-mode)
            ("\\.mm\\'"                           . objc-mode)
            ("\\.xm$"                             . objc-mode)
            ("\\.o\\'"                            . hexl-mode)
            ("\\.max\\'"                          . maxima-mode)
            ("\\.fvwm2rc$"                        . fvwm-mode)
            ("/f\\..*"                            . fvwm-mode)
            ("\\.Xdefaults$"                      . xrdb-mode)
            ("\\.Xenvironment$"                   . xrdb-mode)
            ("\\.Xresources$"                     . xrdb-mode)
            ("\\.tei$"                            . xml-mode)
            ("\\.dps$"                            . pascal-mode)
            ("\\.bash_aliases$"                   . shell-script-mode)
            ("\\.procmailrc$"                     . conf-mode)
            ("fonts.conf"                         . xml-mode)
            ("\\.org$"                            . org-mode)
            ("\\.asciidoc$"                       . adoc-mode)
            ("\\.julius$"                         . js2-mode)
            ("\\.js$"                             . web-mode)
            ("\\.jsx$"                            . web-mode)
            ("\\.html?\\'"                        . web-mode)
            ("\\.phtml\\'"                        . web-mode)
            ("\\.tpl\\.php\\'"                    . web-mode)
            ("\\.[gj]sp\\'"                       . web-mode)
            ("\\.as[cp]x\\'"                      . web-mode)
            ("\\.asp$"                            . web-mode)
            ("\\.erb\\'"                          . web-mode)
            ("\\.mustache\\'"                     . web-mode)
            ("\\.djhtml\\'"                       . web-mode)
            ("\\.tpl$"                            . web-mode)
            ("\\.php$"                            . web-mode)
            ("/rfc[0-9]+\\.txt\\'"                . irfc-mode)
            ("\\.lrc$"                            . emms-lyrics-mode)))

;;; since emacs 22 we can use magic-mode-alist to set mode
  (add-to-list 'magic-mode-alist '("\\(.\\|\n\\)*\n@implementation" . objc-mode))
  (add-to-list 'magic-mode-alist '("\\(.\\|\n\\)*\n@interface" . objc-mode))
  (add-to-list 'magic-mode-alist '("\\(.\\|\n\\)*\n@protocol" . objc-mode)))

(tl/mode-alist-init)

;;; insert DATE and some usefull things
(global-set-key (kbd "C-c i d") 'insert-date-at-current-point)
(defun insert-date-at-current-point (prefix)
  "Insert the current date. With prefix-argument, use ISO format. With
   two prefix arguments, write out the day and month name."
  (interactive "P")
  (let ((format
         (cond
          ((not prefix) "%d.%m.%Y")
          ((equal prefix '(4)) "%Y-%m-%d")
          ;; the standard date format
          ((equal prefix '(16)) "%c")))
        (system-time-locale "C"))
    (if format
        (insert (format-time-string format))
      (call-interactively 'insert-date-with-prompt-date-format))))


;; This command prompts the user for a date format:
(defun insert-date-with-prompt-date-format (&optional format)
  "Wrapper around format-time-string."
  (interactive "MFormat: ")
  (insert (format-time-string format)))

;; insert-sequence-symbol
(defun insert-sequence-symbol (key)
  "Insert the name of the function that key chord executes."
  (interactive "kInseret key description: ")
  (insert (symbol-name (key-binding key))))

(global-set-key (kbd "C-c i s")    'insert-sequence-symbol)

;; insert-sequence-key
(defun insert-sequence-key (key)
  "Inserts a keystroke suitable for use in fcns like global-set-key"
  (interactive "kInseret key chord: ")
  (insert (format "(kbd \"%s\")" (key-description key))))

(global-set-key (kbd "C-c i k") 'insert-sequence-key)



;; ISO Date formatting
(defvar tl/iso-date-format "%Y-%m-%dT%H:%M:%S%z"
  "Format string for ISO dates.")

(defun tl/iso-timestamp (&optional time)
  (format-time-string iso-date-format
                      (or time (current-time))))
(defun tl/insert-iso-timestamp ()
  (interactive)
  (insert (iso-timestamp)))

(defun tl/iso-timestamp-sexp (&optional time)
  (parse-time-string (iso-timestamp)))


;; (defun select-until-end-of-line ()
;;   "Select until the end of a line without killing it."
;;   (interactive)
;;   (copy-region-as-kill (point) (line-end-position)) )

(defun tl/insert-userid ()
  "Insert the users email address"
  (interactive)
  (insert user-full-name " <" user-mail-address ">" ) )

;; (defun recenter-to-first-line ()
;;   "Recenter to the first screenline."
;;   (interactive)
;;   (recenter "1"))
;; (global-set-key  "\C-cl"      'recenter-to-first-line)

(defun tl/point-to-register()
  "Store cursorposition _fast_ in a register.
Use tl/jump-to-register to jump back to the stored
position."
  (interactive)
  (point-to-register 6)
  (message "point-to-register 6"))

(defun tl/jump-to-register()
  "Switches between current cursorposition and position
that was stored with tl/point-to-register."
  (interactive)
  (let ((tmp (point-marker)))
    (jump-to-register 6)
    (set-register 6 tmp)))
(global-set-key (kbd "C-c 6") 'tl/point-to-register)
(global-set-key (kbd "C-c ^") 'tl/jump-to-register)

;;; time-stamp
(setq time-stamp-active t
      time-stamp-warn-inactive t
      time-stamp-line-limit 8           ;; check the first 10 lines of the file for Time-stamp: <>
      time-stamp-format (concat "%:y-%02m-%02d %02H:%02M:%02S " user-full-name))
(add-hook 'write-file-hooks 'time-stamp)
(add-hook 'before-save-hook 'time-stamp)

;; /usr/share/zoneinfo
(setq world-clock-list
      '(("Asia/Chongqing" "China")
        ("UTC" "UTC")
        ("America/Los_Angeles" "Seattle")
        ("America/New_York" "New York")
        ("Asia/Tokyo" "Tokyo")
        ("Europe/London" "London")
        ("Europe/Paris" "Paris")))
(setq world-clock-timer-second 1)
(setq world-clock-time-format " %a, %b %d %H:%M:%S %Z") ; "%A %d %B %R %Z"

;;; file variable
(defun tl/insert-file-variable ()
  "Insert file variable string \"-*- Major-Mode-Name -*-\" with
  comment char"
  (interactive)
  (insert
   (concat comment-start " -*- "
           (substring
            (symbol-name (symbol-value 'major-mode)) 0 -5)
           " -*- " comment-end)))

;;; charset-to-oem
(defun tl/replace-charset-to-oem (start end)
  (interactive "r")
  (save-excursion
    (format-replace-strings '(("　"   .   " ")
                              ("	" .   " ")
                              ("’"    .   "'")
                              ("′"    .   "'")
                              ("‘"    .   "'")
                              ("…"    .   "...")
                              ("“"    .   "\"")
                              ("”"    .   "\"")
                              ("（"   .   "(")
                              ("）"   .   ")")
                              ("："   .   ":")
                              ("•"    .   "ù")
                              ("–"    .   "-")
                              ("—"    .   "--")
                              ("ü"    .   "")
                              ("é"    .   "‚")
                              ("§"    .   "")
                              ("®"    .   "(R)")
                              ("·"    .   "*")
                              ("，"   .   ",")
                              ("×"    .   "x"))
                            nil
                            start
                            end)))

;;; kill/yank
;; Change cutting behaviour:
;; "Many times you'll do a kill-line command with the only intention of getting the contents of the line into the
;; killring. Here's an idea stolen from Slickedit, if you press copy or cut when no region is active you'll copy or cut
;; the current line:"  <http://www.zafar.se/bkz/Articles/EmacsTips>

;; (defadvice kill-ring-save (before slickcopy activate compile)
;;   "When called interactively with no active region, copy a single line instead."
;;   (interactive
;;    (if mark-active
;;        (list (region-beginning) (region-end))
;;        (list (line-beginning-position)
;;              (line-beginning-position 2)))))

(defadvice kill-region (before slickcut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
   (if mark-active
       (list (region-beginning) (region-end))
       (list (line-beginning-position)
             (line-beginning-position 2)))))

;;; versatile kill-ring-save-dwim
(global-set-key (kbd "M-w") 'tl/kill-ring-save-dwim)

(defun tl/kill-ring-save-dwim ()
  "This command dwim on saving text. \n
If region is active, call `kill-ring-save'. Else, call
`sdl-kill-ring-save-thing-at-point'. \n
This command is to be used interactively."
  (interactive)
  (if (use-region-p)
      (call-interactively 'kill-ring-save)
    (call-interactively 'tl/kill-ring-save-thing-at-point)))


;; M-w if thing at point is url or mail address copy it, or copy current line
;; After press M-w, if press following key...
;; key:  copy
;; w     word
;; l     list
;; s     sexp
;; f     file name
;; and it can accpet prefix, M-3 M-w will copy three lines and M-3 M-w w will copy three words.
(defun tl/kill-ring-save-thing-at-point (&optional n)
  "Save THING at point to kill-ring."
  (interactive "p")
  (let ((things '((?l . list) (?f . filename) (?w . word) (?s . sexp)))
        (message-log-max)
        beg t-a-p thing event)
    (cl-flet ((get-thing ()
                         (save-excursion
                           (beginning-of-thing thing)
                           (setq beg (point))
                           (if (= n 1)
                               (end-of-thing thing)
                             (forward-thing thing n))
                           (buffer-substring beg (point)))))
      ;; try detecting url email and fall back to 'line'
      (dolist (thing '(url email line))
        (when (bounds-of-thing-at-point thing)
          (setq t-a-p (get-thing))
          ;; remove the last newline character
          (when (and (eq thing 'line)
                     (>= (length t-a-p) 1)
                     (equal (substring t-a-p -1) "\n"))
            (setq t-a-p (substring t-a-p 0 -1)))
          (kill-new t-a-p)
          (message "%s" t-a-p)
          (cl-return nil)))
      (setq event (read-event nil))
      (when (setq thing (cdr (assoc event things)))
        (clear-this-command-keys t)
        (if (not (bounds-of-thing-at-point thing))
            (message "No %s at point" thing)
          (setq t-a-p (get-thing))
          (kill-new t-a-p 'replace)
          (message "%s" t-a-p))
        (setq last-input-event nil))
      (when last-input-event
        (clear-this-command-keys t)
        (setq unread-command-events (list last-input-event))))))


;; http://blog.binchen.org/posts/convert-multiple-line-into-one-big-string-in-emacs.html
(global-set-key (kbd "C-c C-y") 'tl/strip-convert-lines-into-one-big-string)
(defun tl/strip-convert-lines-into-one-big-string (beg end)
  "strip and convert selected lines into one big string which is copied into kill ring.
When transient-mark-mode is enabled, if no region is active then
only the current line is acted upon.

If the region begins or ends in the middle of a line, that entire line is
copied, even if the region is narrowed to the middle of a line.

Current position is preserved."
  (interactive "r")
  (let (str (orig-pos (point-marker)))
    (save-restriction
      (widen)
      (when (and transient-mark-mode (not (use-region-p)))
        (setq beg (line-beginning-position)
              end (line-beginning-position 2)))

      (goto-char beg)
      (setq beg (line-beginning-position))
      (goto-char end)
      (unless (= (point) (line-beginning-position))
        (setq end (line-beginning-position 2)))

      (goto-char beg)
      (setq str (replace-regexp-in-string
                 "[ \t]*\n"
                 ""
                 (replace-regexp-in-string
                  "^[ \t]+" ""
                  (buffer-substring-no-properties beg end))))
      ;; (message "str=%s" str)
      (kill-new str)
      (goto-char orig-pos))))

;; TODO: use tl/join-lines instead of tl/strip-convert-lines-into-one-big-string
;; https://medium.com/@4d47/join-lines-in-emacs-cc40a55e4539
;; (defun tl/join-lines (n)
;;   "Join N lines."
;;   (interactive "p")
;;   (if (use-region-p)
;;       (let ((fill-column (point-max)))
;;         (fill-region (region-beginning) (region-end)))
;;     (dotimes (_ (abs n))
;;       (delete-indentation (natnump n)))))

;;;
;; Don't bother entering search and replace args if the buffer is read-only. Duh.
;; (defadvice query-replace-read-args (before barf-if-buffer-read-only activate)
;;   "Signal a `buffer-read-only' error if the current buffer is read-only."
;;   (barf-if-buffer-read-only))

;;-----------------------------------------------------------------------------------------------

;;; autoarg mode
;; (autoarg-mode 1)
;; M-x autoarg-mode & autoarg-kp-mode

;;; rename-file-and-buffer
(defun tl/rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file name new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

(global-set-key (kbd "C-c f r")  'tl/rename-file-and-buffer)

(defun tl/move-buffer-and-file (dir)
  "Moves both current buffer and file it's visiting to DIR."
  (interactive "DNew directory: ")
  (let* ((name (buffer-name))
         (filename (buffer-file-name))
         (dir
          (if (string-match dir "\\(?:/\\|\\\\)$")
              (substring dir 0 -1) dir))
         (newname (concat dir "/" name)))

    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
        (progn
          (copy-file filename newname 1)
          (delete-file filename)
          (set-visited-file-name newname)
          (set-buffer-modified-p nil)
          t))))

(global-set-key (kbd "C-c f R") 'tl/move-buffer-and-file)

;;; info
(use-package info
  :defer t
  :config
  ;; make sure info dir in `Info-directory-list' add to `Info-directory-list'
  (mapc (lambda (p) (add-to-list 'Info-directory-list p t)) Info-default-directory-list)
  (add-to-list 'Info-additional-directory-list "~/share/info")
  (general-define-key
   :states 'normal
   :keymaps 'Info-mode-map
   "n"          'Info-next
   "p"          'Info-prev
   "N"          'Info-prev

   "D"          'Info-directory
   "u"          'Info-up
   "L"          'Info-history
   "l"          'Info-history-back

   "d"          'Info-scroll-up
   "e"          'Info-scroll-down
   "f"          'Info-history-forward

   "m"          'Info-menu

   "/"          'Info-search
   "gh"         'Info-help
   "gn"         'Info-goto-node))

;; (defun find-subdirs-containing (dir pattern)
;;   "Return a list of all deep subdirectories of DIR that contain
;; files that match PATTERN."
;;   (let* ((ret nil)
;;          (files (directory-files dir))
;;          (max-lisp-eval-depth 3000))
;;     (while files
;;       (let* ((file (car files))
;;              (path (expand-file-name file dir)))
;;         (if (and (file-directory-p path)
;;                  (not (string-match "^\\.+" file)))
;;             (setq ret (append ret (find-subdirs-containing path pattern)))
;;           (if (string-match pattern file)
;;               (add-to-list 'ret dir))))
;;       (setq files (cdr files)))
;;     ret))

;; (defun add-init-path-to-info-path ()
;;   "Add the subdirectories of init-path that contain info directory
;; files to the Info-directory-list.  This can safely be run many times
;; in a session, without adding multiple copies of the directories.  The
;; new directories are prepended to emacs's initial Info path."
;;   (interactive)
;;   (setq Info-directory-list (append (find-subdirs-containing init-path "^dir$") initial-info-path)))

;; (defun add-info-dir-files-to-path (tree)
;;   "Add all the info files under TREE to info \"dir\" files"
;;   (let* ((info-regex "\\.info$")
;;          (info-dirs (find-subdirs-containing tree info-regex)))
;;     (mapcar (lambda (dir)
;;               (dolist (file (directory-files dir t info-regex))
;;                 (call-process "install-info" nil nil nil
;;                               (format "--dir-file=%s/dir" dir)
;;                               (format "--info-file=%s" file))))
;;             info-dirs)))

;;; Create dir files for any info files in the init-path
;; (add-info-dir-files-to-path init-path)

;; Add the init-path tree to the Info path
;; (require 'info)
;; (info-initialize)
;; (setq initial-info-path Info-directory-list)
;; (add-init-path-to-info-path)

;;;###autoload
(defun tl/insert-char-next-line (arg)
  "insert char below the cursor"
  (interactive "p")
  (let ((col (current-column))
        char)
    (setq char
          (save-excursion
            (forward-line arg)
            (move-to-column col)
            (if (= (current-column) col)
                (char-after))))
    (if char
        (insert-char char 1)
      (message (concat "Can't get charactor in "
                       (if  (< arg 0)
                           "previous"
                         "next")
                       (progn (setq arg (abs arg))
                              (if (= arg 1) ""
                                (concat " " (number-to-string arg))))
                       " line.")))))

;;;###autoload
(defun tl/insert-char-prior-line (arg)
  "insert char above the cursor"
  (interactive "p")
  (tl/insert-char-next-line (- arg)))

;;; Transpose
(global-unset-key (kbd "M-t"))
(global-set-key (kbd "M-t t") 'transpose-words)
(global-set-key (kbd "M-t w") 'transpose-words)
(global-set-key (kbd "M-t M-t") 'transpose-words)
(global-set-key (kbd "M-t l") 'transpose-lines)
(global-set-key (kbd "M-t s") 'transpose-sexps)
(global-set-key (kbd "M-t a") 'anchored-transpose)

;; Anchored transpose
;; https://github.com/emacsmirror/nxhtml/blob/master/util/anchored-transpose.el
(autoload 'anchored-transpose "anchored-transpose" nil t)


;;; browse-kill-ring
;; bind to key-chord "YY"
(use-package browse-kill-ring
  :defer t
  :chords (("YY" . browse-kill-ring))
  :init
  (progn
    (setq browse-kill-ring-quit-action 'save-and-restore)  ; 'bury-and-delete-window
    (setq browse-kill-ring-highlight-current-entry t))
  :config
  (progn
    (browse-kill-ring-default-keybindings)
    (define-key browse-kill-ring-mode-map [down] 'browse-kill-ring-forward)
    (define-key browse-kill-ring-mode-map [up] 'browse-kill-ring-previous)))

;;; kill-ring-search
(use-package kill-ring-search
  :defer t
  :init
  (autoload 'kill-ring-search "kill-ring-search"
    "Search the kill ring in the minibuffer."
    (interactive))
  (global-set-key "\M-\C-y" 'kill-ring-search))

;; undo
(use-package undo-fu :config
  ;; Increase undo history limits to reduce likelihood of data loss
  (setq undo-limit 400000           ; 400kb (default is 160kb)
        undo-strong-limit 3000000   ; 3mb   (default is 240kb)
        undo-outer-limit 48000000))  ; 48mb  (default is 24mb)

(use-package undo-fu-session
  :config
  (setq undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'")))

(global-undo-fu-session-mode +1)


;; https://github.com/casouri/vundo
(use-package vundo
  :commands (vundo)
  :config
  ;; Take less on-screen space.
  (setq vundo-compact-display t)
  ;; (setq vundo-glyph-alist vundo-unicode-symbols)
  ;; (setq vundo-glyph-alist vundo-ascii-symbols)
  ;; (set-face-attribute 'vundo-default nil :family "Symbola")

  ;; Better contrasting highlight.
  ;; (custom-set-faces
  ;;  '(vundo-node ((t (:foreground "#808080"))))
  ;;  '(vundo-stem ((t (:foreground "#808080"))))
  ;;  '(vundo-highlight ((t (:foreground "#FFFF00")))))

  ;; Use `HJKL` VIM-like motion, also Home/End to jump around.
  (define-key vundo-mode-map (kbd "l") #'vundo-forward)
  (define-key vundo-mode-map (kbd "<right>") #'vundo-forward)
  (define-key vundo-mode-map (kbd "h") #'vundo-backward)
  (define-key vundo-mode-map (kbd "<left>") #'vundo-backward)
  (define-key vundo-mode-map (kbd "j") #'vundo-next)
  (define-key vundo-mode-map (kbd "<down>") #'vundo-next)
  (define-key vundo-mode-map (kbd "k") #'vundo-previous)
  (define-key vundo-mode-map (kbd "<up>") #'vundo-previous)
  (define-key vundo-mode-map (kbd "<home>") #'vundo-stem-root)
  (define-key vundo-mode-map (kbd "<end>") #'vundo-stem-end)
  (define-key vundo-mode-map (kbd "q") #'vundo-quit)
  (define-key vundo-mode-map (kbd "C-g") #'vundo-quit)
  (define-key vundo-mode-map (kbd "RET") #'vundo-confirm))

(with-eval-after-load 'evil (evil-define-key 'normal 'global (kbd "C-M-u") 'vundo))

;; https://emacs-china.org/t/evil-u-undo/13293
;; (use-package undo-tree
;;   :bind (("s-z" . undo-tree-undo)
;;          ("s-Z" . undo-tree-redo))
;;   :hook (after-init . global-undo-tree-mode)
;;   :init
;;   (setq-default undo-tree-visualizer-timestamps t)
;;   (setq-default undo-tree-visualizer-diff t)
;;   (setq undo-tree-auto-save-history t)
;;   (setq undo-tree-history-directory-alist `(("." . ,(concat user-emacs-directory "backups/undo-tree"))))
;;   (setq undo-tree-enable-undo-in-region nil)
;;   (setq undo-tree-mode-lighter " UT"))

(use-package bookmark
  :defer t
  :init
  (require 'fringe-helper)
  ;; t means save bookmarks when Emacs is killed, 1 save bookmark every time you
  ;; set bookmark, not only when you exit emacs
  (setq bookmark-save-flag 1)

  (general-define-key
   :states 'normal
   :keymaps 'bookmark-bmenu-mode-map
   (kbd "v")   'bookmark-bmenu-select
   (kbd "L")   'bookmark-bmenu-load)
  :config
  (define-fringe-bitmap
    'bookmark-fringe-mark
    (fringe-helper-convert
     "xx.xx..."
     ".xx.xx.."
     "..xx.xx."
     "...xx.xx"
     "..xx.xx."
     ".xx.xx.."
     "xx.xx...")
    nil
    nil
    'center))

;;; uniq lines
(defun tl-uniq-lines (beg end)
  "Unique lines in region.
Called from a program, there are two arguments:
BEG and END (region to sort)."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (while (not (eobp))
        (kill-line 1)
        (yank)
        (let ((next-line (point)))
          (while
              (re-search-forward
               (format "^%s" (car kill-ring)) nil t)
            (replace-match "" nil nil))
          (goto-char next-line))))))


;;; yank
;; (yank-excluded-properties t)        ; do not paste any properties

;; auto indent region after yank and yank-pop
;; Let yank and yank-pop to indent whatever they just pasted. This is useful if, for example, you
;; copy some code from another file at a different indentation level than you want to paste it at.
;; With these advice, the code will be indented properly relative to wherever you paste it.
;; (defadvice yank (after tl/indent-region activate)
;;   "Auto indent after `yank', if evil mode off."
;;   (when (and (not (boundp 'evil-mode))
;;              (not evil-mode)
;;              (member major-mode '(emacs-lisp-mode
;;                                   lisp-interaction-mode lisp-mode
;;                                   c-mode c++-mode objc-mode
;;                                   latex-mode plain-tex-mode)))
;;     (let ((mark-even-if-inactive t))
;;       (indent-region (region-beginning) (region-end) nil))))

;; (defadvice yank-pop (after tl/indent-region activate)
;;   "Auto indent after `yank-pop', if evil mode off."
;;   (when (and (not (boundp 'evil-mode))
;;              (not evil-mode)
;;              (member major-mode '(emacs-lisp-mode
;;                                   lisp-interaction-mode lisp-mode
;;                                   c-mode c++-mode objc-mode
;;                                   latex-mode plain-tex-mode)))
;;     (let ((mark-even-if-inactive t))
;;       (indent-region (region-beginning) (region-end) nil))))


;; Normally, if you kill a newline and the next line is indented, you will have to execute
;; just-one-space or something similar to get rid of all the extra indentation. This will do that
;; automatically for you, saving some time. I just rebind it to C-k, overriding kill-line, since
;; this is almost always the behavior I want.

;; (defun paloryemac/kill-and-join-forward (&optional arg)
;;   "If at end of line, join with following; otherwise kill line.
;;     Deletes whitespace at join."
;;   (interactive "P")
;;   (if (and (eolp) (not (bolp)))
;;       (delete-indentation t)
;;       (kill-line arg)))

;; (global-set-key (kbd "C-k") 'tl/kill-and-join-forward)

;;; toggle-letter-case
;; http://ergoemacs.org/emacs/modernization_upcase-word.html
(defun tl/toggle-letter-case ()
  "Toggle the letter case of current word or text selection.
Toggles between: “ all lower ”, “ Init Caps ”, “ ALL CAPS ”."
  (interactive)
  (let (p1 p2 (deactivate-mark nil) (case-fold-search nil))
    (if (region-active-p)
        (setq p1 (region-beginning) p2 (region-end))
      (let ((bds (bounds-of-thing-at-point 'word) ) )
        (setq p1 (car bds) p2 (cdr bds)) ) )

    (when (not (eq last-command this-command))
      (save-excursion
        (goto-char p1)
        (cond
         ((looking-at "[[:lower:]][[:lower:]]") (put this-command 'state "all lower"))
         ((looking-at "[[:upper:]][[:upper:]]") (put this-command 'state "all caps") )
         ((looking-at "[[:upper:]][[:lower:]]") (put this-command 'state "init caps") )
         ((looking-at "[[:lower:]]") (put this-command 'state "all lower"))
         ((looking-at "[[:upper:]]") (put this-command 'state "all caps") )
         (t (put this-command 'state "all lower")))))

    (cond
     ((string= "all lower" (get this-command 'state))
      (upcase-initials-region p1 p2) (put this-command 'state "init caps"))
     ((string= "init caps" (get this-command 'state))
      (upcase-region p1 p2) (put this-command 'state "all caps"))
     ((string= "all caps" (get this-command 'state))
      (downcase-region p1 p2) (put this-command 'state "all lower")) )))

(global-set-key (kbd "M-c") 'tl/toggle-letter-case)

;;; coding
(defun tl/revert-to-gbk-dos ()
  (interactive)
  (revert-buffer-with-coding-system 'gbk-dos))

;;----------------------------------------------------------------------------------------------------
;;; stumpwm
;;----------------------------------------------------------------------------------------------------

;; (load "~/src/stumpwm/contrib/stumpwm-mode")
;; (setq stumpwm-shell-program "~/bin/stumpish")

;;; with stumpwm
(defun stumpwm-notifications-add (str)
  (interactive "sNotification: ")
  (start-process "notifications-add" nil
                 "stumpish" "notifications-add" str))


;;;;;;;;;;;;
;; stumpwm ends there------------------------------------------------------------------------------------

(provide '50misc)

;; Local Variables: **
;; outline-regexp: ";;; " **
;; byte-compile-warnings: (not noruntime free-vars) **
;; End: **
