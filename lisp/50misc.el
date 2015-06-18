;;; 05misc.el ---
;; Author: Shihpin Tsing <deftsp@gmail.com>

;;; Personal data
(setq user-full-name "Shihpin Tseng"
      user-mail-address "deftsp@gmail.com"
      change-log-default-name "ChangeLog"
      vc-user-login-name "author")

;;; variables
(setq inhibit-startup-message t
      ;; inhibit-default-init t            ; Non-nil inhibits loading the `default' library.
      gnus-inhibit-startup-message t
      initial-scratch-message nil ;; no message in the scratch buff
      inhibit-startup-echo-area-message t
      column-number-mode t
      size-indication-mode t            ; show file size (emacs 22+)
      view-read-only nil
      ;; system-name "xxxxxx"

      ;; pop-up-windows nil ; don't change my windowconfiguration assure my window-configuration is kept
      ;; x-use-underline-position-properties nil
      read-quoted-char-radix 10         ; accept decimal input when using ^q, e.g.: ^q 13 [RET] -> ^M
      ;; echo unfinished commands after this many seconds of pause.
      echo-keystrokes 1                 ; 0.1
      tooltip-hide-delay 20             ; defautl 10
      yank-excluded-properties t        ; do not paste any properties
      mouse-yank-at-point t
      ;; directory-sep-char ?\\
      confirm-kill-emacs nil ; alternative `yes-or-no-p' ; `y-or-n-p'
      use-dialog-box nil                  ; mouse commands don't use dialog boxes to ask quesary "english"
      ;; ps-multibyte-buffer 'non-latin-printer
      display-time-24hr-format t
      display-time-day-and-date t
      display-time-use-mail-icon t
      display-time-interval 10          ; Seconds between updates of time in the mode line
      window-min-height 4               ; Let's not have too-tiny windows.
      ;; undo-no-redo t
      redisplay-dont-pause t
      mark-even-if-inactive t
      set-mark-command-repeat-pop t
      x-select-enable-clipboard t ; cutting and pasting uses the clipboard.
      x-select-enable-primary t
      ;; default-directory "/"
      visible-bell nil                    ;no f* beep on error.
      ;;ftp-program "ncftp"       ; use ncftp for ftp transfers.
      mouse-autoselect-window nil
      ;; sql-electric-stuff (quote semicolon)
      ;; sql-input-ring-file-name "~/.mysql.history"
      ;; sql-password "mysql123"
      ;; sql-pop-to-buffer-after-send-region t
      ;; sql-product (quote mysql)
      ;; sql-user "root"
      highlight-nonselected-windows nil
      develock-auto-enable nil
      ;; remove no-run-time function warnings
      byte-compile-warnings (quote (noruntime))
      Man-notify-method 'pushy ; make the manpage the current buffer in the current window
      ;; means save bookmarks when Emacs is killed, 1 save bookmark every time you set bookmark, not only when you exit
      ;; emacs
      bookmark-save-flag t
      next-screen-context-lines 2
      mail-user-agent 'gnus-user-agent
      ;; default-enable-multibyte-characters t
      enable-local-variables :safe)

;;; display world time
;; For other cities, please refer to this http://en.wikipedia.org/wiki/List_of_tz_database_time_zones
;;  M-x display-time-world or helm-world-time
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
(defun pl/comint-clear-buffer ()
  (interactive)
  (let ((comint-buffer-maximum-size 0))
    (comint-truncate-buffer)))

(define-key comint-mode-map "\C-c\M-o" #'pl/comint-clear-buffer)


;;; minibuffer
(setq minibuffer-electric-default-mode 1
      resize-mini-windows t
      enable-recursive-minibuffers t)
(minibuffer-depth-indicate-mode 1)
;; don't let the cursor go into minibuffer prompt
;; http://ergoemacs.org/emacs/emacs_stop_cursor_enter_prompt.html
(setq minibuffer-prompt-properties
      '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt))

(setq ring-bell-function 'ignore)
;; Emacs does not beep when you hit `C-g' in the minibuffer or during
;; an `isearch' (http://www.emacswiki.org/cgi-bin/wiki.pl?AlarmBell)
;; (setq ring-bell-function
;;       (lambda ()
;;         (unless (memq this-command
;;                       '(isearch-abort abort-recursive-edit find-file
;;                         exit-minibuffer keyboard-quit))
;;           (ding))))

;;;
;; (when (eq system-type 'darwin)
;;   ;; (setq find-function-C-source-directory "")
;;   (setq source-directory "/Library/Caches/Homebrew/emacs--git"))

;;; Move to trash when deleting stuff
(when (eq system-type 'darwin)
  (setq delete-by-moving-to-trash t
        trash-directory "~/.Trash/__emacs_trash"))


;;; which func mode
(which-function-mode +1)
(setq which-func-modes t) ; enabled in any major mode that supports it.
;; setq-default header-line-format will cause hydra message window can only show one line
;; (setq-default header-line-format '((which-func-mode ("" which-func-format " "))))
(add-hook 'prog-mode-hook
          (lambda ()
            (setq header-line-format
                  '((which-func-mode ("" which-func-format " "))))))
(setq which-func-unknown "⊤") ; "n/a"
;; We remove Which Function Mode from the mode line, because it's mostly invisible here anyway.
;; (setq mode-line-misc-info (assq-delete-all 'which-func-mode mode-line-misc-info))

(if (eq system-type 'gnu/linux)
    (display-battery-mode t))

;; do not create new frame with `open' in Mac OS X
(when (memq window-system '(mac ns))
  (setq ns-pop-up-frames nil))

(setq-default indicate-buffer-boundaries 'left)
;; If you often look at files that are updated (perhaps a log file for a running process), or perhaps ClearCase files
;; (that change when you update the config spec), you'll want to make sure you're looking at the most recent version of
;; the file.
(global-auto-revert-mode 1)
;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
;; (setq auto-revert-verbose nil)

(eval-after-load "mailcap"
  '(mailcap-parse-mailcaps "~/.mailcap" t))


;;; Run 'save-buffers-kill-emacs' without process-killing query
(eval-after-load "cl"
  '(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
     "Prevent annoying \"Active processes exist\" query when you quit Emacs."
     (flet ((process-list () nil)) ad-do-it)))

;; (modify-syntax-entry ?_ "w")            ; now '_' is not considered a word-delimiter
;; (modify-syntax-entry ?- "w")            ; now '-' is not considered a word-delimiter
;; (modify-syntax-entry ?_ "_")
;; (modify-syntax-entry ?' "." text-mode-syntax-table)

;; (quail-set-keyboard-layout "pc102-de")


;; (defun save-buffer-if-visiting-file (&optional args)
;;   "Save the current buffer only if it is visiting a file"
;;   (interactive)
;;   (if (buffer-file-name)
;;       (save-buffer args)))

;;; turncate
;; (set-default 'truncate-lines t) ; This is a BufferLocalVariable don't recommend.
(setq truncate-partial-width-windows t) ;; truncate lines in all windows less than full frame wide.

(global-set-key (kbd "C-c T") 'pl/toggle-truncate-partial-width-windows)
(defun pl/toggle-truncate-partial-width-windows ()
  (interactive)
  (set-default 'truncate-partial-width-windows
               (not truncate-partial-width-windows))
  (message "truncate-partial-width-windows: %S"
           truncate-partial-width-windows))

;; Active Region Highlighting
(transient-mark-mode t)

;;; display-time-string-forms
(display-time-mode 1)

(defface pl/display-time-face '((((type x w32 mac))
                                 ;; #060525
                                 (:foreground "lawn green" :inherit bold))
                                (((type tty))
                                 (:foreground "blue")))
  "Face used to display the time in the mode line.")

;; Name of mail inbox directory
;; check that the file specified by `display-time-mail-file' is nonempty or that the
;; directory `display-time-mail-directory' contains nonempty files.

(if (eq system-type 'gnu/linux)
    (setq display-time-mail-directory "~/Mail/inbox/new/"))

(setq display-time-string-forms
      '((format-time-string "%Y/%m/%d " now)
        (propertize (concat " " 24-hours ":" minutes)
                    'face 'pl/display-time-face)
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

;;; physical line move
;; (autoload 'physical-line-mode "physical-line" "" t)
;; (add-hook 'find-file-hooks 'physical-line-mode )



;; Escape character before non-breaking space (C-q 240) and non-breaking hyphen (C-q 255).
;; (if (boundp 'show-nonbreak-escape) (setq show-nonbreak-escape t))

;; Conventional mouse/arrow movement & selection
;; (pc-selection-mode)

;; When Delete Selection mode is enabled, Transient Mark mode is alsoenabled and typed text replaces the selection if
;; the selection is active.
;; (pending-delete-mode t)
;; pending-delete-mode is an alias for `delete-selection-mode'
;; (delete-selection-mode t)

(fset 'yes-or-no-p 'y-or-n-p)
;;(setq internal-border-width 0)

;; Tell Emacs to obey variables set by the files it reads
;; (setq enable-local-eval t)
;; resize man page to take up whole screen
;; (setq Man-notify-method 'bully)
;;click on URLs in manual pages
(add-hook 'Man-mode-hook 'goto-address)

(setq compilation-window-height 16      ; Some windows's height
      compilation-ask-about-save nil
      ;; compilation-context-lines 3
      ;; compilation-skip-threshold 1
      ;; compilation-skip-visited t
      ;; keep scrolling in compilation result buffer
      compilation-scroll-output t)

(dolist (str '("*Help*" "*Completions*"))
  (add-to-list 'completion-ignored-extensions str))



;; (setq next-error-highlight 3
;;       next-error-highlight-no-select t)

;; compilation-mode has a nice feature so that you can skip over your info and warning level messages and jump right to
;; those nasty errors. Set the variable compilation-skip-threshold to 2 so that M-n and M-p will jump to the next or
;; previous error respectively. The other possible settings for this variable are:

;; 2 — skip anything less than error
;; 1 — skip anything less than warning, or
;; 0 — don’t skip any messages.

;; get intermittent messages to stop typing
;; (type-break-mode)
;; follow-mode allows easier editing of long files
;; (follow-mode t)


;; (mouse-avoidance-mode 'animate) ;; disable it until fix the bug in `org-capture' buffer 'C-x C-q' error
(setq auto-image-file-mode nil)
;;Automatically uncompress .gz files
(auto-compression-mode 1)

;; kill customize buffers on exit
(setq custom-buffer-done-kill t)

;; set unicode data file location. (used by what-cursor-position)
;; (let ((x "~/.emacs.d/UnicodeData.txt"))
;;   (when (file-exists-p x)
;;     (setq describe-char-unicodedata-file x)))

;;; crosshair
;; dependencies hl-line+ col-highlight vline
;; (global-set-key [(control ?+)] 'crosshairs)
(global-set-key [(control ?+)] 'crosshairs-mode)
;; (global-set-key [(control ?+)] 'crosshairs-flash)



(eval-after-load "man" '(require 'man-completion))
;; (defalias 'man 'woman)
(setq woman-show-log nil)
(setq woman-cache-filename "~/.emacs.d/.wmncach.el")
(setq woman-use-own-frame nil)
(setq woman-fill-column 100)
;; Not to lookup POSFIX man page
;; `woman-manpath-man-regexp' should be set before requrie woman.el. So the it should be changed

(eval-after-load "woman"
  '(add-to-list 'woman-manpath "/usr/local/share/man"))
;; before require anything.
(setq woman-manpath-man-regexp "[Mm][Aa][Nn][1-9][^p]*$")         ; "[Mm][Aa][Nn]"

(defun woman-word-at-point ()
  (interactive)
  (let ((woman-use-topic-at-point t))
    (woman)))

;;; Garbage Collection
;; This sets garbage collection to only occur when 6 megabytes are used.
;; Supposedly significantly speeds up startup time. (Seems to work
;; for me,  but my computer is pretty modern. Disable if you are on
;; anything less than 1GHZ).
(setq gc-cons-threshold (max 20000000 gc-cons-threshold)) ; 20MB

;; want to see how often GC happens
;; (setq garbage-collection-messages nil)

;;; Initial register values
;; `C-x r j i' to open init.el,
(mapc #'(lambda (r)
          (let ((file-path (cdr r)))
            (when (file-exists-p file-path)
              (set-register (car r) (cons 'file file-path)))))
      '((?i . "~/.emacs.d/init.el")
        (?g . "~/org/GTD.org")))

;;; file-name-shadow-mode
;; be smart about filenames understand ~/ etc
(when (fboundp 'file-name-shadow-mode)
  (file-name-shadow-mode t))

;;; paren
;;----------------------------------------------------------------------------------------------------
;; use mic-paren instead
;; (show-paren-mode t)
;; (setq show-paren-delay 0
;;       show-paren-style 'parenthesis)    ;expression

;;; mic-paren
(require 'mic-paren nil t)

(if (fboundp 'paren-activate)
    (paren-activate))

(eval-after-load "mic-paren"
  '(progn
     (setq paren-sexp-mode 'mismatch)))


;; rainbow-delimiters
(defun pl/turn-on-rainbow-delimiters-mode ()
  (rainbow-delimiters-mode +1))

(when (fboundp 'rainbow-delimiters-mode)
  (add-hook 'prog-mode-hook 'pl/turn-on-rainbow-delimiters-mode))

(eval-after-load "cc-mode"
  '(define-key c-mode-base-map (kbd "%") 'pl/goto-match-paren))

(defun pl/goto-match-paren (arg)
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

(global-set-key (kbd "C-x %") 'pl/kill-outside-paren-with-elt)
(defun pl/kill-outside-paren-with-elt (arg)
  (interactive "p")
  (if (not (looking-at "[([{]"))
      (up-list -1))
  (copy-region-as-kill (point) (scan-sexps (point) 1))
  (up-list -1)
  (kill-sexp 1)
  (yank 2))

;;; mouse
(when window-system
  (mouse-wheel-mode t)
  ;; scroll one line at a time (less "jumpy" than defaults).
  ;; It can also be a floating point number, specifying the fraction of a full screen to scroll.
  (setq mouse-wheel-scroll-amount '(1  ; one line at a time
                                    ((shift) . 1)
                                    ((control))))
  (setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
  ;; scroll window under mouse
  (setq mouse-wheel-follow-mouse t))

;; (global-set-key (kbd "<mouse-1>") 'mouse-set-point)
;; (global-set-key (kbd "<down-mouse-1>") 'mouse-drag-region)
;; (global-set-key (kbd "<C-down-mouse-1>") 'mouse-buffer-menu)

;; (global-set-key [M-down-mouse-1] 'mouse-drag-secondary-pasting)
;; (global-set-key [M-S-down-mouse-1] 'mouse-drag-secondary-moving)
(global-set-key (kbd "<H-mouse-1>") 'browse-url-at-mouse)

;; mouse button one drags the scroll bar
(global-set-key [vertical-scroll-bar down-mouse-1] 'scroll-bar-drag)

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

;;; scroll
(setq scroll-step 1                     ; default 0, keyboard scroll one line at a time
      scroll-margin 3
      scroll-conservatively 10000)     ; Fix the whole huge-jumps-scrolling-between-windows nastiness
(setq next-screen-context-lines 3)  ; how many from page up/down
(setq scroll-preserve-screen-position nil) ; notice: set this to `t' will casue scrool jump badly
(setq hscroll-margin 1)

;;; Make cursor stay in the same column when scrolling using pgup/dn.
;; Previously pgup/dn clobbers column position, moving it to the beginning of the line.
;; <http://www.dotemacs.de/dotfiles/ElijahDaniel.emacs.html>
;; (defadvice scroll-up (around ewd-scroll-up first act)
;;   "Keep cursor in the same column."
;;   (let ((col (current-column)))
;;     ad-do-it
;;     (move-to-column col)))

;; (defadvice scroll-down (around ewd-scroll-down first act)
;;   "Keep cursor in the same column."
;;   (let ((col (current-column)))
;;     ad-do-it
;;     (move-to-column col)))

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
;; ------- scroll end here -------------------------------------------------------------

;; Might as well limit how many messages fill up in the message buffer.
;; Not that we'll ever get that many, but just in case!
(setq message-log-max 3000)

;; "With argument t, set the random number seed from the current time and pid."
(random t)

(setq cursor-in-non-selected-windows t)

;; kills an entire line if the cursor is at the beginning of line. And try M-0 C-k.
(setq-default kill-whole-line t)
(setq kill-ring-max 200) ; default 60

(setq apropos-do-all t) ; search more extensively
(setq apropos-sort-by-scores t) ; sort matches by scores; best match is shown first.
;;------------------------------------------------------------------------------------------

;;; Spell check
;;Don't use Ispell, but the more modern Aspell
(setq-default ispell-program-name "aspell")

;;;Save new words in pdict without questioning
(setq ispell-silently-savep t)

;;Add good shortcut for flyspell. The hook makes sure when flyspell-mode is on, the buffer gets scanned.
;; (defun flyspell nil
;;   "Do the expected default, which is run flyspell on the whole buffer."
;;   (interactive)
;;   (flyspell-buffer))
;; (add-hook 'flyspell-mode-hook 'flyspell-buffer)

;; After we add a word to ispell or correct something, flyspell's highlighting may become outdated. Let's re-run
;; highlighting after a correction.
;; (defadvice ispell (after advice)
;;   (flyspell-buffer))
;; (ad-activate 'ispell t)
;; (defadvice ispell-word (after advice)
;;   (flyspell-buffer))
;; (ad-activate 'ispell-word t)

;; Programming language modes use flyspell-prog-mode and not normal spell-check.
;; (add-hook 'haskell-mode-xhook 'flyspell-prog-mode)
;; (add-hook 'emacs-lisp-mode-hook (lambda () (flyspell-prog-mode)))
;; (add-hook 'lisp-mode-hook (lambda () (flyspell-prog-mode)))
;; (add-hook 'shell-mode-hook (lambda () (flyspell-prog-mode)))


;;--------------------------------------------------------------------------------------------
;; Use cperl-mode instead of perl-mode
;;(defalias 'perl-mode 'cperl-mode)

;;; whitespace mode
;; If you don't like having lines of code/text with whitespace at the ends, Emacs highlight the offending whitespace.
;; When set, the variable's value becomes buffer local, so set it to true in the mode-hooks for your preferred modes.
;; Or, if you want it on all the time, change the default value with:

;; (if (>= emacs-major-version 21)
;;     (setq-default show-trailing-whitespace t))
;; make whitespace-mode use just basic coloring
(setq whitespace-style (quote (face spaces lines-tail tabs trailing newline space-mark tab-mark newline-mark)))
(setq whitespace-line-column nil) ; use `fill-column' variable value
(setq whitespace-display-mappings
      ;; all numbers are unicode codepoint in decimal. e.g. (insert-char 182 1)
      '((space-mark 32 [183] [46])     ; 32   SPACE 「 」, 183 MIDDLE DOT 「·」, 46 FULL STOP 「.」
        (space-mark 160 [164] [95])    ; 160  NO-BREAK SPACE 「 」, 164 MIDDLE DOT 「¤」, 95 FULL STOP 「_」
        (newline-mark 10 [182 10])     ; 10   LINE FEED, 182 PILCROW SIGN 「¶」
        (tab-mark 9 [9655 9] [92 9])   ; 9 TAB, 9655 WHITE RIGHT-POINTING TRIANGLE 「▷」
        ))

;; auto delete trailing whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)


(setq line-number-display-limit 10000000)

;;; set frame title
(setq frame-title-format
      (list "GNU Emacs "
            emacs-version
            "@" system-name ": "
            '(:eval
              (if buffer-file-name
                  (replace-regexp-in-string
                   (getenv "HOME") "~"
                   (file-name-directory buffer-file-name))
                (buffer-name)))))
(setq icon-title-format frame-title-format)

;; M-x apropos  apropos works better but slower
(setq apropos-do-all t)

;; this gives matlab access to the X11 windowing system, so I can see figures, etc.
;; (setenv "DISPLAY" ":0.0")

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
;; C-x < 和 C-x >
(put 'scroll-left 'disabled nil)
(put 'rmail 'disabled t)

;;; line spacing
;; (setq line-spacing 5)
;; (defun toggle-line-spacing ()
;;   "Toggle line spacing between 1 and 5 pixels."
;;   (interactive)
;;   (if (eq line-spacing 1)
;;       (setq-default line-spacing 5)
;;       (setq-default line-spacing 1)))

;;; fvwm
(setq fvwm-fvwmcommand-path "/usr/bin/FvwmCommand")
;; before modify it, use C-h v auto-mode-alist to check it.
(autoload 'fvwm-mode "fvwm-mode" "Major mode for editing FVWM configuration files." t)

;; (add-hook 'fvwm-mode-hook '(lambda () (fvwm-enable-indentation)))
(autoload 'xrdb-mode "xrdb-mode" "Mode for editing X resource files" t)

;;; auto mode alist
(mapcar #'(lambda (lst) (add-to-list 'auto-mode-alist lst))
        '(("\\.js$"                             . js2-mode)
          ("\\.julius$"                         . js2-mode)
          ("\\.\\(xml\\|rdf\\)\\'"              . sgml-mode)
          ("\\.css\\'"                          . css-mode)
          ("\\.\\(emacs\\|session\\|gnus\\)\\'" . emacs-lisp-mode)
          ("\\.\\(jl\\|sawfishrc\\)\\'"         . sawfish-mode)
          ("\\.scm\\'"                          . scheme-mode)
          ("\\.py\\'"                           . python-mode)
          ("\\.\\(ba\\)?sh\\'"                  . sh-mode)
          ("\\.l\\'"                            . c-mode)
          ("\\.mm\\'"                           . objc-mode)
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
          ("\\.markdown$"                       . markdown-mode)
          ("\\.asciidoc$"                       . adoc-mode)
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
          ("\\.lrc$"                            . emms-lyrics-mode)))

;;; Since emacs 22 we can use magic-mode-alist to set mode
(add-to-list 'magic-mode-alist '("\\(.\\|\n\\)*\n@implementation" . objc-mode))
(add-to-list 'magic-mode-alist '("\\(.\\|\n\\)*\n@interface" . objc-mode))
(add-to-list 'magic-mode-alist '("\\(.\\|\n\\)*\n@protocol" . objc-mode))



;;; insert DATE and some usefull things
(global-set-key (kbd "C-c i d") 'insert-date-at-current-point)

(defun insert-date-at-current-point (prefix)
  "Insert the current date. With prefix-argument, use ISO format. With
   two prefix arguments, write out the day and month name."
  (interactive "P")
  (let ((format (cond
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

(defvar pl/iso-date-format "%Y-%m-%dT%H:%M:%S%z"
  "Format string for ISO dates.")

(defun pl/iso-timestamp (&optional time)
  (format-time-string iso-date-format
                      (or time (current-time))))
(defun pl/insert-iso-timestamp ()
  (interactive)
  (insert (iso-timestamp)))

(defun pl/iso-timestamp-sexp (&optional time)
  (parse-time-string (iso-timestamp)))

;;----------------------------------------------------------------------------------------------------



;; (defun select-until-end-of-line ()
;;   "Select until the end of a line without killing it."
;;   (interactive)
;;   (copy-region-as-kill (point) (line-end-position)) )

(defun pl/insert-userid ()
  "Insert the users email address"
  (interactive)
  (insert user-full-name " <" user-mail-address ">" ) )

;; (defun recenter-to-first-line ()
;;   "Recenter to the first screenline."
;;   (interactive)
;;   (recenter "1"))
;; (global-set-key  "\C-cl"      'recenter-to-first-line)

(defun pl/point-to-register()
  "Store cursorposition _fast_ in a register.
Use pl/jump-to-register to jump back to the stored
position."
  (interactive)
  (point-to-register 6)
  (message "point-to-register 6"))

(defun pl/jump-to-register()
  "Switches between current cursorposition and position
that was stored with pl/point-to-register."
  (interactive)
  (let ((tmp (point-marker)))
    (jump-to-register 6)
    (set-register 6 tmp)))
(global-set-key (kbd "C-c 6") 'pl/point-to-register)
(global-set-key (kbd "C-c ^") 'pl/jump-to-register)
;;===========================================================

;;=============================================
(setq time-stamp-active t
      time-stamp-warn-inactive t
      time-stamp-line-limit 8           ;; check the first 10 lines of the file for Time-stamp: <>
      time-stamp-format (concat "%:y-%02m-%02d %02H:%02M:%02S " user-full-name))
(add-hook 'write-file-hooks 'time-stamp)
(add-hook 'before-save-hook 'time-stamp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun pl/insert-file-variable ()
  "Insert file variable string \"-*- Major-Mode-Name -*-\" with
  comment char"
  (interactive)
  (insert
   (concat comment-start " -*- "
           (substring
            (symbol-name (symbol-value 'major-mode)) 0 -5)
           " -*- " comment-end)))

;; cscope
;; 使用 cscope 浏览源代码,这个 xcscope 是个改进版，为每一次查找的结果使用不同 buffer ，
;; 这样就可以保存以前的结果。

(when (eq system-type 'gnu/linux)
  (require 'xcscope)
  (defadvice cscope-bury-buffer (after bury-buffer activate)
    "bury the current buffer and remove it from the selected window
if it is displayed there."
    (delete-window (get-buffer-window (get-buffer cscope-output-buffer-name)))))


;; C-c s a             设定初始化的目录，一般是你代码的根目录
;; C-s s I             对目录中的相关文件建立列表并进行索引
;; C-c s s             序找符号
;; C-c s g             寻找全局的定义
;; C-c s c             看看指定函数被哪些函数所调用
;; C-c s C             看看指定函数调用了哪些函数
;; C-c s e             寻找正则表达式
;; C-c s f             寻找文件
;; C-c s i             看看指定的文件被哪些文件include

;;; charset-to-oem
(defun pl/replace-charset-to-oem (start end)
  (interactive "r")
  (save-excursion
    (format-replace-strings '(("’"   .   "'")
                              ("′"   .   "'")
                              ("‘"   .   "'")
                              ("…"   .   "...")
                              ("“"   .   "\"")
                              ("”"   .   "\"")
                              ("（"  .   "(")
                              ("）"  .   ")")
                              ("："  .   ":")
                              ("•"   .   "ù")
                              ("–"   .   "-")
                              ("—"   .   "--")
                              ("ü"   .   "")
                              ("é"   .   "‚")
                              ("§"   .   "")
                              ("®"   .   "(R)")
                              ("·"   .   "*")
                              ("，"  .   ",")
                              ("×"   .   "x"))
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
(global-set-key (kbd "M-w") 'pl/kill-ring-save-dwim)

(defun pl/kill-ring-save-dwim ()
  "This command dwim on saving text. \n
If region is active, call `kill-ring-save'. Else, call
`sdl-kill-ring-save-thing-at-point'. \n
This command is to be used interactively."
  (interactive)
  (if (use-region-p)
      (call-interactively 'kill-ring-save)
      (call-interactively 'pl/kill-ring-save-thing-at-point)))



;; M-w if thing at point is url or mail address copy it, or copy current line
;; After press M-w, if press following key...
;; key:  copy
;; w     word
;; l     list
;; s     sexp
;; f     file name
;; and it can accpet prefix, M-3 M-w will copy three lines and M-3 M-w w will copy three words.
(defun pl/kill-ring-save-thing-at-point (&optional n)
  "Save THING at point to kill-ring."
  (interactive "p")
  (let ((things '((?l . list) (?f . filename) (?w . word) (?s . sexp)))
        (message-log-max)
        beg t-a-p thing event)
    (flet ((get-thing ()
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
          (return nil)))
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
(defun pl/rename-file-and-buffer (new-name)
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

(global-set-key (kbd "C-c f r")  'pl/rename-file-and-buffer)

(defun pl/move-buffer-and-file (dir)
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

(global-set-key (kbd "C-c f R") 'pl/move-buffer-and-file)

;;; info
;; make sure info dir in `Info-directory-list' add to `Info-directory-list'
(mapc (lambda (p) (add-to-list 'Info-directory-list p t)) Info-default-directory-list)
(add-to-list 'Info-additional-directory-list "~/share/info")

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



;;----------------------------------------------------------------------------------------------------
;;;; bondage and discipline
;;----------------------------------------------------------------------------------------------------
;; (defvar punishment-counter 0)

;; (defvar punishment-insults '("YOU SUCK!"
;;                              "OBEY ME, INSECT!"
;;                              "OBEY ME, SUBSERVIENT BIOMASS!"
;;                              "ENGAGE IN COPROPHILIA AND THEN EXPIRE!"
;;                              "YOU ARE NOT WORTHY OF EMACS!"))

;; (defface punishment-face '((t (:foreground "black" :background "yellow" :bold t)))
;;   "Face for punishment messages.")

;; (defun punish-me ()
;;   (interactive)
;;   (message "%s" (propertize (nth (random (length punishment-insults))
;;                                  punishment-insults)
;;                             'face 'punishment-face)))

;; (global-set-key (kbd "<up>") 'punish-me)
;; (global-set-key (kbd "<down>") 'punish-me)
;; (global-set-key (kbd "<left>") 'punish-me)
;; (global-set-key (kbd "<right>") 'punish-me)

;; This toggles on/off the menu and tool bars for more editing area.
;; (defvar stark-frame ())
;; (defun stark-frame () "Toggle toobar & menu bar on/off."
;;        (interactive)
;;        (setq stark-frame (not stark-frame))
;;        (tool-bar-mode (if stark-frame 0 1))
;;        (menu-bar-mode (if stark-frame 0 1))
;;        (if stark-frame
;;            (set-frame-size (selected-frame)(frame-width)(+ (frame-height) 1))
;;            (set-frame-size (selected-frame)(frame-width)(- (frame-height) 1))))

;; (global-set-key (kbd "<S-f10>") 'stark-frame)
;;-----------------------------------------------------------------------------------------------------------


;;;###autoload
(defun pl/insert-char-next-line (arg)
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
(defun pl/insert-char-prior-line (arg)
  "insert char above the cursor"
  (interactive "p")
  (pl/insert-char-next-line (- arg)))

;;----------------------------------------------------------------------------------------------------
;; (add-hook 'find-file-hooks
;;           (lambda ()
;;             (if (or (memq major-mode '(change-log-mode))
;;                     (string-match "^makefile.*mode$" (prin1-to-string major-mode)))
;;                 (setq indent-tabs-mode t)
;;                 (setq indent-tabs-mode nil))
;;             ))


;;----------------------------------------------------------------------------------------------------
;; a wonderfully weird feature
;;----------------------------------------------------------------------------------------------------
;; (defun time-after-seconds (seconds)
;;   "returns an internal emacs time that is 'seconds' seconds into the
;;   future."
;;   (seconds-to-time (+ seconds (time-to-seconds (current-time)))))

;; (defun timer-create-callfunc-after-seconds (func seconds)
;;   "run 'func' after 'seconds' seconds."
;;   (let ((timer (timer-create)))
;;     (timer-set-function timer func)
;;     (timer-set-time timer (time-after-seconds seconds))
;;     (timer-activate timer)))

;; (defun callfunc-random-interval (func interval-from-seconds
;; interval-to-seconds)
;;   "run 'func' with a random interval between 'interval-from-seconds'
;;   and
;; 'interval-to-seconds' seconds in a infinite loop."
;;   (funcall func)
;;     (timer-create-callfunc-after-seconds
;;      (list 'lambda '()
;;            (list 'funcall ''callfunc-random-interval
;;                  (list 'quote func) interval-from-seconds
;;                  interval-to-seconds))
;;      (+ interval-from-seconds (random interval-to-seconds))))

;; ;; (callfunc-random-interval (lambda () (insert " foo")) 2 3)

;; and how to make it work like you want it to:

;; (callfunc-random-interval 'color-theme-random (* 60 5) (* 60 60))

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

;; (defun pl/shell-command ()
;;   "Launch a shell command."
;;   (interactive)
;;   (let ((command (read-string "Command: ")))
;;     (shell-command (concat command " &") (concat "*" command "*"))))


;; check for unsaved changes for killing
;; (defun pl/context-kill (arg)
;;   "Kill buffer, taking gnuclient into account."
;;   (interactive "p")
;;   (when (eq major-mode 'erc-mode)
;;     (erc-save-buffer-in-logs))
;;   (when (and (buffer-modified-p)
;;            buffer-file-name
;;            ;; erc buffers will be automatically saved
;;            (not (eq major-mode 'erc-mode))
;;            (= 1 arg))
;;     ;; show diff and error
;;     (when (file-exists-p buffer-file-name)
;;       (diff-buffer-with-file))
;;     (error "Buffer has unsaved changes"))
;;   (if (and (boundp 'gnuserv-minor-mode)
;;          gnuserv-minor-mode)
;;       (gnuserv-edit)
;;       (set-buffer-modified-p nil)
;;       (kill-buffer (current-buffer))))

;; tidy up diffs when closing the file
;; (defun kill-associated-diff-buf ()
;;   (let ((buf (get-buffer "*diff*")))
;;     (when (and (bufferp buf) (not (equal buf (current-buffer))))
;;       (kill-buffer buf))))

;; (add-hook 'kill-buffer-hook 'kill-associated-diff-buf)
;; (global-set-key (kbd "C-x k") 'as/context-kill)

;;; Gnus
(setq gnus-init-file "~/.emacs.d/.gnus.el")
;; (load "~/.emacs.d/.gnus.el") ;; I use C-x m to send mail, load gnus config file
;; (setq gnus-startup-file "~/.emacs.d/.newsrc")
;; gnus默认采用rfc2231对附件文件名进行编码，有些MUA无法识别这种编码。
;; 现在比较流行的方式是采用base64对附件文件名进行编码。可以采用如下设定，让gnus也采用base64编码文件名：

;; 如果你是脱离Gnus发Mail(如C-x m)一定要放在 ~/.emacs而不是 ~/.gnus.el
(defalias 'mail-header-encode-parameter 'rfc2047-encode-parameter)

(eval-after-load "rfc2047"
  '(progn
    (add-to-list 'rfc2047-charset-encoding-alist '(gbk . B))
    (add-to-list 'rfc2047-charset-encoding-alist '(gb18030 . B))))



;; (defun always-scroll-to-bottom (window display-start)
;;   (if (and window (window-live-p window))
;;       (save-selected-window
;;         (select-window window)
;;         (save-restriction
;;           (widen)
;;           (save-excursion
;;             (if (eobp)
;;                 (recenter -1)))))))

;; seeems a little bit dangerous in emacs 20.4
;; (add-hook 'window-scroll-functions 'always-scroll-to-bottom)


;; (defadvice copy-from-above-command
;;     (after xsteve-extend-copy-from-above-command activate compile)
;;   "insert a newline after copying"
;;   (open-line 1))

;; (defadvice copy-from-above-command
;;   (around xsteve-extend-copy-from-above-command activate compile)
;;   "insert a newline after copying"
;;   (let ((insert-new-line (not current-prefix-arg))
;;         (col (current-column)))
;;     ad-do-it
;;     (save-excursion
;;       (back-to-indentation)
;;       (when (> col (current-column))
;;         (setq insert-new-line nil)))
;;     (when insert-new-line
;;       (open-line 1))))


;;; browse-kill-ring
;; bind to key-chord "YY"
(when (require 'browse-kill-ring "browse-kill-ring" t)
  (browse-kill-ring-default-keybindings)
  (setq browse-kill-ring-highlight-current-entry t)
  (setq browse-kill-ring-quit-action 'save-and-restore)  ; 'bury-and-delete-window
  (define-key browse-kill-ring-mode-map [down] 'browse-kill-ring-forward)
  (define-key browse-kill-ring-mode-map [up] 'browse-kill-ring-previous))


;;; kill-ring-search

(autoload 'kill-ring-search "kill-ring-search"
  "Search the kill ring in the minibuffer."
  (interactive))
(global-set-key "\M-\C-y" 'kill-ring-search)


;;; undo-tree
(global-undo-tree-mode t)
(eval-after-load "undo-tree"
  '(progn
    (setq-default undo-tree-visualizer-timestamps t)
    (setq-default undo-tree-visualizer-diff t)
    (setq undo-tree-mode-lighter " UT")))

;;; Table of contents - toc
;; (require 'toc "toc" t)
;; set this mode/file specifically
;; (make-variable-buffer-local 'toc-regexp)
;; (setq toc-regexp ";;\\*\\* *");  ;; that is ';;**' with optional spaces
;; (define-key global-map "\C-c-" 'toc)
;; (define-key occur-mode-map " " 'occur-mode-show-occurrence)

;;; Clipper
;; (require 'clipper "clipper" t)
;; (global-set-key [(control super insert)] 'clipper-create)
;; (global-set-key [(super insert)] 'clipper-insert)
;; (global-set-key [(super delete)] 'clipper-delete)

;;; default contents
;; (when (require 'defaultcontent nil t)
;;   (setq dc-auto-insert-directory "~/.emacs.d/defaultcontent/")
;;   (setq dc-auto-insert-alist
;;         '(("\\.py$"         . "python-template.py")
;;           ("\\.tex$"          ("scrartcl-template.tex" "xsem-template.tex"))
;;           ("\\.vhdl$"         (dc-fill-initial-content-of-file)))))

;;; Mouse embrace minor mode
;; (when (require 'mouse-embrace "mouse-embrace" t)
;;   (define-key mouse-embrace-mode-map [(S-down-mouse-1)] 'mouse-embrace-choose-text))


;;; Blank mode
;; (autoload 'blank-mode "blank-mode" "Toggle blank visualization." t)

;;; tmmoffl
;; (when (require 'tmmofl nil t)
;;   (setq tmmofl-C++-actions
;;         '((font-lock-comment-face
;;            (lambda()
;;              (progn
;;                (let((flyspell-issue-welcome-flag nil))
;;                  (auto-fill-mode 1)
;;                  (abbrev-mode 1)
;;                  (flyspell-mode 1))))
;;            (lambda()
;;              (progn
;;                (auto-fill-mode 0)
;;                (abbrev-mode 0)
;;                (flyspell-mode 0))))))

;;   (setq tmmofl-Python-actions
;;         '((font-lock-comment-face
;;            (lambda() (progn (auto-fill-mode 1)))
;;            (lambda() (progn (auto-fill-mode 0))))
;;           (font-lock-string-face
;;            (lambda() (progn (auto-fill-mode 1)))
;;            (lambda() (progn (auto-fill-mode 0)))))))

;; (add-site-lisp-load-path "mmm-mode/")
;; (when (require 'mmm-auto nil t)
;;   (setq mmm-global-mode 'maybe)

;;example use of the universal mmm mode
;;{%vhdl%} signal a : in bit; {%/vhdl%}

;; (mmm-add-classes
;;  '((embedded-vhdl
;;     :submode vhdl-mode
;;     :front "begin{vhdlcode}.*$"
;;     :back "\\end{vhdlcode}")
;;    (embedded-python
;;     :submode python-mode
;;     :front "begin{pythoncode}.*$"
;;     :back "\\end{pythoncode}")
;;    (embedded-c
;;     :submode c-mode
;;     :front "begin{ccode}.*$"
;;     :back "\\end{ccode}")))
;; (mmm-add-mode-ext-class 'latex-mode "\.tex$" 'embedded-vhdl)
;; (mmm-add-mode-ext-class 'latex-mode "\.tex$" 'embedded-python)
;;(mmm-add-mode-ext-class 'latex-mode "\.tex$" 'embedded-c) ;does not work well

;;mmm-mode example:
;;(mmm-add-classes
;; '((embedded-sql
;;    :submode sql-mode
;;    :front "EXEC SQL"
;;    :back ";")))
;;(setq-default mmm-global-mode t)
;;(mmm-add-mode-ext-class 'c-mode "\.pc$" 'embedded-sql)
;;(mmm-add-mode-ext-class 'c-mode "\.sqc$" 'embedded-sql)
;;(setq-default mmm-never-modes
;;              (append '(ediff-mode) '(text-mode) mmm-never-modes))
;;)

;;; ChangeLog
;; (add-hook 'find-file-hooks
;;           (lambda ()
;;             (if (and (buffer-file-name)
;;                    (string-match "emacs-unicode" (buffer-file-name)))
;;                 (set (make-local-variable 'change-log-default-name)
;;                      "ChangeLog.22"))))

;; (defun uniq-lines (beg end)
;;   "Unique lines in region.
;; Called from a program, there are two arguments:
;; BEG and END (region to sort)."
;;   (interactive "r")
;;   (save-excursion
;;     (save-restriction
;;       (narrow-to-region beg end)
;;       (goto-char (point-min))
;;       (while (not (eobp))
;;         (kill-line 1)
;;         (yank)
;;         (let ((next-line (point)))
;;           (while
;;               (re-search-forward
;;                (format "^%s" (car kill-ring)) nil t)
;;             (replace-match "" nil nil))
;;           (goto-char next-line))))))



;;; auto indent region after yank and yank-pop
;; Let yank and yank-pop to indent whatever they just pasted. This is useful if, for example, you
;; copy some code from another file at a different indentation level than you want to paste it at.
;; With these advice, the code will be indented properly relative to wherever you paste it.
(defadvice yank (after indent-region activate)
  (if (member major-mode
          '(emacs-lisp-mode lisp-interaction-mode lisp-mode
            c-mode c++-mode objc-mode
            latex-mode plain-tex-mode))
      (let ((mark-even-if-inactive t))
        (indent-region (region-beginning) (region-end) nil))))

(defadvice yank-pop (after indent-region activate)
  (if (member major-mode
              '(emacs-lisp-mode lisp-interaction-mode lisp-mode
                                c-mode c++-mode objc-mode
                                latex-mode plain-tex-mode))
      (let ((mark-even-if-inactive t))
        (indent-region (region-beginning) (region-end) nil))))


;; Normally, if you kill a newline and the next line is indented, you will have to execute
;; just-one-space or something similar to get rid of all the extra indentation. This will do that
;; automatically for you, saving some time. I just rebind it to C-k, overriding kill-line, since
;; this is almost always the behavior I want.

;; (defun kill-and-join-forward (&optional arg)
;;   "If at end of line, join with following; otherwise kill line.
;;     Deletes whitespace at join."
;;   (interactive "P")
;;   (if (and (eolp) (not (bolp)))
;;       (delete-indentation t)
;;       (kill-line arg)))

;; (global-set-key (kbd "C-k") 'kill-and-join-forward)

;;; toggle-letter-case
;; http://ergoemacs.org/emacs/modernization_upcase-word.html
(defun pl/toggle-letter-case ()
  "Toggle the letter case of current word or text selection.
Toggles between: “all lower”, “Init Caps”, “ALL CAPS”."
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

(global-set-key (kbd "M-c") 'pl/toggle-letter-case)

;;; Coding
(defun pl/revert-to-gbk-dos ()
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
;; End: **
