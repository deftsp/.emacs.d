
;;--------------------------------------------------------------------------------
;;; Personal data
(setq user-full-name "Shihpin Tseng"
      user-mail-address "deftsp@gmail.com"
      change-log-default-name "ChangeLog"
      vc-user-login-name "author")

;;; variables
(setq custom-file "~/.emacs.d/site-lisp/50customzation.el"
      inhibit-startup-message t
      ;; inhibit-default-init t            ; 可关闭所有全局初始化值
      gnus-inhibit-startup-message t
      initial-scratch-message nil ;; no message in the scratch buff
      inhibit-startup-echo-area-message t
      column-number-mode t
      size-indication-mode t            ; show file size (emacs 22+)
      view-read-only nil
      ;; system-name "xxxxxx"
      ;; indicate-empty-lines 'right

      ;; pop-up-windows nil ; don't change my windowconfiguration assure my window-configuration is kept
      ;; x-use-underline-position-properties nil
      read-quoted-char-radix 10         ; accept decimal input when using ^q, e.g.: ^q 13 [RET] -> ^M
      history-length 250
      ;; echo unfinished commands after this many seconds of pause.
      echo-keystrokes 0                 ; 0.1
      tooltip-hide-delay 20             ; defautl 10
      yank-excluded-properties t        ; do not paste any properties
      mouse-yank-at-point t
      ;; directory-sep-char ?\\
      confirm-kill-emacs nil ; 'y-or-n-p
      use-dialog-box nil                  ; mouse commands don't use dialog boxes to ask questions.
      comint-completion-autolist t
      comint-input-ignoredups t
      comint-prompt-read-only t
      minibuffer-electric-default-mode 1 ; 启用minibuffer，好像是默认设置吧
      enable-recursive-minibuffers t     ;; 递归使用minibuffer.
      ;;允许minibuffer自由变化其大小（指宽度）
      resize-mini-windows t
      find-function-C-source-directory "~/src/emacs/src"
      max-lisp-eval-depth 1500
      max-specpdl-size 3000
      comment-multi-line nil
      ;; comment-column 60
      completion-ignore-case t        ;Do case-insensitive completion.
      ;; don't automatically add new lines when scrolling down at the bottom of a buffer
      next-line-add-newlines nil
      ;; require-final-newline nil        ; let major modes to set it
      track-eol nil
      read-mail-command 'gnus
      ;; paren-sexp-mode t
      ;; temporary-file-directory "~/tmp/"
      read-file-name-completion-ignore-case t
      ;; ispell-dictionary "english"
      ;; ps-multibyte-buffer 'non-latin-printer
      display-time-24hr-format t
      display-time-day-and-date t
      display-time-use-mail-icon t
      display-time-interval 10          ; Seconds between updates of time in the mode line
      window-min-height 4               ; Let's not have too-tiny windows.
      ;; undo-no-redo t
      ;; redisplay-dont-pause t
      ;; mark-even-if-inactive t
      x-select-enable-clipboard t ;;允许 emacs 和外部其他程序的粘贴
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
      ;; save every 20 characters typed (this is the minimum)
      auto-save-interval 1200           ; set autosaving from 300 keystrokes to 1200
      ;; save after 1 second of idle time (default is 30)
      auto-save-timeout 30
      ;; remove no-run-time function warnings
      byte-compile-warnings (quote (noruntime))
      ;;在emacs读man文档时，使用当前buffer
      Man-notify-method 'pushy
      default-major-mode 'text-mode
      bookmark-default-file "~/.emacs.d/.emacs.bmk"
      bookmark-save-flag 1              ; save bookmark every time you set bookmark, not only when you exit emacs
      next-screen-context-lines 2
      mail-user-agent 'gnus-user-agent
      ;; default-enable-multibyte-characters t
      enable-local-variables :safe)

(which-function-mode 1)

(if (eq system-type 'gnu/linux)
    (display-battery-mode t))

;; do not create new frame with `open' in Mac OS X
(when (eq window-system 'ns)
  (setq ns-pop-up-frames nil))


;;; diminish
;; Makes minor mode names in the modeline shorter.
;; (require 'diminish)
;; (diminish 'icicle-mode "")
;; (eval-after-load "filladapt"
;;   '(diminish 'filladapt-mode "Fill"))
;; (eval-after-load "abbrev"
;;   '(diminish 'abbrev-mode "Abv"))
;; (eval-after-load "doxymacs"
;;   '(diminish 'doxymacs-mode "dox"))


(add-hook 'suspend-hook 'do-auto-save) ;; Auto-Save on ^Z

(setq-default indicate-buffer-boundaries 'right)
;; If you often look at files that are updated (perhaps a log file for a running process), or perhaps ClearCase files
;; (that change when you update the config spec), you'll want to make sure you're looking at the most recent version of
;; the file.
(global-auto-revert-mode 1)

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

;; This causes files that I'm editing to be saved automatically by the
;; emacs auto-save functionality.  I'm hoping to break myself of the
;; c-x c-s twitch.
;; (add-hook 'auto-save-hook 'save-buffer-if-visiting-file)

;;; turncate
;; (set-default 'truncate-lines t) ; This is a BufferLocalVariable don't recommend.
(add-hook 'emacs-lisp-mode-hook (lambda () (setq truncate-lines t)))
(setq truncate-partial-width-windows t) ;; truncate lines in all windows less than full frame wide.

(global-set-key (kbd "C-c T") 'toggle-truncate-partial-width-windows)
(defun toggle-truncate-partial-width-windows ()
  (interactive)
  (set-default 'truncate-partial-width-windows
               (not truncate-partial-width-windows))
  (message "truncate-partial-width-windows: %S"
           truncate-partial-width-windows))

;; Active Region Highlighting
(transient-mark-mode t)

;;; display-time-string-forms
(display-time-mode 1)

(defface my-display-time-face '((((type x w32 mac))
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
      '((substring year -2) "/" month "/" day "("dayname")"
        (propertize (concat " " 24-hours ":" minutes)

         'face 'my-display-time-face)
        (if time-zone " ") time-zone (if time-zone " ")
        load
        (if mail
            (concat " "
                    (propertize
                     display-time-mail-string
                     'display
                     (and display-time-use-mail-icon (display-graphic-p)
                          display-time-mail-icon)
                     'help-echo "You have new mail; mouse-2: Read mail"
                     'mouse-face 'mode-line-highlight
                     'local-map
                     (make-mode-line-mouse-map 'mouse-2 read-mail-command)))
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

;;所有的问题用y/n方式，不用yes/no方式。有点懒，只想输入一个字母
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
      completion-ignored-extensions '(".svn/" "CVS/" ".o" "~" ".bin" ".lbin" ".so" ".a" ".ln" ".blg" ".bbl"
                                      ".elc" ".lof" ".glo" ".idx" ".lot" ".dvi" ".fmt" ".tfm" ".pdf" ".class"
                                      ".fas" ".lib" ".mem" ".x86f" ".sparcf" ".fasl" ".ufsl" ".fsl" ".dxl"
                                      ".pfsl" ".dfsl" ".lo" ".la" ".gmo" ".mo" ".toc" ".aux" ".cp" ".fn"
                                      ".ky" ".pg" ".tp" ".vr" ".cps" ".fns" ".kys" ".pgs" ".tps" ".vrs"
                                      ".pyc" ".pyo" ".class" "*Help*" "*Completions*")
      ;; compilation-context-lines 3
      ;; compilation-skip-threshold 1
      ;; compilation-skip-visited t
      ;; keep scrolling in compilation result buffer
      compilation-scroll-output t)

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


;;鼠标自动避开指针，如当你输入的时候，指针到了鼠标的位置，鼠标有点挡住视线了
(mouse-avoidance-mode 'animate)
;;允许自动打开图片，如wiki里面
(setq auto-image-file-mode nil)
;;Automatically uncompress .gz files
(auto-compression-mode 1)

;; Save all tempfile in ~/.tmp
;;(setq auto-save-file-name-transforms (quote ((".*/\\(.*\\)" "~/.tmp/\\1" t))))

;; kill customize buffers on exit ---------------------------------------------
(setq custom-buffer-done-kill t)

;; set unicode data file location. (used by what-cursor-position)
;; (let ((x "~/.emacs.d/UnicodeData.txt"))
;;   (when (file-exists-p x)
;;     (setq describe-char-unicodedata-file x)))


(eval-after-load "man" '(require 'man-completion))
;; add a womans touch ---------------------------------------------------------
;; (defalias 'man 'woman)
(setq woman-show-log nil)
(setq woman-cache-filename "~/.emacs.d/.wmncach.el")
;;设置woman在新buffer中打开，而不是一个新窗口
(setq woman-use-own-frame nil)
(setq woman-fill-column 100)
;; Not to lookup POSFIX man page
;; `woman-manpath-man-regexp' should be set before requrie woman.el. So the it should be changed
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
(setq gc-cons-threshold (max 6000000 gc-cons-threshold))

;; want to see how often GC happens
;; (setq garbage-collection-messages nil)

;;; Initial register values
;; `C-x r j e' to open DotEmacs,
(cond ((file-exists-p (expand-file-name "~/.emacs"))
       (set-register ?e '(file . "~/.emacs")))
      ((file-exists-p (expand-file-name "~/proj/org"))
       (set-register ?t '(file . "~/proj/org/ToDo.org"))))

;;; paren
;;----------------------------------------------------------------------------------------------------
;; better than paren for lisp code
;; (require 'stig-paren)
(show-paren-mode t)
(setq blink-matching-paren-on-screen t)
;;; Also highlight parens
(setq show-paren-delay 0
      show-paren-style 'parenthesis)    ;expression

;; (require 'mic-paren)
;; (paren-activate)
;; (setf paren-priority 'close)


;;; file-name-shadow-mode
;; be smart about filenames understand ~/ etc
(when (fboundp 'file-name-shadow-mode)
  (file-name-shadow-mode t))

;;; Paren
(global-set-key (kbd "C-c %") 'delete-pair-plus)
(define-key lisp-mode-map (kbd "C-c %") 'goto-match-paren)
(define-key emacs-lisp-mode-map (kbd "C-c %") 'goto-match-paren)
(define-key lisp-interaction-mode-map (kbd "C-c %") 'goto-match-paren)
(eval-after-load "scheme"
  '(define-key scheme-mode-map (kbd "C-c %") 'goto-match-paren))
;; (global-set-key "\C-c%" 'self-insert-command) ;C-q %
;; (defun match-paren (arg)
;;   "Go to the matching paren if on a paren; otherwise insert %."
;;   (interactive "p")
;;   (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
;;         ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
;;         (t (self-insert-command (or arg 1)))))


(defun delete-pair-plus (&optional skip-pair-count)
  (interactive)
  (when (not (=  (point-min) (point)))
    (let ((skip-pair-count (if skip-pair-count skip-pair-count 0)))
      (cond ((looking-at "[([{]")
             (if (= skip-pair-count 0)
                 (delete-pair)
               (setq skip-pair-count (1- skip-pair-count))
               (backward-char 1)
               (delete-pair-plus skip-pair-count)))
            (t
             (if (looking-at "[])}]")
                 (setq skip-pair-count (1+ skip-pair-count)))
             (backward-char 1)
             (delete-pair-plus skip-pair-count))))))




(defun goto-match-paren (arg)
  "Go to the matching parenthesis if on paranthesis. Else go to the
   opening paranthesis one level up."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1))
        (t
         (backward-char 1)
         (cond ((looking-at "\\s\)")
                (forward-char 1) (backward-list 1))
               (t
                (while (not (looking-at "\\s("))
                  (backward-char 1)
                  (cond ((looking-at "\\s\)")
                         (message "->> )")
                         (forward-char 1)
                         (backward-list 1)
                         (backward-char 1)))))))))

(defun kill-match-paren (arg)
  "kill pair sexp."
  (interactive "p")
  (cond ((looking-at "[([{]") (kill-sexp 1))
        ((looking-at "[])}]") (forward-char) (backward-kill-sexp 1))
        (t
         (while (not (looking-at "[([{]"))
           (backward-char 1)
           (if (looking-at "[])}]")
               (error "Current sexp is not in paren!")))
         (let ((opoint (point)))
           (forward-list 1)
           (kill-region (1+ opoint) (1- (point))))
         (backward-char))))

(global-set-key (kbd "C-c M-%") 'kill-match-paren)



(defun kill-outside-paren-with-elt (arg)
  (interactive "p")
  (if (not (looking-at "[([{]"))
      (up-list -1))
  (copy-region-as-kill (point) (scan-sexps (point) 1))
  (up-list -1)
  (kill-sexp 1)
  (yank 2))

(global-set-key (kbd "C-x %") 'kill-outside-paren-with-elt)
;; paren end there----------------------------------------------------------------------

(when window-system
  (mouse-wheel-mode t))

;;Scroll;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (set-face-background 'scroll-bar "slategray4")
;; (set-face-foreground 'scroll-bar "white")
;;不要滚动栏，现在都用滚轴鼠标了，可以不用滚动栏了
;;(scroll-bar-mode nil)

;; (require 'smooth-scrolling nil t)
;; (setq smooth-scroll-margin 3)

;; 防止页面滚动时跳动， scroll-margin 3 可以在靠近屏幕边沿6行时就开始滚动，可以很好的看到上下文
(setq scroll-step 1                     ; Scroll by one line at a time
      scroll-margin 3
      scroll-conservatively 10000)     ; Fix the whole huge-jumps-scrolling-between-windows nastiness
;; (setq next-screen-context-lines 2)  ; how many from page up/down
;; What it says. Keeps the cursor in the same relative row during pgups and dwns.
;; (setq scroll-preserve-screen-position t)

;; "Don't hscroll unless needed"- ? More voodoo lisp.
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

;; Insert space/slash after completion
(setq comint-completion-addsuffix t)

;;Might as well limit how many messages fill up in the message buffer.
;;Not that we'll ever get that many, but just in case!
(setq message-log-max 3000)

;;"With argument t, set the random number seed from the current time and pid."
(random t)

(setq cursor-in-non-selected-windows t)

;; kills an entire line if the cursor is at the beginning of line. And try M-0 C-k.
(setq-default kill-whole-line t)
;;设定删除保存记录为200，可以方便以后无限恢复
(setq kill-ring-max 200)

;;增大使用查找函数和变量的寻找范围
(setq apropos-do-all t)
(setq apropos-sort-by-scores t)
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
;; (add-hook 'haskell-mode-hook 'flyspell-prog-mode)
;; (add-hook 'emacs-lisp-mode-hook (lambda () (flyspell-prog-mode)))
;; (add-hook 'lisp-mode-hook (lambda () (flyspell-prog-mode)))
;; (add-hook 'shell-mode-hook (lambda () (flyspell-prog-mode)))


;;--------------------------------------------------------------------------------------------
;; Use cperl-mode instead of perl-mode
;;(defalias 'perl-mode 'cperl-mode)

;; If you don't like having lines of code/text with whitespace at the ends,Emacs highlight the offending whitespace.
;; When set, the variable's value becomes buffer local, so set it to true in the mode-hooks for your preferred modes.
;; Or, if you want it on all the time, change the default value with:

;; (if (>= emacs-major-version 21)
;;     (setq-default show-trailing-whitespace t))

;;I do not want excess trailing whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq line-number-display-limit 10000000)

;;; set frame title
(setq frame-title-format
      (list "GNU Emacs " emacs-version "@" system-name ": " '(:eval
                                                              (if buffer-file-name
                                                                  (replace-regexp-in-string
                                                                   (getenv "HOME") "~"
                                                                   (file-name-directory buffer-file-name))
                                                                  (buffer-name)))))
(setq icon-title-format frame-title-format)

;; (setq-default frame-title-format
;;               (list '((buffer-file-name " %f" (dired-directory
;;                                                dired-directory
;;                                                (revert-buffer-function " %b"
;;                                                                        ("%b - Dir:  " default-directory)))))))


;; M-x apropos  apropos works better but slower
(setq apropos-do-all t)

;; this gives matlab access to the X11 windowing system, so I can see figures, etc.
;; (setenv "DISPLAY" ":0.0")

;;; Enable emacs functionality that is disabled by default

;; overwrite mode is not disabled
;;(put 'overwrite-mode 'disabled nil)
(put 'set-goal-column 'disabled nil)

;; Undo these with C-x n w
(put 'narrow-to-region 'disabled nil)     ; C-x n n
(put 'narrow-to-page   'disabled nil)     ; C-x n p
(put 'narrow-to-defun  'disabled nil)     ; C-x n d

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
;; (autoload 'xrdb-mode "xrdb-mode" "Mode for editing X resource files" t)
;; (autoload 'js2-mode "js2-mode" nil t)
;; (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))


(mapcar #'(lambda (lst)
            (if lst (add-to-list 'auto-mode-alist lst)))
        (list

         '("\\.\\(xml\\|rdf\\)\\'" . sgml-mode)
         '("\\.css\\'" . css-mode)
         '("\\.\\(emacs\\|session\\|gnus\\)\\'" . emacs-lisp-mode)
         '("\\.\\(jl\\|sawfishrc\\)\\'" . sawfish-mode)
         '("\\.scm\\'" . scheme-mode)
         '("\\.py\\'" . python-mode)
         '("\\.\\(ba\\)?sh\\'" . sh-mode)
         '("\\.l\\'" . c-mode)
         '("\\.mm\\'" . objc-mode)
         '("\\.o\\'" . hexl-mode)
         '("\\.max\\'" . maxima-mode)
         '("\\.fvwm2rc$" . fvwm-mode)
         '("/f\\..*" . fvwm-mode)
         ;; ("\\.Xdefaults$"     . xrdb-mode)
         ;; ("\\.Xenvironment$"  . xrdb-mode)
         ;; ("\\.Xresources$"    . xrdb-mode)
         '("\\.tei$"           . xml-mode)
         '("\\.dps$"           . pascal-mode)
         '("\\.bash_aliases$"  . shell-script-mode)
         '("\\.procmailrc$"    . conf-mode)
         '("fonts.conf"        . xml-mode)
         '("\\.org$"           . org-mode)
         '("\\.markdown$"      . markdown-mode)
         '("\\.lrc$"           . emms-lyrics-mode)))

;; Since emacs 22 we can use magic-mode-alist to set mode
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

(defvar iso-date-format "%Y-%m-%dT%H:%M:%S%z"
  "Format string for ISO dates.")

(defun iso-timestamp (&optional time)
  (format-time-string iso-date-format
                      (or time (current-time))))
(defun insert-iso-timestamp ()
  (interactive)
  (insert (iso-timestamp)))

(defun iso-timestamp-sexp (&optional time)
  (parse-time-string (iso-timestamp)))

;;----------------------------------------------------------------------------------------------------

(defun do-nothing ()
  "Just to remind me this keybind is not used anymore."
  (interactive)
  (message "Hi! :-)") )

;; (defun select-until-end-of-line ()
;;   "Select until the end of a line without killing it."
;;   (interactive)
;;   (copy-region-as-kill (point) (line-end-position)) )

(defun insert-userid ()
  "Insert the users email address"
  (interactive)
  (insert user-full-name " <" user-mail-address ">" ) )

;; (defun recenter-to-first-line ()
;;   "Recenter to the first screenline."
;;   (interactive)
;;   (recenter "1"))
;; (global-set-key  "\C-cl"      'recenter-to-first-line)

;;临时记号
(defun tsp-point-to-register()
  "Store cursorposition _fast_ in a register.
Use tsp-jump-to-register to jump back to the stored
position."
  (interactive)
  (point-to-register 6)
  (message "point-to-register 6"))

(defun tsp-jump-to-register()
  "Switches between current cursorposition and position
that was stored with tsp-point-to-register."
  (interactive)
  (let ((tmp (point-marker)))
    (jump-to-register 6)
    (set-register 6 tmp)))
(global-set-key (kbd "C-c 6") 'tsp-point-to-register)
(global-set-key (kbd "C-c ^") 'tsp-jump-to-register)
;;===========================================================

;;=============================================
(setq time-stamp-active t
      time-stamp-warn-inactive t        ;去掉time-stamp的警告？
      time-stamp-line-limit 8           ;; check the first 10 lines of the file for Time-stamp: <>
      time-stamp-format (concat "%:y-%02m-%02d %02H:%02M:%02S " user-full-name)
      )
(add-hook 'write-file-hooks 'time-stamp)
(add-hook 'before-save-hook 'time-stamp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun tsp-insert-file-variable ()
  "Insert file variable string \"-*- Major-Mode-Name -*-\" with
  comment char"
  (interactive)
  (insert
   (concat comment-start " -*- "
           (substring
            (symbol-name (symbol-value 'major-mode)) 0 -5)
           " -*- " comment-end)))

;;--------------------------------------------------------------------------------
;;cscope
;;--------------------------------------------------------------------------------
;;使用 cscope 浏览源代码,这个xcscope是个改进版，为每一次查找的结果使用不同 buffer ，
;;这样就可以保存以前的结果。

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


;;cscope ends there---------------------------------------------------------------


;;----------------------------------------------------------------------

;; (defun tsp-create/switch-scratch ()
;;   (interactive)
;;   (let ((buf (get-buffer "*scratch*")))
;;     (switch-to-buffer (get-buffer-create "*scratch*"))
;;     (when (null buf)
;;       (lisp-interaction-mode))))

;;; Game
;; 把俄罗斯方块的速度调快一些
;; (setq tetris-update-speed-function
;;       (lambda (shapes rows)
;;         (/ 10.0 (+ 80.0 rows))))

;;; charset-to-oem
(defun replace-charset-to-oem (start end)
  (interactive "r")
  (save-excursion
    (format-replace-strings '(("’" . "'")
                              ("′" . "'")
                              ("‘" . "'")
                              ("…" . "...")
                              ("“" . "\"")
                              ("”" . "\"")
                              ("•" . "ù")
                              ("–" . "-")
                              ("—" . "--")
                              ("ü" . "")
                              ("é" . "‚")
                              ("§" . "")
                              ("®" . "(R)")
                              ("·" . "*")
                              ("×" . "x"))
                            nil
                            start
                            end)))
(global-set-key (kbd "C-c t o") 'replace-charset-to-oem)

;; (defun cdf-copy-line (n)
;;     "Copy N lines at point to the kill-ring."
;;     (interactive "p")
;;     (kill-ring-save (line-beginning-position) (line-beginning-position (1+
;; n))))

;;------------------------------------------------------------------------------------------

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
(global-set-key (kbd "M-w") 'kill-ring-save-dwim)

(defun kill-ring-save-dwim ()
  "This command dwim on saving text. \n
If region is active, call `kill-ring-save'. Else, call
`sdl-kill-ring-save-thing-at-point'. \n
This command is to be used interactively."
  (interactive)
  (if (use-region-p)
      (call-interactively 'kill-ring-save)
      (call-interactively 'kill-ring-save-thing-at-point)))

;; M-w if thing at point is url or mail address copy it, or copy current line
;; After press M-w, if press following key...
;; key:  copy
;; w     word
;; l     list
;; s     sexp
;; f     file name

;; and it can accpet prefix, M-3 M-w will copy three lines and M-3 M-w w will copy three words.


(defun kill-ring-save-thing-at-point (&optional n)
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




;;; midnight
;; What would life be like if there were only saving, and never killing? These two settings tell Emacs to, every
;; midnight, kill regular buffers which haven't been used in a while.
;; <http://www.gnu.org/software/emacs/manual/html_node/Kill-Buffer.html>
(require 'midnight)

;; special buffers are cleaned every 12 hours
(setq clean-buffer-list-delay-special (* 12 3600))

(setq midnight-period (* 3 60 60))      ; occur every 3 hours default 86400 (24 hours)
;; note period timer will occuer, even the time when set the timer (boot the emacs) is after
;; "09:30am". see more..  run-at-time.
;; (midnight-delay-set 'midnight-delay "09:30am")
;; set midnight-delay after 3 hours
(midnight-delay-set 'midnight-delay
                    (format-time-string "%H:%M"
                                        (seconds-to-time (+ (time-to-seconds (current-time))
                                                           (* 3 60 60)))))



;; By default, clean-buffer-list will kill buffers that haven't been visited in 3 days, or in the last
;; hour in the case of special buffer that are specified by `clean-buffer-list-kill-buffer-names'

;; As of 2009-10-02, DeskTop mode does not preserve the value of ‘buffer-display-time’ for buffers,
;; so the buffer’s “age” is effectively restarted. This means that buffers restored by a Desktop
;; sessions are considered “new” by CleanBufferList, even though they may be considered“old”.

;; http://www.emacswiki.org/emacs/CleanBufferList
;; The following variables can be used to customize the behavior of the
;; clean-buffer-list (which is run daily at midnight).
;; clean-buffer-list-kill-buffer-names
;; clean-buffer-list-kill-regexps
;; clean-buffer-list-kill-never-buffer-names
;; clean-buffer-list-kill-never-regexps
;; clean-buffer-list-delay-general
;; clean-buffer-list-delay-special

(mapcar (lambda (str) (add-to-list 'clean-buffer-list-kill-buffer-names str))
        '("*buffer-selection*"
          "*Finder*"
          "*Finder Category*"
          "*Finder-package*"
          "*RE-Builder*"
          "*vc-change-log*"))


(mapcar (lambda (str) (add-to-list 'clean-buffer-list-kill-regexps str))
        '("\\`\\*Customize .*\\*\\'"
          "\\`\\*\\(Wo\\)?Man .*\\*\\'"))

(mapcar (lambda (str) (add-to-list 'clean-buffer-list-kill-never-buffer-names str))
        '("*eshell*"
          "*ielm*"
          "*mail*"
          "*w3m*"
          "*w3m-cache*"))

(mapcar (lambda (str) (add-to-list 'clean-buffer-list-kill-never-regexps str))
        '("\\`\\*tramp/.*\\*\\`"
          "\\`\\*ftp .*\\*\\`"))



;;;
;; Don't bother entering search and replace args if the buffer is read-only. Duh.
;; (defadvice query-replace-read-args (before barf-if-buffer-read-only activate)
;;   "Signal a `buffer-read-only' error if the current buffer is read-only."
;;   (barf-if-buffer-read-only))

;;-----------------------------------------------------------------------------------------------

;;; autoarg mode
;; 任何模式下的数字全都是前缀参数了M-2 C-n 只需 2 C-n；但想输入数字时，得用 C-2 C-3 或 23 SPC。
;; (autoarg-mode 1)
;; M-x autoarg-mode & autoarg-kp-mode

;;; rename-file-and-buffer
(defun rename-file-and-buffer (new-name)
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

(global-set-key (kbd "C-c f r")  'rename-file-and-buffer)

(defun move-buffer-and-file (dir)
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

(global-set-key (kbd "C-c f R") 'move-buffer-and-file)
;;------------------------------------------------------------------------------------------

;;; auto-show
;; This file has been obsolete since Emacs 21.1.because Emacs does hscrolling automatically, now.
;; load auto-show (shows lines when cursor moves to right of long line).
;; (require 'auto-show)
;; (auto-show-mode 1)
;; (setq-default auto-show-mode t)
;; will position the cursor to end of output in shell mode.
;; (auto-show-make-point-visible)

;;----------------------------------------------------------------------------------------------------
;;; info

;; (setq Info-dir-contents nil)
;; (pushnew (expand-file-name "~/share/info") Info-default-directory-list :test #'equal)
;; (setq Info-additional-directory-list '("~/share/info"))
;; (setq Info-directory-list Info-default-directory-list)
;; (setq Info-directory-list nil)

;; (defun add-init-path-to-info-path ()
;;   "Add the subdirectories of init-path that contain info directory
;; files to the Info-directory-list.  This can safely be run many times
;; in a session, without adding multiple copies of the directories.  The
;; new directories are prepended to emacs's initial Info path."
;;   (interactive)
;;   (setq Info-directory-list (append (find-subdirs-containing init-path "^dir$") initial-info-path)))

;; Make sure we have /sbin in the path - SUSE puts install-info there
;; (add-to-list 'exec-path "/sbin")

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

;; ;;; Create dir files for any info files in the init-path
;; (add-info-dir-files-to-path init-path)

;; Add the init-path tree to the Info path
;; (require 'info)
;; (info-initialize)
;; (setq initial-info-path Info-directory-list)
;; (add-init-path-to-info-path)


;;;------------------------------------------------------------

;; Display various non-editing buffers in their own frames
;; (setq special-display-buffer-names
;;       (nconc '("*Backtrace*" "*VC-log*" "*compilation*" "*grep*")
;;              special-display-buffer-names))

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
(defun deftsp-insert-char-next-line (arg)
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
(defun deftsp-insert-char-prior-line (arg)
  "insert char above the cursor"
  (interactive "p")
  (tsp-insert-char-next-line (- arg)))

(global-set-key (kbd "M-[") 'deftsp-insert-char-prior-line)
(global-set-key (kbd "M-]") 'deftsp-insert-char-next-line)

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

;;----------------------------------------------------------------------------------------------------
;;;
;; Anchored transpose
(global-set-key [?\C-x ?t] 'anchored-transpose)
(autoload 'anchored-transpose "anchored-transpose" nil t)
;;----------------------------------------------------------------------------------------------------

;; (defun sincronizar()
;;   (interactive)
;;   (shell-command "rsync -u a013775@serdis.dis.ulpgc.es:~diary ~/diary")
;;   (shell-command "rsync -u ~/diary a013775@serdis.dis.ulpgc.es:~/diary"))


;; (defun tsp-shell-command ()
;;   "Launch a shell command."
;;   (interactive)
;;   (let ((command (read-string "Command: ")))
;;     (shell-command (concat command " &") (concat "*" command "*"))))


;; check for unsaved changes for killing
;; (defun tsp-context-kill (arg)
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
(when (require 'browse-kill-ring "browse-kill-ring" t)
  (browse-kill-ring-default-keybindings)
  (setq browse-kill-ring-highlight-current-entry t)
  (setq browse-kill-ring-quit-action 'bury-and-delete-window)
  (define-key browse-kill-ring-mode-map [down] 'browse-kill-ring-forward)
  (define-key browse-kill-ring-mode-map [up] 'browse-kill-ring-previous))
(global-set-key (kbd "C-c k r") 'browse-kill-ring)

;;;
(global-undo-tree-mode t)
(eval-after-load "undo-tree"
  '(progn
    (setq undo-tree-mode-lighter " UT")))


;;; recursive-edit
;; Hitting C-c r e will put you in a "recursive editing mode", that is simply an embedded call to the editing loop. The
;; point here is that you can exit this inner loop, which means that you return from the recursive-edit function. This
;; way, the recursive editing can be guarded by some context-saving macros : here save-window-excursion and
;; save-excursion. Once the user quits the recursive edit, the context is restored, which means here that the windows
;; state, current buffer and position are restored : you're back in the state where your brain was preempted without
;; even needing to remember it.

;; Enter a recursive edit. C-M-c will bring back exactly there
;; `C-M-c' default binding to exit-recursive-edit, it means returning to the
;; unfinished command, which continues execution
;; `C-]'   default binding to abort-recursive-edit, This is like exiting, but
;; also quits the unfinished command immediately.

(defun recursive-edit-save-window-config ()
  (interactive)
  (save-window-excursion
    (save-excursion
      (recursive-edit))))

(global-set-key (kbd "C-c r e") 'recursive-edit-save-window-config)
;; RecursiveEditPreservingWindowConfig
;; One can change the window configuration temporarily using RecursiveEdit?.
;; Inspired by a command posted by ErikNaggum in an Emacs Newsgroup, EmilioLopes
;; wrote this macro:

;; inspired by Erik Naggum's `recursive-edit-with-single-window'
(defmacro recursive-edit-preserving-window-config (body)
  "*Return a command that enters a recursive edit after executing BODY.
 Upon exiting the recursive edit (with \\[exit-recursive-edit] (exit)
 or \\[abort-recursive-edit] (abort)), restore window configuration
 in current frame."
  `(lambda ()
     "See the documentation for `recursive-edit-preserving-window-config'."
     (interactive)
     (save-window-excursion
       ,body
       (recursive-edit))))

;; Use it like this:

(global-set-key (kbd "C-c 0") (recursive-edit-preserving-window-config (delete-window)))
(global-set-key (kbd "C-c 2") (recursive-edit-preserving-window-config
                               (split-window-vertically 20)))
(global-set-key (kbd "C-c 3") (recursive-edit-preserving-window-config
                               (split-window-horizontally -52)))
(global-set-key (kbd "C-c 1") (recursive-edit-preserving-window-config
                               (if (one-window-p 'ignore-minibuffer)
                                   (message "Current window is the only window in its frame")
                                   (delete-other-windows))))

;; Now pressing "C-c 1" will delete all other windows in the current frame and put
;; you into "recursive editing". You know you are in a recursive edit by noting the
;; square brackets around the parentheses that always surround the major and minor
;; mode names. After exiting recursive edit, e.g. by using "C-M-c"
;; ('exit-recursive-edit'), the original window configuration is restored.

;; recursive-edit end there ---------------------------------------------------------------

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
;; (autoload 'highline-mode "highline" "Toggle global minor mode to highlight current line in buffer." t)
;; (autoload 'highline-local-mode "highline" "Toggle local minor mode to highlight current line in buffer."  t)

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
;;   (set-face-background 'mmm-default-submode-face "gray97")
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
;;(set-face-background 'mmm-default-submode-face nil)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; safe locals; we mark these as 'safe', so emacs22+ won't give us annoying
;; warnings
;; (setq safe-local-variable-values
;;       (quote ((auto-recompile . t)
;;               (outline-minor-mode . t)
;;               auto-recompile outline-minor-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Let yank and yank-pop to indent whatever they just pasted. This is useful if, for example, you
;; copy some code from another file at a different indentation level than you want to paste it at.
;; With these advice, the code will be indented properly relative to wherever you paste it.
(defadvice yank (after indent-region activate)
  (if (member major-mode
          '(emacs-lisp-mode lisp-interaction-mode scheme-mode lisp-mode
            c-mode c++-mode objc-mode
            latex-mode plain-tex-mode))
      (let ((mark-even-if-inactive t))
        (indent-region (region-beginning) (region-end) nil))))

(defadvice yank-pop (after indent-region activate)
  (if (member major-mode
          '(emacs-lisp-mode lisp-interaction-mode scheme-mode lisp-mode
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
