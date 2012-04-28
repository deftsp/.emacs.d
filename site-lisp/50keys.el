
;;; --- Keybindings ---

(case system-type
  (darwin
   ;; setting Super ＆ Hyper keys for the Mac keyboard, for emacs running in OS X
   ;; Super/Meta/Hyer
   (setq mac-option-modifier 'super) ; sets the Option key as Super
   (setq mac-command-modifier 'meta) ; sets the Command key as Meta
   (setq mac-control-modifier 'control))
  (wndows-nt
   ;; setting the PC keyboard's various keys to
   ;; Super or Hyper, for emacs running on Windows.
   (setq w32-pass-lwindow-to-system nil
         w32-pass-rwindow-to-system nil
         w32-pass-apps-to-system nil
         ;; Left Windows key
         w32-lwindow-modifier 'super
         ;; Right Windows key
         w32-rwindow-modifier 'super
         ;; Menu key
         w32-apps-modifier 'hyper))
  (gnu/linux
   ;; do nothing. You should set Super and Hyper from your OS
   nil))

(defun global-set-keys (&rest keycommands)
  "Register keys to commands."
  (while keycommands
    (let ((key (car keycommands))
          (command (cadr keycommands)))
      (eval `(global-set-key (kbd ,key) (quote ,command))))
    (setq keycommands (cdr (cdr keycommands)))))


;; register my preferred keybindings
(global-set-keys
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

 "<f9>"    'door-gnus
 "<S-f9>"  'ascii-table-show
 "<C-f9>"  'shell
 "<M-f9>"  'tsp-ansi-term
 "<f10>"   'tsp-w3m-switch-to-buffer
 ;; "<f11>"
 "<S-f11>" 'appt-add
 "<S-f12>" 'recentf-open-files
 "<f12>"   'list-bookmarks
 "<M-f12>" 'recentf-open-files)



(global-set-key (kbd "C-c g f") 'grep-find)
(global-set-key (kbd "C-c r g") 'rgrep)

;;(global-set-key "\C-cw" 'compare-windows)
;; (define-prefix-command 'menu-map)
;; (global-set-key (kbd "<menu>") 'menu-map)
;; (global-set-key (kbd "<menu> <menu>") (lookup-key global-map (kbd "<menu>")))
;; (global-unset-key (kbd "<menu>"))
;; (global-unset-key (kbd "<menu>") (lambda () (interactive) nil))

(global-set-key (kbd "<Scroll_Lock>") (lambda () (interactive) nil))

;;--------------------------------------------------------------------------------------------------------------------

;;使得回车开始下一行的时候，立即缩进，而不是等我写完这一行之后再缩进
(global-set-key "\C-m" 'reindent-then-newline-and-indent)
(global-set-key "\C-j" 'reindent-then-newline-and-indent)
;;(global-set-key "\C-m" 'newline-and-indent)


;; (global-set-key [(control return)] 'set-mark-command)

;;取消一个键的绑定
;;(global-set-key (kbd "C-SPC") 'nil)
;;(global-unset-key "\C-xf")

;;==========================================================
;;使CTRL-h可以删除前面的字符。通常，这将回使你进入帮助系统
;;(define-key global-map "\C-h" 'delete-backward-char)
;;确认CTRL-h也在搜索中
;;(setq search-delete-char (string-to-char "\C-h"))
;;CTRL-underscore可以将帮助绑定到别的地方
;;注意有的终端上CTRL-underscore未被定义
;;(define-key global-map "\C-_" 'help-command) ;;replacement
;;(setq help-char (string-to-char "\C-_"))
;;使ESC-h可以删除前面的单词
;;(define-key global-map "\M-h" 'backward-kill-word)
;;===========================================================

;; (global-set-key (kbd "M-<return>") 'complete-tag)


;; undo C-/ 如果需要重做（Redo）功能，可以先用 C-g 再用 C-/


;; Control tab quotes a tab.
;;(global-set-key [C-tab] "\C-q\t")

;;; imenu
;; (defun his-imenu()
;;   "Call imenu, showing completions."
;;   (interactive)
;;   (setq unread-command-events (list 9))
;;   (imenu (imenu-choose-buffer-index)))

;; I use ido style now
(global-set-key (kbd "C-c i m") 'imenu)

;; (global-set-key  "\C-cn" 'planner-goto-today)
;; (global-set-key (kbd "C-c p t") 'planner-create-task-from-buffer)
;; (global-set-key (kbd "C-c p d") 'planner-diary-add-entry)
;; indent the whole buffer
(global-set-key (kbd "C-c i w") 'deftsp-iwb)
(defun deftsp-iwb ()
  "indent whole buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))
;;----------------------------------------------------------------------
;; iconify-or-deiconify-frame (C-x C-z)

(when window-system
  ;; (global-unset-key "\C-z")
  ;; define a prefix command, make it possible to define key sequence like`C-z c c' 'C-z n`
  ;; (define-prefix-command 'ctl-z-map)
  ;; (global-set-key (kbd "C-z") 'ctl-z-map)
  (global-set-key (kbd "C-c u") 'revert-buffer)) ; how about C-x C-v?


(global-set-key (kbd "C-c o x") 'open-with-xcode)

;;;_, view-mode--------------------------------------------------
(global-set-key (kbd "C-c v") 'view-mode)
;; 为 view-mode 加入 vim 的按键。
;; (setq view-mode-hook
;;       (lambda ()
;;         (define-key view-mode-map "h" 'backward-char)
;;         (define-key view-mode-map "l" 'forward-char)
;;         (define-key view-mode-map "j" 'next-line)
;;         (define-key view-mode-map "k" 'previous-line)))
;;---------------------------------------------------------------

;;Add alternatives to M-x, on the recommendation of Steve Yegge.
;;(global-set-key "\C-c\C-m" 'execute-extended-command)

;;IMPORTANT! This overrides a default binding! I don't use C-l much, and it makes more sense to me for it to kill
;;backwards the line, like C-k kills forward the line.C-u 0 C-k do the same thing.
;;;define the function to kill the characters from the cursor
;;;to the beginning of the current line
;; (defun backward-kill-line (arg)
;;   "Kill chars backward until encountering the end of a line."
;;   (interactive "p")
;;   (kill-line 0))
;; (global-set-key "\C-l" 'backward-kill-line)


;; Enable smart syntax based deletion commands. IMPORTANT! This overrides default bindings!
;;<http://www.zafar.se/bkz/Articles/EmacsTips>
;;(global-set-key [(meta backspace)] 'kill-syntax-backward)
;;(global-set-key [(meta d)] 'kill-syntax-forward)

;; (defun kill-syntax-forward ()
;;   "Kill characters with syntax at point."
;;   (interactive)
;;   (kill-region (point)
;;                (progn (skip-syntax-forward (string (char-syntax (char-after))))
;;                       (point))))
;; (defun kill-syntax-backward ()
;;   "Kill characters with syntax at point."
;;   (interactive)
;;   (kill-region (point)
;;                (progn (skip-syntax-backward (string (char-syntax (char-before))))
;;                       (point))))


;;IMPORTANT! This overrides the default binding! The idea is to sort of imitate Stumpwm for buffer management, so to
;;speak.
;;(global-set-key "\C-n" 'bury-buffer)
;;(global-set-key "\C-p" '(lambda () (interactive) (switch-to-buffer (other-buffer))))


;;Add stumpwm-like buffer movement.  First two require
;;cyclebuffer.el (available in the emacs-goodies-el Debian package).
;; (global-set-key "\356" 'cyclebuffer-forward)    ;;M-n
;; (global-set-key "\360" 'cyclebuffer-backward)   ;;M-p
;; (global-set-key "\215" 'mode-line-other-buffer) ;;M-RET
;;(global-set-key (quote [f5]) 'mode-line-other-buffer) ;;M-RET;
;;;This works becase function keys 7,8,9,11,12 are undefined
;;(global-set-key (quote [C-tab]) 'other-window) ;;C-Tab

;; Need because of urxvt
;;(global-set-key "\M-o\ c" 'forward-word)
;;(global-set-key "\M-o\ d" 'backward-word) ;this overrides some fontlock binding

;;M-down and M-up do nothing! :(  Let's make them do something, like M-left
;; and M-right do.
;;(global-set-key [M-down] '(lambda () (interactive) (progn (forward-line 10) (recenter) ) ))
;;(global-set-key [M-up]   '(lambda () (interactive) (progn (forward-line -10) (recenter) ) ))

;;M-down and M-up do nothing! :( Let's make them do something, like M-left and M-right do.
;;(global-set-key [M-down] '(lambda () (interactive) (progn (forward-line 10) (recenter))))
;;(global-set-key [M-up]   '(lambda () (interactive) (progn (forward-line -10) (recenter) )))


;;----------------------------------------------------------------------------------------------------------------------
;;
;;----------------------------------------------------------------------------------------------------------------------
;;"Redefine the Home/End keys to (nearly) the same as visual studio behavior... special home and end by Shan-leung
;;Maverick WOO <sw77@cornell.edu>" This is complex. In short, the first invocation of Home/End moves to the beginning of
;;the *text* line. A second invocation moves the cursor to the beginning of the *absolute* line. Most of the time this
;;won't matter or even be noticeable, but when it does (in comments, for example) it will be quite convenient.
(global-set-key [home] 'tsp-smart-home)
(global-set-key [end] 'tsp-smart-end)
(defun tsp-smart-home ()
  "Odd home to beginning of line, even home to beginning of
text/code."
  (interactive)
  (if (and (eq last-command 'tsp-smart-home)
           (/= (line-beginning-position) (point)))
      (beginning-of-line)
      (beginning-of-line-text)))
(defun tsp-smart-end ()
  "Odd end to end of line, even end to begin of text/code."
  (interactive)
  (if (and (eq last-command 'tsp-smart-end)
           (= (line-end-position) (point)))
      (end-of-line-text)
      (end-of-line)))
(defun end-of-line-text ()
  "Move to end of current line and skip comments and trailing space.
Require `font-lock'."
  (interactive)
  (end-of-line)
  (let ((bol (line-beginning-position)))
    (unless (eq font-lock-comment-face (get-text-property bol 'face))
      (while (and (/= bol (point))
                  (eq font-lock-comment-face
                      (get-text-property (point) 'face)))
        (backward-char 1))
      (unless (= (point) bol)
        (forward-char 1) (skip-chars-backward " \t\n"))))) ;;Done with home and end keys.
;;-------------------------------------------------------------------------------------------------------------------------

;;--------------------------------------------------------------------------------
;;Unshifted special characters
;;--------------------------------------------------------------------------------

;; In almost any given programming language you will use the special characters a lot more than numbers. Why not
;; optimize for the most common case? While the code below is for the standard US layout it will automatically handle
;; multibyte characters in order to support extended layouts.

;; (defvar *unshifted-special-chars-layout*
;;   '(("1" "!")                           ; from -> to
;;     ("2" "@")
;;     ("3" "#")
;;     ("4" "$")
;;     ("5" "%")
;;     ("6" "^")
;;     ("7" "&")
;;     ("8" "*")
;;     ("9" "(")
;;     ("0" ")")
;;     ("!" "1")
;;     ("@" "2")
;;     ("#" "3")
;;     ("$" "4")
;;     ("%" "5")
;;     ("^" "6")
;;     ("&" "7")
;;     ("*" "8")
;;     ("(" "9")
;;     (")" "0")))

;; (defun mb-str-to-unibyte-char (s)
;;   "Translate first multibyte char in s to internal unibyte representation."
;;   (multibyte-char-to-unibyte (string-to-char s)))

;; (defun remap-keyboard (mapping)
;;   "Setup keyboard translate table using a list of pairwise key-mappings."
;;   (mapcar
;;    (lambda (mb-string-pair)
;;      (apply #'keyboard-translate
;;             (mapcar #'mb-str-to-unibyte-char mb-string-pair)))
;;    mapping))

;; (remap-keyboard *unshifted-special-chars-layout*)
;;Unshifted special characters ends there-------------------------------------------------------------------------------------


;;----------------------------------------------------------------------------------------------------
(defun sfp-page-down ()
  (interactive)
  (setq this-command 'next-line)
  (next-line
   (- (window-text-height)
      next-screen-context-lines)))

(defun sfp-page-up ()
  (interactive)
  (setq this-command 'previous-line)
  (previous-line
   (- (window-text-height)
      next-screen-context-lines)))

(global-set-key (kbd "C-v") 'sfp-page-down)
(global-set-key (kbd "M-v") 'sfp-page-up)

;;------------------------------------------


;;(global-set-key "\C-z" 'set-mark-command)
;; C-x C-SPC pop-global-mark)


;;mouse
;; (global-set-key (kbd "<mouse-1>") 'mouse-set-point)
;; (global-set-key (kbd "<down-mouse-1>") 'mouse-drag-region)
;; (global-set-key (kbd "<C-down-mouse-1>") 'mouse-buffer-menu)

;; (global-set-key [M-down-mouse-1] 'mouse-drag-secondary-pasting)
;; (global-set-key [M-S-down-mouse-1] 'mouse-drag-secondary-moving)
(global-set-key (kbd "<H-mouse-1>") 'browse-url-at-mouse)

;; mouse button one drags the scroll bar
;; (global-set-key [vertical-scroll-bar down-mouse-1] 'scroll-bar-drag)

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

;;----------------------------------------------------------------------------------------------------
;;----------------------------------------------------------------------------------------------------
;; mark一行,如果绑定到C-z，按住Ctrl连续按z可以mark多行,就像C-x z 重复上一个复杂命令的还有 C-x ESC ESC

(defun tsp-mark-line (&optional arg allow-extend)
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
(global-set-key (kbd "H-n") 'tsp-mark-line)

;;-------------------------------------------------------------------------------------------------------

(defun tsp-return-current-point ()
  (interactive)
  (message "Current point is: %d" (point)))
;;----------------------------------------------------------------------------------------------------

;; Ctrl-x +:  balance-windows -- makes all visible windows approximately equal height. This is useful if you've just
;; done Ctrl-x 2 twice in a row, because you'll have two 1/4-height windows and one 1/2-height window. Ctrl-x + makes
;; them all the same height.

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



(global-set-key (kbd "M-Z") 'zap-up-to-char)
(defun zap-up-to-char (arg char)
  "Kill up to and not including ARG'th occurrence of CHAR.
Case is ignored if `case-fold-search' is non-nil in the current buffer.
Goes backward if ARG is negative; error if CHAR not found."
  (interactive "p\ncZap up to char: ")
  (if (char-table-p translation-table-for-input)
      (setq char (or (aref translation-table-for-input char) char)))
  (kill-region (point) (1- (progn
                             (search-forward (char-to-string char) nil nil arg)
                             ;;  (goto-char (if (> arg 0) (1- (point)) (1+ (point))))
                             (point))))
  (backward-char))

(global-set-key (kbd "C-c M-z") 'zap-to-char-save)
(defun zap-to-char-save (arg char)
  "Zap to a character, but save instead of kill."
  (interactive "p\ncZap to char: ")
  (save-excursion
    (zap-to-char arg char)
    (yank)))

;; See also: http://www.emacswiki.org/emacs/ZapToISearch


;; (global-set-key (kbd "H-b") 'backward-delete-char)

;;----------------------------------------------------------------------------------------------------
(global-set-key (kbd "C-c k f") 'tsp-kill-syntax-forward)
(global-set-key (kbd "C-c k b") 'tsp-kill-syntax-backward)

(defun tsp-kill-syntax-forward ()
  "Kill characters with syntax at point."
  (interactive)
  (kill-region (point)
               (progn (skip-syntax-forward (string (char-syntax (char-after))))
                      (point))))

(defun tsp-kill-syntax-backward ()
  "Kill characters with syntax at point."
  (interactive)
  (kill-region (point)
               (progn (skip-syntax-backward (string (char-syntax (char-before))))
                      (point))))
;;----------------------------------------------------------------------------------------------------
;; use yank menu
;; (global-set-key "\C-\M-y"
;;                 (lambda () (interactive )(popup-menu 'yank-menu)))


;;that is cool - now I have many more keys in emacs!!! yeah!!!
;;you can use: shift,ctrl/control, alt, meta, super, hyper, or nothing at all
;;(setq w32-lwindow-modifier 'hyper)  ; lwindow acts as hyper
;;(setq w32-rwindow-modifier t)       ; rwindow is ignored
;;(setq w32-apps-modifier nil)        ; can now bind [apps] et al

;; (setq w32-pass-lwindow-to-system nil
;;       w32-pass-rwindow-to-system nil)

;; (setq w32-lwindow-modifier 'super)  ; lwindow acts as super
;; (setq w32-rwindow-modifier 'hyper)  ; rwindow acts as hyper


;; (setq tsp-global-extra-key-map (make-keymap))
;; (global-set-key [(super z)] tsp-global-extra-key-map)
;; (define-key tsp-global-extra-key-map "b" 'bbdb)
;; (define-key tsp-global-extra-key-map "m" 'bbdb-and-mail-with-default-mailer)

;; (when (eq window-system 'w32)
;;   (setq w32-pass-lwindow-to-system nil
;;         w32-lwindow-modifier 'super
;;         w32-pass-rwindow-to-system nil
;;         w32-rwindow-modifier 'hyper))




;; M-^: delete-indentation
;; join-line is an alias for `delete-indentation'


;; the point doesn’t move, the window does.
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

;;----------------------------------------------------------------------------------------------------

(global-set-key (kbd "H--") '_-SPC)
(defun _-SPC ()
  (interactive)
  (let ((char (following-char)))
    (case char
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



;;; Find the definition of the FUNCTION near point. That's very useful!
(require 'find-func)
;; Define some key bindings for the find-function family of functions.
(find-function-setup-keys)
;; C-x F   find-function
;; C-x 4 F find-function-other-window
;; C-x 5 F find-function-other-frame
;; C-x K   find-function-on-key
;; C-x V   find-variable
;; C-x 4 V find-variable-other-window
;; C-x 5 V find-variable-other-frame


;;;
(global-set-key (kbd "C-x \\") 'align-regexp)


(provide '50keys)