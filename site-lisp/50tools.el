;;;; some useful packages

(autoload 'rfc "rfc-util"
  "Prompt for an rfc number and display it in a new buffer." t)

;;; (require 'pulse-settings)


;;; thing-cmds
;; ‘C-M-SPC’ – `mark-thing’ (overwrites the standard binding for ‘mark-sexp’; `sexp’ is the default thing type)
;; `M-@’ – `cycle-thing-region’ (overwrites the standard binding for ‘mark-word’)
;; ‘C-M-U’ (aka ‘C-M-S-u’) – `mark-enclosing-sexp’
;; ‘C-M-F’ (aka ‘C-M-S-f’) – `mark-enclosing-sexp-forward’
;; ‘C-M-B’ (aka ‘C-M-S-b’) – `mark-enclosing-sexp-backward’
;; ‘C-x down’ – `next-visible-thing-repeat’ (Emacs 22 and later)
;; ‘C-x up’ – `previous-visible-thing-repeat’ (Emacs 22 and later)
;; (thgcmd-bind-keys)
;; (global-set-key (kbd "C-M-?") 'mark-thing) ; vs `mark-sexp', how to press?
;; (global-set-key (kbd "M-@") 'cycle-thing-region) ; vs `mark-word'


;;; expand-region
(global-set-key (kbd "C-=") 'er/expand-region)



;;; let ^L looks beautiful
;; (require 'pp-c-l)
;; (pretty-control-l-mode 1)

;; http://nschum.de/src/emacs/company-mode/

;; (load "~/.emacs.d/packages/nxhtml/autostart.el")
;; (setq nxhtml-skip-welcome t)

;; (require 'misc-fns)

;; (when (require 'pretty-symbols nil t)
;;   (dolist (hook '(lisp-mode-hook emacs-lisp-mode-hook scheme-mode-hook c-mode-common-hook))
;;     (add-hook hook 'turn-on-pretty-symbols-mode)))
;; (global-pretty-symbols-mode 1)

;;; key-chord
(require 'key-chord)
(key-chord-mode 1)
(setq key-chord-one-key-delay 0.12)    ; default 0.2
(setq key-chord-two-keys-delay 0.18)    ; default 0.1


(key-chord-define-global "''" "`'\C-b")
(key-chord-define-global "RR" 'anything-resume)
(key-chord-define-global "HH" 'woman)
(key-chord-define-global "JJ" 'anything)
(key-chord-define-global "OO" 'anything-occur)
(key-chord-define-global "RF" 'ido-choose-from-recentf)

;;; space-chord
(require 'space-chord)
(setq space-chord-delay 0.08)           ; default 0.08
(space-chord-define-global "f" 'find-file)
(space-chord-define-global "r" 'remember)
(space-chord-define-global "b" 'switch-to-buffer)
(space-chord-define-global "c" 'calendar)


;;; linkd
(autoload 'linkd-mode "linkd" "Create or follow hypertext links." t)


;;; uptime
(require 'uptimes nil t)


;; (require 'visible-mark)
;; (visible-mark-mode t)

;; (require 'voctest nil t)
;; (load-library "cdargs")

;;load recentf.el 这个扩展可以帮你保存一个"最近打开的文件"列表
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-saved-items 500)
(setq recentf-max-menu-items 60)
(setq recentf-auto-cleanup 'never) ;;To protect tramp
;;The remote connection is NOT opened
(add-to-list 'recentf-keep 'file-remote-p)
(add-to-list 'recentf-exclude "^/su:")
(add-to-list 'recentf-exclude "^/sudo:")
(add-to-list 'recentf-exclude "\\.gpg$")
;;(recentf-exclude (quote (".ftp:.*" ".sudo:.*")))
;;(recentf-keep (file-remote-p file-readable-p))

;; Unicad is short for Universal Charset Auto Detector. It is an Emacs-Lisp port of Mozilla Universal Charset Detector.
;; Unicad helps Emacs to guess the correct coding system when opening a file. It's designed to work automatically and
;; quietly without user interaction.
;; (require 'unicad nil t)
;; (unicad-enable)
;; (add-hook 'kill-emacs-hook 'unicad-disable)

;; load swbuff.el
;;(load "~/.emacs.d/elisp/swbuff")
;; (require 'swbuff)
;; (global-set-key (kbd "") 'swbuff-switch-to-previous-buffer)
;; (global-set-key (kbd "") 'swbuff-switch-to-next-buffer)
;; (setq swbuff-exclude-buffer-regexps
;;       '("^ " "\\*.*\\*"))
;; (setq swbuff-status-window-layout 'scroll)
;; (setq swbuff-clear-delay 1)
;; (setq swbuff-separator "|")
;; (setq swbuff-window-min-text-height 1)

;; load tabbar.el 你可以用 customize-group RET tabbar RET 来设置它的选项。
                                        ;(load "~/.emacs.d/elisp/tabbar")
;;(if (not window-system)
;;    (progn
;;      (tabbar-mode)))
;;(tabbar-mode)
;;(global-set-key (kbd "C-=") 'tabbar-backward-group)
;;(global-set-key (kbd "C--") 'tabbar-forward-group)
;;(global-set-key (kbd "C-9") 'tabbar-backward)
;;(global-set-key (kbd "C-0") 'tabbar-forward)


(autoload 'wajig "wajig"
  "Create a *wajig* buffer." t)

(autoload 'ascii-table-show "ascii-table-show" "Create a buffer and print the ascii table" t)

;;; go-to-char
(defun pl/go-to-char (n char)
  "Move forward to Nth occurence of CHAR.
Typing `pl/go-to-char-key' again will move forwad to the next
Nth occurence of CHAR."
  (interactive "p\ncGo to char: ")
  (search-forward (string char) nil nil n)
  (while (char-equal (read-char)
                     char)
    (search-forward (string char) nil nil n))
  (setq unread-command-events (list last-input-event)))
(define-key global-map (kbd "C-c C-a") 'pl/go-to-char)




;;----------------------------------------------------------------------------------------------------
;;;line number
;;----------------------------------------------------------------------------------------------------

;;load setnu setnu-plus 这两个包可以用来显示文件的行号。并且根据是否空行和文件行的语法加亮显示不同的数字颜色
;;(load "~/.emacs.d/elisp/setnu")
;;(load "~/.emacs.d/elisp/setnu-plus")
;;(setnu-mode -1)
;;(setq setnu-line-number-format "%3d| ")


;;===========================================================
;; ChunYe Wang
;;(require 'display-line-number)
;; 如果想所有打开的文件都显示行的话就打开下面的注释
;;(global-display-line-number-mode 1)
;; 设置显示格式
;;(setq display-line-number-format "%2d| ")
;; 在 tool bar 上增加一个图标，
;; 注意: 一定要在 load-path 中 可以找到 display-line-nuber.xpm 文件才行。
;;
;;(tool-bar-add-item "display-line-number"
;;                  'display-line-number-mode
;;                   'display-line-number-mode
;;                   :help "display line number!"
;;                   :button (cons :toggle  '(and (boundp
;;                                                display-line-number-mode)
;;                                              display-line-number-mode)))

;;
;; 使用方法
;; M-x display-line-number-mode
;; 用来 toggle 显示行号的模式
;; M-x display-line-number-mode-on
;; 启动显示行号的模式
;; M-x display-line-number-mode-off
;; 关闭显示行号的模式
;; 仅对某种 mode 启动显示行号的模式
;; (add-hook 'c-mode-hook 'display-line-number-mode)

;;; line numbers
;; (global-linum-mode 1) ; always show line numbers
(eval-after-load "linum"
  '(progn
    (set-face-foreground 'linum "#5cb2b3")
    (set-face-background 'linum "#222222")

    (setq linum-format 'my-linum-format) ; dynamic
    (defun my-linum-format (line)
      (propertize (format (let ((w (length (number-to-string
                                            (count-lines
                                             (point-min)
                                             (point-max))))))
                            (concat "%" (number-to-string w) "d|"))
                          line)
                  'face 'linum))))


;;; highlight current line in buffer
(require 'hl-line)
(eval-after-load "hl-line"
  '(progn
     (global-hl-line-mode t)
     (set-face-background 'hl-line "#222222")))

;; (require 'highlight-current-line)
;; (highlight-current-line-on t)

;;; buffer action
;; http://xwl.appspot.com/ref/buffer-action.el
(autoload 'buffer-action-compile "buffer-action")
(autoload 'buffer-action-run "buffer-action")
(setq buffer-action-table '((c-mode    "gcc -Wall -ggdb -Wextra %f -lm -o %n" "%n" "./%n")
                            (c++-mode  "g++ %f -lm -o %n" "%n" "./%n")
                            (java-mode "javac %n" "%n.class" "java %n")
                            (makefile-mode "make" nil nil)
                            ("\\.pl$" "perl -cw %f" nil "perl -s %f")
                            ("\\.php$" nil nil "php %f")
                            ("\\.tex$" "latex %f" "%n.dvi" "xdvi %n.dvi &")
                            (texinfo-mode (lambda ()
                                            (save-excursion
                                              ;; (texinfo-make-menu)
                                              (texinfo-all-menus-update)
                                              (texinfo-every-node-update)
                                              (save-buffer))
                                            (makeinfo-buffer))
                             "%n.info"
                             (lambda ()
                               (Info-revert-find-node
                                (replace-regexp-in-string
                                 "\\.texinfo*$" ".info" (buffer-action-replace "%F"))
                                (makeinfo-current-node))))
                            (emacs-lisp-mode (lambda ()
                                               (byte-compile-file (buffer-action-replace "%f")))
                             "%n.elc"
                             eval-buffer)
                            ("\\.info$" nil nil (lambda () (info (buffer-file-name))))
                            ("\\.dot$" "dot -Tjpg %f -o %n.jpg" "%n.png" "qiv %f &")))


;;可以为重名的 buffer 在前面加上其父目录的名字来让 buffer 的名字区分开来，而不是单纯的加一个没有太多意义的序号
(require 'uniquify)
;;当寻找一个同名的文件，自动关联上那个文件？
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "\\`\\*")
;; (toggle-uniquify-buffer-names)

;;===========================================================================
;;about header2

;;(load "~/.emacs.d/elisp/header2")
;; Update file headers when write files.
;;(add-hook 'write-file-hooks 'update-file-header)
;; Create headers for file buffers in my favorite modes.
;;(add-hook 'emacs-lisp-mode-hook 'auto-make-header)
;;(add-hook 'c-mode-common-hook   'auto-make-header)

;; (defsubst pl/header-author ()
;;   "Insert current user's name (`user-full-name') as this file's author."
;;   (insert header-prefix-string "Author: " (user-full-name) " <kirby1985@gmail.com>" "\n")
;;   )

;; (defsubst pl/header-svn-keyword ()
;;   "Insert $Id$."
;;   (insert header-prefix-string "$Id$" "\n"))

;; (setq make-header-hook '(
;;                          ;;header-mode-line
;;                          header-title
;;                          pl/header-svn-keyword
;;                          ;;header-blank
;;                          ;;header-file-name
;;                          header-description
;;                          ;;header-status
;;                          ;;header-author
;;                          pl/header-author
;;                          ;;header-maintainer
;;                          ;;header-copyright
;;                          header-creation-date
;;                          ;;header-rcs-id
;;                          ;;header-version
;;                          ;;header-sccs
;;                          ;;header-modification-date
;;                          ;;header-modification-author
;;                          ;;header-update-count
;;                          ;;xbheader-url
;;                          ;;header-keywords
;;                          ;;header-compatibility
;;                          ;;header-blank
;;                          ;;header-lib-requires
;;                          ;;header-end-line
;;                          ;;header-commentary
;;                          ;;header-blank
;;                          ;;header-blank
;;                          ;;header-blank
;;                          ;;header-end-line
;;                          ;;header-history
;;                          ;;header-blank
;;                          ;;header-blank
;;                          ;; header-rcs-log
;;                          header-end-line
;;                          ;;header-free-software
;;                          ;;header-code
;;                          ;;header-eof
;;                          ))

;;===========================================================================


;;(load "~/.emacs.d/elisp/htmlize")

;;It converts PDF, PS and DVI files to a set of PNG files, one PNG for each page, and displays
;; git-clone http://www.tsdh.de/repos/git/doc-view.git
;;(load "~/.emacs.d/elisp/doc-view")

;;----------------------------------------------------------------------------------------------------
;;Moving lines
;;----------------------------------------------------------------------------------------------------

;; Many times you'll kill a line with the intention of pasting it back a couple of lines up/below.

(global-set-key (kbd "H-k") 'move-line-up)
(global-set-key (kbd "H-j") 'move-line-down)

(defun move-line (&optional n)
  "Move current line N (1) lines up/down leaving point in place."
  (interactive "p")
  (when (null n)
    (setq n 1))
  (let ((col (current-column)))
    (beginning-of-line)
    (next-line 1)
    (transpose-lines n)
    (previous-line 1)
    (move-to-column col)))

(defun move-line-up (n)
  "Moves current line N (1) lines up leaving point in place."
  (interactive "p")
  (move-line (if (null n) -1 (- n))))

(defun move-line-down (n)
  "Moves current line N (1) lines down leaving point in place."
  (interactive "p")
  (move-line (if (null n) 1 n)))
;;Moving lines ends there---------------------------------------------------------------------------------------


;;; Open new line
(defun pl/open-line-with-indent ()
  "open-line with indent without moving cursor."
  (interactive)
  (save-excursion
    (open-line 1)
    (next-line 1)
    (indent-according-to-mode)
    (next-line -1)))

(global-set-key (kbd "C-o") 'pl/open-line-with-indent)


;; behave like vi's o command
(defun pl/vi-open-next-line (arg)
  "Move to the next line (like vi) and then opens a line."
  (interactive "p")
  (end-of-line)
  (open-line arg)
  (next-line 1)
  (indent-according-to-mode))

(global-set-key (kbd "M-o") 'pl/vi-open-next-line)


;; behave like vi's O command
;; (global-set-key (kbd "M-O") 'pl/vi-open-above-line)
;; (defun pl/vi-open-above-line (arg)
;;   "Open a new line before the current one."
;;   (interactive "p")
;;   (beginning-of-line)
;;   (open-line arg)
;;   (indent-according-to-mode))


;;------------------------------------------------------------------------------------------
;;Shell command on region
;;------------------------------------------------------------------------------------------
;;A somewhat insanely powerful trick, evaluate a region via a shell command and replace the region with the resulting
;;output. Normally you would access this command via C-u M-| but since we're trying to optimize things a bit:

;; (local-set-key [(meta ?!)] 'custom-shell-command-on-region)

;; (defun custom-shell-command-on-region nil
;;   "Replace region with ``shell-command-on-region''.
;; By default, this will make mark active if it is not and then
;; prompt you for a shell command to run and replaces region with
;; the results.  This is handy for doing things like getting
;; external program locations in scripts and running grep and
;; whatnot on a region."
;;   (interactive)
;;   (save-excursion
;;     (if (equal mark-active nil)
;;         (push-mark nil nil -1))
;;     (setq string
;;           (read-from-minibuffer "Shell command on region: " nil nil nil
;;                                 'shell-command-history))
;;     (shell-command-on-region (region-beginning) (region-end) string -1)
;;                                         ; Get rid of final newline cause I normally did by hand anyway.
;;     (delete-char -1)))
;;Shell-command-on-region----------------------------------------------------------------------

;;Global mark ring
;;Emacs keeps track of the locations where you were previously working and allows you to re-visit them by issuing the
;;pop-global-mark command (C-u C-Space).

;;-----------------------------------------------------------------------------------------------------------
;;Bookmarking - history stack
;;-----------------------------------------------------------------------------------------------------------
;;Use the following to maintain a stack of buffer locations which you can use as a simple bookmarking system.
(global-set-key (kbd "C-c r SPC") 'point-stack-push)
(global-set-key (kbd "C-c r p") 'point-stack-pop)

(defvar point-stack nil)
(defun point-stack-push ()
  "Push current location and buffer info onto stack."
  (interactive)
  (message "Location marked.")
  (setq point-stack (cons (list (current-buffer) (point)) point-stack)))

(defun point-stack-pop ()
  "Pop a location off the stack and move to buffer"
  (interactive)
  (if (null point-stack)
      (message "Stack is empty.")
      (switch-to-buffer (caar point-stack))
      (goto-char (cadar point-stack))
      (setq point-stack (cdr point-stack))))


;;Turns tabs into spaces
;; (defun pl/untabify ()
;;   "My untabify function as discussed and described at
;;  http://www.jwz.org/doc/tabs-vs-spaces.html
;;  and improved by Claus Brunzema:
;;  - return nil to get `write-contents-hooks' to work correctly
;;    (see documentation there)
;;  - `make-local-hook' instead of `make-local-variable'
;;  - when instead of if"
;;   (save-excursion
;;     (goto-char (point-min))
;;     (when (search-forward "\t" nil t)
;;       (untabify (1- (point)) (point-max)))
;;     nil))
;; (add-hook 'some-mode-hook
;;           '(lambda ()
;;             (make-local-hook 'write-contents-hooks)
;;             (add-hook 'write-contents-hooks 'pl/untabify nil t)))


;;Enable undoc; a mode which edits MS Word .doc files.
;;<http://www.ccs.neu.edu/home/guttman/undoc.el>
(autoload 'undoc "undoc" "A minor mode which kills MS Word files dead." t)
(autoload 'undoc-current-buffer "undoc" "" t)
(autoload 'undoc-region-after-mime-decode "undoc" "" t)

;;The normal man command doesn't allow tab-completion.
;;iman is a wrapper around man which does so allow.
;;<http://homepage1.nifty.com/bmonkey/emacs/elisp/iman.el>
(autoload 'iman "iman" "Call the viewers of man pages and GNU Info with completion."
          t nil)

;; A game for fast typers! A game for Emacs!
(autoload 'typing-of-emacs "typing" "The Typing Of Emacs, a game." t)

;; Sets +x on scripts stating with a shebang
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;;--------------------------------------------------------------------------------
;; shell-toggle.el
;; shell-toggle-patched.el eshell port.
(autoload 'shell-toggle "shell-toggle"
  "Toggles between the *shell* buffer and whatever buffer you are editing." t)
(autoload 'shell-toggle-cd "shell-toggle"
  "Pops up a shell-buffer and insert a \"cd <file-dir>\" command." t)
(global-set-key (kbd "C-c g S") 'shell-toggle)    ;see also shell-toggle
(global-set-key (kbd "C-c g s") 'shell-toggle-cd) ;see also shell-toggle-cd

;;-----------------------------------------------------------------------------------------------

;; (defun pl/transpose-buffers ()
;;   "Transpose this buffer and the buffer in other window"
;;   (interactive)
;;   (let ((current-window-buffer (current-buffer)))
;;     (other-window 1)
;;     (let ((other-window-buffer (current-buffer)))
;;       (switch-to-buffer current-window-buffer)
;;       (other-window -1)
;;       (switch-to-buffer other-window-buffer)))
;;   (other-window 1))


;; (defun pl/value-to-string (value)
;;   "Convert VALUE to string.
;; This function will automatically identify the type of VALUE, and invoke
;; the appropiate conversion function"
;;   (cond ((symbolp value)
;;          (symbol-name value))
;;         ((numberp value)
;;          (number-to-string value))
;;         (t
;;          (error "Cannot convert value to string."))))


;; (defun pl/read-lines-in-buffer (&optional buffer)
;;   "Return list of lines in current buffer.
;; If BUFFER if non-nil, switch to BUFFER before reading lines. The list returned
;; will be in reverse with regard to the sequence of lines in the buffer read.
;; Empty lines will not be ignored."
;;   (save-excursion
;;     (when buffer
;;       (set-buffer buffer))
;;     (let (stringlist)
;;       ;; Start from beginning of buffer, remembering to save point.
;;       (goto-char (point-min))
;;       (while (not (eobp))
;;         ;; Because we push the new line to the front of the list, and we start
;;         ;; from the beginning of the buffer, the list will be backwards.
;;         ;; Should this be fixed?
;;         (push (buffer-substring-no-properties
;;                (line-beginning-position)
;;                (line-end-position))
;;               stringlist)
;;         (forward-line))
;;       stringlist)))

;;----------------------------------------------------------------------------------------------------
;;convert a buffer from dos ^M end of lines to unix end of lines
;;----------------------------------------------------------------------------------------------------
(defun dos2unix ()
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\r" nil t) (replace-match "")))

;;vice versa
(defun unix2dos ()
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\n" nil t) (replace-match "\r\n")))
;;----------------------------------------------------------------------------------------------------


;;----------------------------------------------------------------------------------------------------
;; Author: Patrick Gundlach
;; nice mark - shows mark as a highlighted 'cursor' so user 'always'
;; sees where the mark is. Especially nice for killing a region.

;; (defvar pl/mark-overlay nil
;;   "Overlay to show the position where the mark is")
;; (make-variable-buffer-local 'pl/mark-overlay)

;; (put 'pl/mark-mark 'face 'secondary-selection)

;; (defvar pl/mark-old-position nil
;;   "The position the mark was at. To be able to compare with the
;; current position")

;; (defun pl/show-mark ()
;;   "Display an overlay where the mark is at. Should be hooked into
;; activate-mark-hook"
;;   (unless pl/mark-overlay
;;     (setq pl/mark-overlay (make-overlay 0 0))
;;     (overlay-put pl/mark-overlay 'category 'pl/mark-mark))
;;   (let ((here (mark t)))
;;     (when here
;;       (move-overlay pl/mark-overlay here (1+ here)))))

;; (defadvice  exchange-point-and-mark (after pl/mark-exchange-point-and-mark)
;;   "Show visual marker"
;;   (pl/show-mark))

;; (ad-activate 'exchange-point-and-mark)
;; (add-hook 'activate-mark-hook 'pl/show-mark)
;; ----------------------------------------------------------------------------------------------------
;; mark word be bound to M-@
;; (global-set-key (kbd "C-c SPC") 'pl/mark-current-word)
(global-set-key (kbd "C-c k w") 'pl/kill-current-word)
(defun pl/mark-current-word ()
  "Put point at beginning of current word, set mark at end."
  (interactive)
  (let* ((opoint (point))
         (word (current-word))
         (word-length (length word)))
    (if (save-excursion
          ;; Avoid signaling error when moving beyond buffer.
          (if (> (point-min)  (- (point) word-length))
              (goto-char (point-min))
              (forward-char (- (length word))))
          (search-forward word (+ opoint (length word))
                          'noerror))
        (progn (push-mark (match-end 0) nil t)
               (goto-char (match-beginning 0)))
        (error "No word at point"))))

;; (defun mark-current-word ()
;;   "Put point at beginning of current word, set mark at end."
;;   (interactive)
;;   (unless (or (looking-at "\\<") (not (current-word t)))
;;     (backward-word))
;;   (mark-word nil t))


(defun pl/kill-current-word ()
  "kill current word."
  (interactive)
  (let* ((opoint (point))
         (word (current-word))
         (word-length (length word)))
    (if (save-excursion
          ;; Avoid signaling error when moving beyond buffer.
          (if (> (point-min)  (- (point) word-length))
              (goto-char (point-min))
              (forward-char (- (length word))))
          (search-forward word (+ opoint (length word))
                          'noerror))
        (kill-region (goto-char (match-beginning 0))
                     (match-end 0))
        (error "No word at point"))))

;;; replace-word-at-point
(autoload 'word-at-point "thingatpt" nil t)

(defun replace-word-at-point (from to)
  "Replace word at point."
  (interactive (let ((from (word-at-point)))
                 (list from (query-replace-read-to from "Replace" nil))))
  (query-replace from to))

(global-set-key (kbd "C-c r w") 'replace-word-at-point)

(require 'gse-number-rect)
(global-set-key "\C-xru" 'gse-number-rectangle)

(defun yank-secondary ()
  "Insert the secondary selection at point.
  Moves point to the end of the inserted text.  Does not change mark."
  (interactive) (insert (x-get-selection 'SECONDARY)))


;; add in your commonly used packages/include directories here, for
;; example, SDL or OpenGL. this shouldn't slow down cpp, even if
;; you've got a lot of them
;; (setq c-eldoc-includes "`pkg-config gtk+-2.0 --cflags` -I./ -I../ ")
;; (load "c-eldoc")
;; (add-hook 'c-mode-hook 'c-turn-on-eldoc-mode)


;;----------------------------------------------------------------------------------------------------
;;Copy current line to next line
;;----------------------------------------------------------------------------------------------------

(defun ue-select-line-down ()
  "like Shift+down in UltraEdit."
  (interactive)
  (let ((s (point)))
    (setq next-line-add-newlines t)
    (next-line 1)
    (setq next-line-add-newlines nil)
    (kill-new (buffer-substring s (point)))))

(defun pl/dup-line-down ()
  "duplicate this line at next line"
  (interactive)
  (let ((c (current-column)))
    (beginning-of-line)
    (ue-select-line-down)
    (beginning-of-line)
    (yank)
    (previous-line 1)
    (move-to-column c)))
(global-set-key (kbd "<M-S-return>") 'pl/dup-line-down)
;;----------------------------------------------------------------------------------------------------

(defun pl/strip-all-blank-lines ()
  "Strip all blank lines in current buffer."
  (interactive)
  (save-excursion
    (while (re-search-forward "^[ \t]*\n" nil t)
      (replace-match "" t t))))

;; resolve file names
(defun pl/resolve-file-name (file type)
  "Resolve file name in various ways.

file is the abosolute filename.

type stands for different kinds of resolve.

 F  absolute pathname            ( /usr/local/bin/netscape.bin )
 f  file name without directory  ( netscape.bin )
 n  file name without extention  ( netscape )
 e  extention of file name       ( bin )"
  (cond
    ((string= type "F") file)
    ((string= type "f") (file-name-nondirectory file))
    ((string= type "n") (file-name-sans-extension (file-name-nondirectory file)))
    (t (file-name-extension file))))

;; insert line number before each line.
(defun pl/numerate-lines ()
  "Insert line numbers into buffer"
  (interactive)
  (save-excursion
    (let ((max (count-lines (point-min) (point-max)))
          (line 1))
      (goto-char (point-min))
      (while (<= line max)
        (insert (format "%4d " line))
        (beginning-of-line 2)
        (setq line (+ line 1))))))


;; a simple way of aligning columns
(defun his-align-cols (start end max-cols)
  "Align text between point and mark as columns.  Columns are separated by
whitespace characters.  Prefix arg means align that many columns. (default
is all)"
  (interactive "r\nP")
  (save-excursion
    (let ((p start)
          pos
          end-of-line
          word
          count
          (max-cols (if (numberp max-cols) (max 0 (1- max-cols)) nil))
          (pos-list nil)
          (ref-list nil))

      ;; find the positions
      (goto-char start)
      (while (< p end)
        (beginning-of-line)
        (setq count 0)
        (setq end-of-line (save-excursion (end-of-line) (point)))
        (re-search-forward "^\\s-*" end-of-line t)
        (setq pos (current-column))     ;start of first word
        (if (null (car ref-list))
            (setq pos-list (list pos))
            (setq pos-list (list (max pos (car ref-list))))
            (setq ref-list (cdr ref-list)))
        (while (and (if max-cols (< count max-cols) t)
                    (re-search-forward "\\s-+" end-of-line t))
          (setq count (1+ count))
          (setq word (- (current-column) pos))
          ;; length of next word including following whitespaces
          (setq pos (current-column))
          (if (null (car ref-list))
              (setq pos-list (cons word pos-list))
              (setq pos-list (cons (max word (car ref-list)) pos-list))
              (setq ref-list (cdr ref-list))))
        (while ref-list
          (setq pos-list (cons (car ref-list) pos-list))
          (setq ref-list (cdr ref-list)))
        (setq ref-list (nreverse pos-list))
        (forward-line)
        (setq p (point)))

      ;; align the cols starting with last row
      (setq pos-list (copy-sequence ref-list))
      (setq start
            (save-excursion (goto-char start) (beginning-of-line) (point)))
      (goto-char end)
      (beginning-of-line)
      (while (>= p start)
        (beginning-of-line)
        (setq count 0)
        (setq end-of-line (save-excursion (end-of-line) (point)))
        (re-search-forward "^\\s-*" end-of-line t)
        (goto-char (match-end 0))
        (setq pos (nth count pos-list))
        (while (< (current-column) pos)
          (insert-char ?\040 1))
        (setq end-of-line (save-excursion (end-of-line) (point)))
        (while (and (if max-cols (< count max-cols) t)
                    (re-search-forward "\\s-+" end-of-line t))
          (setq count (1+ count))
          (setq pos   (+  pos (nth count pos-list)))
          (goto-char (match-end 0))
          (while (< (current-column) pos)
            (insert-char ?\040 1))
          (setq end-of-line (save-excursion (end-of-line) (point))))
        (forward-line -1)
        (if (= p (point-min)) (setq p (1- p))
            (setq p (point)))))))

;; count Chinese, English words
(defun pl/count-ce-word (beg end)
  "Count Chinese and English words in marked region."
  (interactive "r")
  (let ((cn-word 0)
        (en-word 0)
        (total-word 0)
        (total-byte 0))
    (setq cn-word (count-matches "\\cc" beg end)
          en-word (count-matches "\\w+\\W" beg end))
    (setq total-word (+ cn-word en-word)
          total-byte (+ cn-word (abs (- beg end))))
    (message (format "Total: %d (cn: %d, en: %d) words, %d bytes."
                     total-word cn-word en-word total-byte))))

;; pl/word-count-analysis (how many times a word has appeared).
(defun pl/word-count-analysis (start end)
  "Count how many times each word is used in the region.
    Punctuation is ignored."
  (interactive "r")
  (let (words)
    (save-excursion
      (goto-char start)
      (while (re-search-forward "\\w+" end t)
        (let* ((word (intern (match-string 0)))
               (cell (assq word words)))
          (if cell
              (setcdr cell (1+ (cdr cell)))
              (setq words (cons (cons word 1) words))))))
    (when (interactive-p)
      (message "%S" words))
    words))


(defun pl/hide-buffer ()
  "Hide current buffer, and enlarge the other one if exists."
  (interactive)
  (delete-windows-on (buffer-name)))

(defun pl/list-ref (list ref)
  "Return the ref-th element of list."
  (if (= ref 0)
      (car list)
      (pl/list-ref (cdr list) (1- ref))))

(defun pl/info (file)
  (interactive
   (list (read-file-name "info: ")))
  (info file))

;; dos <--> unix
(defun his-dos2unix ()
  "\r\n --> \r."
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\r" nil t)
    (replace-match "")))

(defun his-unix2dos ()
  "\n --> \r\n."
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\n" nil t)
    (replace-match "\r\n")))

(defun pl/delete-line (&optional arg)
  "Delete the rest of the current line; if no nonblanks there, delete thru newline.
With prefix argument, delete that many lines from point.
Negative arguments delete lines backward.
With zero argument, deletes the text before point on the current line.

Note its difference between `pl/delete-line' and `kill-line' is
that, the deleted contents won't be inserted to the `kill-ring'."
  (if arg
      (dotimes (i arg)
        (delete-region (point) (save-excursion (forward-line)
                                               (point))))
      (if (eolp)
          (delete-region (point) (save-excursion (forward-line)
                                                 (point)))
          (delete-region (point) (save-excursion (end-of-line)
                                                 (point))))))

;; (defun pl/soft-kill-ring-save (beg end)
;;   "Same as `kill-ring-save' except it will convert hard newlines to soft newlines.
;; This could be useful for copying texts from Emacs and pasting it to blog websites."
;;   (interactive "r")
;;   (let ((content (buffer-substring-no-properties beg end)))
;;     (with-temp-buffer
;;       (insert content)
;;       (insert "\n")
;;       (goto-char (point-min))
;;       (move-beginning-of-line 2)
;;       (while (not (eobp))
;;         (if (looking-at "\n")
;;             (move-beginning-of-line 3)
;;           (if (looking-at "[[:ascii:]]")
;;               (progn (backward-delete-char-untabify 1)
;;                      (insert " "))
;;             (backward-delete-char-untabify 1))
;;           (move-beginning-of-line 2)))
;;       (copy-region-as-kill (point-min) (point-max)))))




;;----------------------------------------------------------------------------------------------------
;;; Cool utility function to refresh all open buffers
;;----------------------------------------------------------------------------------------------------
(defun pl/revert-all-buffers()
  "Refreshs all open buffers from their respective files"
  (interactive)
  (let* ((list (buffer-list))
         (buffer (car list)))
    (while buffer
      (if (string-match "\\*" (buffer-name buffer))
          (progn
            (setq list (cdr list))
            (setq buffer (car list)))
          (progn
            (set-buffer buffer)
            (revert-buffer t t t)
            (setq list (cdr list))
            (setq buffer (car list))))))
  (message "Refreshing open files"))
;;----------------------------------------------------------------------------------------------------

;;
;; Para poner quotes/backquotes alrededor de un string.
;;
;; (defalias 'single-quote-balanced (read-kbd-macro
;;                                   " C-q ` ESC C-s [ SPC C-q TAB C-q LFD ] RET <left> ' "))
;; (global-set-key [?\C-`] 'single-quote-balanced)

;; (defalias 'single-quote-balanced-pos2 (read-kbd-macro
;;                                        (concat "<left> C-SPC ESC C-r [ SPC C-q LFD C-q "
;;                                                "TAB ] RET <right> ` C-x C-x <right> '")))

;; (defun single-quote-balanced-pos3()
;;   (interactive)
;;   ;;  (message "word: \"%s\"" (thing-at-point 'word))
;;   (save-excursion
;;     (forward-word -1)
;;     (insert "`")
;;     (forward-word +1)
;;     (insert "'")))

;; (defun single-quote-balanced-pos()
;;   (interactive)
;;   (let ((white-space-re "\\(\\s-\\|\n\\)")
;;         (end-pos))
;;     (save-excursion
;;       (cond ((re-search-forward white-space-re nil t)
;;              (forward-char -1))
;;             (t (goto-char (point-max))))
;;       (insert "'")
;;       (setq end-pos (point))
;;       (forward-char -1)
;;       (cond ((re-search-backward white-space-re nil t)
;;              (forward-char +1))
;;             (t (goto-char (point-min))))
;;       (insert "`"))
;;     (goto-char end-pos)
;;     (forward-char +1)))


;; (defun xmsg (string &optional geom process-name)
;;   "Invoke xmessage(1) to display message STRING.
;; However, do nothing if `window-system' is not `x'.  Other args
;; are optional: GEOM is the window geometry (default \"+0+0\");
;; PROCESS-NAME is the process object's name (default \"xmessage\")."
;;   (interactive "sMessage: ")
;;   (when (eq 'x window-system)
;;     (start-process (or process-name "xmessage") nil "xmessage"
;;                    "-geometry" (or geom "+0+0")
;;                    (or string "Emacs says hi!"))))

;; It's often nice to find the true path to a file or directory.
;; (defun resolve-sym-link ()
;;   "Replace the string at the point with the true path."
;;   (interactive)
;;   (beginning-of-line)
;;   (let* ((file (buffer-substring (point)
;;                                  (save-excursion (end-of-line) (point))))
;;          (file-dir (file-name-directory file))
;;          (file-true-dir (file-truename file-dir))
;;          (file-name (file-name-nondirectory file)))
;;     (delete-region (point) (save-excursion (end-of-line) (point)))
;;     (insert (concat file-true-dir file-name))))

;; (define-key minibuffer-local-completion-map (kbd "C-r") 'resolve-sym-link)

;; (defconst grepsource-command
;;   "find . -name "*.c" -or -name "*.cc" -or -name "*.cpp" -or -name "*.m" -or -name "*.mm" -or -name "*.java" -or -name "*.h" -or -name "*.hh" -or -name "*.hpp" -or -name "*.el" | xargs grep -n "
;;   "Base command for grepsource. (Basically quoted version of
;; whatever my current grepsource alias is.")
;; (defun grepsource (cmd-args)
;;   "Invoke an in-emacs equivalent of my grepsource bash alias,
;; defaulting withing the current project."
;;   (interactive (list (read-from-minibuffer "Grep project for string: ")))
;;   (let ((default-directory (or local-project-root default-directory)))
;;     (grep (concat grepsource-command """
;;                   (replace-regexp-in-string """ "\\\\"" cmd-args) """))))


;; (defun intelligent-close ()
;;   "quit a frame the same way no matter what kind of frame you are on.
;; This method, when bound to C-x C-c, allows you to close an emacs frame the
;; same way, whether it's the sole window you have open, or whether it's
;; a "child" frame of a "parent" frame.  If you're like me, and use emacs in
;; a windowing environment, you probably have lots of frames open at any given
;; time.  Well, it's a pain to remember to do Ctrl-x 5 0 to dispose of a child
;; frame, and to remember to do C-x C-x to close the main frame (and if you're
;; not careful, doing so will take all the child frames away with it).  This
;; is my solution to that: an intelligent close-frame operation that works in
;; all cases (even in an emacs -nw session).
;; Stolen from http://www.dotemacs.de/dotfiles/BenjaminRutt.emacs.html."
;;   (interactive)
;;   (if (eq (car (visible-frame-list)) (selected-frame))
;;       ;;for parent/master frame...
;;       (if (> (length (visible-frame-list)) 1)
;;           ;;close a parent with children present
;;           (delete-frame (selected-frame))
;;           ;;close a parent with no children present
;;           (save-buffers-kill-emacs))
;;       ;;close a child frame
;;       (delete-frame (selected-frame))))



;;;_. boxquote
(require 'boxquote nil t)


(global-set-key (kbd "C-c b y")   'boxquote-yank)
(global-set-key (kbd "C-c b r")   'boxquote-region)
(global-set-key (kbd "C-c b u")   'boxquote-unbox-region)
(global-set-key (kbd "C-c b t")   'boxquote-title)
(global-set-key (kbd "C-c b i")   'boxquote-insert-file)
(global-set-key (kbd "C-c b k")   'boxquote-kill)
(global-set-key (kbd "C-c b s")   'boxquote-shell-command)

(global-set-key (kbd "C-c b b")   'boxquote-buffer)
(global-set-key (kbd "C-c b p")   'boxquote-paragraph)
(global-set-key (kbd "C-c b n")   'boxquote-narrow-to-boxquote)

(global-set-key (kbd "C-c b w")   'boxquote-where-is)
(global-set-key (kbd "C-c b d f") 'boxquote-describe-function)
(global-set-key (kbd "C-c b d k") 'boxquote-describe-key)
(global-set-key (kbd "C-c b d v") 'boxquote-describe-variable)


;; (when (require 'autoinfo nil t)
;;   (setq-default autoinfo-mode nil))
;; I use w3m-decode-entities-string to let it able to display unicode
;; (defun autoinfo-handle-google-response (status)
;;   "Handle response returned by Google."
;;   (message "")
;;   (let ((response (buffer-string)))
;;     (funcall autoinfo-show-result-function
;;              (if (string-match "Definitions of.*?\\(<li>.*?\\)<br>" response)
;;                  (let ((results (match-string 1 response)))
;;                    (concat "Definitions by Google:\n"
;;                            (w3m-decode-entities-string (replace-regexp-in-string "<li>" "\n- " results))))

;;                  "No definition found"))))

(when (require 'mm-url nil t)
  (require 'mm-url)
  (defun google-define-word-or-phrase (query)
    (interactive "sInsert word or phrase to search: ")
    (let* ((url (concat "http://www.google.com.pe/search?hl=en&q=define%3A"
                        (replace-regexp-in-string " " "+" query)))
           (definition
            (save-excursion
              (with-temp-buffer
                (mm-url-insert url)
                (goto-char (point-min))
                (if (search-forward "No definitions found of " nil t)
                    "No definitions found"
                    (buffer-substring (search-forward "<li>") (- (search-forward "<") 1)))))))
      (message "%s: %s" query definition)))

  (global-set-key (kbd "C-c d g") 'google-define-word-or-phrase))





;;; fuzz-match.el
(require 'fuzzy-match)

(autoload 'lisp-spell-symbol "fuzzy-match"
  "Perform spell checking on Lisp symbol preceding point." t)
;; This will define the key M-# (ESC #) to call `lisp-spell-symbol'.
(define-key esc-map "#" 'lisp-spell-symbol)

;;; replace-recent-char
(global-set-key (kbd "M-R")  'replace-recent-char)
(defun replace-recent-char ()
  "Replace-recent-character is interactive function for quick corrections of
recenlty typed text. It first prompts for character to search backwards. If
such character is found, following options are shown:
1, repeat the character to search in previous text.
2, M-R for delete of the found character.
3, C-t for trasposition of the found and the following character.
4, TAB for promt for character to insert after the found character.
5, ESC for no operation.
6, Any other insertable character will replace found character."
  (interactive)
  (let* ((repev (read-char "Replace back character:" t))
         (repstr (string repev)))
    (labels
        ((check () (if (fboundp 'flyspell-word) (flyspell-word)))
         (rec ()
           (save-excursion
             (let ((point (search-backward repstr (point-at-bol -3) t)))
               (if point
                   (let (repwithev
                         (ov (make-overlay point (1+ point))))
                     (overlay-put ov 'face isearch)
                     (overlay-put ov 'priority 1)
                     (unwind-protect
                          (setq repwithev
                                (read-char "Replace with (repeat  previous, M-R  delete, C-t  transpose, TAB  insert):" t))
                       (delete-overlay ov))
                     (cond ((equal repwithev (event-convert-list '(meta ?R)))
                            (delete-char 1)
                            ;; (check)
                            (message (format "Character \"%s\" deleted." repstr)))
                           ((equal repwithev (event-convert-list '(control ?t)))
                            (forward-char)
                            (transpose-chars 1)
                            ;; (check)
                            (message "Transposed."))
                           ((equal repwithev ?\t)
                            (forward-char)
                            (insert-char (read-char "Character to insert after match:" t) 1 t)
                            ;; (check)
                            (message "Insert."))
                           ((equal repwithev ?\e)
                            (message "Replace aborted."))
                           ((equal repwithev repev)
                            (rec))
                           (t
                            (delete-char 1)
                            (insert-char repwithev 1 t)
                            ;; (check)
                            (message
                             (format "Replace \"%s\" -> \"%s\" done." repstr (string repwithev))))))
                   (message (format "\"%s\" is not recent." repstr)))))))
      (rec))))

;; (dec|inc)rement number at point
;; The following functions allow you increment or decrement what they think is a number under point:
;; (defun increment-number-at-point (&optional amount)
;;   "Increment the number under point by `amount'"
;;   (interactive "p")
;;   (let ((num (number-at-point)))
;;     (when (numberp num)
;;       (let ((newnum (+ num amount))
;;             (p (point)))
;;         (save-excursion
;;           (skip-chars-backward "-.0123456789")
;;           (delete-region (point) (+ (point) (length (number-to-string num))))
;;           (insert (number-to-string newnum)))
;;         (goto-char p)))))

;; (defun decrement-number-at-point (&optional amount)
;;   (interactive "p")
;;   "Decrement the number under point by `amount'"
;;   (increment-number-at-point (- (abs amount))))
;; I don't use the arrow keys so I have the above functions bound like this:
;; (define-key global-map (kbd "<C-up>") 'increment-number-at-point)
;; (define-key global-map (kbd "<C-down>") 'decrement-number-at-point)


(autoload 'incr-dwim "incr"
  "Use `incr-try-alist' to find most possible method to increase thing at point or region" t)
(autoload 'decr-dwim "incr" t)
(eval-after-load "incr"
  '(setq incr-enable-feature
    '(number rotate roman date han-number)))

(global-set-key (kbd "C-c x") 'incr-dwim)
(global-set-key (kbd "C-c z") 'decr-dwim)

;; global
;; put global-tags-mode.el into load-path


;;(setq load-path (cons "/home/owner/global" load-path))
;;(autoload 'gtags-mode "gtags" "" t)

;;; ff-find-other-file and friends
(eval-after-load "find-file"
  '(progn
     (push '("\\.scm\\'" ("#.scm")) cc-other-file-alist)
     (push '("\\#.scm\\'" (".scm")) cc-other-file-alist)
     (push ".m" (cadr (assoc "\\.h\\'" cc-other-file-alist)))
     (push ".mm" (cadr (assoc "\\.h\\'" cc-other-file-alist)))
     (push '("\\.m\\'" (".h")) cc-other-file-alist)
     (push '("\\.mm\\'" (".h")) cc-other-file-alist)))


(defadvice ff-get-file-name (around ff-get-file-name-framework
                                    (search-dirs
                                     fname-stub
                                     &optional suffix-list))
  "Search for Mac framework headers as well as POSIX headers."
  (or
   (if (string-match "\\(.*?\\)/\\(.*\\)" fname-stub)
       (let* ((framework (match-string 1 fname-stub))
              (header (match-string 2 fname-stub))
              (fname-stub (concat framework ".framework/Headers/" header)))
         ad-do-it))
   ad-do-it))
(ad-enable-advice 'ff-get-file-name 'around 'ff-get-file-name-framework)
(ad-activate 'ff-get-file-name)

(when (eq system-type 'darwin)
  (setq cc-search-directories '("." "../include" "/usr/include" "/usr/local/include/*"
                                "/System/Library/Frameworks" "/Library/Frameworks")))

(eval-after-load "scheme"
  '(define-key scheme-mode-map (kbd "C-c S") 'ff-find-other-file))

(eval-after-load "cc-mode"
  '(define-key c-mode-base-map (kbd "C-c S") 'ff-find-other-file))



;;; FindingNonAsciiCharacters
(defun find-first-non-ascii-char ()
  "Find the first non-ascii character from point onwards."
  (interactive)
  (let (point)
    (save-excursion
      (setq point
            (catch 'non-ascii
              (while (not (eobp))
                (or (eq (char-charset (following-char))
                        'ascii)
                    (throw 'non-ascii (point)))
                (forward-char 1)))))
    (if point
        (goto-char point)
        (message "No non-ascii characters."))))

(autoload 'who-calls "who-calls" "Display all known callers of a function" t)
(define-key emacs-lisp-mode-map "\C-c\C-w" 'who-calls)

;; (require 'generic-apt-install)

;;; Insert a path into the current buffer
(defun pl/insert-path (file)
  "insert file"
  (interactive "FPath: ")
  (insert (expand-file-name file)))


;; FIXME: it will make file be changed not line M-w
(defun kill-save-rectangle (start end &optional fill)
  "Save the rectangle as if killed, but don't kill it.  See
`kill-rectangle' for more information."
  (interactive "r\nP")
  (kill-rectangle start end fill)
  (goto-char start)
  (yank-rectangle))

;; I bound it to C-x r M-k to compliment C-x r k (just like M-w compliments C-w):
(global-set-key (kbd "C-x r M-k") 'kill-save-rectangle)

;; smex
(eval-after-load "smex"
  '(progn
     ;; (smex-initialize)                  ; el-get has do it
     (global-set-key (kbd "M-x") 'smex)
     ;; (global-set-key (kbd "M-X") 'smex-major-mode-commands) ;
     ;; This is your old M-x.
     (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)))


;;; ace jump
;; 'C-c SPC' is used by Org mode
;; gud-break is bound to C-c C-b, C-x SPC, C-x C-a C-b.
(define-key global-map (kbd "C-x SPC") 'ace-jump-mode)

;;; jump-char
;; <char> :: move to the next match in the current direction.
;; ; :: next match forward (towards end of buffer)
;; , :: next match backward (towards beginning of buffer)
;; C-c C-c :: invoke ace-jump-mode if available (also <M-/>)
(require 'jump-char)
(global-set-key (kbd "ESC ESC j") 'jump-char-forward)
(global-set-key (kbd "ESC ESC J") 'jump-char-backward)

;;; cclookup
;; add cclookup to your loadpath, ex) ~/.emacs.d/cclookup
(setq cclookup-dir "~/.emacs.d/lisp/cclookup")
(add-to-list 'load-path cclookup-dir)

;; load cclookup when compile time
(eval-when-compile (require 'cclookup))

;; set executable file and db file
(setq cclookup-program (concat cclookup-dir "/cclookup.py"))
(setq cclookup-db-file (concat cclookup-dir "/cclookup.db"))

;; to speedup, just load it on demand
(autoload 'cclookup-lookup "cclookup"
  "Lookup SEARCH-TERM in the Python HTML indexes." t)

(autoload 'cclookup-update "cclookup"
  "Run cclookup-update and create the database at `cclookup-db-file'." t)

;;; per-window-point
(require 'per-window-point)
(pwp-mode 1)

;;; mark-multiple
(require 'inline-string-rectangle)
(global-set-key (kbd "C-x r t") 'inline-string-rectangle)

(require 'mark-more-like-this)
(global-set-key (kbd "C-<") 'mark-previous-like-this)
(global-set-key (kbd "C->") 'mark-next-like-this)
(global-set-key (kbd "C-M-m") 'mark-more-like-this) ; like the other two, but takes an argument (negative is previous)
(global-set-key (kbd "C-*") 'mark-all-like-this)

;; (require 'rename-sgml-tag)              ;
;; (define-key sgml-mode-map (kbd "C-c C-r") 'rename-sgml-tag)

;; (require 'js2-rename-var)
;; (define-key js2-mode-map (kbd "C-c C-r") 'js2-rename-var)


(provide '50tools)
