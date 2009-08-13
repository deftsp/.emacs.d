;;; lazycat-toolkit.el --- My toolkit

;; Filename: lazycat-toolkit.el
;; Description: My toolkit
;; Author: Andy Stewart lazycat.manatee@gmail.com
;; Maintainer: Andy Stewart lazycat.manatee@gmail.com
;; Copyright (C) 2009, Andy Stewart, all rights reserved.
;; Created: 2009-01-10 23:18:47
;; Version: 0.1
;; Last-Updated: 2009-01-10 23:18:47
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/lazycat-toolkit.el
;; Keywords: lazycat, toolkit
;; Compatibility: GNU Emacs 23.0.60.1
;;
;; Features that might be required by this library:
;;
;;
;;

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commcentary:
;;
;; I always write some code piece, and those code is miscellaneous.
;; So i clean up those code in this file.
;;
;; I always use those code like toolkit.
;;
;; Hope you like... ;)
;;
;; And have some code is i collections, not my code.
;; I forget author name that write those code.
;; I put it in *My collections*.
;;
;; Thanks all emacser!
;;

;;; Installation:
;;
;; Put lazycat-toolkit.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'lazycat-toolkit)
;;
;; No need more.

;;; Change log:
;;
;; 2009/01/10
;;      * First released.
;;

;;; Acknowledgements:
;;
;;
;;

;;; TODO
;;
;;
;;

;;; Require
(require 'cl)
(require 'tabbar)
(require 'thingatpt)
(require 'tramp)
(require 'windresize)
(require 'cycle-buffer)
(require 'color-moccur)
(require 'ascii)
(require 'ecb)
(require 'xcscope)

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Variable ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar killed-process-name nil
  "The name of that have killed process.")

(defvar find-file-root-prefix "/sudo:root@localhost:"
  "The prefix of root user use in Emacs.")

(defvar point-stack nil
  "The stack position.")

(defvar wget-hide-status t
  "Default hide wget download window.")

(defvar startup-open-file-list nil
  "File list that startup open.")

(defvar startup-close-file-list nil
  "Buffer list that startup close.")

(defvar toggle-one-window-window-configuration nil
  "The window configuration use for `toggle-one-window'.")

(defvar reading-frame-parameter
  '((top. 900)
    (left . 690)
    (height . 20)
    (width . 100))
  "Frame parameter for reading frame.")

(defvar my-name "")
(defvar my-full-name "")
(defvar my-mail "")
(defvar my-homepage "")
(defvar my-password "")

(defvar my-home-directory "")
(defvar my-mldonkey-download-directory "")
(defvar my-default-download-directory "")
(defvar my-resource-backup-directory "")
(defvar my-book-directory "")
(defvar my-reading-directory "")
(defvar my-translate-png-directory "")
(defvar my-music-default-directory "")
(defvar my-picture-directory "")
(defvar my-lyrics-directory "")
(defvar my-emacs-lisp-package-directory "")
(defvar my-notes-directory "")
(defvar my-emacs-backup-directory "")
(defvar my-screenshots-storage-directory "")
(defvar my-emlue-download-directory "")
(defvar my-elisp-directory "")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Advices ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Prevent annoying \"Active processes exist\" query when you quit Emacs."
  (flet ((process-list ())) ad-do-it))

(defadvice list-load-path-shadows (around hidden-window-if-found-nothing activate)
  "This advice hidden output window if found nothing."
  (let (window-config (current-window-configuration))
    ad-do-it
    (with-current-buffer "*Shadows*"
      (goto-char (point-min))
      (when (search-forward-regexp "^No Emacs Lisp load-path shadowings were found$" nil t)
        (kill-buffer)
        (message "No Emacs Lisp load-path shadowings were found.")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Shell enhanced ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun shell-command-execute (cmd &rest arg-list)
  "Execute shell command asynchronously.
Argument CMD the main command.
Optional argument ARG-LIST command arguments."
  (interactive)
  (let* (arg-element arg)
    (dolist (arg-element arg-list)
      (setq arg (concat arg " " arg-element)))
    (shell-command-asynchronously (concat cmd arg))))

(defun shell-command-asynchronously (cmd)
  "Execute shell command asynchronously.
Argument CMD the main command."
  (start-process-shell-command cmd nil cmd))

(defun shell-command-synchronously (cmd)
  "Execute shell command synchronously.
Argument CMD the main command."
  (shell-command cmd))

(defun shell-aliase (alias)
  "Execute shell aliase in term.
Argument ALIAS the alias name that define in bash."
  (interactive)
  (shell-command (concat "bash -c -i " alias " &"))
  (sit-for 1)
  (with-current-buffer "*Async Shell Command*"
    (goto-char (point-min))
    (if (search-forward-regexp (format "\\[sudo\\] password for %s: $" (user-login-name)) nil t)
        (progn
          (insert my-password)
          (comint-send-input))
      (call-interactively 'move-end-of-line))))

(defadvice shell-command (around surpress-popup-window)
  "This advice is surpress popup window for show information."
  (let ((old-window-configuration (current-window-configuration)))
    ad-do-it
    (set-window-configuration old-window-configuration)))

(defun shell-command-surpress-popup-window (command &optional output-buffer error-buffer)
  "This function is like `shell-command', but surpress popup-window."
  (interactive
   (list
    (read-shell-command "Shell command: " nil nil
                        (and buffer-file-name
                             (file-relative-name buffer-file-name)))
    current-prefix-arg
    shell-command-default-error-buffer))
  (ad-enable-advice 'shell-command 'around 'surpress-popup-window)
  (ad-activate 'shell-command)
  (shell-command command output-buffer error-buffer)
  (ad-disable-advice 'shell-command 'around 'surpress-popup-window)
  (ad-activate 'shell-command))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Killall ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun killall ()
  "Execute command 'killall'."
  (interactive)
  (let ((process-name killed-process-name))
    (setq process-name (read-string (format "Killall: (%s) " process-name) nil nil process-name))
    (setq killed-process-name process-name)
    (shell-command-execute "killall" process-name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Kill buffer ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun kill-current-mode-buffers ()
  "Kill all buffers that major mode same with current mode."
  (interactive)
  (kill-special-mode-buffers major-mode))

(defun kill-special-mode-buffers (mode)
  "Kill all buffers that major MODE same with special."
  (interactive)
  (let ((count 0))
    (dolist (buffer (buffer-list))
      (set-buffer buffer)
      (when (equal major-mode mode)
        (incf count)
        (kill-buffer buffer)))
    (message "Killed %s buffer%s" count (if (> count 1) "s" ""))))

(defun kill-all-buffers-except-current ()
  "Kill all buffers except current buffer."
  (interactive)
  (let ((current-buf (current-buffer)))
    (dolist (buffer (buffer-list))
      (set-buffer buffer)
      (unless (eq current-buf buffer)
        (kill-buffer buffer)))))

(defun kill-other-window-buffer ()
  "Kill the buffer in other window."
  (interactive)
  (other-window +1)
  (kill-this-buffer)
  (other-window -1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Fullscreen ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun fullscreen ()
  "Fullscreen."
  (interactive)
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                         '(1 "_NET_WM_STATE_FULLSCREEN" 0))) ;if first parameter is '1', can't toggle fullscreen status

(defun toggle-fullscreen ()
  "Toggle fullscreen status."
  (interactive)
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                         '(2 "_NET_WM_STATE_FULLSCREEN" 0))) ;if first parameter is '2', can toggle fullscreen status

(defun reading-mode()
  "Reading mode."
  (interactive)
  (toggle-fullscreen)
  (modify-frame-parameters nil reading-frame-parameter))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Scroll ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun scroll-up-one-line()
  "Scroll up one line."
  (interactive)
  (scroll-up 1))

(defun scroll-down-one-line()
  "Scroll down one line."
  (interactive)
  (scroll-down 1))

(defun scroll-other-window-up-line ()
  "Scroll other window up one line."
  (interactive)
  (scroll-other-window 1))

(defun scroll-other-window-down-line ()
  "Scroll other window line down."
  (interactive)
  (scroll-other-window-down 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Insert line number ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun insert-line-number (beg end &optional start-line)
  "Insert line numbers into buffer."
  (interactive "r")
  (save-excursion
    (let ((max (count-lines beg end))
          (line (or start-line 1))
          (counter 1))
      (goto-char beg)
      (while (<= counter max)
        (insert (format "%0d " line))
        (beginning-of-line 2)
        (incf line)
        (incf counter)))))

(defun insert-line-number+ ()
  "Insert line number into buffer."
  (interactive)
  (if mark-active
      (insert-line-number (region-beginning) (region-end) (read-number "Start line: "))
    (insert-line-number (point-min) (point-max))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Strip line ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun strip-blank-lines()
  "Strip all blank lines in select area of buffer,
if not select any area, then strip all blank lines of buffer."
  (interactive)
  (strip-regular-expression-string "^[ \t]*\n")
  (message "Have strip blanks line. ^_^"))

(defun strip-line-number()
  "Strip all line number in select area of buffer,
if not select any area, then strip all line number of buffer."
  (interactive)
  (strip-regular-expression-string "^[0-9]+")
  (message "Have strip line number. ^_^"))

(defun strip-regular-expression-string (regular-expression)
  "Strip all string that match REGULAR-EXPRESSION in select area of buffer.
If not select any area, then strip current buffer"
  (interactive)
  (let ((begin (point-min))             ;initialization make select all buffer
        (end (point-max)))
    (if mark-active                     ;if have select some area of buffer, then strip this area
        (setq begin (region-beginning)
              end (region-end)))
    (save-excursion                                              ;save position
      (goto-char end)                                            ;goto end position
      (while (and (> (point) begin)                              ;when above beginning position
                  (re-search-backward regular-expression nil t)) ;and find string that match regular expression
        (replace-match "" t t)))))                               ;replace target string with null

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Delete window or buffer ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun delete-buffer-and-window (buffer-name)
  "Delete buffer and window that special.
Argument BUFFER-NAME the buffer name that will delete."
  (interactive)
  (if (bufferp (get-buffer buffer-name))
      (progn
        (delete-buffer-window buffer-name)
        (kill-buffer (get-buffer buffer-name)))
    (message "Buffer %s is not exist." buffer-name)))

(defun delete-current-buffer-and-window ()
  "Delete current buffer and window."
  (interactive)
  (delete-buffer-and-window (buffer-name)))

(defun delete-buffer-window (buffer-name)
  "Delete the window of special buffer.
Argument BUFFER-NAME the buffer name that will delete."
  (interactive)
  (if (bufferp (get-buffer buffer-name))
      (delete-window (get-buffer-window (get-buffer buffer-name)))
    (message "Buffer %s is not exist." buffer-name)))

(defun delete-current-buffer-window ()
  "Delete the window of current buffer."
  (interactive)
  (delete-buffer-window (current-buffer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Move line ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun open-newline-above (arg)
  "Move to the previous line (like vi) and then opens a line."
  (interactive "p")
  (beginning-of-line)
  (open-line arg)
  (if (not (member major-mode '(haskell-mode org-mode literate-haskell-mode)))
      (indent-according-to-mode)
    (beginning-of-line)))

(defun open-newline-below (arg)
  "Move to the next line (like vi) and then opens a line."
  (interactive "p")
  (end-of-line)
  (open-line arg)
  (call-interactively 'next-line arg)
  (if (not (member major-mode '(haskell-mode org-mode literate-haskell-mode)))
      (indent-according-to-mode)
    (beginning-of-line)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Kill syntax ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun kill-syntax-forward+ (&optional arg)
  "Kill ARG set of syntax characters after point.
And if `completion-auto-mode' is active,
use function `completion-delete'."
  (interactive "p")
  (if (member 'auto-completion-mode minor-mode-list)
      (completion-delete 'kill-syntax-forward arg)
    (kill-syntax-forward arg)))

(defun kill-syntax-backward+ (&optional arg)
  "Kill ARG set of syntax characters preceding point."
  (interactive "p")
  (if (member 'auto-completion-mode minor-mode-list)
      (completion-backward-delete 'kill-syntax-forward (- arg))
    (kill-syntax-forward (- arg))))

(defun kill-syntax-forward (&optional arg)
  "Kill ARG set of syntax characters after point."
  (interactive "p")
  (let ((arg (or arg 1))
        (inc (if (and arg (< arg 0)) 1 -1))
        (opoint (point)))
    (while (or                          ;(not (= arg 0)) ;; This condition is implied.
            (and (> arg 0) (not (eobp)))
            (and (< arg 0) (not (bobp))))
      (if (> arg 0)
          (skip-syntax-forward (string (char-syntax (char-after))))
        (skip-syntax-backward (string (char-syntax (char-before)))))
      (setq arg (+ arg inc)))
    (if (and (> arg 0) (eobp))
        (message "End of buffer"))
    (if (and (< arg 0) (bobp))
        (message "Beginning of buffer"))
    (kill-region opoint (point))))

(defun kill-syntax-backward (&optional arg)
  "Kill ARG set of syntax characters preceding point."
  (interactive "p")
  (kill-syntax-forward (- 0 (or arg 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Character ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun upcase-char (arg)
  "Uppercase for character."
  (interactive "P")
  (upcase-region (point) (+ (point) (or arg 1)))
  (forward-char (or arg 1)))

(defun downcase-char (arg)
  "Downcase for character."
  (interactive "P")
  (downcase-region (point) (+ (point) (or arg 1)))
  (forward-char (or arg 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Window move ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun select-next-window ()
  "Select next window."
  (interactive)
  (other-window +1))

(defun select-prev-window ()
  "Select next window."
  (interactive)
  (other-window -1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Comment or Uncomment ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun comment-or-uncomment-region+ ()
  "This function is to comment or uncomment a line or a region."
  (interactive)
  (let (beg end)
    (if mark-active
        (progn
          (setq beg (region-beginning))
          (setq end (region-end)))
      (setq beg (line-beginning-position))
      (setq end (line-end-position)))
    (save-excursion
      (comment-or-uncomment-region beg end))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Comment move ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun comment-part-move-up (n)
  "Move comment part up."
  (interactive "p")
  (comment-part-move (or (- n) -1)))

(defun comment-part-move-down (n)
  "Move comment part down."
  (interactive "p")
  (comment-part-move (or n 1)))

(defun comment-part-move (&optional n)
  "Move comment part."
  (or n (setq n 1))
  (let (cmt-current cmt-another cs-current cs-another)
    ;; If current line have comment, paste it.
    (setq cmt-current (comment-paste))
    (when cmt-current
      (setq cs-current (current-column)))
    ;; If another line have comment, paste it.
    (forward-line n)
    (setq cmt-another (comment-paste))
    (when cmt-another
      (setq cs-another (current-column)))
    ;; Paste another comment in current line.
    (forward-line (- n))
    (when cmt-another
      (if cs-current
          (goto-column cs-current)
        (end-of-line))
      (insert cmt-another))
    ;; Paste current comment in another line.
    (forward-line n)
    (when cmt-current
      (if cs-another
          (goto-column cs-another)
        (end-of-line))
      (insert cmt-current))
    ;; Indent comment, from up to down.
    (if (> n 0)
        (progn                          ;comment move down
          (forward-line (- n))
          (if cmt-another (comment-indent))
          (forward-line n)
          (if cmt-current (comment-indent)))
      (if cmt-current (comment-indent)) ;comment move up
      (save-excursion
        (forward-line (- n))
        (if cmt-another (comment-indent))))))

(defun comment-paste ()
  "Paste comment part of current line.
If have return comment, otherwise return nil."
  (let (cs ce cmt)
    (setq cs (comment-on-line-p))
    (if cs                              ;If have comment start position
        (progn
          (goto-char cs)
          (skip-syntax-backward " ")
          (setq cs (point))
          (comment-forward)
          (setq ce (if (bolp) (1- (point)) (point))) ;get comment end position
          (setq cmt (buffer-substring cs ce))        ;get comment
          (kill-region cs ce)                        ;kill region between comment start and end
          (goto-char cs)                             ;revert position
          cmt)
      nil)))

(defun comment-on-line-p ()
  "Whether have comment part on current line.
If have comment return COMMENT-START, otherwise return nil."
  (save-excursion
    (beginning-of-line)
    (comment-search-forward (line-end-position) t)))

(defun comment-dwim-next-line (&optional reversed)
  "Move to next line and comment dwim.
Optional argument REVERSED default is move next line, if reversed is non-nil move previous line."
  (interactive)
  (if reversed
      (call-interactively 'previous-line)
    (call-interactively 'next-line))
  (call-interactively 'comment-dwim))

(defun comment-dwim-prev-line ()
  "Move to previous line and comment dwim."
  (interactive)
  (comment-dwim-next-line 't))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Comment copy ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun comment-copy (arg)
  "Copy the first comment on this line, if any.
With prefix ARG, copy comments on that many lines starting with this one."
  (interactive "P")
  (comment-normalize-vars)
  (dotimes (_ (prefix-numeric-value arg))
    (save-excursion
      (beginning-of-line)
      (let ((cs (comment-search-forward (line-end-position) t)))
        (when cs
          (goto-char cs)
          (skip-syntax-backward " ")
          (setq cs (point))
          (comment-forward)
          (kill-ring-save cs (if (bolp) (1- (point)) (point)))
          (indent-according-to-mode))))
    (if arg (forward-line 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Indent ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun refresh-file ()
  "Automatic reload current file."
  (interactive)
  (cond ((eq major-mode 'emacs-lisp-mode)
         (indent-buffer)
         (indent-comment-buffer)
         (save-buffer)
         (byte-compile-file buffer-file-name t))
        ((member major-mode '(lisp-mode c-mode perl-mode))
         (indent-buffer)
         (indent-comment-buffer)
         (save-buffer))
        ((eq major-mode 'haskell-mode)
         (indent-comment-buffer)
         (save-buffer))
        (t (message "Current mode is not supported, so not reload"))))

(defun indent-buffer ()
  "Automatic format current buffer."
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)
    (delete-trailing-whitespace)
    (untabify (point-min) (point-max))))

(defun indent-comment-buffer ()
  "Indent comment of buffer."
  (interactive)
  (indent-comment-region (point-min) (point-max)))

(defun indent-comment-region (start end)
  "Indent region."
  (interactive "r")
  (save-excursion
    ;; Indent comment need process lines from top to down,
    ;; if from bottom to up, will make indent comment failed.
    (let (temp-position)
      (goto-char start)
      (while (< (point) end)
        (if (comment-search-forward end t)
            (progn
              (setq temp-position (point))
              (comment-indent)
              (decf end (- temp-position (point))))
          (goto-char end))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Move line ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun move-text-internal (arg)
  "Move region (transient-mark-mode active) or current line."
  (let ((remember-point (point)))
    ;; Can't get correct effect of `transpose-lines'
    ;; when `point-max' is not at beginning of line
    ;; So fix this bug.
    (goto-char (point-max))
    (if (not (bolp)) (newline))         ;add newline to fix
    (goto-char remember-point)
    ;; logic code start
    (cond ((and mark-active transient-mark-mode)
           (if (> (point) (mark))
               (exchange-point-and-mark))
           (let ((column (current-column))
                 (text (delete-and-extract-region (point) (mark))))
             (forward-line arg)
             (move-to-column column t)
             (set-mark (point))
             (insert text)
             (exchange-point-and-mark)
             (setq deactivate-mark nil)))
          (t
           (let ((column (current-column)))
             (beginning-of-line)
             (when (or (> arg 0) (not (bobp)))
               (forward-line 1)
               (when (or (< arg 0) (not (eobp)))
                 (transpose-lines arg))
               (forward-line -1))
             (move-to-column column t))
           ))))

(defun move-text-up (arg)
  "Move region (transient-mark-mode active) or current line ARG lines up."
  (interactive "*p")
  (move-text-internal (- arg)))

(defun move-text-down (arg)
  "Move region (transient-mar-mode active) or current line (ARG lines) down."
  (interactive "*p")
  (move-text-internal arg))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Pair move ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun go-to-next-pair-right()
  "To right of next match parentheses."
  (interactive)
  (while (not (looking-at "\\(['\">)}]\\|]\\)")) (forward-char 1))
  (forward-char 1))

(defun go-to-next-pair-left()
  "To left of previous match parentheses."
  (interactive)
  (backward-char 1)
  (while (not (looking-at "\\(['\"<({]\\|[[]\\)")) (backward-char 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Find define ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun find-function-or-variable-at-point (&optional prefix)
  "Find function or variable define at current point."
  (interactive "P")
  (if (equal 0 (variable-at-point))     ;if have not variable at current point
      (if (function-called-at-point)    ;if have function call at current point
          (call-interactively (if (null prefix) 'find-function 'find-function-other-window))
        (if (face-at-point)             ;if have face define at current point
            (call-interactively (if (null prefix) 'find-face-definition 'find-face-definition-other-window))
          (message "Nothing at point.")))
    (call-interactively (if (null prefix) 'find-variable 'find-variable-other-window))))

(defun find-face-definition-other-window (face)
  "Find FACE definition at other window."
  (interactive (find-function-read 'defface))
  (find-function-do-it face 'defface 'switch-to-buffer-other-window))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Keystroke bind ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun basic-set-key-alist (key-alist &optional keymap key-prefix)
  "This function is to little type when define key binding.
`KEYMAP' is a add keymap for some binding, default is `current-global-map'.
`KEY-ALIST' is a alist contain main-key and command.
`KEY-PREFIX' is a add prefix for some binding, default is nil."
  (let (key def)
    (or keymap (setq keymap (current-global-map)))
    (if key-prefix
        (setq key-prefix (concat key-prefix " "))
      (setq key-prefix ""))
    (dolist (element key-alist)
      (setq key (car element))
      (setq def (cdr element))
      (cond ((stringp key) (setq key (read-kbd-macro (concat key-prefix key))))
            ((vectorp key) nil)
            (t (signal 'wrong-type-argument (list 'array key))))
      (define-key keymap key def))))

(defun basic-unset-key-list (key-list &optional keymap)
  "This function is to little type when unset key binding.
`KEYMAP' is add keymap for some binding, default is `current-global-map'
`KEY-LIST' is list contain key."
  (let (key)
    (or keymap (setq keymap (current-global-map)))
    (dolist (key key-list)
      (cond ((stringp key) (setq key (read-kbd-macro (concat key))))
            ((vectorp key) nil)
            (t (signal 'wrong-type-argument (list 'array key))))
      (define-key keymap key nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Duplicate line ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun duplicate-line-above (&optional reverse)
  "Duplicate current line above."
  (interactive)
  (let ((origianl-column (current-column))
        line-content)
    (setq line-content (buffer-substring (line-beginning-position) (line-end-position)))
    (beginning-of-line)
    (and reverse (forward-line +1))
    (newline +1)
    (forward-line -1)
    (insert line-content)
    (goto-column origianl-column)))

(defun duplicate-line-below ()
  "Duplicate current line below"
  (interactive)
  (duplicate-line-above t))

(defun duplicate-line-above-comment (&optional reverse)
  "Duplicate current line above, and comment current line."
  (interactive)
  (if reverse
      (duplicate-line-below)
    (duplicate-line-above))
  (save-excursion
    (if reverse
        (forward-line -1)
      (forward-line +1))
    (comment-or-uncomment-region+)))

(defun duplicate-line-below-comment ()
  "Duplicate current line below, and comment current line."
  (interactive)
  (duplicate-line-above-comment t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Cycle buffer ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cycle-buffer-in-special-mode (special-mode)
  "Cycle in special mode."
  (setq cycle-buffer-filter nil)
  (setq cycle-buffer-filter (cons '(eq major-mode special-mode) cycle-buffer-filter))
  (cycle-buffer-backward-permissive 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Find file ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun find-file-root (file)
  "Find file with root."
  (interactive "fFind file as sudo: ")
  (find-file (concat find-file-root-prefix file)))

(defun find-file-smb(file)
  "Access file through samba protocol."
  (interactive "fFind file as samba: ")
  (find-file (concat "/smb:" file)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Date and time ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun insert-standard-date ()
  "Inserts standard date time string."
  (interactive)
  (insert (format-time-string "%Y-%m-%d %T")))

(defun insert-changelog-date ()
  "Insert changelog date, like yyyy/mm/dd."
  (interactive)
  (insert (format-time-string "%Y/%m/%d")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Outward or Inward window ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun outward-window ()
  "Outward enlarge current window."
  (interactive)
  (windresize-up-inwards '-1)
  (windresize-down-inwards '-1)
  (windresize-right-inwards '-1)
  (windresize-left-inwards '-1))

(defun inward-window ()
  "Inward shrink current window."
  (interactive)
  (windresize-up-inwards '1)
  (windresize-down-inwards '1)
  (windresize-right-inwards '1)
  (windresize-left-inwards '1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Notes create and search ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun notes-search(str)
  "Notes search"
  (interactive
   (list
    (moccur-grep-read-regexp moccur-grep-default-mask)))
  (moccur-grep-find my-notes-directory str))

(defun notes-new(str)
  "Create a new notes."
  (interactive "sNotes name: ")
  (find-file (concat my-notes-directory str ".org")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Wget ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun wget-hide ()
  "Hide wget download information."
  (interactive)
  (delete-buffer-window wget-process-buffer)
  (setq wget-hide-status t))

(defun wget-show ()
  (interactive)
  (call-interactively 'wget-state-of-progress)
  (setq wget-hide-status nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Button ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun forward-button-with-line-begin ()
  "Move to next button with line begin."
  (interactive)
  (call-interactively 'forward-button)
  (while (not (bolp))
    (call-interactively 'forward-button)))

(defun backward-button-with-line-begin ()
  "Move to previous button with line begin."
  (interactive)
  (call-interactively 'backward-button)
  (while (not (bolp))
    (call-interactively 'backward-button)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Single functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun open-and-run-lisp (file)
  "This function open a file at TOP window, and execute `run-lisp' at LOWER window."
  (interactive "FFile: ")
  (delete-other-windows)
  (find-file file)
  (split-window-vertically -20)
  (other-window 1)
  (with-current-buffer (current-buffer)
    (call-interactively 'run-lisp)))

(defun toggle-one-window ()
  "Toggle between window layout and one window."
  (interactive)
  (if (one-window-p t)
      (if toggle-one-window-window-configuration
          (progn
            (set-window-configuration toggle-one-window-window-configuration)
            (setq toggle-one-window-window-configuration nil))
        (message "No other windows exist."))
    (setq toggle-one-window-window-configuration (current-window-configuration))
    (delete-other-windows)))

(defun emacs-exit ()
  "Exit emacs."
  (interactive)
  (if (get-buffer "*Group*")
      (gnus-group-exit))
  (newsticker--cache-save)
  (see-you-again))

(defun window-number-jump ()
  "Jump to nth window."
  (interactive)
  (window-number-select (read-number "Window number: ")))

(defun ascii-toggle ()
  "Toggle ascii table"
  (interactive)
  (if ascii-display
      (ascii-off)
    (ascii-on)))

(defun dot-emacs()
  "Open dot emacs file."
  (interactive)
  (find-file "~/.emacs"))

(defun clean-emacs-backup ()
  "Clean backup file that emacs automatically generate."
  (interactive)
  (let ((file-number (- (length (directory-files my-emacs-backup-directory)) 2)))
    (if (> file-number 0)
        (if (yes-or-no-p (format "Are you sure to clean backup file (%s) ?" file-number))
            (progn
              (shell-command-execute (concat "rm " my-emacs-backup-directory "*"))
              (message "Have clean backup file that emacs automatically generate")))
      (message "Haven't backup files!"))))

(defun startup-open ()
  "The files that need open when emacs startup."
  (interactive)
  (let* ((file-list startup-open-file-list)
         file-name)
    (dolist (file-name file-list)
      (find-file file-name))))

(defun startup-close ()
  "Close when emacs startup."
  (interactive)
  (dolist (file-name startup-close-file-list)
    (if (get-buffer file-name)
        (kill-buffer file-name))))

(defun unmark-all-buffers ()
  "Unmark all have marked buffers."
  (interactive)
  (let* ((list (buffer-list))
         (current-element (current-buffer))
         element)
    (save-excursion
      (dolist (element list)
        (switch-to-buffer element)
        (deactivate-mark)))
    (switch-to-buffer current-element)
    (deactivate-mark)))

(defun match-paren (arg)
  "Go to the matching parenthesis if on parenthesis, otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(\\|\\s\{\\|\\s\[")
         (forward-list))
        ((looking-back "\\s\)\\|\\s\}\\|\\s\\]")
         (backward-list))
        (t (self-insert-command (or arg 1)))))

(defun goto-column (number)
  "Untabify, and go to a column NUMBER within the current line (0 is beginning of the line)."
  (interactive "nColumn number: ")
  (move-to-column number t))

(defun elisp-index-search+ ()
  "Look up TOPIC in the indies of the Emacs Lisp Reference Manual."
  (interactive)
  (let ((topic (symbol-name (symbol-at-point))))
    (setq topic (read-string (format "Subject to look up <%s>: " topic) nil nil topic))
    (info "elisp")
    (Info-index topic)))

(defun switch-to-scratch ()
  "Select buffer *scratch* in the current window.
Optional second arg NORECORD non-nil means
do not put this buffer at the front of the list of recently selected ones."
  (interactive)
  (switch-to-buffer "*scratch*"))

(defun ielm-toggle ()
  "Toggle ielm buffer."
  (interactive)
  (let ((ielm-buffer-name "*ielm*"))
    (if (get-buffer ielm-buffer-name)
        (if (string-equal ielm-buffer-name (buffer-name))
            (bury-buffer)
          (switch-to-buffer ielm-buffer-name))
      (ielm))))

(defun remove-control-M (&optional quiet)
  "Remove ^M at end of line in the whole buffer.
When QUIET is non-nil, don't show report message."
  (interactive)
  (save-match-data
    (save-excursion
      (let ((remove-count 0))
        (goto-char (point-min))
        (while (re-search-forward "$" (point-max) t)
          (incf remove-count)
          (replace-match "" nil nil))
        (or quiet (message (format "%d ^M removed from buffer." remove-count)))))))

(defun colp ()
  "Return t if point is first non-whitespace character of line."
  (let (current-point)
    (setq current-point (point))
    (save-excursion
      (back-to-indentation)
      (equal current-point (point)))))

(defun blank-line-p ()
  "Return t if current line is blank line.
Otherwise, return nil."
  (save-excursion
    (beginning-of-line)
    (looking-at "[ \t]*$")))

(defun is-digit (x)
  (cond ((stringp x) (is-digit (string-to-char x)))
        ((integerp x) (and (<= ?0 x) (<= x ?9)))
        (t nil)))

(defun is-letter (x)
  (cond ((stringp x) (is-letter (string-to-char x)))
        ((integerp x) (not (equal (downcase x) (upcase x))))
        (t nil)))

(defun only-comment-p ()
  "Return t if only comment in current line.
Otherwise return nil."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (if (search-forward comment-start (line-end-position) t)
        (progn
          (backward-char (length comment-start))
          (equal (point)
                 (progn
                   (back-to-indentation)
                   (point))))
      nil)))

(defun fileline-to-alist (file)
  "Transform line in special file to element of list.
And return list."
  (let (return-list)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (while (not (eobp))
        (push (buffer-substring-no-properties (line-beginning-position) (line-end-position)) return-list)
        (forward-line +1))
      (nreverse return-list))))

(defun hibernate-disk ()
  "Hibernate disk, this need you install `hibernate' first.
And make command `sudo hibernate-disk' with alias `sl'."
  (interactive)
  (shell-aliase "sl"))

(defun handler-buffer-exit-close ()
  "Handle buffer close when buffer process is exit.
If you want to some buffer close automatically when its' process is over,
just add this function hook it ."
  (when (ignore-errors (get-buffer-process (current-buffer)))
    (set-process-sentinel (get-buffer-process (current-buffer))
                          (lambda (proc change)
                            (when (string-match "\\(finish\\|exit\\|broken\\)" change)
                              (kill-buffer (process-buffer proc)))))))

(defun mark-line ()
  "Mark one whole line, similar to `mark-paragraph'."
  (interactive)
  (beginning-of-line)
  (if mark-active
      (exchange-point-and-mark)
    (push-mark nil nil t))
  (forward-line)
  (exchange-point-and-mark))

(defun hide-mouse ()
  "Hide mouse by unclutter.
To use this extensions, you need install unclutter in your system."
  (interactive)
  (shell-command-execute "unclutter -idle 1"))

(defun save-screenshots (name)
  "Save shot full-screen.
To use this function, you need install scrot."
  (interactive "sPicture Name: ")
  (shell-command-execute "scrot" (concat my-screenshots-storage-directory name ".png"))
  (message (concat "You have save screen as " name ".png at: " my-screenshots-storage-directory)))

(defun ecb-toggle-visible ()
  "Toggle the visibility of the ecb windows.
If ecb is not active start ecb with ecb-activate
If this function is called with a prefix argument: call ecb-deactivate"
  (interactive)
  (if ecb-activated-window-configuration
      (ecb-deactivate)
    (ecb-activate)))

(defun my-doxymacs-return ()
  "Advanced C-m for doxymacs multiline comments.
Inserts `*' at the reigning of the new line if
unless return was pressed outside the comment"
  (interactive)
  (let ((current-point (point))
        is-inside)
    (setq is-inside
          (if (search-backward "*/" nil t)
              ;; there are some comment endings - search forward
              (if (search-forward "/*" current-point t)
                  't
                'nil)
            ;; it's the only comment - search backward
            (goto-char current-point)
            (if (search-backward "/*" nil t)
                't
              'nil)))
    ;; go to last char position
    (goto-char current-point)
    ;; the point is inside some comment, insert `*'
    (if is-inside
        (progn
          (insert "\n*")
          (indent-for-tab-command)
          (insert " "))
      ;; else insert only new-line
      (insert "\n"))))

(defun clean-recentf-history ()
  "Clean recentf history of file assoc."
  (interactive)
  (setq recentf-list '())
  (message "Have clean recentf history."))

(defun open-current-log-keyboard-command ()
  "Open log keyboard command of current buffer."
  (interactive)
  (mwe:log-keyboard-commands)
  (mwe:open-command-log-buffer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; My collections ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Below code is i collections, not my code.
;;; I forget the author name that write those code.
;;; Thanks all emacser! ;)

(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (rename-file name new-name 1)
        (rename-buffer new-name)
        (set-visited-file-name new-name)
        (set-buffer-modified-p nil)))))

(defun move-buffer-file (dir)
  "Move both current buffer and file it's visiting to DIR."
  (interactive "DNew directory: ")
  (let* ((name (buffer-name))
         (filename (buffer-file-name))
         (dir
          (if (string-match dir "\\(?:/\\|\\\\)$")
              (substring dir 0 -1) dir))
         (newname (concat dir "/" name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (copy-file filename newname 1)
      (delete-file filename)
      (set-visited-file-name newname)
      (set-buffer-modified-p nil)
      t)))

(defun zap-back-to-char (arg char)
  "No need to enter C-- to zap back."
  (interactive "p\ncZap back to char: ")
  (zap-to-char (- arg) char))

(defun goto-longest-line (beg end)
  "Goto the longest line of current buffer."
  (interactive `,(region-or-buffer-limits))
  (when (= beg end) (error "The buffer is empty"))
  (when (eq this-command last-command) (forward-line 1) (setq beg (point)))
  (goto-char beg)
  (let* ((start-line (line-number-at-pos))
         (max-width 0)
         (line start-line)
         long-lines col)
    (when (eobp) (error "End of buffer"))
    (while (and (not (eobp)) (or (not mark-active) (< (point) end)))
      (end-of-line)
      (setq col (current-column))
      (when (>= col max-width)
        (if (= col max-width)
            (setq long-lines (cons line long-lines))
          (setq long-lines (list line)))
        (setq max-width col))
      (forward-line 1)
      (setq line (1+ line)))
    (setq long-lines (nreverse long-lines))
    (let ((lines long-lines))
      (while (and lines (> start-line (car lines))) (pop lines))
      (goto-line (or (car lines) start-line)))
    (when (interactive-p)
      (let ((others (cdr long-lines)))
        (message
         "Line %d: %d chars%s (%d lines measured)"
         (car long-lines) max-width
         (concat (and others (format ", Others: {%s}"
                                     (mapconcat (lambda (line) (format "%d" line))
                                                (cdr long-lines) ", "))))
         (- line start-line))))
    (list (car long-lines) max-width (cdr long-lines) (- line start-line))))

(defun region-or-buffer-limits ()
  "Return the start and end of the region as a list, smallest first.
If the region is not active or empty, then bob and eob are used."
  (if (or (not mark-active) (null (mark)) (= (point) (mark)))
      (list (point-min) (point-max))
    (if (< (point) (mark)) (list (point) (mark)) (list (mark) (point)))))

(defun current-line-move-to-top()
  "Move current line to top of buffer."
  (interactive)
  (recenter 0))

(defun buffer-order-next-mark (arg)
  "Jump to next mark."
  (interactive "p")
  (when (mark)
    (let* ((p (point))
           (m (mark))
           (n p)
           (count (if (null arg) 1 arg))
           (abscount (abs count))
           (rel
            (funcall
             (if (< 0 count) 'identity 'reverse)
             (sort (cons (cons 0 p)
                         (cons (cons (- m p) m)
                               (if mark-ring
                                   (mapcar (lambda (mrm)
                                             (cons (- mrm p) mrm))
                                           mark-ring)
                                 nil)))
                   (lambda (c d) (< (car c) (car d))))))
           (cur rel))
      (while (and (numberp (caar cur)) (/= (caar cur) 0))
        (setq cur (cdr cur)))
      (while (and (numberp (caadr cur)) (= (caadr cur) 0))
        (setq cur (cdr cur)))
      (while (< 0 abscount)
        (setq cur (cdr cur))
        (when (null cur) (setq cur rel))
        (setq abscount (- abscount 1)))
      (when (number-or-marker-p (cdar cur))
        (goto-char (cdar cur))))))

(defun buffer-order-prev-mark (arg)
  "Jump to previous mark."
  (interactive "p")
  (buffer-order-next-mark
   (or (- arg) -1)))

(defun remember-init ()
  "Remember current position and setup."
  (interactive)
  (point-to-register 8)
  (message "Have remember one position"))

(defun remember-jump ()
  "Jump to latest position and setup."
  (interactive)
  (let ((tmp (point-marker)))
    (jump-to-register 8)
    (set-register 8 tmp))
  (message "Have back to remember position"))

(defun point-stack-push ()
  "Push current point in stack."
  (interactive)
  (message "Location marked.")
  (setq point-stack (cons (list (current-buffer) (point)) point-stack)))

(defun point-stack-pop ()
  "Pop point from stack."
  (interactive)
  (if (null point-stack)
      (message "Stack is empty.")
    (switch-to-buffer (caar point-stack))
    (goto-char (cadar point-stack))
    (setq point-stack (cdr point-stack))))

(defun copy-buffer-file-name-as-kill(choice)
  "Copy the buffer-file-name to the kill-ring"
  (interactive "cCopy Buffer Name (F) Full, (D) Directory, (N) Name")
  (let ((new-kill-string)
        (name (if (eq major-mode 'dired-mode)
                  (dired-get-filename)
                (or (buffer-file-name) ""))))
    (cond ((eq choice ?f)
           (setq new-kill-string name))
          ((eq choice ?d)
           (setq new-kill-string (file-name-directory name)))
          ((eq choice ?n)
           (setq new-kill-string (file-name-nondirectory name)))
          (t (message "Quit")))
    (when new-kill-string
      (message "%s copied" new-kill-string)
      (kill-new new-kill-string))))

(defun count-words ()
  "Count the number of word in buffer, include Chinese."
  (interactive)
  (let ((begin (point-min))
        (end (point-max)))
    (if mark-active
        (setq begin (region-beginning)
              end (region-end)))
    (count-ce-words begin end)))

(defun count-ce-words (beg end)
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
    (message (format "Total: %d (CN: %d, EN: %d) words, %d bytes."
                     total-word cn-word en-word total-byte))))

(defun goto-percent (percent)
  "Goto PERCENT of buffer."
  (interactive "nGoto percent: ")
  (goto-char (/ (* percent (point-max)) 100)))

(defun kill-and-join-forward (&optional arg)
  "Delete empty line in select region."
  (interactive "P")
  (if (and (eolp) (not (bolp)))
      (progn
        (forward-char 1)
        (just-one-space 0)
        (backward-char 1)
        (kill-line arg))
    (kill-line arg)))

(defun describe-hash (variable &optional buffer)
  "Display the full documentation of VARIABLE (a symbol).
Returns the documentation as a string, also.
If VARIABLE has a buffer-local value in BUFFER (default to the current buffer),
it is displayed along with the global value."
  (interactive
   (let ((v (variable-at-point))
         (enable-recursive-minibuffers t)
         val)
     (setq val (completing-read
                (if (and (symbolp v)
                         (hash-table-p (symbol-value v)))
                    (format
                     "Describe hash-map (default %s): " v)
                  "Describe hash-map: ")
                obarray
                (lambda (atom) (and (boundp atom)
                                    (hash-table-p (symbol-value atom))))
                t nil nil
                (if (hash-table-p v) (symbol-name v))))
     (list (if (equal val "")
               v (intern val)))))
  (with-output-to-temp-buffer (help-buffer)
    (maphash (lambda (key value)
               (pp key)
               (princ " => ")
               (pp value)
               (terpri))
             (symbol-value variable))))

(defun prettyfy-string (string &optional after)
  "Strip starting and ending whitespace and pretty `STRING'.
Replace any chars after AFTER with '...'.
Argument STRING the string that need pretty."
  (let ((replace-map (list
                      (cons "^[ \t]*" "")
                      (cons "[ \t]*$" "")
                      (cons (concat "^\\(.\\{"
                                    (or (number-to-string after) "10")
                                    "\\}\\).*")
                            "\\1..."))))
    (dolist (replace replace-map)
      (when (string-match (car replace) string)
        (setq string (replace-match (cdr replace) nil nil string))))
    string))

(defun ido-sort-mtime ()
  "Sort ido item by modified time."
  (let (ido-temp-list)
    (setq ido-temp-list
          (sort ido-temp-list
                (lambda (a b)
                  (let ((ta (nth 5 (file-attributes (concat ido-current-directory a))))
                        (tb (nth 5 (file-attributes (concat ido-current-directory b)))))
                    (if (= (nth 0 ta) (nth 0 tb))
                        (> (nth 1 ta) (nth 1 tb))
                      (> (nth 0 ta) (nth 0 tb)))))))
    (ido-to-end ;; move . files to end (again)
     (delq nil (mapcar
                (lambda (x) (if (string-equal (substring x 0 1) ".") x))
                ido-temp-list)))))

(defun delete-chars-hungry-forward (&optional reverse)
  "Delete chars forward use `hungry' style.
Optional argument REVERSE default is delete forward, if reverse is non-nil delete backward."
  (delete-region
   (point)
   (progn
     (if reverse
         (skip-chars-backward " \t\n\r")
       (skip-chars-forward " \t\n\r"))
     (point))))

(defun delete-chars-hungry-backward ()
  "Delete chars backward use `hungry' style."
  (delete-chars-hungry-forward t))

(defun reverse-chars-in-region (start end)
  "Reverse the region character by character without reversing lines."
  (interactive "r")
  (let ((str (buffer-substring start end)))
    (delete-region start end)
    (dolist (line (split-string str "\n"))
      (let ((chars (mapcar (lambda (c)
                             (or (matching-paren c)
                                 c))
                           (reverse (append line nil)))))
        (when chars
          (apply 'insert chars))
        (newline)))))

(defun uniquify-buffer-lines ()
  (while
      (progn
        (goto-char (point-min))
        (re-search-forward "^\\(.*\\)\n\\(\\(.*\n\\)*\\)\\1$" nil t))
    (if (= 0 (length (match-string 1)))
        (replace-match "\\2")
      (replace-match "\\1\n\\2"))))

(defun delete-other-windows-vertically+ ()
  "Delete all windows above or below the current window."
  (interactive)
  (let ((win (selected-window)))
    (save-excursion
      (while (condition-case nil (windmove-up) (error nil))
        (delete-window)
        (select-window win))
      (while (condition-case nil (windmove-down) (error nil))
        (delete-window)
        (select-window win)))))

(defun delete-other-windows-horizontally+ ()
  "Delete all windows left or right of the current window."
  (interactive)
  (let ((win (selected-window)))
    (save-excursion
      (while (condition-case nil (windmove-left) (error nil))
        (delete-window)
        (select-window win))
      (while (condition-case nil (windmove-right) (error nil))
        (delete-window)
        (select-window win)))))

(defun replace-match+ (object match-str replace-str)
  "Replace `MATCH-STR' of `OBJECT' with `REPLACE-STR'."
  (string-match match-str object)
  (replace-match replace-str nil nil object 0))

(defun root-user-p ()
  "Return true if the current user is the superuser, root, with user
  id zero."
  (zerop (user-real-uid)))

(defun another-window ()
  "Select the next window or split the current one."
  (interactive)
  (cond ((one-window-p)
         (split-window-vertically)
         (other-window 1)
         (switch-to-buffer nil))
        (t (other-window 1))))

(defun match-at-point (regexp)
  "Return the buffer substring around point matching REGEXP.
Look for a match starting at or before point.  Move back a character
at a time while still looking at a match ending at the same point.  If
no match is found at or before point, return the first match after
point, or nil if there is no match in the buffer."
  (let ((backup nil) (start nil) (end nil))
    (save-excursion
      (setq backup
            (or (looking-at regexp)
                (and (re-search-forward regexp nil 'limit)
                     (setq end t)
                     (goto-char (match-beginning 0))
                     nil)
                ;; failed search doesn't change match-data
                (re-search-backward regexp nil t)))
      (if (or backup end) (setq start (match-beginning 0)
                                end (match-end 0)))
      (if backup
          (while (and (not (bobp))
                      (progn (backward-char) t)
                      (looking-at regexp)
                      (= (match-end 0) end))
            (setq start (point)))
        (or (bobp) (re-search-forward regexp nil t))))
    (and start
         (progn (goto-char end) t)
         (buffer-substring start end))))

(defun number-at-point ()
  "Return the number at or before point as an integer."
  (let ((n (match-at-point "[0-9]+")))
    (if n (string-to-number n)
      (error "No number found"))))

(defun lock-screen ()
  "Lock screen using (zone) and xtrlock
calls M-x zone on all frames and runs xtrlock.
To use this extension, you need install xtrlock in your system."
  (interactive)
  (save-excursion
    (set-process-sentinel
     (start-process "xtrlock" nil "xtrlock")
     '(lambda (process event)
        (zone-leave-me-alone)))
    (zone)))

(defun underline-line-with (char)
  "Insert some char below at current line."
  (interactive "cType one char: ")
  (save-excursion
    (let ((length (- (point-at-eol) (point-at-bol))))
      (end-of-line)
      (insert "\n")
      (insert (make-string length char)))))

(defun try-require (&rest args)
  "Attempt to load a library or module. Return true if all of the libraries
given as arguments are successfully loaded"
  (if (member nil
              (mapcar (lambda (thing)
                        (condition-case e
                            (if (stringp thing)
                                (load-library thing)
                              (require thing))
                          (file-error () nil)))
                      args))
      nil
    t))

(defun sticky-window-keep-window-visible ()
  "Insure the buffer associated with the current window stays visible.
This is handy for ERC buffers where you would like to see the
conversation while you work in other windows within the frame.
This is intended to be used with `sticky-window-delete-window'."
  (interactive)
  (set-window-dedicated-p (selected-window) t))

(defun sticky-window-delete-window ()
  "This is intended to be a replacement for `delete-window', but
that avoids deleting windows that have been marked as dedicated
with `sticky-window-keep-window-visible'."
  (interactive)
  (let ((window (selected-window)))
    (if (and (not current-prefix-arg) (window-dedicated-p window))
        (error "This is a dedicated window. Use C-u prefix on this keybinding to really delete it.")
      (set-window-dedicated-p (selected-window) nil)
      (delete-window window))))

(defun sticky-window-delete-other-windows ()
  "Delete all other windows that are not marked to be visible with `sticky-window-keep-window-visible'."
  (interactive)
  (mapcar (lambda (window)
            (if (not (window-dedicated-p window))
                (delete-window window)))
          (cdr (window-list))))

(provide 'lazycat-toolkit)

;;; lazycat-toolkit.el ends here

;;; LocalWords:  lazycat emacser windresize ascii config shadowings cmd Async
;;; LocalWords:  login surpress Killall killall buf Fullscreen fullscreen cmt
;;; LocalWords:  opoint Uncomment uncomment ce dwim dotimes untabify origianl
;;; LocalWords:  fFind smb yyyy str sNotes FFile newsticker unmark paren ielm
;;; LocalWords:  nColumn NORECORD fileline sl fullpath unclutter scrot sPicture
;;; LocalWords:  doxymacs multiline recentf mwe sNew DNew newname ncZap pos eob
;;; LocalWords:  abscount mrm tmp cCopy nGoto minibuffers obarray terpri ido tb
;;; LocalWords:  prettyfy mtime uniquify dep sym dentry deps subr uid cType eol
;;; LocalWords:  bol args keybinding
