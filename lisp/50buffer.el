;;; 50buffer.el ---
;; Author: Shihpin Tseng <deftsp@gmail.com>

;;; clean buffer list
;;; midnight
;; What would life be like if there were only saving, and never killing? These two settings tell Emacs to, every
;; midnight, kill regular buffers which haven't been used in a while.
;; <http://www.gnu.org/software/emacs/manual/html_node/Kill-Buffer.html>
(require 'midnight)

;; By default, `clean-buffer-list' will kill buffers that haven't been visited in 3 days, or in the last
;; hour in the case of special buffer that are specified by `clean-buffer-list-kill-buffer-names'
(setq clean-buffer-list-delay-general 2)
;; special buffers are cleaned every 6 hours
(setq clean-buffer-list-delay-special (* 6 3600))

(setq midnight-period (* 3 60 60))      ; occur every 3 hours default 86400 (24 hours)
;; note period timer will occuer, even the time when set the timer (boot the emacs) is after
;; "09:30am". see more..  run-at-time.
;; (midnight-delay-set 'midnight-delay "09:30am")
;; set midnight-delay after 3 hours
(midnight-delay-set 'midnight-delay (tl/future-time-string (* 3 60 60)))



;; As of 2009-10-02, DeskTop mode does not preserve the value of 'buffer-display-time' for buffers,
;; so the buffer's "age" is effectively restarted. This means that buffers restored by a Desktop
;; sessions are considered "new" by CleanBufferList, even though they may be considered"old".

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

(defun tl/safe-revert-buffer ()
  "Prompt before reverting the file."
  (interactive)
  (revert-buffer nil nil))

(defun tl/safe-erase-buffer ()
  "Prompt before erasing the content of the file."
  (interactive)
  (if (y-or-n-p (format "Erase content of buffer %s ? " (current-buffer)))
      (erase-buffer)))

;; https://tsdh.wordpress.com/2007/03/28/deleting-windows-vertically-or-horizontally/
(defun tl/maximize-horizontally ()
  "Delete all windows left or right of the current window."
  (interactive)
  (require 'windmove)
  (save-excursion
    (while (condition-case nil (windmove-left) (error nil))
      (delete-window))
    (while (condition-case nil (windmove-right) (error nil))
      (delete-window))))

(defun tl/toggle-centered-buffer-mode ()
  "Toggle `tl-centered-buffer-mode'."
  (interactive)
  (when (require 'centered-buffer-mode nil t)
    (call-interactively 'tl-centered-buffer-mode)))

(defun tl/toggle-centered-buffer-mode-frame ()
  "Open current buffer in the new frame centered and without mode-line."
  (interactive)
  (when (require 'centered-buffer-mode nil t)
    (switch-to-buffer-other-frame (current-buffer) t)
    (toggle-frame-fullscreen)
    (run-with-idle-timer
     ;; FIXME: We need this delay to make sure that the
     ;; `toggle-frame-fullscreen' fully "finished"
     ;; it will be better to use something more reliable
     ;; instead :)
     1
     nil
     (lambda ()
       (call-interactively 'tl-centered-buffer-mode)
       (setq mode-line-format nil)))))

(defun tl/centered-buffer-mode-full-width ()
  "Center buffer in the frame."
  ;; FIXME Needs new key-binding.
  (interactive)
  (when (require 'centered-buffer-mode nil t)
    (tl/maximize-horizontally)
    (call-interactively 'tl-centered-buffer-mode)))

(defun tl/new-empty-buffer (&optional split)
  "Create a new buffer called untitled(<n>).
A SPLIT argument with the value: `left',
`below', `above' or `right', opens the new
buffer in a split window."
  (interactive)
  (let ((newbuf (generate-new-buffer "untitled")))
    (case split
      ('left  (split-window-horizontally))
      ('below (tl/split-window-vertically-and-switch))
      ('above (split-window-vertically))
      ('right (tl/split-window-horizontally-and-switch)))
    ;; Prompt to save on `save-some-buffers' with positive PRED
    (with-current-buffer newbuf
      (setq-local buffer-offer-save t))
    ;; pass non-nil force-same-window to prevent `switch-to-buffer' from
    ;; displaying buffer in another window
    (switch-to-buffer newbuf nil 'force-same-window)))

(defun tl/new-empty-buffer-left ()
  "Create a new buffer called untitled(<n>),
in a split window to the left."
  (interactive)
  (tl/new-empty-buffer 'left))

(defun tl/new-empty-buffer-below ()
  "Create a new buffer called untitled(<n>),
in a split window below."
  (interactive)
  (tl/new-empty-buffer 'below))

(defun tl/new-empty-buffer-above ()
  "Create a new buffer called untitled(<n>),
in a split window above."
  (interactive)
  (tl/new-empty-buffer 'above))

(defun tl/new-empty-buffer-right ()
  "Create a new buffer called untitled(<n>),
in a split window to the right."
  (interactive)
  (tl/new-empty-buffer 'right))

;; http://stackoverflow.com/a/10216338/4869
(defun tl/copy-whole-buffer-to-clipboard ()
  "Copy entire buffer to clipboard"
  (interactive)
  (clipboard-kill-ring-save (point-min) (point-max)))

(defun tl/copy-clipboard-to-whole-buffer ()
  "Copy clipboard and replace buffer"
  (interactive)
  (delete-region (point-min) (point-max))
  (clipboard-yank)
  (deactivate-mark))

;; our own implementation of kill-this-buffer from menu-bar.el
(defun tl/kill-this-buffer (&optional arg)
  "Kill the current buffer.
If the universal prefix argument is used then kill also the window."
  (interactive "P")
  (if (window-minibuffer-p)
      (abort-recursive-edit)
    (if (equal '(4) arg)
        (kill-buffer-and-window)
      (kill-buffer))))

(defun tl/ace-kill-this-buffer (&optional arg)
  "Ace kill visible buffer in a window.
If the universal prefix argument is used then kill also the window."
  (interactive "P")
  (require 'ace-window)
  (let (golden-ratio-mode)
    (aw-select
     " Ace - Kill buffer in Window"
     (lambda (window)
       (with-selected-window window
         (tl/kill-this-buffer arg))))))

;; found at http://emacswiki.org/emacs/KillingBuffers
(defun tl/kill-other-buffers (&optional arg)
  "Kill all other buffers.
If the universal prefix argument is used then will the windows too."
  (interactive "P")
  (when (yes-or-no-p (format "Killing all buffers except \"%s\"? "
                             (buffer-name)))
    (mapc 'kill-buffer (delq (current-buffer) (buffer-list)))
    (when (equal '(4) arg) (delete-other-windows))
    (message "Buffers deleted!")))


(provide '50buffer)
