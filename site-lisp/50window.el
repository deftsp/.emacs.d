;;; 50window.el ---

;; Copyright (C) 2009  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>
;; Keywords:

;;; key binding
(global-set-key (kbd"C-x x") 'delete-window)
(when window-system
  (global-set-key (kbd "M-`") 'other-window))

(global-set-key (kbd "H-M-,") '(lambda() (interactive) (scroll-other-window -1)))
(global-set-key (kbd "H-M-.") '(lambda() (interactive) (scroll-other-window 1)))

(global-set-key (kbd "H-[") 'shrink-window)
(global-set-key (kbd "H-]") 'enlarge-window)
(global-set-key (kbd "H-M-[") 'shrink-window-horizontally)
(global-set-key (kbd "H-M-]") 'enlarge-window-horizontally)


;;; toggle window dedicaton
;; set-window-dedicated-p
;; When a window is dedicated to its buffer, `display-buffer' will refrain
;; from displaying another buffer in it.
(defun pl/toggle-current-window-dedication ()
  (interactive)
  (let* ((window (selected-window))
         (dedicated (window-dedicated-p window)))
    (set-window-dedicated-p window (not dedicated))
    (message "Window %sdedicated to %s"
             (if dedicated "no longer " "")
             (buffer-name))))

(global-set-key (kbd "M-D") 'pl/toggle-current-window-dedication)

;;; window extension
;; http://www.emacswiki.org/emacs/window-extension.el
;; sticky-window-delete-other-windows

;;; winner mode
(winner-mode t)
(global-set-key (kbd "H-r") 'winner-redo)
(global-set-key (kbd "H-u") 'winner)
(global-set-key (kbd "H-u") 'winner-undo)

;;; windmove
(require 'windmove)
;; use key chord "jn "jp" "fn" "fp" instead
;; (global-set-key (kbd "M-P") 'windmove-up)
;; (global-set-key (kbd "M-N") 'windmove-down)
;; (global-set-key (kbd "M-B") 'windmove-left)
;; (global-set-key (kbd "M-F") 'windmove-right)


;;; horizontal <==> vertical
;; horizontal-to-vertical
(defun pl/window-horizontal-to-vertical ()
  "Switches from a horizontal split to a vertical split."
  (interactive)
  (let ((one-buf (window-buffer (selected-window)))
        (buf-point (point)))
    (other-window 1)
    (delete-other-windows)
    (split-window-horizontally)
    (switch-to-buffer one-buf)
    (goto-char buf-point)))

;; vertical-to-horizontal
(defun pl/window-vertical-to-horizontal ()
  "Switches from a vertical split to a horizontal split."
  (interactive)
  (let ((one-buf (window-buffer (selected-window)))
        (buf-point (point)))
    (other-window 1)
    (delete-other-windows)
    (split-window-vertically)
    (switch-to-buffer one-buf)
    (goto-char buf-point)))

;;; switch two windows
(global-set-key (kbd "C-x !") 'pl/swap-windows)
;; someday might want to rotate windows if more than 2 of them
(defun pl/swap-windows ()
  "If you have 2 windows, it swaps them."
  (interactive)
  (cond ((not (= (count-windows) 2))
         (message "You need exactly 2 windows to do this."))
        (t
         (let* ((w1 (first (window-list)))
                (w2 (second (window-list)))
                (b1 (window-buffer w1))
                (b2 (window-buffer w2))
                (s1 (window-start w1))
                (s2 (window-start w2)))
           (set-window-buffer w1 b2)
           (set-window-buffer w2 b1)
           (set-window-start w1 s2)
           (set-window-start w2 s1)))))


;;; for wide-screen display
;; http://emacswiki.org/emacs/display-buffer-for-wide-screen.el
;; minimum width of window to be split horizontally for `display-buffer'
(setq split-width-threshold 200
      split-height-threshold 20)

;; (defvaralias 'split-window-horizontally-threshold-width 'split-width-threshold)

;;; This function is originally written by Tassilo Horn.
;;; Rubikitch modified slightly.
;;; http://www.mail-archive.com/emacs-pretest-bug@gnu.org/msg11469.html
;; (defun display-buffer-function-according-to-window-width (buffer force-other-window &rest ignored)
;;   "If BUFFER is visible, select it.

;; If it's not visible and there's only one window, split the
;; current window and select BUFFER in the new window. If the
;; current window (before the split) is more than
;; `split-window-horizontally-threshold-width' columns wide,
;; split horizontally, else split vertically.

;; If the current buffer contains more than one window, select
;; BUFFER in the least recently used window.

;; This function returns the window which holds BUFFER.

;; FORCE-OTHER-WINDOW is ignored."
;;   (or (get-buffer-window buffer)
;;       (and special-display-function
;;            (or (member (buffer-name buffer) special-display-buffer-names)
;;                (some (lambda (re) (string-match re (buffer-name buffer))) special-display-regexps))
;;            (funcall special-display-function buffer))
;;       (if (one-window-p)
;;           (let ((new-win (if (> (window-width) (or split-window-horizontally-threshold-width 160)) ;originally 165
;;                              (split-window-horizontally)
;;                            (split-window-vertically))))
;;             (set-window-buffer new-win buffer)
;;             new-win)
;;         (let ((new-win (get-lru-window)))
;;           (set-window-buffer new-win buffer)
;;           new-win))))

;; (setq display-buffer-function 'display-buffer-function-according-to-window-width)



;;;;;;;;;;;;;;;;;

;;; special window
;; thanks to http://stackoverflow.com/questions/1002091/how-to-force-emacs-not-to-display-buffer-in-a-specific-window
;; (mapcar #'(lambda (buffer-name)
;;             (add-to-list 'special-display-buffer-names buffer-name))
;;         (list "*Ido Completions*" "*Completions*"))

;; (setq special-display-function 'pl/display-special-buffer-popup)
;; (add-to-list 'special-display-regexps ".*")  ; match any window

;; (defun pl/display-special-buffer-popup (buffer &optional args)
;;   "put the special buffers in the bottom right"
;;   ;; The top left corner of the frame is considered to be row 0,
;;   ;; column 0.
;;   (let* ((target-window (window-at 6 (- (frame-height) 6)))
;;          (pop-up-windows t)
;;          (window (window--try-to-split-window target-window)))
;;     (set-window-buffer window buffer)))

;;; recursive-edit
;; Hitting key chord ",r" will put you in a "recursive editing mode", that is simply an embedded call to the editing loop. The
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

;;; ",r" key-chord
(defun pl/recursive-edit-save-window-config ()
  (interactive)
  (save-window-excursion
    (save-excursion
      (recursive-edit))))

;; RecursiveEditPreservingWindowConfig
;; One can change the window configuration temporarily using RecursiveEdit?.
;; Inspired by a command posted by ErikNaggum in an Emacs Newsgroup, EmilioLopes
;; wrote this macro:

;; inspired by Erik Naggum's `recursive-edit-with-single-window'
(defmacro pl/recursive-edit-preserving-window-config (body)
  "*Return a command that enters a recursive edit after executing BODY.
 Upon exiting the recursive edit (with \\[exit-recursive-edit] (exit)
 or \\[abort-recursive-edit] (abort)), restore window configuration
 in current frame."
  `(lambda ()
     "See the documentation for `pl/recursive-edit-preserving-window-config'."
     (interactive)
     (save-window-excursion
       ,body
       (recursive-edit))))

;; Use it like this:

(global-set-key (kbd "C-c 0") (pl/recursive-edit-preserving-window-config (delete-window)))
(global-set-key (kbd "C-c 2") (pl/recursive-edit-preserving-window-config
                               (split-window-vertically 20)))
(global-set-key (kbd "C-c 3") (pl/recursive-edit-preserving-window-config
                               (split-window-horizontally -52)))
(global-set-key (kbd "C-c 1") (pl/recursive-edit-preserving-window-config
                               (if (one-window-p 'ignore-minibuffer)
                                   (message "Current window is the only window in its frame")
                                 (delete-other-windows))))

;; Now pressing "C-c 1" will delete all other windows in the current frame and put
;; you into "recursive editing". You know you are in a recursive edit by noting the
;; square brackets around the parentheses that always surround the major and minor
;; mode names. After exiting recursive edit, e.g. by using "C-M-c"
;; ('exit-recursive-edit'), the original window configuration is restored.

;; recursive-edit end there ---------------------------------------------------------------



;;; switch window
(eval-after-load "switch-window"
  '(setq switch-window-shortcut-style 'alphabet))

;;; golden-ratio.el
(require 'golden-ratio nil t)
(eval-after-load "golden-ratio"
  '(progn
     (golden-ratio-mode t)
     (add-to-list 'golden-ratio-exclude-modes "ediff-mode")
     (add-to-list 'golden-ratio-exclude-modes "w3m-select-buffer-mode")
     (add-to-list 'golden-ratio-exclude-modes "w3m-mode")
     (add-to-list 'golden-ratio-inhibit-functions 'pl/helm-alive-p)
     (require 'ediff nil t)
     (if (boundp 'ediff-this-buffer-ediff-sessions)
         (add-to-list 'golden-ratio-inhibit-functions 'pl/ediff-comparison-buffer-p))))

(defun pl/ediff-comparison-buffer-p ()
  ediff-this-buffer-ediff-sessions)

(defun pl/helm-alive-p ()
  (if (boundp 'helm-alive-p)
      (symbol-value 'helm-alive-p)))

(provide '50window)
