;;; 50window.el ---

;; Copyright (C) 2009  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>
;; Keywords:


;; set-window-dedicated-p
;; When a window is dedicated to its buffer, `display-buffer' will refrain
;; from displaying another buffer in it.

;; http://www.emacswiki.org/emacs/window-extension.el
;; sticky-window-delete-other-windows




(winner-mode t)

(require 'windmove)
(global-set-keys "H-r" 'winner-redo
                 "H-u" 'winner-undo
                 "M-P" 'windmove-up
                 "M-N" 'windmove-down
                 "M-B" 'windmove-left
                 "M-F" 'windmove-right)

;; (global-set-key "\C-x2" '(lambda ()
;;                           (interactive)
;;                           (split-window-vertically -22)))
;; (global-set-key "\C-x3" '(lambda ()
;;                           (interactive)
;;                           (split-window-horizontally -80)))

(global-set-key (kbd "H-M-,") '(lambda() (interactive) (scroll-other-window -1)))
(global-set-key (kbd "H-M-.") '(lambda() (interactive) (scroll-other-window 1)))


(global-set-key (kbd "H-[") 'shrink-window)
(global-set-key (kbd "H-]") 'enlarge-window)
(global-set-key (kbd "H-M-[") 'shrink-window-horizontally)
(global-set-key (kbd "H-M-]") 'enlarge-window-horizontally)

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


(defun toggle-current-window-dedication ()
  (interactive)
  (let* ((window    (selected-window))
         (dedicated (window-dedicated-p window)))
    (set-window-dedicated-p window (not dedicated))
    (message "Window %sdedicated to %s"
             (if dedicated "no longer " "")
             (buffer-name))))

(global-set-key (kbd "M-D") 'toggle-current-window-dedication)

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



;;; switch window
(eval-after-load "switch-window"
  '(setq switch-window-shortcut-style 'alphabet))

(provide '50window)
