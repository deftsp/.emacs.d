;;; 50window-operate.el ---

;; Copyright (C) 2009  S.P.Tseng

;; Author: S.P.Tseng <deftsp@gmail.com>
;; Keywords:

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
(defun deftsp-window-horizontal-to-vertical ()
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
(defun deftsp-window-vertical-to-horizontal ()
  "Switches from a vertical split to a horizontal split."
  (interactive)
  (let ((one-buf (window-buffer (selected-window)))
        (buf-point (point)))
    (other-window 1)
    (delete-other-windows)
    (split-window-vertically)
    (switch-to-buffer one-buf)
    (goto-char buf-point)))

(global-set-key (kbd "C-x !") 'deftsp-swap-windows)
;; someday might want to rotate windows if more than 2 of them
(defun deftsp-swap-windows ()
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