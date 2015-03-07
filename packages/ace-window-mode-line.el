;;; ace-window-mode-line.el --- Show ace window key on mode-line. -*- lexical-binding: t -*-

;; Copyright (C) 2014 Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>

;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; Add ace-window key to mode-line
;; Thanks to window-numbering.el.

;;; Code:
(require 'ace-window)

(defcustom aw-mode-line-key-display-on t
  "Toggles Workgroups' mode-line display."
  :type 'boolean
  :group 'ace-window
  :set (lambda (sym val)
         (custom-set-default sym val)
         (force-mode-line-update)))

(defface aw-mode-line-face
  '((t (:foreground "red")))
  "Face used for ace window key in the mode-line.")

(defcustom ace-window-mode-line-format "#%c" "ace window mode-line format.")

(defcustom ace-window-mode-line-position 1
  "The position in the mode-line `ace-window-mode' displays the number.")

(defvar aw--table nil "window and key hash map")

(defvar aw--mode-line-show-key nil "ace window mode line show key mode.")

(defun aw--update ()
  (let* ((key-list aw-keys)
         (wnd-list (aw-window-list)))
    (clrhash aw--table)
    (dolist (win wnd-list)
      (let ((key (car key-list)))
        (when key
          (setq key-list (cdr key-list))
          (puthash win key aw--table))))))

(defun ace-window-mode-line-show-key (&optional arg)
  "Show ace window key on mode-line on and off
ARG is nil - toggle
ARG >= 1   - turn on
ARG == 0   - turn off
ARG is anything else, turn on `workgroups-mode'."
  (interactive (list current-prefix-arg))
  (setq aw--mode-line-show-key
        (cond ((not arg) (not aw--mode-line-show-key))
              ((integerp arg) (if (> arg 0) t nil))
              (t)))
  (cond (aw--mode-line-show-key
         (unless aw--table
           (save-excursion
             (setq aw--table (make-hash-table :size (length aw-keys)))
             (aw--install-mode-line)
             (add-hook 'window-configuration-change-hook 'aw--update))))
        (t
         (aw--clear-mode-line)
         (remove-hook 'window-configuration-change-hook 'aw--update)
         (setq aw--table nil)))
  (message (concat "Ace window mode line show key mode: "
                   (if aw--mode-line-show-key "on" "off")))
  aw--mode-line-show-key)

(defun aw--mode-line-key-string (&optional window)
  "Format the WINDOW to string."
  (let* ((k (gethash (or window (selected-window)) aw--table))
         (s (format ace-window-mode-line-format k)))
    (propertize s 'face 'aw-mode-line-face)))

(defun aw--install-mode-line (&optional position)
  "Install the window number from `ace-window-mode' to the mode-line at POSITION."
  (let ((mode-line (default-value 'mode-line-format))
        (res))
    (dotimes (i (min (or position ace-window-mode-line-position)
                     (length mode-line)))
      (push (car mode-line) res)
      (pop mode-line))
    (push '(aw-mode-line-key-display-on (:eval (aw--mode-line-key-string))) res)
    (while mode-line
      (push (car mode-line) res)
      (pop mode-line))
    (set-default 'mode-line-format (nreverse res)))
  (force-mode-line-update t))

(defun aw--clear-mode-line ()
  "Remove the window number of `ace-window-mode' from the mode-line."
  (let ((mode-line (default-value 'mode-line-format))
        (res))
    (while mode-line
      (let ((item (car mode-line)))
        (unless (equal item '(aw-mode-line-key-display-on
                              (:eval (aw--mode-line-key-string))) )
          (push item res)))
      (pop mode-line))
    (set-default 'mode-line-format (nreverse res)))
  (force-mode-line-update t))

(provide 'ace-window-mode-line)
;;; ace-window-mode-line ends here
