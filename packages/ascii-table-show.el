;;; ascii-table-show.el ---

;; Copyright (C) 2008  S.P.Tseng

;; Author: S.P.Tseng <deftsp@gmail.com>
;; Keywords:

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:


(defvar ascii-table-mode-map nil "Keymap for ascii-table mode")
(unless ascii-table-mode-map
  (setq ascii-table-mode-map (make-sparse-keymap))
  (suppress-keymap ascii-table-mode-map)

  (define-key ascii-table-mode-map "." '(lambda () (goto-char (point-main))))
  (define-key ascii-table-mode-map "q" 'kill-this-buffer)
  (define-key ascii-table-mode-map " " 'scroll-up)
  (define-key ascii-table-mode-map (kbd "DEL") 'scroll-down))

(defun ascii-table-mode ()
  "This is a mode for display ascii-table"
  (kill-all-local-variables)
  (buffer-disable-undo)
  (use-local-map ascii-table-mode-map)
  (setq major-mode 'ascii-table-mode)
  (setq mode-name "ascii-table"))

;;;###autoload
(defun ascii-table-show ()
  "Print the ascii table"
  (interactive)
  (switch-to-buffer "*ASCII table*")
  (auto-fill-mode -1)
  (erase-buffer)
  (let ((i   0)
        (tmp 0))
    (insert (propertize
             "                      [ASCII table] (0 thru 255)\n\n"
             'face font-lock-comment-face))
    (insert (propertize
             "OCT  Dec Hex     Char | OCT  Dec Hex     Char | OCT  Dec Hex     Char | OCT Dec  Hex     Char\n"
             'face font-lock-comment-face))
    (while (< i 32)
      (dolist (tmp (list i (+ 32 i) (+ 64 i) (+ 96 i)))
        (insert (concat
                 (propertize (format "%3o  " tmp)
                             'face font-lock-variable-name-face)
                 (propertize (format "%3d  " tmp)
                             'face font-lock-constant-face)
                 (propertize (format "%2x  " tmp)
                             'face font-lock-function-name-face)
                 "   "
                 (propertize (format "%4s" (single-key-description tmp))
                             'face font-lock-string-face)
                 (unless (= tmp (+ 96 i))
                   (propertize " | " 'face font-lock-variable-name-face)))))
      (newline)
      (setq i (+ i 1)))
    (newline)
    (setq i 128)
    (while (< i 160)
      (dolist (tmp (list i (+ 32 i) (+ 64 i) (+ 96 i)))
        (insert (concat
                 (propertize (format "%3o  " tmp)
                             'face font-lock-variable-name-face)
                 (propertize (format "%3d  " tmp)
                             'face font-lock-constant-face)
                 (propertize (format "%2x  " tmp)
                             'face font-lock-function-name-face)
                 "   "
                 (propertize (format "%4s" (single-key-description tmp))
                             'face font-lock-string-face)
                 (unless (= tmp (+ 96 i))
                   (propertize " | " 'face font-lock-variable-name-face)))))
      (newline)
      (setq i (+ i 1)))

    (goto-char (point-min)))
  (ascii-table-mode)
  (toggle-read-only 1))

(provide 'ascii-table-show)
;;; ascii-table-show.el ends here
