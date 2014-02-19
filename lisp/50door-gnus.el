;;; 50door-gnus.el ---

;; Copyright (C) 2009  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

(defvar winring-gnus-name "Gnus")

(defun door-gnus nil
  "Switch between gnus and non-gnus buffers, preserving window configurations."
  (interactive)
  (let ((alive-p (gnus-alive-p)))
    (if (string= winring-gnus-name winring-name)
        (if alive-p
            (winring-prev-configuration)
            (gnus 2))
        (winring+-jump-to-configuration winring-gnus-name)
        (unless (gnus-alive-p)
          (gnus 2)))))

(defun gnus-group-exit-or-door-bury (arg)
  (interactive "p")
  (if (= 4 arg)
      (progn
        (gnus-group-exit)
        (if (member winring-gnus-name (winring+-names))
            (winring+-delete-configuration winring-gnus-name)))
      (if (string= winring-gnus-name winring-name)
          (winring-prev-configuration)
          (bury-buffer))))

(eval-after-load "gnus"
  '(add-hook 'gnus-group-mode-hook '(lambda ()
                                     (define-key gnus-group-mode-map (kbd "q")
                                      'gnus-group-exit-or-door-bury))))
(global-set-key (kbd "C-c G") 'door-gnus)




;;; Code:



(provide '50door-gnus)
;;; 50door-gnus.el ends here
