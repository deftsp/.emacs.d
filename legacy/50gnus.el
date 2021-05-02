;;; 50gnus.el ---

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

;;; Code:
(use-package gnus
  :defer t
  :init
  (progn
    (setq gnus-init-file "~/.emacs.d/.gnus.el")
    ;; (setq gnus-startup-file "~/.emacs.d/.newsrc")
    (setq gnus-inhibit-startup-message t))
  :config
  (progn
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

    (define-key gnus-group-mode-map (kbd "q") 'gnus-group-exit-or-door-bury)
    (global-set-key (kbd "C-c G") 'door-gnus)))


;; gnus 默认采用 rfc2231 对附件文件名进行编码，有些 MUA 无法识别这种编码。
;; 现在比较流行的方式是采用 base64 对附件文件名进行编码。可以采用如下设定，让 gnus 也采用 base64 编码文件名：

;; 如果你是脱离 Gnus 发 Mail(如 C-x m)一定要放在 ~/.emacs 而不是 ~/.gnus.el
(defalias 'mail-header-encode-parameter 'rfc2047-encode-parameter)

(eval-after-load "rfc2047"
  '(progn
     (add-to-list 'rfc2047-charset-encoding-alist '(gbk . B))
     (add-to-list 'rfc2047-charset-encoding-alist '(gb18030 . B))))


(provide '50gnus)
;;; 50gnus.el ends here
