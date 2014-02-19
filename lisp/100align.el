;;; 100align.el ---

;; Copyright (C) 2012  Shihpin Tseng

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

;;; Code:

;;; align

(defvar pl/c-align-rules-list nil "C/C++ mode align rules")

(eval-after-load "align"
  '(progn
     (setq pl/c-align-rules-list
           `((c-comment-one-line
              (regexp . "[^- \t]\\(\\s-*\\)/\\*.*\\*/$")
              (group  . 1)
              (repeat . nil))

             (c-macro-definition
              (regexp   . "^\\s-*#\\s-*define\\s-+\\S-+\\(\\s-+\\)")
              (modes . align-c++-modes)

             (c-macro-line-continuation
              (regexp   . "\\(\\s-*\\)\\\\$")
              (column   . c-backslash-column))

             (c-variable-declaration
              (regexp   . ,(concat "[*&0-9A-Za-z_]>?[&*]*\\(\\s-+[*&]*\\)"
                                   "[A-Za-z_][0-9A-Za-z:_]*\\s-*\\(\\()\\|"
                                   "=[^=\n].*\\|(.*)\\|\\(\\[.*\\]\\)*\\)?"
                                   "\\s-*[;,]\\|)\\s-*$\\)"))
              (group    . 1)
              (justify  . t)
              (valid
               . ,(function
                   (lambda ()
                     (not (or (save-excursion
                                (goto-char (match-beginning 1))
                                (backward-word 1)
                                (looking-at
                                 "\\(goto\\|return\\|new\\|delete\\|throw\\)"))
                              (if (and (boundp 'font-lock-mode) font-lock-mode)
                                  (eq (cadr (memq 'face (text-properties-at (point))))
                                      'font-lock-comment-face)
                                (eq (caar (c-guess-basic-syntax)) 'c))))))))

             (c-assignment
              (regexp   . ,(concat "[^-=!^&*+<>/| \t\n]\\(\\s-*[-=!^&*+<>/|]*\\)"
                                   "=\\(\\s-*\\)\\([^= \t\n]\\|$\\)"))
              (group    . (1 2))
              (justify  . t)
              (tab-stop . nil))

             (c-chain-logic
              (regexp   . "\\(\\s-*\\)\\(&&\\|||\\|\\<and\\>\\|\\<or\\>\\)")
              (modes    . align-c++-modes)
              (valid    . ,(function
                            (lambda ()
                              (save-excursion
                                (goto-char (match-end 2))
                                (looking-at "\\s-*\\(/[*/]\\|$\\)")))))))))
     (add-hook 'c-mode-hook (lambda () (setq align-mode-rules-list pl/c-align-rules-list)))
     (add-hook 'c++-mode-hook (lambda () (setq align-mode-rules-list pl/c-align-rules-list)))))



(provide '100align)
;;; 50align.el ends here
