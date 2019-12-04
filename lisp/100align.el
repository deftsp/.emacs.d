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

;;; Code:

;;; align
(defvar tl/c-align-rules-list nil "C/C++ mode align rules")

(use-package align
  :defer t
  :init
  (progn
    (setq tl/c-align-rules-list
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
    (add-hook 'c-mode-hook (lambda () (setq align-mode-rules-list tl/c-align-rules-list)))
    (add-hook 'c++-mode-hook (lambda () (setq align-mode-rules-list tl/c-align-rules-list)))))


;; BEGIN align functions

;; modified function from http://emacswiki.org/emacs/AlignCommands
(defun tl/align-repeat (start end regexp &optional justify-right after)
  "Repeat alignment with respect to the given regular expression.
If JUSTIFY-RIGHT is non nil justify to the right instead of the
left. If AFTER is non-nil, add whitespace to the left instead of
the right."
  (interactive "r\nsAlign regexp: ")
  (let* ((ws-regexp (if (string-empty-p regexp)
                        "\\(\\s-+\\)"
                      "\\(\\s-*\\)"))
         (complete-regexp (if after
                              (concat regexp ws-regexp)
                            (concat ws-regexp regexp)))
         (group (if justify-right -1 1)))

    (unless (use-region-p)
      (save-excursion
        (while (and
                (string-match-p complete-regexp (thing-at-point 'line))
                (= 0 (forward-line -1)))
          (setq start (point-at-bol))))
      (save-excursion
        (while (and
                (string-match-p complete-regexp (thing-at-point 'line))
                (= 0 (forward-line 1)))
          (setq end (point-at-eol)))))

    (align-regexp start end complete-regexp group 1 t)))

;; Modified answer from http://emacs.stackexchange.com/questions/47/align-vertical-columns-of-numbers-on-the-decimal-point
(defun tl/align-repeat-decimal (start end)
  "Align a table of numbers on decimal points and dollar signs (both optional)"
  (interactive "r")
  (require 'align)
  (align-region start end nil
                '((nil (regexp . "\\([\t ]*\\)\\$?\\([\t ]+[0-9]+\\)\\.?")
                       (repeat . t)
                       (group 1 2)
                       (spacing 1 1)
                       (justify nil t)))
                nil))

(defmacro tl|create-align-repeat-x (name regexp &optional justify-right default-after)
  (let ((new-func (intern (concat "tl/align-repeat-" name))))
    `(defun ,new-func (start end switch)
       (interactive "r\nP")
       (let ((after (not (eq (if switch t nil) (if ,default-after t nil)))))
         (tl/align-repeat start end ,regexp ,justify-right after)))))

(tl|create-align-repeat-x "comma" "," nil t)
(tl|create-align-repeat-x "semicolon" ";" nil t)
(tl|create-align-repeat-x "colon" ":" nil t)
(tl|create-align-repeat-x "equal" "=")
(tl|create-align-repeat-x "math-oper" "[+\\-*/]")
(tl|create-align-repeat-x "percent" "%")
(tl|create-align-repeat-x "ampersand" "&")
(tl|create-align-repeat-x "bar" "|")
(tl|create-align-repeat-x "left-paren" "(")
(tl|create-align-repeat-x "right-paren" ")" t)
(tl|create-align-repeat-x "left-curly-brace" "{")
(tl|create-align-repeat-x "right-curly-brace" "}" t)
(tl|create-align-repeat-x "left-square-brace" "\\[")
(tl|create-align-repeat-x "right-square-brace" "\\]" t)
(tl|create-align-repeat-x "backslash" "\\\\")
(tl|create-align-repeat-x "quote" " '")

;; END align functions


(provide '100align)
;;; 50align.el ends here
