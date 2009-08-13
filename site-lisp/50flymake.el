;;; 50flymake.el ---

;; Copyright (C) 2008  S.P.Tseng

;; Author: S.P.Tseng <deftsp@gmail.com>

;;----------------------------------------------------------------------------------------------------
;; flymake mode
;;----------------------------------------------------------------------------------------------------
;; What you need is only a Makefile with an extra target "check-syntax":

;; check-syntax:
;;       gcc -o nul -Wall -Wextra -fsyntax-only $(CHK_SOURCES)
;;flymode mode ends here------------------------------------------------------------------------------

;;; align
;; (when (require 'align nil t)
;;   (setq xsteve-c-align-rules-list
;;         `((c-comment-one-line
;;            (regexp . "[^- \t]\\(\\s-*\\)/\\*.*\\*/$")
;;            (group  . 1)
;;            (repeat . nil))

;;           (c-macro-definition
;;            (regexp   . "^\\s-*#\\s-*define\\s-+\\S-+\\(\\s-+\\)"))

;;           (c-macro-line-continuation
;;            (regexp   . "\\(\\s-*\\)\\\\$")
;;            (column   . c-backslash-column))

;;           (c-variable-declaration
;;            (regexp   . ,(concat "[*&0-9A-Za-z_]>?[&*]*\\(\\s-+[*&]*\\)"
;;                                 "[A-Za-z_][0-9A-Za-z:_]*\\s-*\\(\\()\\|"
;;                                 "=[^=\n].*\\|(.*)\\|\\(\\[.*\\]\\)*\\)?"
;;                                 "\\s-*[;,]\\|)\\s-*$\\)"))
;;            (group    . 1)
;;            (justify  . t)
;;            (valid
;;             . ,(function
;;                 (lambda ()
;;                   (not (or (save-excursion
;;                              (goto-char (match-beginning 1))
;;                              (backward-word 1)
;;                              (looking-at
;;                               "\\(goto\\|return\\|new\\|delete\\|throw\\)"))
;;                            (if (and (boundp 'font-lock-mode) font-lock-mode)
;;                                (eq (cadr (memq 'face (text-properties-at (point))))
;;                                    'font-lock-comment-face)
;;                              (eq (caar (c-guess-basic-syntax)) 'c))))))))

;;           (c-assignment
;;            (regexp   . ,(concat "[^-=!^&*+<>/| \t\n]\\(\\s-*[-=!^&*+<>/|]*\\)"
;;                                 "=\\(\\s-*\\)\\([^= \t\n]\\|$\\)"))
;;            (group    . (1 2))
;;            (justify  . t)
;;            (tab-stop . nil))

;;           (c-chain-logic
;;            (regexp   . "\\(\\s-*\\)\\(&&\\|||\\|\\<and\\>\\|\\<or\\>\\)")
;;            (modes    . align-c++-modes)
;;            (valid    . ,(function
;;                          (lambda ()
;;                            (save-excursion
;;                              (goto-char (match-end 2))
;;                              (looking-at "\\s-*\\(/[*/]\\|$\\)"))))))
;;           ))

;;   (add-hook 'c-mode-hook (lambda () (setq align-mode-rules-list xsteve-c-align-rules-list))))
