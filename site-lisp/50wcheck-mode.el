;;; 50wcheck-mode.el ---

;; Copyright (C) 2012  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>


(global-set-key (kbd "C-c w s") 'wcheck-mode)
(global-set-key (kbd "C-c w l") 'wcheck-change-language)
(global-set-key (kbd "C-c w c") 'wcheck-actions)
(global-set-key (kbd "C-c w n") 'wcheck-jump-forward)
(global-set-key (kbd "C-c w p") 'wcheck-jump-backward)



;; Ensure that the variable exists.
(defvar wcheck-language-data nil)

(pushnew '("Highlight Commentaires"
           (program . pl/commentaires-keywords-detec)
           (face . font-lock-warning-face)
           (regexp-start . "\\<")
           (regexp-body . "\\w*\\+?:?")
           (regexp-end . "")
           (read-or-skip-faces
            ((emacs-lisp-mode c-mode c++-mode objc-mode)
             read font-lock-comment-face font-lock-doc-face)
            (nil)))
         wcheck-language-data :test #'equal)

(defvar pl/commentaires-keywords
  '("\\<\\(FIXME\\):"
    "\\<\\(XXX\\+\\):"
    "\\<\\(TODO\\):"
    "\\<\\(BUG\\):"
    "\\<\\(WARNING\\)"
    "\\<\\(NOTE\\)"
    "\\<\\(NOTES\\)"
    "\\<\\(DEBUG\\)"
    "\\<\\(OUTPUT\\)"
    "\\<\\(IMPORTANT\\)"))

(defun pl/commentaires-keywords-detec (strings)
  (let (found)
    (dolist (string strings found)
      (dolist (regexp pl/commentaires-keywords)
        (when (string-match regexp string)
          (push (match-string-no-properties 1 string) found)
          (return))))))

(provide '50wcheck-mode)
;;; 50wcheck-mode.el ends here
