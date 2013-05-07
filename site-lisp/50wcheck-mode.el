;;; 50wcheck-mode.el ---

;; Copyright (C) 2012  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>

(defvar pl/wcheck-mode-map (make-keymap) "wcheck mode key map")
(define-key pl/wcheck-mode-map "s" 'wcheck-mode)
(define-key pl/wcheck-mode-map "l" 'wcheck-change-language)
(define-key pl/wcheck-mode-map "c" 'wcheck-actions)
(define-key pl/wcheck-mode-map "n" 'wcheck-jump-forward)
(define-key pl/wcheck-mode-map "p" 'wcheck-jump-backward)


(if (fboundp 'key-chord-define-global)
    (key-chord-define-global ",w" pl/wcheck-mode-map))



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
