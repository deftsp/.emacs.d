;;; 50wcheck-mode.el ---

;; Copyright (C) 2012  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>

(defvar paloryemacs/wcheck-mode-map (make-keymap) "wcheck mode key map")
(define-key paloryemacs/wcheck-mode-map "s" 'wcheck-mode)
(define-key paloryemacs/wcheck-mode-map "l" 'wcheck-change-language)
(define-key paloryemacs/wcheck-mode-map "c" 'wcheck-actions)
(define-key paloryemacs/wcheck-mode-map "n" 'wcheck-jump-forward)
(define-key paloryemacs/wcheck-mode-map "p" 'wcheck-jump-backward)


(if (fboundp 'key-chord-define-global)
    (key-chord-define-global ",w" paloryemacs/wcheck-mode-map))


(setq wcheck-language-data
      '(("Highlight Commentaires"
         (program . paloryemacs/commentaires-keywords-detec)
         (face . font-lock-warning-face)
         (regexp-start . "\\<")
         (regexp-body . "\\w*\\+?:?")
         (regexp-end . "")
         (read-or-skip-faces
          ((emacs-lisp-mode c-mode c++-mode objc-mode haskell-mode)
           read font-lock-comment-face font-lock-doc-face)
          (nil)))
        ("Trailing whitespace"
         (program . identity)
         (action-program . (lambda (marked-text)
                             (list (cons "Remove whitespace" ""))))
         (face . highlight)
         (regexp-start . "")
         (regexp-body . "[ \t]+")
         (regexp-end . "$")
         (regexp-discard . "")
         (read-or-skip-faces
          (nil)))))

(defvar paloryemacs/commentaires-keywords
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

(defun paloryemacs/commentaires-keywords-detec (strings)
  (let (found)
    (dolist (string strings found)
      (dolist (regexp paloryemacs/commentaires-keywords)
        (when (string-match regexp string)
          (push (match-string-no-properties 1 string) found)
          (return))))))

(provide '50wcheck-mode)
;;; 50wcheck-mode.el ends here
