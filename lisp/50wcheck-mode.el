;;; 50wcheck-mode.el ---

;; Copyright (C) 2012  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>

(use-package wcheck-mode
  :defer t
  :init
  (setq-default wcheck-language "Highlight Commentaires")
  (add-hook 'prog-mode-hook 'wcheck-mode)

  (defvar tl/wcheck-mode-map (make-keymap) "wcheck mode key map")
  (define-key tl/wcheck-mode-map "s" 'wcheck-mode)
  (define-key tl/wcheck-mode-map "l" 'wcheck-change-language)
  (define-key tl/wcheck-mode-map "c" 'wcheck-actions)
  (define-key tl/wcheck-mode-map "a" 'wcheck-actions)
  (define-key tl/wcheck-mode-map "n" 'wcheck-jump-forward)
  (define-key tl/wcheck-mode-map "p" 'wcheck-jump-backward)
  (with-eval-after-load "key-chord"
    (key-chord-define-global ",w" tl/wcheck-mode-map)))

(setq wcheck-language-data-defaults
      '((read-or-skip-faces
         (latex-mode
          read
          nil ; normal text
          font-latex-bold-face
          font-latex-italic-face
          font-latex-sectioning-0-face
          font-latex-sectioning-1-face
          font-latex-sectioning-2-face
          font-latex-sectioning-3-face
          font-latex-sectioning-4-face
          font-latex-sectioning-5-face
          font-latex-slide-title-face
          font-lock-type-face))))

(setq wcheck-language-data
      '(("Highlight Commentaires"
         (program . tl/commentaires-keywords-detect)
         (face . font-lock-warning-face)
         (regexp-start . "\\<")
         (regexp-body . "\\w*\\+?:?")
         (regexp-end . "")
         (read-or-skip-faces
          ((emacs-lisp-mode c-mode c++-mode objc-mode python-mode lua-mode haskell-mode)
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

(defvar tl/commentaires-keywords
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

(defun tl/commentaires-keywords-detect (strings)
  (let (found)
    (dolist (string strings found)
      (dolist (regexp tl/commentaires-keywords)
        (when (string-match regexp string)
          (push (match-string-no-properties 1 string) found)
          (return))))))

(provide '50wcheck-mode)
;;; 50wcheck-mode.el ends here
