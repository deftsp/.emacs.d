;;; 50tab-completion.el ---

;; Copyright (C) 2008  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>
;; Keywords:

;; (require 'tabkey2)
;; (setq tabkey2-message-style 'echo-area
;;       tabkey2-alternate-key nil)
;; (tabkey2-mode t)

;; --------------------------------------------------------------------------------
;; tab completion
;; (defun tl/indent-or-complete ()
;;   "Complete if point is at end of a word, otherwise indent line."
;;   (interactive)
;;   (if (looking-at "\\>")
;;       (hippie-expand nil)
;;       (indent-for-tab-command)))

;; (add-hook 'c-mode-common-hook
;;           (function (lambda ()
;;             (local-set-key (kbd "<tab>") 'tl/indent-or-complete))))


;; "To hit tab to auto-complete (like bash does) put this in
;; your.emacs:"<http://www.emacswiki.org/cgi-bin/wiki/EmacsNiftyTricks>
;; (defun indent-or-complete ()
;;   "Complete if point is at end of line, and indent line."
;;   (interactive)
;;   (if (and (looking-at "$") (not (looking-back "^\\s-*")))
;;       (hippie-expand nil))
;;   (indent-for-tab-command))
;; (add-hook 'find-file-hooks (function (lambda ()
;;                              (local-set-key (kbd "<tab>") 'indent-or-complete))))


;;; Smart Tab
;; Here's what I use every day:
;; * it's minibuffer compliant, so you can set it globally and not worry about what modes need it
;; * it calls 'indent-region' if mark is active.
;; * otherwise it expands if at the end of a symbol, or indents the current line.
;; * prefixed by C-u, always smart indent without completing.

;; It feels very natural (to me at least), tab always do what I mean.

;; Note the use of "\\_>" instead of "\\>": this allows to tab-expand symbols, according to the major mode. Like
;; "(goto-...", or "BOOST_..." (usefull when coding in Lisp or C).

;; (define-key emacs-lisp-mode-map  [(tab)] 'smart-tab)

;; (defvar smart-tab-using-hippie-expand nil
;;   "turn this on if you want to use hippie-expand completion.")

;; (defun smart-tab (prefix)
;;   "Needs `transient-mark-mode' to be on. This smart tab is
;; minibuffer compliant: it acts as usual in the minibuffer.

;; In all other buffers: if PREFIX is \\[universal-argument], calls
;; `smart-indent'. Else if point is at the end of a symbol,
;; expands it. Else calls `smart-indent'."
;;   (interactive "P")
;;   (if (minibufferp)
;;       (minibuffer-complete)
;;       (if (smart-tab-must-expand prefix)
;;           (if smart-tab-using-hippie-expand
;;               (hippie-expand nil)
;;               (dabbrev-expand nil))
;;           (smart-indent))))

;; (defun smart-tab ()
;;   "This smart tab is minibuffer compliant: it acts as usual in
;;     the minibuffer. Else, if mark is active, indents region. Else if
;;     point is at the end of a symbol, expands it. Else indents the
;;     current line."
;;   (interactive)
;;   (if (minibufferp)
;;       (unless (minibuffer-complete)
;;         (dabbrev-expand nil))
;;       (if mark-active
;;           (indent-region (region-beginning)
;;                          (region-end))
;;           (if (looking-at "\\_>")
;;               (dabbrev-expand nil)
;;               (indent-for-tab-command)))))


;; (defun smart-indent ()
;;   "Indents region if mark is active, or current line otherwise."
;;   (interactive)
;;   (if mark-active
;;       (indent-region (region-beginning)
;;                      (region-end))
;;       (indent-for-tab-command)))

;; (defun smart-tab-must-expand (&optional prefix)
;;   "If PREFIX is \\[universal-argument], answers no.
;; Otherwise, analyses point position and answers."
;;   (unless (or (consp prefix)
;;              mark-active)
;;     (looking-at "\\_>")))


(provide '50tab-completion)
