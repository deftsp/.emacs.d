;;; tsp-haskell.el ---

;; Copyright (C) 2007  Shihpin Tseng

;; Keywords:

;; (load "~/.emacs.d/packages/haskell-mode/haskell-site-file")
(setq haskell-font-lock-symbols t)
;; (require 'haskell-mode nil t)

(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)

(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)
;; Note that the three indentation modules are mutually exclusive - add at
;; most one. In preference for the more advanced.


;; (setq auto-mode-alist
;;       (append auto-mode-alist
;;               '(("\\.[hg]s$"  . haskell-mode)
;;                 ("\\.hi$"     . haskell-mode)
;;                 ("\\.l[hg]s$" . literate-haskell-mode))))
;; (autoload 'haskell-mode "haskell-mode" "Major mode for editing Haskell scripts." t)
;; (autoload 'literate-haskell-mode "haskell-mode" "Major mode for editing literate Haskell scripts." t)
;; ;;<http://www.emacswiki.org/cgi-bin/wiki/HaskellMode>
;; (add-hook 'haskell-mode-hook
;;           #'(lambda ()
;;               (setq comment-padding " "
;;                     comment-start "--")))

;;Default behaviour is to always jump to the ghci window. Let's jump back automatically except for errors.
;; (defadvice haskell-ghci-load-file (after name)
;;   (other-window 1))
;; (ad-activate 'haskell-ghci-load-file t)

;;"Here comes a short Emacs helper for Haskell coders. It allows you to hoogle
;;lookup the symbol currently under your cursor." Hoogle is a Haskell search engine.
;;<http://clemens.endorphin.org/weblog/archives/2007-01.shtml#e2007-01-09T09_57_26.txt>
;;(require 'hoogle nil t)
;;(define-key haskell-mode-map [?\C-c ?h] 'hoogle-lookup)



;;; Unicodifying symbols (Pretty Lambda for Haskell-mode)

;; In Haskell code, you can end up using a lot of mathematical symbols. It is possible to hack the
;; fontifying features of Emacs to change the ASCII textual representations of arrows and operators
;; into the nice-looking real symbols, much like you could with TeX. The following code is a
;; compilation of Emacs lisp code found on the Emacs wiki on the Pretty Lambda page (that page also
;; has examples of how to apply the general Unicode defuns to other languages):

;; HOWEVER: due to the symbols taking up less space, this has the unfortunate side effect of
;; changing the indentation stops that the indent key offers. This will mean that your code may not
;; look properly aligned to those who do not have this feature in their editor, or could even mean
;; that your code means something different to how it looks. (It is possible to contrive an example
;; that looks correct in emacs, but actually fails to compile). The following is left for interest,
;; but probably should NOT be used. Haskell-mode has included this feature for a long time now, so
;; you probably just need to (setq haskell-font-lock-symbols t) in your .emacs to use this feature.

;; (defun unicode-symbol (name)
;;   "Translate a symbolic name for a Unicode character -- e.g., LEFT-ARROW
;;  or GREATER-THAN into an actual Unicode character code. "
;;   (decode-char 'ucs (case name
;;                       ('left-arrow 8592)
;;                       ('up-arrow 8593)
;;                       ('right-arrow 8594)
;;                       ('down-arrow 8595)
;;                       ('double-vertical-bar #X2551)
;;                       ('equal #X003d)
;;                       ('not-equal #X2260)
;;                       ('identical #X2261)
;;                       ('not-identical #X2262)
;;                       ('less-than #X003c)
;;                       ('greater-than #X003e)
;;                       ('less-than-or-equal-to #X2264)
;;                       ('greater-than-or-equal-to #X2265)
;;                       ('logical-and #X2227)
;;                       ('logical-or #X2228)
;;                       ('logical-neg #X00AC)
;;                       ('nil #X2205)
;;                       ('horizontal-ellipsis #X2026)
;;                       ('double-exclamation #X203C)
;;                       ('prime #X2032)
;;                       ('double-prime #X2033)
;;                       ('for-all #X2200)
;;                       ('there-exists #X2203)
;;                       ('element-of #X2208)
;;                       ('square-root #X221A)
;;                       ('squared #X00B2)
;;                       ('cubed #X00B3)
;;                       ('lambda #X03BB)
;;                       ('alpha #X03B1)
;;                       ('beta #X03B2)
;;                       ('gamma #X03B3)
;;                       ('delta #X03B4))))

;; (defun substitute-pattern-with-unicode (pattern symbol)
;;   "Add a font lock hook to replace the matched part of PATTERN with the
;;      Unicode symbol SYMBOL looked up with UNICODE-SYMBOL."
;;   (interactive)
;;   (font-lock-add-keywords
;;    nil `((,pattern
;;         (0 (progn (compose-region (match-beginning 1) (match-end 1)
;;                                   ,(unicode-symbol symbol)
;;                                   'decompose-region)
;;                   nil))))))

;; (defun substitute-patterns-with-unicode (patterns)
;;   "Call SUBSTITUTE-PATTERN-WITH-UNICODE repeatedly."
;;   (mapcar #'(lambda (x)
;;               (substitute-pattern-with-unicode (car x)
;;                                                (cdr x)))
;;           patterns))

;; (defun haskell-unicode ()
;;   (interactive)
;;   (substitute-patterns-with-unicode
;;    (list (cons "\\(<-\\)" 'left-arrow)
;;          (cons "\\(->\\)" 'right-arrow)
;;          (cons "\\(==\\)" 'identical)
;;          (cons "\\(/=\\)" 'not-identical)
;;          (cons "\\(()\\)" 'nil)
;;          (cons "\\<\\(sqrt\\)\\>" 'square-root)
;;          (cons "\\(&&\\)" 'logical-and)
;;          (cons "\\(||\\)" 'logical-or)
;;          (cons "\\<\\(not\\)\\>" 'logical-neg)
;;          (cons "\\(>\\)\\[^=\\]" 'greater-than)
;;          (cons "\\(<\\)\\[^=\\]" 'less-than)
;;          (cons "\\(>=\\)" 'greater-than-or-equal-to)
;;          (cons "\\(<=\\)" 'less-than-or-equal-to)
;;          (cons "\\<\\(alpha\\)\\>" 'alpha)
;;          (cons "\\<\\(beta\\)\\>" 'beta)
;;          (cons "\\<\\(gamma\\)\\>" 'gamma)
;;          (cons "\\<\\(delta\\)\\>" 'delta)
;;          (cons "\\(''\\)" 'double-prime)
;;          (cons "\\('\\)" 'prime)
;;          (cons "\\(!!\\)" 'double-exclamation)
;;          (cons "\\(\\.\\.\\)" 'horizontal-ellipsis))))

;; (add-hook 'haskell-mode-hook 'haskell-unicode)



(provide '50haskell)