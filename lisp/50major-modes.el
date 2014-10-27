;;; -*- mode: Emacs-Lisp -*-

;;; auto insert the matching closing delimiter
;; electric pair mode is a global minor mode
;; (electric-pair-mode 1) ; use smartparens instead

;;; imenu
;; (require 'imenu)
;; (setq imenu-sort-function 'imenu--sort-by-name)
;; (setq imenu-max-items 40)
;; TODO: hacking it
;; (setq-default imenu-create-index-function #'imenu-create-index-using-ctags)

;; (defun imenu--completion-buffer (index-alist &optional prompt)
;;   ;; Create a list for this buffer only when needed.
;;   (let ((name (thing-at-point 'symbol))
;;         choice
;;         (prepared-index-alist
;;          (if (not imenu-space-replacement) index-alist
;;              (mapcar
;;               (lambda (item)
;;                 (cons (subst-char-in-string ?\s (aref imenu-space-replacement 0)
;;                                             (car item))
;;                       (cdr item)))
;;               index-alist))))
;;     (when (stringp name)
;;       (setq name (or (imenu-find-default name prepared-index-alist) name)))
;;     (setq name (ido-completing-read
;;                 "Index item: "
;;                 (mapcar 'car prepared-index-alist)
;;                 nil t nil 'imenu--history-list
;;                 (and name (imenu--in-alist name prepared-index-alist) name)))
;;     (when (stringp name)
;;       (setq choice (assoc name prepared-index-alist))
;;       (if (imenu--subalist-p choice)
;;           (imenu--completion-buffer (cdr choice) prompt)
;;           choice))))



;;; indent
;; use space to indent instead of TAB character to indent, but for makefile-mode.
(setq-default indent-tabs-mode nil) ; Prevent Extraneous Tabs
;; you can use "M-x untabify" to change tab to space of a region
(set-default 'tab-width 4)
;; (setq tab-stop-list '(4 8 12 16 20 24 28 32 36))
;;(setq standard-indent 2) ; default 4


;;; font-lock
;; (defun pl/font-lock-add-commentaires-keywords (m)
;;   (font-lock-add-keywords m '(("\\<\\(FIXME\\):" 1 font-lock-warning-face prepend)
;;                               ("\\<\\(XXX+\\):" 1 font-lock-warning-face prepend)
;;                               ("\\<\\(TODO\\):" 1 font-lock-warning-face prepend)
;;                               ("\\<\\(BUG\\):" 1 font-lock-warning-face prepend)
;;                               ("\\<\\(td_[-.a-z0-9_]*;?\\)\\>" . font-lock-builtin-face)
;;                               ("\\<\\(WARNING\\)" 1 font-lock-warning-face t)
;;                               ("\\<\\(NOTE\\)" 1 font-lock-warning-face t)
;;                               ("\\<\\(NOTES\\)" 1 font-lock-warning-face t)
;;                               ("\\<\\(DEBUG\\)" 1 font-lock-warning-face t)
;;                               ("\\<\\(OUTPUT\\)" 1 font-lock-warning-face t)
;;                               ("\\<\\(IMPORTANT\\)" 1 font-lock-warning-face t)
;;                               ;; highlight line that are too long
;;                               ("^[^\n]\\{121\\}\\(.*\\)$" 1 font-lock-warning-face t))))


;; (pl/font-lock-add-commentaires-keywords 'emacs-lisp-mode)

;; (eval-after-load "scheme"
;;   '(progn
;;     (pl/font-lock-add-commentaires-keywords 'scheme-mode)))

(eval-after-load "cc-mode"
  '(progn
    ;; (dolist (m '(c-mode objc-mode c++-mode))        ; Colorisation : C/C++/Object-C : Commentaires
    ;; (pl/font-lock-add-commentaires-keywords m))
     (dolist (type (list "UCHAR" "USHORT" "ULONG" "BOOL" "BOOLEAN" "LPCTSTR" "C[A-Z]\\sw+" "\\sw+_t"))
      (add-to-list 'c-font-lock-extra-types type))))

(font-lock-add-keywords
 'emacs-lisp-mode
 '(("\\<\\(set\\|setq\\|require-soft\\|quote\\|when-available\\|add-hook\\)\\>" .
    font-lock-function-name-face)
   ("\\<\\(nil\\|\\t\\)\\_>" . font-lock-constant-face)))

;;; auto close *compilation* buffer, if no compile error
;; (setq compilation-finish-functions 'compile-autoclose)
;; (defun compile-autoclose (buf str)
;;   (let ((exit-abnormalp (string-match "exited abnormally" str)))
;;     (cond ((and (not exit-abnormalp) (equal (buffer-name) "*compilation*"))
;;            ;; the compilation window go away in 0.5 seconds
;;            (run-at-time 0.5 nil
;;                         'delete-windows-on buf
;;                         (message "pppp")
;;                         ;; It will switch back to whatever buffer was in your other window
;;                         ;; (get-buffer-window buffer t)
;;                         )
;;            (message "NO COMPILATION ERRORS!"))
;;           ((and exit-abnormalp (equal (buffer-name) "*grep*"))
;;            (run-at-time 0.5 nil 'delete-windows-on buf)
;;            (message "NO SUCH STRING IN CURRENT DIRECTORY"))
;;           (t (message "Oh, I catch it!")))))
;;--------------------------------------------------------------------------------

;;; _+ comment current line
;; Original idea from
;; http://www.opensubscriber.com/message/emacs-devel@gnu.org/10971693.html
;; (global-set-key (kbd "M-;") 'pl/comment-dwim)
;; (defun pl/comment-dwim (&optional arg)
;;   "Replacement for the comment-dwim command.
;;         If no region is selected and current line is not blank and we are not at the end of the line,
;;         then comment current line.
;;         Replaces default behaviour of comment-dwim, when it inserts comment at the end of the line."
;;   (interactive "*P")
;;   (comment-normalize-vars)
;;   (if (and (not (region-active-p)) (not (looking-at "[ \t]*$")))
;;       (comment-or-uncomment-region (line-beginning-position) (line-end-position))
;;     (comment-dwim arg)))

(global-set-key (kbd "C-;") 'pl/toggle-comment)
(defun pl/toggle-comment (&optional line)
  "This function is to comment or uncomment a line or a region"
  (interactive "P")
  (let ((line (unless (or line (and mark-active (not (equal (mark) (point)))))
                t)))
    (if line
       (save-excursion
         (comment-or-uncomment-region
          (progn
            (beginning-of-line)
            (point))
          (progn
            (end-of-line)
            (point))))
       (call-interactively 'comment-or-uncomment-region))))

;;------------------------------------------------------------------------------------------
;; now '-' is not considered a word-delimiter
;; (add-hook 'emacs-lisp-mode-hook '(lambda ()
;;                                   (modify-syntax-entry ?- "w")))
;; (modify-syntax-entry ?- "w" emacs-lisp-mode-syntax-table)

;;;;; eldoc
;; mini-buffer 中显示 point 处 eLisp 函数的定义格式。
(autoload 'turn-on-eldoc-mode "eldoc" nil t)
(eval-after-load "eldoc"
  '(progn
    (require 'eldoc-extension nil t)
    ;; make ElDoc aware of ParEdit's most used commands. Whenever the listed commands are used,
    ;; ElDoc will automatically refresh the minibuffer.
    ;; (eldoc-add-command
    ;;  'paredit-backward-delete
    ;;  'paredit-close-round)
    ))

(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)

;; (add-hook 'rcirc-mode-hook 'turn-on-eldoc-mode)
;;;;; symbol highlight mode

;; (require 'tooltip-help)
;; (define-key emacs-lisp-mode-map (kbd "<f1>") 'th-show-help)

;;; elisp-slime-nav
;; install with el-get
;; Elisp go-to-definition with M-. and back again with M-,
;; (add-hook 'emacs-lisp-mode-hook (lambda () (elisp-slime-nav-mode t)))
;; (eval-after-load 'elisp-slime-nav '(diminish 'elisp-slime-nav-mode))

;;; aggressive-indent-mode
(defun pl/turn-on-aggressive-indent-mode ()
  (aggressive-indent-mode +1))

(when (fboundp 'global-aggressive-indent-mode )
  (global-aggressive-indent-mode 1)
  (add-to-list 'aggressive-indent-excluded-modes 'html-mode)
  (add-to-list 'aggressive-indent-excluded-modes 'haskell-mode)
  (dolist (l '(c-mode-common-hook ; c-mode-common-hook
               emacs-lisp-mode-hook))
    (add-to-list l #'pl/turn-on-aggressive-indent-mode)))

;;; highlight-symbol
(when (fboundp 'highlight-symbol-mode)
  (add-hook 'prog-mode-hook (lambda () (highlight-symbol-mode t))))

(eval-after-load "highlight-symbol"
  '(progn
     ;; (add-hook 'lisp-mode-hook 'highlight-symbol-mode)
     ;; (add-hook 'emacs-lisp-mode-hook 'highlight-symbol-mode)
     ;; (add-hook 'scheme-mode-hook 'highlight-symbol-mode)

     ;; (global-set-key (kbd "M-s j") 'highlight-symbol-at-point)
     ;; (global-set-key (kbd "M-s *") 'highlight-symbol-next)
     ;; (global-set-key (kbd "M-s #") 'highlight-symbol-prev)
     ;; (global-set-key (kbd "C-*") 'highlight-symbol-next)
     ;; (global-set-key (kbd "C-#") 'highlight-symbol-prev)
     (setq highlight-symbol-idle-delay 1.2)))


;;----------------------------------------------------------------------------------------------------
;; insert -*- MODENAME -*- tag

(defun pl/insert-file-variable ()
  "Insert file variable string \"-*- Major-Mode-Name -*-\" with
  comment char"
  (interactive)
  (insert
   (concat comment-start " -*- "
           (substring
            (symbol-name (symbol-value 'major-mode)) 0 -5)
           " -*- " comment-end)))
;;----------------------------------------------------------------------------------------------------

;;;;; Let's make Haskell and Lisp look more like the math they should!
;; <http://www.emacswiki.org/cgi-bin/wiki/PrettyLambda>
;; (defun pretty-greek ()
;;   (let ((greek '("alpha" "beta" "gamma" "delta" "epsilon" "zeta" "eta" "theta" "iota" "kappa" "lambda" "mu"
;;                  "nu" "xi" "omicron" "pi" "rho" "sigma_final" "sigma" "tau" "upsilon" "phi" "chi" "psi" "omega")))
;;     (loop for word in greek
;;        for code = 97 then (+ 1 code)
;;        do  (let ((greek-char (make-char 'greek-iso8859-7 code)))
;;              (font-lock-add-keywords nil
;;                                      `((,(concatenate 'string "\\(^\\|[^a-zA-Z0-9]\\)\\(" word "\\)[a-zA-Z]")
;;                                          (0 (progn (decompose-region (match-beginning 2) (match-end 2))
;;                                                    nil)))))
;;              (font-lock-add-keywords nil
;;                                      `((,(concatenate 'string "\\(^\\|[^a-zA-Z0-9]\\)\\(" word "\\)[^a-zA-Z]")
;;                                          (0 (progn (compose-region (match-beginning 2) (match-end 2)
;;                                                                    ,greek-char)
;;                                                    nil)))))))))
;; (add-hook 'lisp-mode-hook 'pretty-greek)
;; (add-hook 'emacs-lisp-mode-hook 'pretty-greek)

;;----------------------------------------------------------------------------------------------------

;;; auto compile el file
;; (defun byte-compile-visited-file ()
;;   (let ((byte-compile-verbose t))
;;     (unless (eq major-mode 'sawfish-mode)
;;       (byte-compile-file buffer-file-name))))

;; (add-hook 'emacs-lisp-mode-hook
;;           (lambda ()
;;             (when buffer-file-name
;;               (add-hook 'after-save-hook 'byte-compile-visited-file nil t))))

;;----------------------------------------------------------------------------------------------------
;; (defun unicode-symbol (name)
;;   "Translate a symbolic name for a Unicode character -- e.g., LEFT-ARROW
;;   or GREATER-THAN into an actual Unicode character code. "
;;   (decode-char 'ucs (case name
;;                       ;; arrows
;;                       ('left-arrow 8592)
;;                       ('up-arrow 8593)
;;                       ('right-arrow 8594)
;;                       ('down-arrow 8595)

;;                       ;; boxes
;;                       ('double-vertical-bar #X2551)

;;                       ;; relational operators
;;                       ('equal #X003d)
;;                       ('not-equal #X2260)
;;                       ('identical #X2261)
;;                       ('not-identical #X2262)
;;                       ;; ('less-than #X003c)
;;                       ;; ('greater-than #X003e)
;;                       ;;To replace '<' and '>', monaco's seems not perfect
;;                       ('less-than #X2039)
;;                       ('greater-than #X203A)
;;                       ('less-than-or-equal-to #X2264)
;;                       ('greater-than-or-equal-to #X2265)

;;                       ;; logical operators
;;                       ('logical-and #X2227)
;;                       ('logical-or #X2228)
;;                       ('logical-neg #X00AC)

;;                       ;; misc
;;                       ('nil #X2205)
;;                       ('horizontal-ellipsis #X2026)
;;                       ('double-exclamation #X203C)
;;                       ('prime #X2032)
;;                       ('double-prime #X2033)
;;                       ('for-all #X2200)
;;                       ('there-exists #X2203)
;;                       ('element-of #X2208)

;;                       ;; mathematical operators
;;                       ('square-root #X221A)
;;                       ('squared #X00B2)
;;                       ('cubed #X00B3)

;;                       ;; letters
;;                       ('lambda #X03BB)
;;                       ('alpha #X03B1)
;;                       ('beta #X03B2)
;;                       ('gamma #X03B3)
;;                       ('delta #X03B4))))

;; (defun substitute-pattern-with-unicode (pattern symbol)
;;   "Add a font lock hook to replace the matched part of PATTERN with the
;;   Unicode symbol SYMBOL looked up with UNICODE-SYMBOL."
;;   (interactive)
;;   (font-lock-add-keywords
;;    nil `((,pattern (0 (progn (compose-region (match-beginning 1) (match-end 1)
;;                                              ,(unicode-symbol symbol))
;;                              nil))))))
;; (defun substitute-patterns-with-unicode (patterns)
;;   "Call SUBSTITUTE-PATTERN-WITH-UNICODE repeatedly."
;;   (mapcar #'(lambda (x)
;;               (substitute-pattern-with-unicode (car x)
;;                                                (cdr x)))
;;           patterns))
;;~~~~~~~~~ ----------------------------------------------------------------------------------------------------

;; (defun lisp-unicode ()
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


;; (defun pl/fonts-replace ()
;;   (interactive)
;;   (substitute-patterns-with-unicode
;;    (list (cons "\\(<\\)" 'less-than)
;;          (cons "\\(>\\)" 'greater-than))))

;; (add-hook 'lisp-mode-hook 'lisp-unicode)
;; (add-hook 'emacs-lisp-mode-hook 'lisp-unicode)
;;----------------------------------------------------------------------------------------------------

;;; pretty lambda
;; (defun pl/pretty-lambdas ()
;;   (font-lock-add-keywords
;;    nil `(("(?\\(lambda\\>\\)"
;;           (0 (progn (compose-region (match-beginning 1) (match-end 1)
;;                                     ,(make-char 'greek-iso8859-7 107))
;;                     nil))))))

;; (add-hook 'prog-mode-hook 'pl/pretty-lambdas)

;; function to comment a region using #if 0 -----------------------------------
;; (defun if0-region (p1 p2)
;;   (interactive "r")
;;   (let* ()
;;     (goto-char p1)
;;     (beginning-of-line)

;;     (insert "#if 0\n")
;;     (goto-char (+ p2 +9))
;;     (beginning-of-line)
;;     (insert "#endif\n")))



;;; xmodmaprc -- XModMapMode

(define-generic-mode 'xmodmap-mode
  '(?!)
  '("add" "clear" "keycode")
  nil
  '("Xmodmap\\'" "xmodmaprc\\'")
  nil
  "Simple mode for xmodmap files.")

;;; gtk-look
;; (autoload 'gtk-lookup-symbol "gtk-look" nil t)
;; (define-key global-map [?\C-h ?\C-j] 'gtk-lookup-symbol)


(provide '50major-modes)
