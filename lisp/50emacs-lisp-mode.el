;;; 50emacs-lisp-mode.el ---                         -*- lexical-binding: t; -*-

;; Copyright (C) 2014  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>

;;; Commentary:

;;; TODO: try https://github.com/noctuid/lispyville
;;; Code:

;;; tooltip-help
;; (require 'tooltip-help)
;; (define-key emacs-lisp-mode-map (kbd "<f1>") 'th-show-help)

;;; eldoc
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

(defun tl/emacs-lisp-mode-init ()
  (setq fill-column 120)
  ;; (setq emacs-lisp-docstring-fill-column 65)
  ;; (set (make-local-variable 'lisp-indent-function)
  ;;      'common-lisp-indent-function)

  (when (fboundp 'show-smartparens-mode)
    (show-smartparens-mode +1))
  (set (make-local-variable 'lisp-indent-function)
       #'tl/lisp-indent-function)
  (turn-on-eldoc-mode))

(add-hook 'emacs-lisp-mode-hook 'tl/emacs-lisp-mode-init)

(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)
;; (add-hook 'rcirc-mode-hook 'turn-on-eldoc-mode)

;;; do not considered '-' as word-delimiter
;; (add-hook 'emacs-lisp-mode-hook '(lambda () (modify-syntax-entry ?- "w")))
;; (modify-syntax-entry ?- "w" emacs-lisp-mode-syntax-table)

;;; elisp-slime-nav
;; install with el-get
;; Elisp go-to-definition with M-. and back again with M-,
(use-package elisp-slime-nav
  :defer t
  :diminish elisp-slime-nav-mode
  :init
  (progn
    (add-hook 'emacs-lisp-mode-hook 'turn-on-elisp-slime-nav-mode)
    (dolist (mode '(emacs-lisp-mode lisp-interaction-mode))
      (tl/declare-prefix-for-mode mode "mg" "find-symbol")
      (tl/declare-prefix-for-mode mode "mh" "help")
      (tl/set-leader-keys-for-major-mode mode
        "hh" 'elisp-slime-nav-describe-elisp-thing-at-point))))

;;; auto compile el file
;; (defun byte-compile-visited-file ()
;;   (let ((byte-compile-verbose t))
;;     (unless (eq major-mode 'sawfish-mode)
;;       (byte-compile-file buffer-file-name))))

;; (add-hook 'emacs-lisp-mode-hook
;;           (lambda ()
;;             (when buffer-file-name
;;               (add-hook 'after-save-hook 'byte-compile-visited-file nil t))))

;; https://github.com/Wilfred/elisp-refs
(use-package elisp-refs
  :defer t)

(font-lock-add-keywords
 'emacs-lisp-mode
 '(("\\<\\(set\\|setq\\|require-soft\\|quote\\|when-available\\|add-hook\\)\\>" .
    font-lock-function-name-face)
   ("\\<\\(nil\\|\\t\\)\\_>" . font-lock-constant-face)))

;;; imenu
(setq tl/emacs-lisp-imenu-generic-expression
      '((nil "^\\s-*(def\\(un\\|subst\\|macro\\|advice\\)\
\\s-+\\([-A-Za-z0-9+/]+\\)" 2)
        ("*Vars*" "^\\s-*(def\\(var\\|const\\)\
\\s-+\\([-A-Za-z0-9+/]+\\)" 2)
        ("*Types*"
         "^\\s-*\
 (def\\(type\\|struct\\|class\\|ine-condition\\)\
\\s-+\\([-A-Za-z0-9+/]+\\)" 2)
        (nil ";;[;]\\{1,8\\} \\(.*$\\)" 1) ; the default one
        ("Sections" "^;;;; \\(.+\\)$" 1)))


(defun tl/imenu-elisp-init ()
  (setq imenu-prev-index-position-function nil)
  (setq imenu-generic-expression tl/emacs-lisp-imenu-generic-expression))


(add-hook 'emacs-lisp-mode-hook 'tl/imenu-elisp-init)

;; Idea from http://www.reddit.com/r/emacs/comments/312ge1/i_created_this_function_because_i_was_tired_of/
(defun tl/eval-current-form ()
  "Find and evaluate the current def* or set* command.
Unlike `eval-defun', this does not go to topmost function."
  (interactive)
  (save-excursion
    (search-backward-regexp "(def\\|(set")
    (forward-list)
    (call-interactively 'eval-last-sexp)))

;; smartparens integration

(defun tl/eval-current-form-sp (&optional arg)
  "Call `eval-last-sexp' after moving out of one level of
parentheses. Will exit any strings and/or comments first.
An optional ARG can be used which is passed to `sp-up-sexp' to move out of more
than one sexp.
Requires smartparens because all movement is done using `sp-up-sexp'."
  (interactive "p")
  (require 'smartparens)
  (let ((evil-move-beyond-eol t))
    ;; evil-move-beyond-eol disables the evil advices around eval-last-sexp
    (save-excursion
      (let ((max 10))
        (while (and (> max 0)
                    (sp-point-in-string-or-comment))
          (decf max)
          (sp-up-sexp)))
      (sp-up-sexp arg)
      (call-interactively 'eval-last-sexp))))

(defun tl/eval-current-symbol-sp ()
  "Call `eval-last-sexp' on the symbol around point.
Requires smartparens because all movement is done using `sp-forward-symbol'."
  (interactive)
  (require 'smartparens)
  (let ((evil-move-beyond-eol t))
    ;; evil-move-beyond-eol disables the evil advices around eval-last-sexp
    (save-excursion
      (sp-forward-symbol)
      (call-interactively 'eval-last-sexp))))

(defun tl/init-emacs-lisp ()
  (dolist (mode '(emacs-lisp-mode lisp-interaction-mode))
    (tl/declare-prefix-for-mode mode "mc" "compile")
    (tl/declare-prefix-for-mode mode "me" "eval")
    (tl/declare-prefix-for-mode mode "mt" "tests")
    (tl/set-leader-keys-for-major-mode mode
      "cc" 'emacs-lisp-byte-compile
      ;; "e$" 'lisp-state-eval-sexp-end-of-line
      "eb" 'eval-buffer
      "eC" 'tl/eval-current-form
      "ee" 'eval-last-sexp
      "er" 'eval-region
      "ef" 'eval-defun
      ;; "el" 'lisp-state-eval-sexp-end-of-line
      ;; ","  'lisp-state-toggle-lisp-state
      "tb" 'tl/ert-run-tests-buffer
      "tq" 'ert
      "f" 'describe-function
      "k" 'describe-key
      "hh" 'elisp-slime-nav-describe-elisp-thing-at-point
      "gg" 'elisp-slime-nav-find-elisp-thing-at-point
      "v" 'describe-variable)))

(tl/init-emacs-lisp)

(use-package helpful
  :defer t
  :commands (helpful-callable helpful-variable helpful-key helpful-at-point)
  :init
  (progn
    (dolist (mode '(emacs-lisp-mode lisp-interaction-mode))
      (tl/set-leader-keys-for-major-mode mode
        "hC" 'helpful-command
        "hf" 'helpful-callable
        "hF" 'helpful-function
        "hv" 'helpful-variable
        "hk" 'helpful-key
        "hd" 'helpful-at-point
        "h." 'helpful-at-point))))

;;; elisp keywords aligned
;; https://github.com/Fuco1/.emacs.d
;; https://emacs.stackexchange.com/questions/10230/how-to-indent-keywords-aligned
;; redefines the silly indent of keyword lists
;; before
;;   (:foo bar
;;         :baz qux)
;; after
;;   (:foo bar
;;    :baz qux)
(defun tl/lisp-indent-function (indent-point state)
  "This function is the normal value of the variable `lisp-indent-function'.
The function `calculate-lisp-indent' calls this to determine
if the arguments of a Lisp function call should be indented specially.
INDENT-POINT is the position at which the line being indented begins.
Point is located at the point to indent under (for default indentation);
STATE is the `parse-partial-sexp' state for that position.
If the current line is in a call to a Lisp function that has a non-nil
property `lisp-indent-function' (or the deprecated `lisp-indent-hook'),
it specifies how to indent.  The property value can be:
* `defun', meaning indent `defun'-style
  \(this is also the case if there is no property and the function
  has a name that begins with \"def\", and three or more arguments);
* an integer N, meaning indent the first N arguments specially
  (like ordinary function arguments), and then indent any further
  arguments like a body;
* a function to call that returns the indentation (or nil).
  `lisp-indent-function' calls this function with the same two arguments
  that it itself received.
This function returns either the indentation to use, or nil if the
Lisp function does not specify a special indentation."
  (let ((normal-indent (current-column))
        (orig-point (point)))
    (goto-char (1+ (elt state 1)))
    (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
    (cond
     ;; car of form doesn't seem to be a symbol, or is a keyword
     ((and (elt state 2)
           (or (not (looking-at "\\sw\\|\\s_"))
               (looking-at ":")))
      (if (not (> (save-excursion (forward-line 1) (point))
                  calculate-lisp-indent-last-sexp))
          (progn (goto-char calculate-lisp-indent-last-sexp)
                 (beginning-of-line)
                 (parse-partial-sexp (point)
                                     calculate-lisp-indent-last-sexp 0 t)))
      ;; Indent under the list or under the first sexp on the same
      ;; line as calculate-lisp-indent-last-sexp.  Note that first
      ;; thing on that line has to be complete sexp since we are
      ;; inside the innermost containing sexp.
      (backward-prefix-chars)
      (current-column))
     ((and (save-excursion
             (goto-char indent-point)
             (skip-syntax-forward " ")
             (not (looking-at ":")))
           (save-excursion
             (goto-char orig-point)
             (looking-at ":")))
      (save-excursion
        (goto-char (+ 2 (elt state 1)))
        (current-column)))
     (t
      (let ((function (buffer-substring (point)
                                        (progn (forward-sexp 1) (point))))
            method)
        (setq method (or (function-get (intern-soft function)
                                       'lisp-indent-function)
                         (get (intern-soft function) 'lisp-indent-hook)))
        (cond ((or (eq method 'defun)
                   (and (null method)
                        (> (length function) 3)
                        (string-match "\\`def" function)))
               (lisp-indent-defform state indent-point))
              ((integerp method)
               (lisp-indent-specform method state
                                     indent-point normal-indent))
              (method
               (funcall method indent-point state))))))))

(provide '50emacs-lisp-mode)
;;; 50emacs-lisp-mode.el ends here
