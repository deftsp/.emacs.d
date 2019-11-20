;;; 50outline-mode.el ---

;; Copyright (C) 2008  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>

;; M-x outline-minor-mode (enable Outline minor mode)
;; C-c @ C-t / M-x outline-hide-body (hide all of buffer except headings)
;; C-c @ C-a / M-x outline-show-all (show all of the text in the buffer)
;; C-c @ TAB / M-x outline-show-children (show all direct subheadings of this heading)
;; C-c @ C-k / M-x outline-show-branches (show all subheadings, but not bodies)
;; M-x outline-previous-heading (go to previous heading)
;; M-x outline-next-heading (go to next heading)
;; C-c @ C-p / M-x outline-previous-visible-heading (go to previous visible heading)
;; C-c @ C-n / M-x outline-next-visible-heading (go to next visible heading)

;; For lisp-modes, the value matches comments starting with three semicolons and opening parens on the first column.
;; ";;; \\|(...."



;;Change outline commands prefix to `M-o'
(global-unset-key (kbd "M-S-o"))
(setq outline-minor-mode-prefix "\M-\S-o") ; default "\C-c\C-o"

(use-package outline
  :defer t
  :config
  (require 'foldout)

  (use-package outline-magic
    :config
    (define-key outline-minor-mode-map (kbd "<C-tab>") 'outline-cycle))

  (define-key outline-mode-map [menu-bar] nil)
  (define-key outline-minor-mode-map [menu-bar] nil))



;;; change the characters outline mode uses for ellipsis (`…’ by default).
(set-display-table-slot standard-display-table
                        'selective-display (string-to-vector " ◦◦◦ "))


;;; Explorer like Key-Bindings ---------------------------

(defun body-p ()
  (save-excursion
    (outline-back-to-heading)
    (outline-end-of-heading)
    (and (not (eobp))
         (progn (forward-char 1)
                (not (outline-on-heading-p))))))

(defun body-visible-p ()
  (save-excursion
    (outline-back-to-heading)
    (outline-end-of-heading)
    (not (outline-invisible-p))))

(defun subheadings-p ()
  (save-excursion
    (outline-back-to-heading)
    (let ((level (outline-level)))
      (outline-next-heading)
      (and (not (eobp))
         (< level (outline-level))))))

(defun subheadings-visible-p ()
  (interactive)
  (save-excursion
    (outline-next-heading)
    (not (outline-invisible-p))))

(defun outline-do-close ()
  (interactive)
  (if (outline-on-heading-p)
      (cond ((and (body-p) (body-visible-p))
             (hide-entry))
            ((and (subheadings-p)
                (subheadings-visible-p))
             (hide-subtree))
            (t (outline-previous-visible-heading 1)))
      (outline-back-to-heading t)))

(defun outline-do-open ()
  (interactive)
  (if (outline-on-heading-p)
      (cond ((and (subheadings-p)
                (not (subheadings-visible-p)))
             (show-children))
            ((and (body-p)
                (not (body-visible-p)))
             (show-entry))
            (t (show-entry)))
      (outline-next-visible-heading 1)))

;; (define-key outline-mode-map (kbd "H-,") 'outline-do-open)
;; (define-key outline-mode-map (kbd "H-.") 'outline-do-close)
;; (define-key outline-minor-mode-map (kbd "H-,") 'outline-do-open)
;; (define-key outline-minor-mode-map (kbd "H-.") 'outline-do-close)

;; (define-key outline-mode-map (kbd "H-,") 'org-cycle)
;; (define-key outline-mode-map (kbd "H-.") 'org-global-cycle)
;; (define-key outline-minor-mode-map (kbd "H-,") 'org-cycle)
;; (define-key outline-minor-mode-map (kbd "H-.") 'org-global-cycle)
;; Explorer like Key-Bindings ends here --------------------------------

;;; Code Editing

;; For years i wanted to modify outline-minor-mode, so that comments starting at the beginning of line were left
;; visible. Now I add the condition that they don't get marked as headers. Here's the code:

;; (defun outline-flag-region-make-overlay (from to) ;mmc
;;   (let ((o (make-overlay from to)))
;;     (overlay-put o 'invisible 'outline)
;;     (overlay-put o 'isearch-open-invisible
;;                  'outline-isearch-open-invisible)
;;     o))

;; (defun outline-flag-region (from to flag) ;mmc
;;   "Hides or shows lines from FROM to TO, according to FLAG.
;; If FLAG is nil then text is shown, while if FLAG is t the text is hidden."
;;   (save-excursion
;;     (goto-char from)
;;     (end-of-line)
;;     (outline-discard-overlays (point) to 'outline)
;;     (if flag
;;         ;;
;;         (let ((beginning (point))
;;               (regexp (concat "^" (regexp-quote comment-start)))
;;               )
;;           (while (re-search-forward regexp to 't)
;;             (goto-char (match-beginning 0))
;;             (if (> (- (point) beginning) 2)
;;                 (outline-flag-region-make-overlay beginning (point)))
;;             ;(goto-char
;;             (end-of-line)
;;             (setq beginning (point)))
;;           (outline-flag-region-make-overlay beginning to)))
;;     (run-hooks 'outline-view-change-hook)))

;;; outline minor mode
(defun paloryemacs/outline-local-set-regexp (regexp &optional fun)
  "Set `outline-regexp' locally to REGEXP and `outline-level' to FUN."
  (set (make-local-variable 'outline-regexp) regexp)
  (if fun
      (set (make-local-variable 'outline-level) fun)))

;; all text-based major modes run `text-mode-hook', and all programming language modes run
;; `prog-mode-hook', prior to running their own mode hooks
(add-hook 'prog-mode-hook (lambda () (outline-minor-mode t)))

;;; c-mode
;; "[:blank:]*\\(.*{\\|.*}\\)"
;; "[^ #\t\n]\\|[:blank:]*\\([{}]\\|[^* \t\n\^M\^L]\\|\\*+[a-zA-Z_0-9=(]\\)"
;; "[ \t]*\\([^* \t\n\^M\^L]\\|\\*+[a-zA-Z_0-9=(]\\)"
(defconst paloryemacs/c-mode-common-outline-regexp (concat
                                           "^"                ; beginning of line is required
                                           "\\(template[ \t]*<[^>]+>[ \t]*\\)?" ; there may be a "template <...>"
                                           "\\([a-zA-Z0-9_:]+[ \t]+\\)?" ; type specs; there can be no
                                           "\\([a-zA-Z0-9_:]+[ \t]+\\)?" ; more than 3 tokens, right?

                                           "\\("                ; last type spec including */&
                                           "[a-zA-Z0-9_:]+"
                                           "\\([ \t]*[*&]+[ \t]*\\|[ \t]+\\)" ; either pointer/ref sign or whitespace
                                           "\\)?"                ; if there is a last type spec
                                           "\\("                ; name; take that into the imenu entry
                                           "[a-zA-Z0-9_:~]+" ; member function, ctor or dtor...
                                        ; (may not contain * because then
                                        ; "a::operator char*" would become "char*"!)
                                           "\\|"
                                           "\\([a-zA-Z0-9_:~]*::\\)?operator"
                                           "[^a-zA-Z1-9_][^(]*" ; ...or operator
                                           " \\)"
                                           "[ \t]*([^)]*)[ \t\n]*[^ ;]" ; require something other than a ; after
                                           )
  "outline regexp for C and C++")

(add-hook 'c-mode-hook (lambda ()
                         (paloryemacs/outline-local-set-regexp paloryemacs/c-mode-common-outline-regexp)))

(add-hook 'c++-mode-hook (lambda ()
                           (paloryemacs/outline-local-set-regexp paloryemacs/c-mode-common-outline-regexp)))


;;; Haskell
;; outline uses this regexp to find headers. I match lines with no indent and indented
;; some lines, such as "--" ... "class"
(defconst paloryemacs/haskell-mode-outline-regexp
  "^[^\t ].*\\|^.*[\t ]+\\(where\\|of\\|do\\|in\\|if\\|then\\|else\\|let\\|module\\|import\\|deriving\\|instance\\|class\\)[\t\n ]")

(eval-after-load "haskell-mode"
  '(add-hook 'haskell-mode-hook (lambda () (paloryemacs/outline-local-set-regexp paloryemacs/haskell-mode-outline-regexp))))

;;; Go
(defconst paloryemacs/go-mode-outline-regexp
  "//\\.\\|//[^\r\n\f][^\r\n\f]\\|pack\\|func\\|impo\\|cons\\|var.\\|type\\|\t\t*....")

(eval-after-load "go-mode"
  '(add-hook 'go-mode-hook (lambda () (paloryemacs/outline-local-set-regexp paloryemacs/go-mode-outline-regexp))))


;; Lua
(defconst paloryemacs/lua-mode-outline-regexp
  "function \\|local \\|-- \\|--\\[\\[ \\|if \\|while \\|repeat$\\|for ")

(eval-after-load "lua-mode"
  '(add-hook 'lua-mode-hook (lambda () (paloryemacs/outline-local-set-regexp paloryemacs/lua-mode-outline-regexp))))

;; (add-hook 'php-mode-user-hook
;;           '(lambda ()
;;             (outline-minor-mode t)
;;             (setq outline-regexp " *\\(private funct\\|public funct\\|funct\\|class\\|#head\\)")
;;             (hide-sublevels 1)))

;; (add-hook 'python-mode-hook
;;           '(lambda ()
;;             (outline-minor-mode t)
;;             (setq outline-regexp " *\\(def \\|clas\\|#hea\\)")
;;             (hide-sublevels 1)))

;; (add-hook 'change-log-mode-hook
;;           (lambda ()
;;             (outline-local-set-regexp "[[:digit:]]+")
;;             (outline-minor-mode t)))

;; (add-hook 'emacs-lisp-mode-hook
;;           (function (lambda ()
;;                       (outline-local-set-regexp ";;; \\|(....")
;;                       (outline-minor-mode 1))))




(provide '50outline-mode)


;; Local Variables:
;; outline-regexp: ";;; "
;; mode: outline-minor
;; End:
