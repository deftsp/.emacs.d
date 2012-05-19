;;; 50outline-mode.el ---

;; Copyright (C) 2008  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>

;;Change the prefix for outline commands
(setq outline-minor-mode-prefix (kbd "M-O")) ; "\C-c\C-o"

(eval-after-load "outline"
  '(require 'foldout))

;; M-x outline-minor-mode (enable Outline minor mode)
;; C-c @ C-t / M-x hide-body (hide all of buffer except headings)
;; C-c @ C-a / M-x show-all (show all of the text in the buffer)
;; C-c @ TAB / M-x show-children (show all direct subheadings of this heading)
;; C-c @ C-k / M-x show-branches (show all subheadings, but not bodies)
;; M-x outline-previous-heading (go to previous heading)
;; M-x outline-next-heading (go to next heading)
;; C-c @ C-p / M-x outline-previous-visible-heading (go to previous visible heading)
;; C-c @ C-n / M-x outline-next-visible-heading (go to next visible heading)

;; For lisp-modes, the value matches comments starting with three semicolons and opening parens on the first column.
;; ";;; \\|(...."

;;; key bindings
(eval-after-load "outline"
  '(let ((map (lookup-key outline-minor-mode-map
                          outline-minor-mode-prefix)))
	 (define-key map (kbd "a") 'show-all)
	 (define-key map (kbd "b") 'outline-backward-same-level)
	 (define-key map (kbd "c") 'hide-entry)
	 (define-key map (kbd "d") 'hide-subtree)
     (define-key map (kbd "e") 'show-entry)
	 (define-key map (kbd "f") 'outline-forward-same-level)
	 (define-key map (kbd "TAB") 'show-children)
	 (define-key map (kbd "k") 'show-branches)
	 (define-key map (kbd "l") 'hide-leaves)
	 (define-key map (kbd "RET") 'outline-insert-heading)
	 (define-key map (kbd "n") 'outline-next-visible-heading)
     (define-key map (kbd "o") 'hide-other)
     (define-key map (kbd "p") 'outline-previous-visible-heading)
	 (define-key map (kbd "q") 'hide-sublevels)
	 (define-key map (kbd "s") 'show-subtree)
	 (define-key map (kbd "t") 'hide-body)
	 (define-key map (kbd "u") 'outline-up-heading)
	 (define-key map (kbd "v") 'outline-move-subtree-down)
	 (define-key map (kbd "x") 'foldout-exit-fold)
	 (define-key map (kbd "z") 'foldout-zoom-subtree)
	 (define-key map (kbd "^") 'outline-move-subtree-up)
	 (define-key map (kbd "@") 'outline-mark-subtree)
	 (define-key map (kbd "<") 'outline-promote)
	 (define-key map (kbd ">") 'outline-demote)))

(eval-after-load 'outline
  '(progn
     (require 'outline-magic)
     (define-key outline-minor-mode-map (kbd "<C-tab>") 'outline-cycle)))


;;; change the characters outline mode uses for ellipsis (`…’ by default).
(set-display-table-slot
 standard-display-table
 'selective-display
 (let ((face-offset (* (face-id 'shadow) (lsh 1 22))))
   (vconcat (mapcar (lambda (c) (+ face-offset c)) " [...] "))))


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
(defun pl/outline-local-set-regexp (regexp &optional fun)
  ;; Set `outline-regexp' locally to REGEXP and `outline-level' to FUN.
  (set (make-local-variable 'outline-regexp) regexp)
  (if fun
    (set (make-local-variable 'outline-level) fun)))

;; all text-based major modes run `text-mode-hook', and all programming language modes run
;; `prog-mode-hook', prior to running their own mode hooks
(add-hook 'prog-mode-hook 'outline-minor-mode)

;;; c-mode
;; "[:blank:]*\\(.*{\\|.*}\\)"
;; "[^ #\t\n]\\|[:blank:]*\\([{}]\\|[^* \t\n\^M\^L]\\|\\*+[a-zA-Z_0-9=(]\\)"
;; "[ \t]*\\([^* \t\n\^M\^L]\\|\\*+[a-zA-Z_0-9=(]\\)"
(defconst pl/c-mode-common-outline-regexp (concat
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

;; (hide-sublevels 2)
;; (hide-body)
;; (outline-local-set-regexp "[^ #\t\n]\\|[:blank:]*\\(.*{\\|.*}\\)")


;; (add-hook 'php-mode-user-hook
;;           '(lambda ()
;;             (outline-minor-mode)
;;             (setq outline-regexp " *\\(private funct\\|public funct\\|funct\\|class\\|#head\\)")
;;             (hide-sublevels 1)))

;; (add-hook 'python-mode-hook
;;           '(lambda ()
;;             (outline-minor-mode)
;;             (setq outline-regexp " *\\(def \\|clas\\|#hea\\)")
;;             (hide-sublevels 1)))

;; (add-hook 'change-log-mode-hook
;;           (lambda ()
;;             (outline-local-set-regexp "[[:digit:]]+")
;;             (outline-minor-mode)))

;; (add-hook 'emacs-lisp-mode-hook
;;           (function (lambda ()
;;                       (outline-local-set-regexp ";;; \\|(....")
;;                       (outline-minor-mode 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; safe locals; we mark these as 'safe', so emacs22+ won't give us annoying
;; warnings
;; (setq safe-local-variable-values
;;       (quote ((auto-recompile . t)
;;               (outline-minor-mode . t)
;;               auto-recompile outline-minor-mode)))


(provide '50outline-mode)


;; Local Variables:
;; outline-regexp: ";;; "
;; mode: outline-minor
;; End:
