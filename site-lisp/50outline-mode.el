;;; tsp-outline-mode.el ---

;; Copyright (C) 2008  S.P.Tseng

;; Author: S.P.Tseng <deftsp@gmail.com>

;;Change the prefix for outline commands
;; (setq outline-minor-mode-prefix (kbd "C-o"))

(require 'outline nil t)
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
  '(mapc (lambda (bind)
           (define-key outline-minor-mode-map
               (car bind)
             (cdr bind)))
    `((,(kbd "H-o n") . outline-next-visible-heading)
      (,(kbd "H-o p") . outline-previous-visible-heading)
      (,(kbd "H-o f") . outline-forward-same-level)
      (,(kbd "H-o b") . outline-backward-same-level)
      (,(kbd "H-o u") . outline-up-heading)

      (,(kbd "H-o t") . hide-body)
      (,(kbd "H-o a") . show-all)
      (,(kbd "H-o q") . hide-sublevels)

      (,(kbd "H-o d") . hide-subtree)
      (,(kbd "H-o s") . show-subtree)
      (,(kbd "H-o TAB") . show-children)

      (,(kbd "H-o c") . hide-entry)
      (,(kbd "H-o e") . show-entry)
      (,(kbd "H-o l") . hide-leaves)

      (,(kbd "H-o k") . show-branches)

      (,(kbd "H-o o") . hide-other)
      (,(kbd "H-o RET") . outline-insert-heading)
      (,(kbd "H-o v") . outline-move-subtree-down)
      (,(kbd "H-o ^") . outline-move-subtree-up)
      (,(kbd "H-o @") . outline-mark-subtree)
      (,(kbd "H-o <") . outline-promote)
      (,(kbd "H-o >") . outline-demote))))



;; Better use (set (make-local-variable 'outline-regexp) "...") as the above changes the global value.

;; Even less obvious is how to use a different Face:
;; (set-display-table-slot standard-display-table
;;                         'selective-display
;;                         (let ((face-offset (* (face-id font-lock-keyword-face) (expt 2 19))))
;;                           (vconcat (mapcar (lambda (c) (+ face-offset c)) " ..."))))


;;;----------------------------------------------------------------------------------------------------

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

(add-hook 'change-log-mode-hook
          (lambda ()
            (setq outline-regexp "[[:digit:]]+")))
;; note that the "^" is *implicit* at the beginning of the regexp

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


(provide '50outline-mode)


;; Local Variables:
;; outline-regexp: ";;; "
;; mode: outline-minor
;; End:
