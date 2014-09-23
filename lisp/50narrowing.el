;;; 50narrowing.el ---                               -*- lexical-binding: t; -*-

;; Copyright (C) 2014  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>
;;; Code:

;;; Narrowing
;; Undo these with C-x n w
(put 'narrow-to-region 'disabled nil)     ; C-x n n
(put 'narrow-to-page   'disabled nil)     ; C-x n p
(put 'narrow-to-defun  'disabled nil)     ; C-x n d

;;; narrow to region indirect
;; http://demonastery.org/2013/04/emacs-narrow-to-region-indirect/
;; http://www.wisdomandwonder.com/link/8610/lightweight-multiple-modes-for-semi-literate-programming
(global-set-key (kbd "C-x n i") 'pl/narrow-to-region-indirect)

(defun pl/narrow-to-region-indirect (boundary-start boundary-end)
  "Edit the current region in a new, cloned, indirect buffer.
 
This function is responsible for helping the operator to easily
manipulate a subset of a buffer's contents within a new buffer. The
newly created clone buffer is created with `clone-indirect-buffer',
so all of its behaviors apply. 
 
The subset chosen for manipulation is narrowed by
`narrow-to-region'. When the clone buffer is created, the lines in
which the start and end of the boundary occur are included at the
end the new clone buffer name to serve as a reminder for its
'true source'."
  (interactive "*r")
  (deactivate-mark)
  (let* ((boundary-start (if (< boundary-start 1) (point-min)
                           boundary-start))
         (boundary-end (if (<= boundary-end boundary-start) (point-max)
                         boundary-end))
         (buf-name (generate-new-buffer-name
                    (concat (buffer-name) "/INDIRECT["
                            (number-to-string boundary-start) ", "
                            (number-to-string boundary-end) "]")))
         (buf (clone-indirect-buffer buf-name +1 +1)))
    (with-current-buffer buf
      (narrow-to-region boundary-start boundary-end)
      (goto-char (point-min))
      (switch-to-buffer buf))))

(when (boundp 'ido-ignore-buffers)
  (add-to-list 'ido-ignore-buffers "^.*/INDIRECT\\[.*\\]$"))

(eval-after-load "evil"
  '(progn
     (define-key evil-normal-state-map ",n" 'evil-narrow-indirect)
     (define-key evil-visual-state-map ",n" 'evil-narrow-indirect)

     (evil-define-operator evil-narrow-indirect (beg end type)
       "Indirectly narrow the region from BEG to END."
       (interactive "<R>")
       (evil-normal-state)
       (pl/narrow-to-region-indirect beg end))))

;;; narrow-indirect.el --- Narrow using an indirect buffer that is a clone
;; http://www.emacswiki.org/emacs/NarrowIndirect
(eval-after-load "narrow-indirect"
  '(progn
     (setq ni-buf-name-prefix "IB%")
     (define-key ctl-x-4-map "nd" 'ni-narrow-to-defun-other-window)
     (define-key ctl-x-4-map "nn" 'ni-narrow-to-region-other-window)
     (define-key ctl-x-4-map "np" 'ni-narrow-to-page-other-window)))


;; http://endlessparentheses.com/emacs-narrow-or-widen-dwim.html
(defun pl/narrow-or-widen-dwim (p)
  "If the buffer is narrowed, it widens. Otherwise, it narrows intelligently.
Intelligently means: region, org-src-block, org-subtree, or defun,
whichever applies first.
Narrowing to org-src-block actually calls `org-edit-src-code'.

With prefix P, don't widen, just narrow even if buffer is already
narrowed."
  (interactive "P")
  (declare (interactive-only))
  (cond ((and (buffer-narrowed-p) (not p)) (widen))
        ((region-active-p)
         (narrow-to-region (region-beginning) (region-end)))
        ((derived-mode-p 'org-mode)
         ;; `org-edit-src-code' is not a real narrowing command.
         ;; Remove this first conditional if you don't want it.
         (cond ((org-in-src-block-p)
                (org-edit-src-code)
                (delete-other-windows))
               ((org-at-block-p)
                (org-narrow-to-block))
               (t (org-narrow-to-subtree))))
        (t (narrow-to-defun))))


(provide '50narrowing)
;;; 50narrowing.el ends here
