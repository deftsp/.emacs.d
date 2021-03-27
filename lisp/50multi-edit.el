;;; 50multi-edit.el ---                               -*- lexical-binding: t; -*-

;; https://github.com/gabesoft/evil-mc/blob/master/evil-mc-known-commands.el
;; [[https://hungyi.net/posts/hydra-for-evil-mc/][An evil-mc Emacs Hydra - Hung-Yi’s Journal]]
(use-package evil-mc
  :commands (evil-mc-make-cursor-here
             evil-mc-make-all-cursors
	         evil-mc-undo-all-cursors evil-mc-pause-cursors
	         evil-mc-resume-cursors
	         evil-mc-make-and-goto-first-cursor
	         evil-mc-make-and-goto-last-cursor
	         evil-mc-make-cursor-move-next-line
	         evil-mc-make-cursor-move-prev-line
	         evil-mc-make-cursor-at-pos evil-mc-has-cursors-p
	         evil-mc-make-and-goto-next-cursor
	         evil-mc-skip-and-goto-next-cursor
	         evil-mc-make-and-goto-prev-cursor
	         evil-mc-skip-and-goto-prev-cursor
	         evil-mc-make-and-goto-next-match
	         evil-mc-skip-and-goto-next-match
	         evil-mc-skip-and-goto-next-match
	         evil-mc-make-and-goto-prev-match
	         evil-mc-skip-and-goto-prev-match)
  :init
  (setq evil-mc-one-cursor-show-mode-line-text nil)
  (setq evil-mc-enable-bar-cursor nil)

  (setq evil-mc-incompatible-minor-modes
        '(evil-escape-mode
	      aggressive-indent-mode
          flycheck-mode
          flyspell-mode
          haskell-indent-mode
          haskell-indentation-mode
          lispy-mode
	      yas-minor-mode))
  ;; (global-evil-mc-mode +1)
  (add-hook 'prog-mode-hook 'turn-on-evil-mc-mode)
  (add-hook 'text-mode-hook 'turn-on-evil-mc-mode)

  :config
  (advice-add 'evil-mc-undo-all-cursors :after #'tl/anzu-reset-mode-line))

(defun tl/anzu-reset-mode-line ()
  (if (fboundp 'anzu--reset-mode-line)
      (anzu--reset-mode-line)))

(general-define-key
 :states 'normal
 "gz" 'tl-mc-hydra/body)

(defhydra tl-mc-hydra (:color pink
                       :hint nil
                       :pre (evil-mc-pause-cursors))
  "
^Match^            ^Line-wise^           ^Manual^
^^^^^^----------------------------------------------------
_Z_: match all     _J_: make & go down   _z_: toggle here
_m_: make & next   _K_: make & go up     _r_: remove last
_M_: make & prev   ^ ^                   _R_: remove all
_n_: skip & next   ^ ^                   _p_: pause/resume
_N_: skip & prev

Current pattern: %`evil-mc-pattern

"
  ("Z" #'evil-mc-make-all-cursors)
  ("m" #'evil-mc-make-and-goto-next-match)
  ("M" #'evil-mc-make-and-goto-prev-match)
  ("n" #'evil-mc-skip-and-goto-next-match)
  ("N" #'evil-mc-skip-and-goto-prev-match)
  ("J" #'evil-mc-make-cursor-move-next-line)
  ("K" #'evil-mc-make-cursor-move-prev-line)
  ("z" #'+multiple-cursors/evil-mc-toggle-cursor-here)
  ("r" #'+multiple-cursors/evil-mc-undo-cursor)
  ("R" #'evil-mc-undo-all-cursors)
  ("p" #'+multiple-cursors/evil-mc-toggle-cursors)
  ("q" #'evil-mc-resume-cursors "quit" :color blue)
  ("<escape>" #'evil-mc-resume-cursors "quit" :color blue))


(defun +multiple-cursors/evil-mc-toggle-cursors ()
  "Toggle frozen state of evil-mc cursors."
  (interactive)
  (unless (evil-mc-has-cursors-p)
    (user-error "No cursors exist to be toggled"))
  (setq evil-mc-frozen (not (and (evil-mc-has-cursors-p)
                                 evil-mc-frozen)))
  (if evil-mc-frozen
      (message "evil-mc paused")
    (message "evil-mc resumed")))

(evil-define-command +multiple-cursors/evil-mc-toggle-cursor-here ()
  "Create a cursor at point. If in visual block or line mode, then create
cursors on each line of the selection, on the column of the cursor. Otherwise
pauses cursors."
  :repeat nil
  :keep-visual nil
  :evil-mc t
  (interactive)
  (cond ((and (evil-mc-has-cursors-p)
              (evil-normal-state-p)
              (let* ((pos (point))
                     (cursor (cl-find-if (lambda (cursor)
                                           (eq pos (evil-mc-get-cursor-start cursor)))
                                         evil-mc-cursor-list)))
                (when cursor
                  (evil-mc-delete-cursor cursor)
                  (setq evil-mc-cursor-list (delq cursor evil-mc-cursor-list))
                  t))))

        ((memq evil-this-type '(block line))
         (let ((col (evil-column))
               (line-at-pt (line-number-at-pos)))
           ;; Fix off-by-one error
           (when (= evil-visual-direction 1)
             (cl-decf col)
             (backward-char))
           (save-excursion
             (evil-apply-on-block
              (lambda (ibeg _)
                (unless (or (= line-at-pt (line-number-at-pos ibeg))
                            (invisible-p ibeg))
                  (goto-char ibeg)
                  (move-to-column col)
                  (when (= (current-column) col)
                    (evil-mc-make-cursor-here))))
              evil-visual-beginning
              (if (eq evil-this-type 'line) (1- evil-visual-end) evil-visual-end)
              nil)
             (evil-exit-visual-state))))
        (t
         (evil-mc-pause-cursors)
         ;; I assume I don't want the cursors to move yet
         (evil-mc-make-cursor-here))))

(evil-define-command +multiple-cursors/evil-mc-undo-cursor ()
  "Undos last cursor, or all cursors in visual region."
  :repeat nil
  :evil-mc t
  (interactive)
  (if (evil-visual-state-p)
      (or (mapc (lambda (c)
                  (evil-mc-delete-cursor c)
                  (setq evil-mc-cursor-list (delq c evil-mc-cursor-list)))
                (cl-remove-if-not
                 (lambda (pos)
                   (and (>= pos evil-visual-beginning)
                        (<  pos evil-visual-end)))
                 evil-mc-cursor-list
                 :key #'evil-mc-get-cursor-start))
          (message "No cursors to undo in region"))
    (evil-mc-undo-last-added-cursor)))

(provide '50multi-edit)
