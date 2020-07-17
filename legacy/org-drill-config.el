;;; org-drill-config.el ---                          -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>

;; org-capture template of org-drill
;; ("d" "Drill" entry (file+headline "~/org/drill/playground.org" "Pond")
;;  "* Q: %?       :drill:\n\n** A:\n"
;;  :kill-buffer t
;;  :empty-lines-after 1)

;;; org-drill
(setq org-drill-spaced-repetition-algorithm 'sm5
      org-drill-use-visible-cloze-face-p nil
      org-drill-learn-fraction 0.45
      org-drill-maximum-items-per-session 40
      org-drill-maximum-duration 30   ; 30 minutes
      org-drill-scope 'file ; use `org-drill-directory' to drill whole directory
      org-drill-leech-method 'warn
      org-drill-sm5-initial-interval 4.0
      org-drill-adjust-intervals-for-early-and-late-repetitions-p t
      org-drill-add-random-noise-to-intervals-p t)


;;; Fix org-drill and use space key as prefix key in evil mode
;; Space key have been used as prefix key. When org-drill presentation prompt,
;; press space key one time, it will expect other key sequence. Switch to
;; `evil-emacs-state' to prevent this and recover it when org-drill finish.
;; (defadvice org-drill (before tl/org-drill-switch-to-evil-emacs-state activate)
;;   "Switch to evil-emacs-state before org-drill begin."
;;   (tl/evil-state-cycle 'insert))

;; (defadvice org-drill (after tl/org-drill-recover-evil-state activate)
;;   "Recover the evil state which saved before org-drill begin."
;;   (tl/evil-state-cycle))

;; use evil-save-state to wrap it
(defun tl/evil-org-drill ()
  "Switch to evil insert state, execute `org-drill' then restore the state."
  (interactive)
  (evil-save-state
    (evil-change-state 'insert)
    (org-drill)))

(defun tl/evil-org-drill-directory ()
  "Switch to evil insert state, execute `org-drill-directory' then restore the state."
  (interactive)
  (evil-save-state
    (evil-change-state 'insert)
    (org-drill-directory)))

(defun tl/evil-org-drill-resume ()
  "Switch to evil insert state, execute `org-drill-resume' then restore the state."
  (interactive)
  (evil-save-state
    (evil-change-state 'insert)
    (org-drill-resume)))

(defun tl/evil-org-drill-again ()
  "Switch to evil insert state, execute `org-drill-again' then restore the state."
  (interactive)
  (evil-save-state
    (evil-change-state 'insert)
    (org-drill-again)))

(defun tl/evil-org-drill-cram ()
  "Switch to evil insert state, execute `org-drill-cram' then restore the state."
  (interactive)
  (evil-save-state
    (evil-change-state 'insert)
    (org-drill-cram)))
