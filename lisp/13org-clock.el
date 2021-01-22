;;; 13org-clock.el ---                               -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>
;; Keywords:

(defvar tl/org-clock-linkage-with-hammerspoon t)


;;; clock
(use-package org-clock
  :after (org)
  :defer t
  :init
  ;; TODO: call `org-resolve-clocks' when emacs startup
  (progn
    ;; To save the clock history across Emacs sessions
    (global-set-key (kbd "C-S-g") 'org-clock-goto) ; jump to current task from anywhere
    (setq org-clock-sound "~/.emacs.d/ni.wav")
    (setq org-clock-clocked-in-display 'mode-line)
    (setq org-clock-history-length 32)
    ;; Save the running clock and all clock history when exiting Emacs, load it on startup
    (setq org-clock-persist t)
    ;; If idle for more than 15 minutes, resolve the things by asking what to do
    ;; with the clock time
    ;; k 	keep some or all minutes and stay clocked in
    ;; K 	keep some or all minutes and clock out
    ;; s 	keep 0 minutes, and subtract some amount from the clock, clocking back in
    ;; S 	keep 0 minutes, subtract some amount from the clock, and clock out
    ;; C 	cancel the clock altogether
    (setq org-clock-idle-time nil)       ;; 30
    (setq org-clock-into-drawer "CLOCK") ; Have a special :CLOCK: drawer for clocks
    (setq org-clock-in-resume t)
    ;; Do not prompt to resume an active clock, just resume it
    (setq org-clock-persist-query-resume nil)
    (setq org-clock-out-remove-zero-time-clocks t)
    ;; Change tasks to whatever when clocking in
    (setq org-clock-in-switch-to-state "NEXT")
    ;; Clock out when moving task to a done state
    (setq org-clock-out-when-done t)
    ;; Enable auto clock resolution for finding open clocks
    (setq org-clock-auto-clock-resolution (quote when-no-clock-is-running))
    ;; Include current clocking task in clock reports
    (setq org-clock-report-include-clocking-task t)
    (with-eval-after-load 'org
      (org-clock-persistence-insinuate)))
  :config
  (progn
    ;; the function `fit-window-to-buffer' in `org-clock-select-task' shrink the
    ;; window height overmuch(However, run it in  edebug have no problem). Add a
    ;; empty line and double full shape space `　' for better look. Of course,
    ;; it a dirty fix.

    ;; TODO: It is seems caused by unmatched font and mode-line height, the help
    ;; message of org-clock-resolve which used fit-window-to-buffer have the
    ;; same problem.
    (add-hook 'org-clock-before-select-task-hook 'tl//add-extra-empty-lines-to-clock-task-select-buffer)
    (defun tl//add-extra-empty-lines-to-clock-task-select-buffer ()
      (if (string= (buffer-name) "*Clock Task Select*")
          (insert "\n　")
        (error "Run in *Clock Task Select* buffer only.")))

    (defun tl/update-hammerspoon-org-clock-bar ()
      (interactive)
      (let ((text (if (and (boundp 'org-mode-line-string)
                           org-mode-line-string
                           (fboundp' org-clocking-p)
                           (memq 'org-mode-line-string global-mode-string)
                           (org-clocking-p))
                      (substring-no-properties org-mode-line-string)
                    "Nothing !!!")))
        (tl/open-hammerspoon-url
         "org_clock_update"
         "agenda_next_info"
         text
         "task_overrun"
         (if org-clock-task-overrun "true" "false"))))

    (defun tl/org-clock-force-mode-line-update ()
      (org-clock-update-mode-line)
      (tl/update-hammerspoon-org-clock-bar))

    (when tl/org-clock-linkage-with-hammerspoon
      ;; org-clock-in will call `org-clock-update-mode-line'
      ;; (add-hook 'org-clock-in-hook 'tl/org-clock-force-mode-line-update)

      ;; `org-clock-cancel' and `org-clock-out' remove the `org-mode-line-string'
      ;; from `global-mode-string' then call `force-mode-line-update' instead of
      ;; `org-clock-update-mode-line' which have been advice to update hammerspoon
      ;; org_clock_bar
      (add-hook 'org-clock-cancel-hook 'tl/org-clock-force-mode-line-update)
      (add-hook 'org-clock-out-hook 'tl/org-clock-force-mode-line-update)

      ;; (advice-remove 'org-clock-update-mode-line #'tl/update-hammerspoon-org-clock-bar)
      (advice-add 'org-clock-update-mode-line :after #'tl/update-hammerspoon-org-clock-bar))))


(defun tl/org-insert-clock-range ()
  (interactive)
  (org-clock-in nil (apply 'encode-time (org-parse-time-string (org-read-date))))
  (org-resolve-clocks))

(use-package org-mru-clock
  :defer t
  :commands (org-mru-clock-in org-mru-clock-select-recent-task)
  :bind* (("C-c C-x i" . org-mru-clock-in)
          ("C-c C-x C-j" . org-mru-clock-select-recent-task))
  :init
  (defun tl/org-mru-clock-exclude ()
    "predicate for `org-mru-clock-predicate', excluding DONE and :ARCHIVE:."
    (not (or (org-entry-is-done-p)
             (member org-archive-tag (org-get-tags-at)))))

  (setq org-mru-clock-files #'org-agenda-files
        org-mru-clock-how-many 100
        org-mru-clock-keep-formatting t
        org-mru-clock-predicate 'tl/org-mru-clock-exclude
        org-mru-clock-completing-read #'ivy-completing-read))

;;; get the clock summary by tags
(defun tl/org-clock-summary-by-tags (include-tags timerange &optional tstart tend noinsert)
  "Get the clock summary by tags."
  (interactive "P")
  (let* ((timerange-numeric-value (prefix-numeric-value timerange))
         (files (org-add-archive-files (org-agenda-files)))
         (tags-time-alist (mapcar (lambda (tag) `(,tag . 0)) include-tags))
         (output-string "")
         (seconds-of-day 86400)
         (tstart (or tstart (org-time-today)))
         (tend (or tend (+ tstart seconds-of-day)))
         h m done-something)
    (dolist (file files)
      (let ((org-agenda-buffer (if (file-exists-p file)
                                   (org-get-agenda-file-buffer file)
                                 (error "No such file %s" file))))
        (with-current-buffer org-agenda-buffer
          (dolist (current-tag include-tags)
            (org-clock-sum tstart tend #'(lambda ()
                                           (let ((head-tags (org-get-tags-at)))
                                             (member current-tag head-tags))))
            (setcdr (assoc current-tag tags-time-alist)
                    (+ org-clock-file-total-minutes (cdr (assoc current-tag tags-time-alist))))))))
    (dolist (item tags-time-alist)
      (unless (equal (cdr item) 0)
        (setq done-something t)
        (setq h (/ (cdr item) 60)
              m (- (cdr item) (* 60 h)))
        (setq output-string (concat output-string (format "[-%s-] %.2d:%.2d\n" (car item) h m)))))
    (unless done-something
      (setq output-string (concat output-string "[-Nothing-] Done nothing!!!\n")))
    (unless noinsert
      (insert output-string))
    output-string))



(provide '13org-clock)
