;;; 13org-agenda.el ---

;; https://www.reddit.com/r/orgmode/comments/6ybjjw/aligned_agenda_view_anyway_to_make_this_more/
(use-package org-agenda
  :after (org)
  :init
  (setq org-agenda-files `(,(concat org-directory "/agenda"))
        ;; speedup and optimization
        ;; https://punchagan.muse-amuse.in/posts/how-i-learnt-to-use-emacs-profiler.html
        ;; http://orgmode.org/worg/agenda-optimization.html
        org-agenda-inhibit-startup t
        ;; org-agenda-use-tag-inheritance nil ; set this on a per-command in org-agenda-custom-commands
        org-agenda-show-all-dates t
        org-agenda-span 'day          ; 'week
        org-agenda-start-with-log-mode nil
        org-agenda-start-with-clockreport-mode nil
        org-agenda-view-columns-initially nil
        ;; exclude scheduled items from the global TODO list.
        org-agenda-todo-ignore-scheduled 'all
        org-agenda-todo-ignore-with-date t
        ;; org-agenda-todo-ignore-deadlines 'all
        ;; org-agenda-tags-todo-honor-ignore-options t
        org-agenda-skip-scheduled-if-done t
        org-agenda-skip-deadline-if-done t
        org-agenda-skip-timestamp-if-done t
        org-agenda-todo-list-sublevels t
        ;; I use the diary only for sexp entries and holidays, and Ι have put them into org file.
        org-agenda-include-diary nil
        ;; agenda view always starts out by showing me the next seven days.
        org-agenda-start-on-weekday nil
        org-agenda-restore-windows-after-quit t
        org-agenda-repeating-timestamp-show-all nil
        org-agenda-use-time-grid t
        org-agenda-block-separator ?▰
        org-agenda-dim-blocked-tasks t ; 'invisible
        org-agenda-window-frame-fractions '(0.6 . 0.85) ; the min and max height of the agenda window as a fraction of frame height.
        org-agenda-show-future-repeats 'next
        org-agenda-prefer-last-repeat nil)

  (setq org-agenda-format-date 'tl/org-agenda-format-date-aligned)

  (setq org-agenda-time-grid '((daily today remove-match) ; require-timed
                               (300 600 900 1200 1500 1800 2100 2400)
                               "......"
                               "----------------"))

  ;; (setq org-agenda-deadline-leaders '("!D!: " "D%2d: " "")) ; default ("Deadline:  " "In %3d d.: " "%2d d. ago: ")
  ;; (setq org-agenda-scheduled-leaders '("Scheduled: " "Sched.%2dx: ")) ; default '("Scheduled: " "Sched.%2dx: ")
  ;; (org-agenda-todo-keyword-format "")
  (setq org-agenda-prefix-format
        '(
          ;; (agenda . " %i %-12:c%-12t% s") ; agenda
          (agenda  . "  %-13:c%?-12t% s")
		  ;; (timeline . "%-9:T%?-2t% s")   ; timeline
          (todo . " %i %-13:c")       ; todo, alltodo
          (tags . " %i %-13:c")       ; tags, tags-todo, stuck
          (search . " %i %-13:c")))   ; search

  (setq org-agenda-breadcrumbs-separator " ⊇ ")
  ;; (setq org-agenda-category-icon-alist
  ;;       '(("Visitors" "~/.emacs.d/icons/org/visitors.png" nil nil :ascent center)
  ;;         ("\\(Party\\|Celeb\\)" "~/.emacs.d/icons/org/party.png" nil nil :ascent center)
  ;;         ("Org" "~/.emacs.d/icons/org/org.png" nil nil :ascent center)
  ;;         ("Medical" "~/.emacs.d/icons/org/medical.png" nil nil :ascent center)
  ;;         ("Music" "~/.emacs.d/icons/org/music.png" nil nil :ascent center)
  ;;         ("Trip" "~/.emacs.d/icons/org/trip.png" nil nil :ascent center)
  ;;         ("Train" "~/.emacs.d/icons/org/train.png" nil nil :ascent center)
  ;;         ("Anniv" "~/.emacs.d/icons/org/anniversary.png" nil nil :ascent center)
  ;;         ("Debian" "~/.emacs.d/icons/org/debian.png" nil nil :ascent center)
  ;;         ("Plants" "~/.emacs.d/icons/org/tree.png" nil nil :ascent center)
  ;;         ("Reading" "~/.emacs.d/icons/org/book.png" nil nil :ascent center)
  ;;         ("\\(Holidays\\|Vacation\\)" "~/.emacs.d/icons/org/holidays.png" nil nil :ascent center)
  ;;         (".*" '(space . (:width (16))))))
  (tl/set-org-agenda-custom-commands)
  :config
  (progn
    (defun tl/org-agenda-mode-init ()
      (setq truncate-lines t)
      (define-key org-agenda-mode-map " " 'org-agenda-cycle-show))

    (add-hook 'org-agenda-mode-hook 'tl/org-agenda-mode-init)

    ;; Agenda clock report
    ;;  v R to toggle it. In evilify state, use C-v R
    (setq org-agenda-clockreport-parameter-plist
          '(:link t :maxlevel 6 :fileskip0 t :compact t :narrow 60 :score 0))
    (setq org-agenda-clock-consistency-checks
          '(:max-duration "5:00"
            :min-duration 0
            :max-gap 15
            :gap-ok-around ("4:00" "13:00")
            :default-face '((:background "#9e3e83") (:foreground "#d2e2f2"))))

    ;; Place tags close to the right-hand side of the window
    (add-hook 'org-agenda-finalize-hook 'tl/org-agenda-adjust-tags-column)
    (defun tl/org-agenda-adjust-tags-column ()
      "Put the agenda tags by the right border of the agenda window."
      (setq org-agenda-tags-column (- 10 (window-text-width)))
      (org-agenda-align-tags))

    ;; https://blog.aaronbieber.com/2016/09/25/agenda-interactions-primer.html

    ;; Pressing c by itself to open my default capture command, which is a TODO
    ;; entry (the shortcut key is “a” and that is passed to org-capture). If I
    ;; use a prefix (by pressing C-u c), it will open the default (“vanilla”)
    ;; Org Mode capture dialog, prompting me to pick a capture type, where I can
    ;; choose my “note” type or others I have developed.
    (defun tl/org-agenda-capture (&optional vanilla)
      "Capture a task in agenda mode, using the date at point.

If VANILLA is non-nil, run the standard `org-capture'."
      (interactive "P")
      (if vanilla
          (org-capture)
        (let ((org-overriding-default-time (org-get-cursor-date)))
          (org-capture nil "a"))))

    ;; evilify agenda mode
    (org-defkey org-agenda-mode-map "|" nil)  ;'org-agenda-filter-remove-all
    (org-defkey org-agenda-mode-map "\\" nil) ;'org-agenda-query-not-cmd
    (org-defkey org-agenda-mode-map (kbd "C-n") nil)
    (org-defkey org-agenda-mode-map (kbd "G") nil) ;'org-agenda-toggle-time-grid

    ;; https://blog.aaronbieber.com/2016/09/25/agenda-interactions-primer.html
    (defun tl/org-agenda-next-header ()
      "Jump to the next header in an agenda series."
      (interactive)
      (tl//org-agenda-goto-header))

    (defun tl/org-agenda-previous-header ()
      "Jump to the previous header in an agenda series."
      (interactive)
      (tl//org-agenda-goto-header t))

    (defun tl//org-agenda-goto-header (&optional backwards)
      "Find the next agenda series header forwards or BACKWARDS."
      (let ((pos (save-excursion
                   (goto-char (if backwards
                                  (line-beginning-position)
                                (line-end-position)))
                   (let* ((find-func (if backwards
                                         'previous-single-property-change
                                       'next-single-property-change))
                          (end-func (if backwards
                                        'max
                                      'min))
                          (all-pos-raw (list (funcall find-func (point) 'org-agenda-structural-header)
                                             (funcall find-func (point) 'org-agenda-date-header)))
                          (all-pos (cl-remove-if-not 'numberp all-pos-raw))
                          (prop-pos (if all-pos (apply end-func all-pos) nil)))
                     prop-pos))))
        (if pos (goto-char pos))
        (if backwards (goto-char (line-beginning-position)))))

    (general-define-key
     :states 'normal
     :keymaps 'org-agenda-mode-map
     (kbd "C-h") nil
     ;; open
     (kbd "<tab>") 'org-agenda-goto
     (kbd "<return>") 'org-agenda-switch-to
     (kbd "S-<return>") 'org-agenda-goto
     (kbd "M-<return>") 'org-agenda-recenter
     ;; with ", SPC" to 'org-agenda-show-and-scroll-up
     (kbd "<delete>") 'org-agenda-show-scroll-down
     (kbd "<backspace>") 'org-agenda-show-scroll-down

     ;; motion
     "j" 'org-agenda-next-line
     "k" 'org-agenda-previous-line
     "gj" 'tl/org-agenda-next-header
     "gk" 'tl/org-agenda-previous-header
     (kbd "C-j") 'org-agenda-next-item
     (kbd "C-k") 'org-agenda-previous-item
     (kbd "[") 'org-agenda-earlier      ; default to 'org-agenda-manipulate-query-add
     (kbd "]") 'org-agenda-later        ; default to 'org-agenda-manipulate-query-subtract

     ;; manipulation
     ;; We follow standard org-mode bindings (not org-agenda bindings):
     ;; <HJKL> change todo items and priorities.
     ;; M-<jk> drag lines.
     ;; M-<hl> cannot demote/promote, we use it for "do-date".
     "J" 'org-agenda-priority-down      ; default bind to 'org-agenda-clock-goto
     "K" 'org-agenda-priority-up
     "H" 'org-agenda-do-date-earlier
     "L" 'org-agenda-do-date-later
     (kbd "M-j") 'org-agenda-drag-line-forward
     (kbd "M-k") 'org-agenda-drag-line-backward
     (kbd "C-S-h") 'org-agenda-todo-previousset ; Original binding "C-S-<left>"
     (kbd "C-S-l") 'org-agenda-todo-nextset     ; Original binding "C-S-<right>"

     ;; undo
     "u" 'org-agenda-undo

     ;; actions
     "dd" 'org-agenda-kill
     "dA" 'org-agenda-archive
     "da" 'org-agenda-archive-default-with-confirmation
     ;; "c" default bind to 'org-agenda-goto-calendar
     "ct" 'org-agenda-set-tags
     "ce" 'org-agenda-set-effort
     "cT" 'org-timer-set-timer
     "i" 'org-agenda-diary-entry
     "a" 'org-agenda-add-note
     "A" 'org-agenda-append-agenda
     "C" 'tl/org-agenda-capture         ; default to 'org-agenda-convert-date
     "E" 'org-agenda-entry-text-mode

     ;; mark
     "m" 'org-agenda-bulk-toggle
     "~" 'org-agenda-bulk-toggle-all    ; default bind to 'org-agenda-limit-interactively
     "*" 'org-agenda-bulk-mark-all
     "%" 'org-agenda-bulk-mark-regexp
     "M" 'org-agenda-bulk-unmark-all    ; default bind to 'org-agenda-phases-of-moon
     "x" 'org-agenda-bulk-action

     ;; refresh
     "gr" 'org-agenda-redo
     "gR" 'org-agenda-redo-all

     ;; quit
     "ZQ" 'org-agenda-exit
     "ZZ" 'org-agenda-quit

     ;; display
     ;; "Dispatch" can prefix the following:
     ;; 'org-agenda-toggle-deadlines
     ;; 'org-agenda-toggle-diary
     ;; 'org-agenda-follow-mode
     ;; 'org-agenda-log-mode
     ;; 'org-agenda-entry-text-mode
     ;; 'org-agenda-toggle-time-grid
     ;; 'org-agenda-day-view
     ;; 'org-agenda-week-view
     ;; 'org-agenda-year-view
     "z" 'org-agenda-view-mode-dispatch ; defautl bind to 'org-agenda-add-note
     "ZD" 'org-agenda-dim-blocked-tasks

     ;; filter
     ;; f default bind to 'org-agenda-later
     "fc" 'org-agenda-filter-by-category
     "fr" 'org-agenda-filter-by-regexp
     "fx" 'org-agenda-filter-by-regexp
     "fe" 'org-agenda-filter-by-effort
     "ft" 'org-agenda-filter-by-tag
     "f^" 'org-agenda-filter-by-top-headline
     "fh" 'org-agenda-filter-by-top-headline
     "fs" 'org-agenda-limit-interactively
     "fd" 'org-agenda-filter-remove-all
     "F" 'org-agenda-filter-remove-all

     ;; clock
     "I" 'org-agenda-clock-in           ; Original binding
     "O" 'org-agenda-clock-out          ; Original binding

     "cg" 'org-agenda-clock-goto
     "cc" 'org-agenda-clock-cancel
     "cr" 'org-agenda-clockreport-mode

     ;; go and show
     "." 'org-agenda-goto-today         ; TODO: What about evil-repeat?
     "gc" 'org-agenda-goto-calendar
     "gC" 'org-agenda-convert-date
     "gd" 'org-agenda-goto-date
     "gh" 'org-agenda-holidays
     "gm" 'org-agenda-phases-of-moon
     "gs" 'org-agenda-sunrise-sunset
     "gt" 'org-agenda-show-tags

     "p" 'org-agenda-date-prompt
     "P" 'org-agenda-show-the-flagging-note

     "s"  'org-save-all-org-buffers ; Original binding "C-x C-s"

     ;; todo
     "t"  'org-agenda-todo

     ;; Others
     "+" 'org-agenda-manipulate-query-add
     "-" 'org-agenda-manipulate-query-subtract
     (kbd "M-SPC") 'tl/org-agenda/body)

    (dolist (prefix '(("mC" . "clocks")
                      ("md" . "dates")
                      ("mi" . "insert")
                      ("ms" . "trees/subtrees")))
      (tl/declare-prefix-for-mode 'org-agenda-mode
        (car prefix) (cdr prefix)))

    (tl/set-leader-keys-for-mode 'org-agenda-mode
      "C-c C-t" 'org-todo-yesterday

      "a" 'org-agenda
      "ie" 'org-agenda-set-effort
      "ip" 'org-agenda-set-property
      "it" 'org-agenda-set-tags
      "sr" 'org-agenda-refile

      ;; Entry
      "h:" 'org-agenda-set-tags
      "hA" 'org-agenda-archive-default
      "hk" 'org-agenda-kill
      "hp" 'org-agenda-priority
      "hr" 'org-agenda-refile
      "ht" 'org-agenda-todo

      ;; Visit entry
      "SPC" 'org-agenda-show-and-scroll-up
      "<tab>" 'org-agenda-goto
      "TAB" 'org-agenda-goto
      "RET" 'org-agenda-switch-to
      "o"   'link-hint-open-link

      ;; Date
      "ds" 'org-agenda-schedule
      "dd" 'org-agenda-deadline
      "dt" 'org-agenda-date-prompt
      "+" 'org-agenda-do-date-later
      "-" 'org-agenda-do-date-earlier

      ;; View
      "vd" 'org-agenda-day-view
      "vw" 'org-agenda-week-view
      "vt" 'org-agenda-fortnight-view
      "vm" 'org-agenda-month-view
      "vy" 'org-agenda-year-view
      "vn" 'org-agenda-later
      "vp" 'org-agenda-earlier
      "vr" 'org-agenda-reset-view

      ;; Toggle mode
      "tf" 'org-agenda-follow-mode
      "tl" 'org-agenda-log-mode
      "ta" 'org-agenda-archives-mode
      "tr" 'org-agenda-clockreport-mode
      "td" 'org-agenda-toggle-diary

      ;; Filter
      "ft" 'org-agenda-filter-by-tag
      "fr" 'org-agenda-filter-by-tag-refine
      "fc" 'org-agenda-filter-by-category
      "fh" 'org-agenda-filter-by-top-headline
      "fx" 'org-agenda-filter-by-regexp
      "fd" 'org-agenda-filter-remove-all

      ;; Clock
      "ci" 'org-agenda-clock-in
      "cj" 'org-agenda-clock-goto
      "co" 'org-agenda-clock-out
      "cq" 'org-agenda-clock-cancel

      "gr" 'org-agenda-redo
      "." 'org-agenda-goto-today
      "gd" 'org-agenda-goto-date)))

;; https://emacs.stackexchange.com/a/9792/361
(defun tl/org-agenda-level-ident ()
  (let ((level (org-current-level)))
    (if (> level 1)
        (concat (make-string (* 2 (1- level))  #x20) "· ") ; ○►
        "")))

;; https://emacs.stackexchange.com/a/9793/361
(defun tl/org-entry-subtree-in-state-get (state property)
  (save-excursion
    (save-restriction
      (org-narrow-to-subtree)
      (goto-char (point-max))
      (save-match-data
        (cl-loop while (re-search-backward org-heading-regexp nil t)
                 when (string-equal state (org-get-todo-state))
                 collect (org-entry-get (point) property))))))


(defun tl/org-effort-sum-current-item-in-state (state)
  (org-duration-from-minutes
   (cl-loop for minutes in (tl/org-entry-subtree-in-state-get state "Effort")
            sum (org-duration-to-minutes (or minutes "0:00")))))

(defun tl/org-agenda-projects-prefix ()
  (format "[%s] %s" (tl/org-effort-sum-current-item-in-state "TODO") (tl/org-agenda-level-ident)))


(defun tl/set-org-agenda-custom-commands ()
  ;; Use tags-todo like this (tags-todo "+PRIORITY={A}") will be very slow,
  ;; however add more restricts, like CATEGORY, it works fine

  ;; http://orgmode.org/manual/Matching-tags-and-properties.html
  ;; https://blog.aaronbieber.com/2016/09/24/an-agenda-for-life-with-org-mode.html
  (setq org-agenda-custom-commands
        `((" " "Daily agenda and all TODOs"
           ((agenda "" ((org-agenda-span 'day)
                        ;; (org-agenda-prefix-format " %(let ((scheduled (org-get-scheduled-time (point)))) (if scheduled (format-time-string \"%Y-%m-%d\" scheduled) \"\")) %i %-12:c")
                        ;; (org-agenda-skip-function '(or (org-agenda-skip-entry-if 'deadline)))
                        (org-agenda-sorting-strategy '(time-up
                                                       priority-down
                                                       todo-state-down
                                                       effort-up
                                                       habit-down
                                                       category-keep))))
            (tags-todo "TODO=\"NEXT\""
                       ((org-agenda-sorting-strategy '(priority-down tag-up))
                        (org-agenda-overriding-header "NEXT Tasks:")))
            (tags-todo "CATEGORY=\"Proj\"&LEVEL<=3"
                       ((org-agenda-sorting-strategy '(priority-down tag-up))
                        ;; N.B. that there is a third bug or oddity here: you can only use %(expression) once
                        (org-agenda-prefix-format " %i %(tl/org-agenda-projects-prefix)")
                        (org-agenda-skip-function '(or
                                                    (org-agenda-skip-entry-if 'todo 'done )))
                        (org-agenda-overriding-header "Projects:")))

            ;; (tags "+PRIORITY=\"A\"+CATEGORY={Inbox\\|Task\\|Project}"
            ;;       ((org-agenda-skip-function
            ;;         '(org-agenda-skip-entry-if 'todo 'done))

            ;;        (org-agenda-sorting-strategy '(tag-up priority-down))
            ;;        ;; (org-agenda-todo-keyword-format "")
            ;;        (org-agenda-overriding-header "High Priority Unfinished Tasks:")))

            (alltodo ""
                     ((org-agenda-skip-function '(or (tl/org-agenda-skip-subtree-if-habit)
                                                     (tl/org-agenda-skip-subtree-if-priority ?A)
                                                     (tl/org-agenda-skip-project)
                                                     (tl/org-agenda-skip-if-blocked)
                                                     (org-agenda-skip-entry-if 'regexp "\\* NEXT")
                                                     (org-agenda-skip-entry-if 'scheduled 'deadline)))
                      (org-agenda-overriding-header "Inbox:")
                      (org-agenda-sorting-strategy '(priority-down
                                                     category-keep)))))
           ;; ((org-agenda-compact-blocks t))
           ((org-agenda-block-separator ?▰)))
          ("A" "Today's Priority #A Tasks" agenda ""
           ((org-agenda-entry-types '(:scheduled))
            (org-agenda-span 'day)
            (org-agenda-skip-function '(org-agenda-skip-entry-if 'notregexp "\\=.*\\[#A\\]"))
            (org-agenda-overriding-header "Today's Priority #A Tasks: ")))

          ("b" "Today's Priority #A and #B tasks" agenda ""
           ((org-agenda-entry-types '(:scheduled))
            (org-agenda-span 'day)
            (org-agenda-overriding-header "Today's priority #A and #B Tasks: ")
            (org-agenda-skip-function '(org-agenda-skip-entry-if 'regexp "\\=.*\\[#C\\]"))))

          ("B" "Blocked entries" alltodo ""
           ((org-agenda-skip-function '(tl/org-agenda-skip-if-not-blocked))
            (org-enforce-todo-checkbox-dependencies nil)))

          ("c" "Appointment Calendar" agenda ""
           ((org-agenda-overriding-header "Appointment Calendar")
            (org-agenda-sorting-strategy '(time-up))
            (org-agenda-span 14)
            (org-agenda-regexp-filter-preset '("+APPT"))))

          ;; ("c" todo "DONE|DEFERRED|CANCELLED") ; not include scheduled TODO entiries

          ("D" "Done Tasks" tags "TODO=\"DONE\""
           ((org-agenda-overriding-header "Done Tasks")))

          ("d" "Deadlined tasks" tags "TODO<>\"\"&TODO<>{DONE\\|CANCELED\\|NOTE\\|PROJECT}"
           ((org-agenda-overriding-header "Deadlined Tasks: ")
            (org-agenda-skip-function '(org-agenda-skip-entry-if 'notdeadline))
            (org-agenda-sorting-strategy '(category-up))))

          ("e" "Emacs Tasks" tags "emacs&LEVEL<=2"
           ((org-agenda-overriding-header "Emacs Tasks")))

          ;; ;; to create a sparse tree (again: current buffer only) with all entries containing the word `FIXME'.
          ("f" occur-tree "\\<FIXME\\>")

          ;; ("g" "GeekTool Agenda"
          ;;  ((agenda ""))
          ;;  ((org-agenda-todo-keyword-format "%-11s")
          ;;   (org-agenda-prefix-format "  %-10T%?-16t% s")
          ;;   (org-agenda-show-inherited-tags nil)
          ;;   (org-agenda-remove-tags 'prefix)
          ;;   (org-agenda-tags-column 70))
          ;;  (,(concat org-directory  "/Agenda.txt")))

          ("h" "Habits" tags-todo "TODO<>\"\""
           ((org-agenda-overriding-header "Habits")
            (org-agenda-entry-types '(:scheduled))
            (org-agenda-skip-function '(or (org-agenda-skip-entry-if 'notscheduled)
                                           (tl/org-agenda-skip-subtree-if-not-habit)))
            (org-agenda-sorting-strategy
             '(todo-state-down effort-up category-keep))))
          ("O" "All TODOs" tags "TODO<>\"\"" ((org-agenda-overriding-header "All TODOs")))
          ;; ("p" tags "+project-TODO=\"DONE\"-TODO=\"CANCELLED\"")

          ;; Weekly Review block agenda
          ("r" . "Weekly Review")
          ("ri" "Collect loose materials and process Inbox"
           tags "CATEGORY=\"Inbox\"&LEVEL>1"
           ((org-agenda-overriding-header "Inbox items to process:")
            (org-agenda-prefix-format "  ")))
          ("rn" "Review Next Actions\n    Archive completed actions, review for further action steps"
           ((todo "DONE|CANCELLED|FAILED|FIXED" ((org-agenda-overriding-header "Done/Dropped Items (to archive):")
                                                 (org-agenda-cmp-user-defined (tl/cmp-date-property "CLOSED"))
                                                 (org-agenda-sorting-strategy '(user-defined-up))))
            (tags-todo "NEXT" ((org-agenda-overriding-header "Next Actions:")
                               (org-agenda-sorting-strategy '(time-up category-up alpha-up))
                               (org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled))))
            (tags-todo "NEXT" ((org-agenda-overriding-header "Scheduled Actions:")
                               (org-agenda-sorting-strategy '(time-up category-up alpha-up))
                               (org-agenda-skip-function '(org-agenda-skip-entry-if 'notscheduled)))))
           ((org-agenda-prefix-format "%-12:c ")))
          ("rc" "Review Previous Calendar"
           ((agenda "" ((org-agenda-start-day
                         (concat "-" (number-to-string
                                      (- 6 (nth 6 (decode-time)))) "d"))
                        (org-agenda-span 7)
                        (org-agenda-repeating-timestamp-show-all t)
                        (org-agenda-entry-types '(:deadline :timestamp :sexp)) ; show due tasks, meetings
                        (org-agenda-show-log t)
                        (org-agenda-prefix-format "%-12t% s")))))
          ("S" "Scheduled tasks" tags "TODO<>\"\"&TODO<>{APPT\\|DONE\\|CANCELED\\|NOTE}"
           ((org-agenda-overriding-header "Scheduled tasks: ")
            (org-agenda-skip-function '(or (tl/org-agenda-skip-subtree-if-habit)
                                           ;; (org-agenda-skip-entry-if 'notregexp "\\=.*\\[#A\\]")
                                           (org-agenda-skip-entry-if 'notscheduled)))
            (org-agenda-sorting-strategy '(category-up))))

          ("u" "Unscheduled tasks" tags "TODO<>\"\"&TODO<>{DONE\\|CANCELED\\|NOTE}"
           ((org-agenda-overriding-header "Unscheduled tasks: ")
            (org-agenda-skip-function '(org-agenda-skip-entry-if
                                        'scheduled
                                        'deadline
                                        'timestamp
                                        'regexp
                                        "\\* \\(DEFERRED\\|SOMEDAY\\)"))
            (org-agenda-sorting-strategy '(user-defined-up))
            (org-agenda-prefix-format "%-11c%5(tl/org-todo-age) ")))

          ;; ("u" alltodo "Unscheduled TODO entries"
          ;;  ((org-agenda-skip-function
          ;;    '(org-agenda-skip-entry-if 'scheduled 'deadline 'regexp "<[^>\n]+>"))
          ;;   (org-agenda-overriding-header "Unscheduled TODO entries: ")))

          ("U" "Deferred tasks" tags "TODO=\"DEFERRED\""
           ((org-agenda-overriding-header "Deferred tasks:")
            (org-agenda-sorting-strategy '(user-defined-up))
            (org-agenda-prefix-format "%-11c%5(tl/org-todo-age) ")))


          ;; ("w" todo "WAITING" )
          ;; ("W" todo-tree "WAITING")
          ("w" "Unscheduled work-related tasks" tags "TODO<>\"\"&TODO<>{DONE\\|CANCELED\\|NOTE}"
           ((org-agenda-overriding-header "Unscheduled work-related tasks")
            (org-agenda-sorting-strategy '(todo-state-up priority-down category-up))
            (org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled 'deadline 'timestamp))
            (org-agenda-prefix-format "%-11c%5(tl/org-todo-age) ")))

          ("W" "Waiting/delegated tasks" tags "TODO=\"WAITING\"|TODO=\"DELEGATED\""
           ((org-agenda-overriding-header "Waiting/delegated tasks:")
            (org-agenda-sorting-strategy '(todo-state-up priority-down category-up))))

          ("Y" "Someday tasks" tags "TODO=\"SOMEDAY\""
           ((org-agenda-overriding-header "Someday tasks:")
            (org-agenda-sorting-strategy '(user-defined-up))
            (org-agenda-prefix-format "%-11c%5(tl/org-todo-age) "))))))


(use-package org-clock-convenience
  :commands (org-clock-convenience-timestamp-up
             org-clock-convenience-timestamp-down
             org-clock-convenience-goto-ts
             org-clock-convenience-goto-last-clockout)
  :bind (:map org-agenda-mode-map
         ;; ("ö" . org-clock-convenience-fill-gap)
         ;; ("é" . org-clock-convenience-fill-gap-both)
         ("s-k" . org-clock-convenience-timestamp-up)
         ("s-j" . org-clock-convenience-timestamp-down)))

;;; Other frame
(defun tl/update-org-agenda-window-setup (&rest args)
  (let* ((attrs (display-monitor-attributes-list))
         (window-config
          (if (= (length attrs) 1)
              'reorganize-frame
            (dolist (attr attrs)
              (let ((name (assoc-default 'name attr))
                    (frames (assoc-default 'frames attr)))
                (when (member (selected-frame) frames)
                  (if (string= name dottl-org-agenda-screen-name)
                      (return 'only-window)
                    (return 'other-frame))))))))
    (setq org-agenda-window-setup window-config)))

;; (tl/update-org-agenda-window-setup)

;; (defun tl//update-hammerspoon-agenda-layout ()
;;   (tl/open-hammerspoon-url "update_agenda_layout"))

(advice-add 'org-agenda-prepare-window :before #'tl/update-org-agenda-window-setup)
;; (advice-remove 'org-agenda-prepare-window #'tl/update-org-agenda-window-setup)

;; https://blog.aaronbieber.com/2016/09/24/an-agenda-for-life-with-org-mode.html

;; the “lowest priority” is the upper bounding ASCII value of the letters
;; used, such that the difference of that value and the entry’s priority letter
;; value multiplied by 1,000 is the numeric priority.

;; The default “lowest priority” value is 67, and the ASCII value of “A” is
;; 65, so the numeric value of priority “A” is 2,000, “B” (ASCII value 66)
;; is 1,000, and “C” (ASCII value 67) is 0.

;; For whatever reason, there are no internal Org functions to easily extract
;; just the priority letter, but I wanted my function to accept the letter
;; rather than the numeric value so I just convert that to its corresponding
;; number and use org-get-priority to compare the entry’s value to the given
;; one.
(defun tl/org-agenda-skip-subtree-if-priority (priority)
  "Skip an agenda subtree if it has a priority of PRIORITY.
PRIORITY may be one of the characters ?A, ?B, or ?C."
  (let ((subtree-end (save-excursion (org-end-of-subtree t)))
        (pri-value (* 1000 (- org-lowest-priority priority)))
        (pri-current (org-get-priority (thing-at-point 'line t))))
    (if (= pri-value pri-current)
        subtree-end
      nil)))


(defun tl/org-agenda-skip-subtree-if-not-priority (priority)
  "Skip an agenda subtree if it don't have a priority of PRIORITY.
PRIORITY may be one of the characters ?A, ?B, or ?C."
  (let ((subtree-end (save-excursion (org-end-of-subtree t)))
        (pri-value (* 1000 (- org-lowest-priority priority)))
        (pri-current (org-get-priority (thing-at-point 'line t))))
    (if (= pri-value pri-current)
        nil
      subtree-end)))

;; https://emacs.stackexchange.com/questions/14724/emacs-org-mode-how-to-make-agenda-views-of-blocked-parent-tasks
(defun tl/org-agenda-skip-if-not-blocked ()
  (let ((next-headline (save-excursion
                         (or (outline-next-heading) (point-max)))))
    (if (not (org-entry-blocked-p)) next-headline)))

(defun tl/org-agenda-skip-if-blocked ()
  (let ((next-headline (save-excursion
                         (or (outline-next-heading) (point-max)))))
    (if (org-entry-blocked-p) next-headline)))

(defun tl/org-agenda-skip-subtree-if-habit ()
  "Skip an agenda entry if it has a STYLE property equal to \"habit\"."
  (let ((subtree-end (save-excursion (org-end-of-subtree t))))
    (if (string= (org-entry-get nil "STYLE") "habit")
        subtree-end
      nil)))

(defun tl/org-agenda-skip-subtree-if-not-habit ()
  "Skip an agenda entry if it has a STYLE property equal to \"habit\"."
  (let ((subtree-end (save-excursion (org-end-of-subtree t))))
    (if (string= (org-entry-get nil "STYLE") "habit")
        nil
      subtree-end)))


(defun tl/org-agenda-skip-project ()
  "Skip an agenda entry if it has a STYLE property equal to \"habit\"."
  (let ((subtree-end (save-excursion (org-end-of-subtree t))))
    (if (string= (org-entry-get nil "CATEGORY") "Proj")
        subtree-end
      nil)))


;; https://emacs.cafe/emacs/orgmode/gtd/2017/06/30/orgmode-gtd.html
;; can be use to  only show the first action to be done (or next action) for each project
;; TODO: test it and add category filter
;; (defun tl/org-agenda-skip-all-siblings-but-first ()
;;   "Skip all but the first non-done entry."
;;   (let (should-skip-entry)
;;     (unless (tl/org-current-is-todo)
;;       (setq should-skip-entry t))
;;     (save-excursion
;;       (while (and (not should-skip-entry) (org-goto-sibling t))
;;         (when (tl/org-current-is-todo)
;;           (setq should-skip-entry t))))
;;     (when should-skip-entry
;;       (or (outline-next-heading)
;;           (goto-char (point-max))))))

;; (defun tl/org-current-is-todo ()
;;   (string= "TODO" (org-get-todo-state)))

;; from http://emacs.stackexchange.com/questions/26351/custom-sorting-for-agenda
;; being used in a org agenda custom command below
(defun tl/cmp-date-property (prop)
  "Compare two `org-mode' agenda entries, `A' and `B', by some date property. If a is before b, return -1. If a is after b, return 1. If they are equal return t."
  (lexical-let ((prop prop))
    #'(lambda (a b)
        (let* ((a-pos (get-text-property 0 'org-marker a))
               (b-pos (get-text-property 0 'org-marker b))
               (a-date (or (org-entry-get a-pos prop)
                           (format "<%s>" (org-read-date t nil "now"))))
               (b-date (or (org-entry-get b-pos prop)
                           (format "<%s>" (org-read-date t nil "now"))))
               (cmp (compare-strings a-date nil nil b-date nil nil)))
          (if (eq cmp t) nil (signum cmp))))))


;; https://github.com/jwiegley/dot-emacs/blob/master/dot-org.el
(defun tl/org-todo-age-time (&optional pos)
  (let ((stamp (org-entry-get (or pos (point)) "CREATED" t)))
    (when stamp
      (time-subtract (current-time)
                     (org-time-string-to-time
                      stamp)))))

(defun tl/org-todo-age (&optional pos)
  (let* ((age-time (tl/org-todo-age-time pos))
         (days (if age-time (time-to-number-of-days age-time) nil)))
    (cond ((null days) "Unk")
          ((< days 1)   "today")
          ((< days 7)   (format "%dd" days))
          ((< days 30)  (format "%.1fw" (/ days 7.0)))
          ((< days 358) (format "%.1fM" (/ days 30.0)))
          (t            (format "%.1fY" (/ days 365.0))))))


(defun tl/org-compare-todo-age (a b)
  (let ((time-a (tl/org-todo-age-time (get-text-property 0 'org-hd-marker a)))
        (time-b (tl/org-todo-age-time (get-text-property 0 'org-hd-marker b))))
    (cond ((null time-a) -1)
          ((null time-b) 1)
          ((and (null time-a) (null time-b)) 1)
          (t (if (time-less-p time-a time-b)
                 -1
               (if (equal time-a time-b) 0 1))))))



;; Base on https://github.com/tumashu/emacs-helper/blob/0f69885eceab7e20fa998ff9c79e977622952346/eh-org.el#L776
(defun tl/org-agenda-format-date-aligned (date)
  (require 'cal-iso)
  (let* ((dayname (calendar-day-name date))
         (day (cadr date))
         (day-of-week (calendar-day-of-week date))
         (month (car date))
         ;; (monthname (calendar-month-name month))
         (year (nth 2 date))
         (iso-week (org-days-to-iso-week
                    (calendar-absolute-from-gregorian date)))
         ;; (weekyear (cond ((and (= month 1) (>= iso-week 52))
         ;;                  (1- year))
         ;;                 ((and (= month 12) (<= iso-week 1))
         ;;                  (1+ year))
         ;;                 (t year)))
         (cn-date (calendar-chinese-from-absolute
                   (calendar-absolute-from-gregorian date)))
         ;; (cn-year (cadr cn-date))
         (cn-month (cl-caddr cn-date))
         (cn-day (cl-cadddr cn-date))
         ;; (cn-month-name
         ;;  ["正月" "二月" "三月" "四月" "五月" "六月"
         ;;   "七月" "八月" "九月" "十月" "冬月" "腊月"])
         (cn-day-name
          ["初一" "初二" "初三" "初四" "初五" "初六" "初七" "初八" "初九" "初十"
           "十一" "十二" "十三" "十四" "十五" "十六" "十七" "十八" "十九" "二十"
           "廿一" "廿二" "廿三" "廿四" "廿五" "廿六" "廿七" "廿八" "廿九" "三十"
           "卅一" "卅二" "卅三" "卅四" "卅五" "卅六" "卅七" "卅八" "卅九" "卅十"])
         (extra (format "(%s%s%s)"
                        ;; (if (or (eq org-agenda-current-span 'day)
                        ;;         (= day-of-week 1)
                        ;;         (= cn-day 1))
                        ;;     (aref cn-month-name (1-  (floor cn-month)))
                        ;;   "")
                        (if (or (= day-of-week 1)
                                (= cn-day 1))
                            (if (integerp cn-month) "" "[闰]")
                          "")
                        (aref cn-day-name (1- cn-day))
                        (if (= day-of-week 1)
                            (format "，第%02d 周" iso-week)
                          ""))))
    (format "%04d-%02d-%02d %-10s %s"
            year month day dayname extra)))


;;; work with appt
;; If you delete an appointment from your Org agenda file, the corresponding
;; alert is not deleted.
;; (defadvice org-agenda-to-appt (before wickedcool activate)
;;   "Clear the appt-time-msg-list."
;;   (setq appt-time-msg-list nil))

(defun tl//quiet-no-event-message (orig-fun &rest args)
  "Do annoy me when no event"
  (tl/with-suppress-message "No event to add"
    (apply orig-fun args)))

(advice-add 'org-agenda-to-appt :around #'tl//quiet-no-event-message)
;; (advice-remove 'org-agenda-to-appt #'tl//quiet-no-event-message)


(defun tl/org-agenda-to-appt ()
  (setq appt-time-msg-list nil)
  ;; Dangerous!!! do not use `appt-add', this might remove entries added by `appt-add' manually.
  (org-agenda-to-appt t "TODO"))
;; update appt
(run-at-time "24:01" (* 0.5 60 60) 'tl/org-agenda-to-appt) ;; update every half an hour

;; update appt each time agenda opened
(add-hook 'org-agenda-finalize-hook 'tl/org-agenda-to-appt)

;;; jump to agenda buffer when idle
;; get the idea from http://www.dbrunner.de/it/org-mode.html
;; I give a little update. execute recursive edit before pop a new window
;; see also `org-agenda-restore-windows-after-quit'
(defun tl/jump-to-org-agenda (&optional arg)
  "Jump to or refresh 'org-agenda buffer with respected the frame.
When called by interactive, ARG default to nil then jump to 'org-agenda',
otherwise just refresh the org agenda buffer."
  (interactive "p")
  (let* ((bn (if (boundp 'org-agenda-buffer-name)
                 org-agenda-buffer-name
               "*Org Agenda*"))
         (buf (and bn (get-buffer bn)))
         (wind (and buf (get-buffer-window buf 'visible))))
    (unless (window-minibuffer-p (selected-window))
      (if (and wind (not arg))
          (with-selected-window wind
            (org-agenda-redo)
            (org-fit-window-to-buffer))
        (org-agenda nil " "))
      (goto-char (point-min)))))

(with-eval-after-load 'evil
  (define-key evil-normal-state-map (kbd "S-SPC") 'tl/jump-to-org-agenda))

;; every 20 minutes
(defvar tl//jump-to-org-agenda-timer nil)

(defun tl/idle-jump-to-org-agenda (secs)
  (when tl//jump-to-org-agenda-timer
    (cancel-timer tl//jump-to-org-agenda-timer)
    (setq tl//jump-to-org-agenda-timer nil))
  (setq tl//jump-to-org-agenda-timer
        (run-with-idle-timer secs t 'tl/jump-to-org-agenda)))

;; every idle 20 minutes jump to org-agenda
(tl/idle-jump-to-org-agenda (* 20 60))

(defhydra tl/org-agenda (:color teal)
  "
Headline^^            Visit entry^^               Filter^^                    Date^^               Toggle mode^^        View^^             Clock^^        Other^^
--------^^---------   -----------^^------------   ------^^-----------------   ----^^-------------  -----------^^------  ----^^---------    -----^^------  -----^^-----------
[_ht_] set status     [_SPC_] in other window     [_ft_] by tag               [_ds_] schedule      [_tf_] follow        [_vd_] day         [_cI_] in      [_gr_] reload
[_hk_] kill           [_TAB_] & go to location    [_fr_] refine by tag        [_dd_] set deadline  [_tl_] log           [_vw_] week        [_cO_] out     [_._]  go to today
[_hR_] refile         [_RET_] & del other windows [_fc_] by category          [_dt_] timestamp     [_ta_] archive       [_vt_] fortnight   [_cq_] cancel  [_gd_] go to date
[_hA_] archive        [_o_]   link                [_fh_] by top headline      [_+_]  do later      [_tr_] clock report  [_vm_] month       [_cj_] jump    ^^
[_h:_] set tags       ^^                          [_fx_] by regexp            [_-_]  do earlier    [_td_] diaries       [_vy_] year        ^^             ^^
[_hp_] set priority   ^^                          [_fd_] delete all filters   ^^                   ^^                   [_vn_] next span   ^^             ^^
^^                    ^^                          ^^                          ^^                   ^^                   [_vp_] prev span   ^^             ^^
^^                    ^^                          ^^                          ^^                   ^^                   [_vr_] reset       ^^             ^^
[_q_] quit
"
  ;; Entry
  ("h:" org-agenda-set-tags)
  ("hA" org-agenda-archive-default)
  ("hk" org-agenda-kill)
  ("hp" org-agenda-priority)
  ("hR" org-agenda-refile)
  ("ht" org-agenda-todo)

  ;; Visit entry
  ("SPC" org-agenda-show-and-scroll-up)
  ("<tab>" org-agenda-goto :exit t)
  ("TAB" org-agenda-goto :exit t)
  ("RET" org-agenda-switch-to :exit t)
  ("o"   link-hint-open-link :exit t)

  ;; Date
  ("ds" org-agenda-schedule)
  ("dd" org-agenda-deadline)
  ("dt" org-agenda-date-prompt)
  ("+" org-agenda-do-date-later)
  ("-" org-agenda-do-date-earlier)

  ;; View
  ("vd" org-agenda-day-view)
  ("vw" org-agenda-week-view)
  ("vt" org-agenda-fortnight-view)
  ("vm" org-agenda-month-view)
  ("vy" org-agenda-year-view)
  ("vn" org-agenda-later)
  ("vp" org-agenda-earlier)
  ("vr" org-agenda-reset-view)

  ;; Toggle mode
  ("tf" org-agenda-follow-mode)
  ("tl" org-agenda-log-mode)
  ("ta" org-agenda-archives-mode)
  ("tr" org-agenda-clockreport-mode)
  ("td" org-agenda-toggle-diary)

  ;; Filter
  ("ft" org-agenda-filter-by-tag)
  ("fr" org-agenda-filter-by-tag-refine)
  ("fc" org-agenda-filter-by-category)
  ("fh" org-agenda-filter-by-top-headline)
  ("fx" org-agenda-filter-by-regexp)
  ("fd" org-agenda-filter-remove-all)

  ;; Clock
  ("cI" org-agenda-clock-in :exit t)
  ("cj" org-agenda-clock-goto :exit t)
  ("cO" org-agenda-clock-out)
  ("cq" org-agenda-clock-cancel)

  ;; Other
  ("<escape>" nil :exit t)
  ("q" nil :exit t)
  ("gr" org-agenda-redo)
  ("." org-agenda-goto-today)
  ("gd" org-agenda-goto-date))


;; time grid color block when `org-agenda-log-mode' on
;; https://zhuanlan.zhihu.com/p/143258279
;; https://emacs-china.org/t/org-agenda/8679/29
(defun tl/org-agenda-time-grid-spacing ()
  "Set different line spacing w.r.t. time duration."
  (save-excursion
    (let* ((background (alist-get 'background-mode (frame-parameters)))
           (background-dark-p (string= background "dark"))
           (colors (list "#1ABC9C" "#2ECC71" "#3498DB" "#9966ff"))
           pos
           duration)
      (nconc colors colors)
      (goto-char (point-min))
      (while (setq pos (next-single-property-change (point) 'duration))
        (goto-char pos)
        (when (and (not (equal pos (point-at-eol)))
                   (setq duration (org-get-at-bol 'duration)))
          (let ((line-height (if (< duration 30) 1.0 (+ 0.5 (/ duration 60))))
                (ov (make-overlay (point-at-bol) (1+ (point-at-eol)))))
            (overlay-put ov 'face `(:background ,(car colors)
                                    :foreground
                                    ,(if background-dark-p "black" "white")))
            (setq colors (cdr colors))
            (overlay-put ov 'line-height line-height)
            (overlay-put ov 'line-spacing (1- line-height))))))))

(add-hook 'org-agenda-finalize-hook #'tl/org-agenda-time-grid-spacing)


(provide '13org-agenda)
