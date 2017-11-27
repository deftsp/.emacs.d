;;; 13org-mode.el ---

;; Copyright (C) 2007  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>
;; Keywords:

;;; Install
;; (add-to-list 'load-path "~/.emacs.d/site-lisp/org-mode/lisp")
;; (add-to-list 'load-path "~/.emacs.d/site-lisp/org-mode/contrib/lisp" t)
;; $ git pull
;; $ make autoloads EMACS=/Applications/Emacs.app/Contents/MacOS/Emacs
;; $ make EMACS=/Applications/Emacs.app/Contents/MacOS/Emacs
;; $ make doc EMACS=/Applications/Emacs.app/Contents/MacOS/Emacs

;; (require 'org-loaddefs)

;;; org-opaml
;; https://github.com/edavis/org-opml
;; C-c C-e m

(use-package org
  :defer t
  :init
  (progn
    (global-set-key (kbd "C-c l") 'org-store-link)
    (global-set-key (kbd "C-c a") 'org-agenda)
    (global-set-key (kbd "C-c L") 'org-insert-link-global)
    ;; Follow a link or time-stamp like Org mode does, in any mode.
    ;; if no appropriate application, it will use mailcap's config to set `org-file-apps'
    ;; (global-set-key "\C-c o" 'org-open-at-point-global)

    (setq org-directory "~/org"
          org-special-ctrl-a/e t
          org-cycle-separator-lines 2
          org-cycle-include-plain-lists t
          org-hide-leading-stars t
          org-log-done 'time
          org-log-reschedule 'time
          org-log-redeadline 'time
          org-log-refile 'time
          org-log-into-drawer "LOGBOOK"
          org-startup-with-inline-images t
          org-image-actual-width nil
          org-ctrl-k-protect-subtree t ; give a query for delete
          ;; add more org level face
          ;; org-n-level-faces at most 8, if less than 8, then level-1 face gets
          ;; re-used level N+1 etc.
          ;; (add-to-list 'org-level-faces 'paloryemacs/org-level-9)
          ;; (setq org-n-level-faces (length org-level-faces)))
          ;; org-catch-invisible-edits 'smart
          ;; org-startup-indented t
          ;; Default target for storing notes. Used as a fall back file for org-capture.el, for templates that do not
          ;; specify a target file.
          org-fontify-emphasized-text t
          org-fontify-done-headline t
          org-default-notes-file (concat org-directory "/notes.org")
          org-fast-tag-selection-single-key (quote expert)
          org-reverse-note-order t
          org-deadline-warning-days 7
          org-return-follows-link t
          org-startup-folded t
          org-startup-truncated t
          org-blank-before-new-entry '((heading . t) (plain-list-item . auto))
          org-time-stamp-rounding-minutes (quote (0 5))
          org-pretty-entities nil ; use pretty things for the clocktable
          org-enforce-todo-dependencies t
          org-enforce-todo-checkbox-dependencies t
          org-display-internal-link-with-indirect-buffer nil)
    (when window-system
      ;; … ↴, ⬎, ⤷, ⤵, ▼ and ⋱.
      (setq org-ellipsis " ◦◦◦ "))
    ;; global Effort estimate values
    (setq org-global-properties
          '(("Effort_ALL" .
             "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 0:00")))
    ;;        1    2    3    4    5    6    7    8    9    0
    ;; These are the hotkeys ^^

    ;; Set default column view headings: Task Priority Effort Clock_Summary
    (setq org-columns-default-format "%50ITEM(Task) %2PRIORITY %10Effort(Effort){:} %10CLOCKSUM")
    (setq org-use-fast-todo-selection t
          ;; `!' for a timestamp, `@' for a note with timestamp
          ;; `/!' means that in addition to the note taken when entering the state,
          ;; a timestamp should be recorded when leaving the WAIT state, if and only if the
          ;; target state does not configure logging for entering it.
          org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "DELEGATED(l)" "APPT(a)" "|" "DONE(d)" "DEFERRED(f)" "CANCELLED(c@)")
                              (sequence "WAITING(w@/!)" "HOLD(h@/!)" "SOMEDAY(S!)" "PROJECT(P@)" "OPEN(O@)" "|" "CANCELLED(c@/!)")
                              (sequence "REPORT(r)" "BUG(b)" "KNOWNCAUSE(k)" "|" "FIXED(f)")
                              (sequence "QUOTE(q!)" "QUOTED(Q!)" "|" "APPROVED(A@)" "EXPIRED(E@)" "REJECTED(R@)"))
          org-todo-keyword-faces (quote (("TODO"      . (:foreground "red"          :weight bold))
                                         ("NEXT"      . (:foreground "#d33682"         :weight bold))
                                         ("WAITING"   . (:foreground "orange"       :weight bold))
                                         ("SOMEDAY"   . (:foreground "magenta"      :weight bold))
                                         ("DONE"      . (:foreground "forest green" :weight bold :strike-through t))
                                         ("CANCELLED" . (:foreground "forest green" :weight bold :strike-through t))
                                         ("DEFERRED"  . (:foreground "forest green" :weight bold :strike-through t))
                                         ("FIXED"     . (:foreground "forest green" :weight bold :strike-through t))
                                         ("APPROVED"  . (:foreground "forest green" :weight bold :strike-through t))
                                         ("EXPIRED"   . (:foreground "forest green" :weight bold :strike-through t))
                                         ("REJECTED"  . (:foreground "forest green" :weight bold :strike-through t))
                                         ("QUOTE"     . (:foreground "red"          :weight bold))
                                         ("QUOTED"    . (:foreground "magenta"      :weight bold))
                                         ("APPROVED"  . (:foreground "forest green" :weight bold))
                                         ("EXPIRED"   . (:foreground "forest green" :weight bold))
                                         ("REJECTED"  . (:foreground "forest green" :weight bold))
                                         ("OPEN"      . (:foreground "deep pink"    :weight bold))
                                         ("PROJECT"   . (:foreground "red"          :weight bold))))
          org-tag-persistent-alist '((:startgroup . nil) ("@office" . ?o) ("@home" . ?h) ("@shopping" . ?s) ("@tennisclub" . ?t) (:endgroup . nil)
                                     (:startgroup . nil) ("online" . ?O) ("offline" . ?F) (:endgroup . nil)
                                     (:startgroup . nil) ("business" . ?B) ("personal" . ?P) (:endgroup . nil)
                                     ("drill"   . ?d)
                                     ("hacking" . ?H)
                                     ("exercise". ?E)
                                     ("mail"    . ?M)
                                     ("movie"   . nil)
                                     ("misc"    . ?m)
                                     ("reading" . ?r))
          org-tag-alist '(("project" . ?p))))
  :config
  (progn
    (require '50calendar)
    (when (eq system-type 'darwin)
      (add-to-list 'org-modules 'org-mac-link))
    (add-to-list 'org-modules 'org-habit)
    (add-to-list 'org-modules 'org-expiry)
    (add-to-list 'org-modules 'org-mouse)
    (add-to-list 'org-modules 'org-annotate-file)
    (add-to-list 'org-modules 'org-info)
    (add-to-list 'org-modules 'org-man)
    (add-to-list 'org-modules 'org-eval)
    (add-to-list 'org-modules 'org-panel)
    (add-to-list 'org-modules 'org-expiry)
    (add-to-list 'org-modules 'org-toc)
    (add-to-list 'org-modules 'org-drill)
    (add-to-list 'org-modules 'org-depend)
    ;; https://github.com/Somelauw/evil-org-mode/blob/master/doc/keythemes.org
    (use-package evil-org
      :init
      (progn
        (add-hook 'org-mode-hook 'paloryemacs//evil-org-mode))
      :config
      (progn
        (evil-org-set-key-theme '(navigation textobjects todo additional))))

    (defun paloryemacs/org-mode-init ()
      (display-line-numbers-mode -1))

    (add-to-list 'org-mode-hook 'paloryemacs/org-mode-init)

    (org-defkey org-mode-map (kbd "C-c C-x t") 'paloryemacs/org-clock-summary-today-by-tags)
    ;; Undefine C-c [ and C-c ] since this breaks my
    ;; org-agenda files when directories are include It
    ;; expands the files in the directories individually
    (org-defkey org-mode-map (kbd "C-c [") 'undefined)
    (org-defkey org-mode-map (kbd "C-c ]") 'undefined)

    (add-to-list 'org-structure-template-alist
                 '("p" ":PROPERTIES:\n?\n:END:"))
    (add-to-list 'org-structure-template-alist
                 `("eh" ,(concat ":EXPORT_FILE_NAME: ?\n"
                                 ":EXPORT_TITLE:\n"
                                 ":EXPORT_OPTIONS: toc:nil html-postamble:nil num:nil")))


    (defmacro paloryemacs|org-emphasize (fname char)
      "Make function for setting the emphasis in org mode"
      `(defun ,fname () (interactive)
              (org-emphasize ,char)))

    (dolist (prefix '(("mC" . "clocks")
                      ("md" . "dates")
                      ("me" . "export")
                      ("mh" . "headings")
                      ("mi" . "insert")
                      ("miD" . "download")
                      ("ms" . "trees/subtrees")
                      ("mT" . "toggles")
                      ("mt" . "tables")
                      ("mtd" . "delete")
                      ("mti" . "insert")
                      ("mtt" . "toggle")
                      ("mx" . "text")))
      (paloryemacs/declare-prefix-for-mode 'org-mode (car prefix) (cdr prefix)))
    (paloryemacs/set-leader-keys-for-major-mode 'org-mode
      "C-l" 'paloryemacs/reflash-indentation

      "'" 'org-edit-special
      "c" 'org-capture
      "Cc" 'org-clock-cancel
      "Ci" 'org-clock-in
      "Co" 'org-clock-out
      "Cr" 'org-resolve-clocks
      "Cd" 'org-clock-display ; C-c C-x C-d

      "dd" 'org-deadline
      "ds" 'org-schedule
      "dt" 'org-time-stamp
      "dT" 'org-time-stamp-inactive
      "ee" 'org-export-dispatch

      "a" 'org-agenda

      "Te" 'org-toggle-pretty-entities
      "Ti" 'org-toggle-inline-images
      "Tl" 'org-toggle-link-display
      "Tt" 'org-show-todo-tree
      "TT" 'org-todo
      ;; "TV" 'space-doc-mode
      "Tx" 'org-toggle-latex-fragment

      ;; More cycling options (timestamps, headlines, items, properties)
      "L" 'org-shiftright
      "H" 'org-shiftleft
      "J" 'org-shiftdown
      "K" 'org-shiftup

      ;; Change between TODO sets
      "C-S-l" 'org-shiftcontrolright
      "C-S-h" 'org-shiftcontrolleft
      "C-S-j" 'org-shiftcontroldown
      "C-S-k" 'org-shiftcontrolup

      ;; Subtree editing
      "sa" 'org-archive-subtree
      "sb" 'org-tree-to-indirect-buffer
      "sh" 'org-promote-subtree
      "sj" 'org-move-subtree-down
      "sk" 'org-move-subtree-up
      "sl" 'org-demote-subtree
      "sn" 'org-narrow-to-subtree
      "sN" 'widen
      "sr" 'org-refile
      "ss" 'org-sparse-tree
      "sS" 'org-sort

      ;; tables
      "ta" 'org-table-align
      "tb" 'org-table-blank-field
      "tc" 'org-table-convert
      "tdc" 'org-table-delete-column
      "tdr" 'org-table-kill-row
      "te" 'org-table-eval-formula
      "tE" 'org-table-export
      "th" 'org-table-previous-field
      "tH" 'org-table-move-column-left
      "tic" 'org-table-insert-column
      "tih" 'org-table-insert-hline
      "tiH" 'org-table-hline-and-move
      "tir" 'org-table-insert-row
      "tI" 'org-table-import
      "tj" 'org-table-next-row
      "tJ" 'org-table-move-row-down
      "tK" 'org-table-move-row-up
      "tl" 'org-table-next-field
      "tL" 'org-table-move-column-right
      "tn" 'org-table-create
      "tN" 'org-table-create-with-table.el
      "tr" 'org-table-recalculate
      "ts" 'org-table-sort-lines
      "ttf" 'org-table-toggle-formula-debugger
      "tto" 'org-table-toggle-coordinate-overlays
      "tw" 'org-table-wrap-region


      ;; Multi-purpose keys
      (or dotpaloryemacs-major-mode-leader-key ",") 'org-ctrl-c-ctrl-c
      "*" 'org-ctrl-c-star
      "-" 'org-ctrl-c-minus
      "#" 'org-update-statistics-cookies
      ;; https://github.com/Somelauw/evil-org-mode/issues/21
      "RET" 'org-ctrl-c-ret
      ;; https://github.com/syl20bnr/spacemacs/issues/9603
      ;; M-RET is a core Spacemacs key binding and also a limitation of it, in terminal C-m is RET
      "M-RET" 'org-meta-return
      ;; attachments
      "A" 'org-attach
      ;; insertion
      "id" 'org-insert-drawer
      "ie" 'org-set-effort
      "if" 'org-footnote-new
      "ih" 'org-insert-heading
      "iH" 'org-insert-heading-after-current
      "iK" 'paloryemacs/insert-keybinding-org
      "il" 'org-insert-link
      "ip" 'org-set-property
      "is" 'org-insert-subheading
      "it" 'org-set-tags

      ;; region manipulation
      "xb" (paloryemacs|org-emphasize paloryemacs/org-bold ?*)
      "xc" (paloryemacs|org-emphasize paloryemacs/org-code ?~)
      "xi" (paloryemacs|org-emphasize paloryemacs/org-italic ?/)
      "xo" 'org-open-at-point
      "xr" (paloryemacs|org-emphasize paloryemacs/org-clear ? )
      "xs" (paloryemacs|org-emphasize paloryemacs/org-strike-through ?+)
      "xu" (paloryemacs|org-emphasize paloryemacs/org-underline ?_)
      "xv" (paloryemacs|org-emphasize paloryemacs/org-verbose ?=)

      "px" 'org-publish
      "pp" 'org-publish-current-project
      "pf" 'org-publish-current-file
      "pa" 'org-publish-all)

    ;; Add global evil-leader mappings. Used to access org-agenda
    ;; functionalities – and a few others commands – from any other mode.
    (paloryemacs/declare-prefix "ao" "org")
    (paloryemacs/declare-prefix "aok" "clock")
    (paloryemacs/set-leader-keys
      ;; org-agenda
      "ao#" 'org-agenda-list-stuck-projects
      "ao/" 'org-occur-in-agenda-files
      "aoa" 'org-agenda-list
      "aoc" 'org-capture
      "aoe" 'org-store-agenda-views
      "aoki" 'org-clock-in-last
      "aokj" 'org-clock-jump-to-current-clock
      "aoko" 'org-clock-out
      "aol" 'org-store-link
      "aom" 'org-tags-view
      "aoo" 'org-agenda
      "aos" 'org-search-view
      "aot" 'org-todo-list
      ;; SPC C- capture/colors
      "Cc" 'org-capture)

    ;; We add this key mapping because an Emacs user can change
    ;; `dotpaloryemacs-major-mode-emacs-leader-key' to `C-c' and the key binding
    ;; C-c ' is shadowed by `spacemacs/default-pop-shell', effectively making
    ;; the Emacs user unable to exit src block editing.
    (define-key org-src-mode-map
      (kbd (concat dotpaloryemacs-major-mode-emacs-leader-key " '"))
      'org-edit-src-exit)

    ;; Evilify the calendar tool on C-c .
    (define-key org-read-date-minibuffer-local-map (kbd "M-h")
      (lambda () (interactive)
        (org-eval-in-calendar '(calendar-backward-day 1))))
    (define-key org-read-date-minibuffer-local-map (kbd "M-l")
      (lambda () (interactive)
        (org-eval-in-calendar '(calendar-forward-day 1))))
    (define-key org-read-date-minibuffer-local-map (kbd "M-k")
      (lambda () (interactive)
        (org-eval-in-calendar '(calendar-backward-week 1))))
    (define-key org-read-date-minibuffer-local-map (kbd "M-j")
      (lambda () (interactive)
        (org-eval-in-calendar '(calendar-forward-week 1))))
    (define-key org-read-date-minibuffer-local-map (kbd "M-H")
      (lambda () (interactive)
        (org-eval-in-calendar '(calendar-backward-month 1))))
    (define-key org-read-date-minibuffer-local-map (kbd "M-L")
      (lambda () (interactive)
        (org-eval-in-calendar '(calendar-forward-month 1))))
    (define-key org-read-date-minibuffer-local-map (kbd "M-K")
      (lambda () (interactive)
        (org-eval-in-calendar '(calendar-backward-year 1))))
    (define-key org-read-date-minibuffer-local-map (kbd "M-J")
      (lambda () (interactive)
        (org-eval-in-calendar '(calendar-forward-year 1))))))


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
(defun paloryemacs/org-agenda-skip-subtree-if-priority (priority)
  "Skip an agenda subtree if it has a priority of PRIORITY.
PRIORITY may be one of the characters ?A, ?B, or ?C."
  (let ((subtree-end (save-excursion (org-end-of-subtree t)))
        (pri-value (* 1000 (- org-lowest-priority priority)))
        (pri-current (org-get-priority (thing-at-point 'line t))))
    (if (= pri-value pri-current)
        subtree-end
      nil)))


(defun paloryemacs/org-agenda-skip-subtree-if-not-priority (priority)
  "Skip an agenda subtree if it don't have a priority of PRIORITY.
PRIORITY may be one of the characters ?A, ?B, or ?C."
  (let ((subtree-end (save-excursion (org-end-of-subtree t)))
        (pri-value (* 1000 (- org-lowest-priority priority)))
        (pri-current (org-get-priority (thing-at-point 'line t))))
    (if (= pri-value pri-current)
        nil
      subtree-end)))

(defun paloryemacs/org-agenda-skip-subtree-if-habit ()
  "Skip an agenda entry if it has a STYLE property equal to \"habit\"."
  (let ((subtree-end (save-excursion (org-end-of-subtree t))))
    (if (string= (org-entry-get nil "STYLE") "habit")
        subtree-end
      nil)))

(defun paloryemacs/org-agenda-skip-subtree-if-not-habit ()
  "Skip an agenda entry if it has a STYLE property equal to \"habit\"."
  (let ((subtree-end (save-excursion (org-end-of-subtree t))))
    (if (string= (org-entry-get nil "STYLE") "habit")
        nil
      subtree-end)))


;; https://github.com/jwiegley/dot-emacs/blob/master/dot-org.el
(defun paloryemacs/org-todo-age-time (&optional pos)
  (let ((stamp (org-entry-get (or pos (point)) "CREATED" t)))
    (when stamp
      (time-subtract (current-time)
                     (org-time-string-to-time
                      stamp)))))

(defun paloryemacs/org-todo-age (&optional pos)
  (let* ((age-time (paloryemacs/org-todo-age-time pos))
         (days (if age-time (time-to-number-of-days age-time) nil)))
    (cond ((null days) "Unk")
          ((< days 1)   "today")
          ((< days 7)   (format "%dd" days))
          ((< days 30)  (format "%.1fw" (/ days 7.0)))
          ((< days 358) (format "%.1fM" (/ days 30.0)))
          (t            (format "%.1fY" (/ days 365.0))))))


(defun paloryemacs/org-compare-todo-age (a b)
  (let ((time-a (paloryemacs/org-todo-age-time (get-text-property 0 'org-hd-marker a)))
        (time-b (paloryemacs/org-todo-age-time (get-text-property 0 'org-hd-marker b))))
    (cond ((null time-a) -1)
          ((null time-b) 1)
          ((and (null time-a) (null time-b)) 1)
          (t (if (time-less-p time-a time-b)
                 -1
               (if (equal time-a time-b) 0 1))))))

(defun paloryemacs/set-org-agenda-custom-commands ()
  ;; Use tags-todo like this (tags-todo "+PRIORITY={A}") will be very slow,
  ;; however add more restricts, like CATEGORY, it works fine

  ;; http://orgmode.org/manual/Matching-tags-and-properties.html
  ;; https://blog.aaronbieber.com/2016/09/24/an-agenda-for-life-with-org-mode.html
  (setq org-agenda-custom-commands
        `(("A" "Today's Priority #A Tasks" agenda ""
           ((org-agenda-entry-types '(:scheduled))
            (org-agenda-span 'day)
            (org-agenda-skip-function '(org-agenda-skip-entry-if 'notregexp "\\=.*\\[#A\\]"))
            (org-agenda-overriding-header "Today's Priority #A Tasks: ")))

          ("b" "Today's Priority #A and #B tasks" agenda ""
           ((org-agenda-entry-types '(:scheduled))
            (org-agenda-span 'day)
            (org-agenda-overriding-header "Today's priority #A and #B Tasks: ")
            (org-agenda-skip-function '(org-agenda-skip-entry-if 'regexp "\\=.*\\[#C\\]"))))

          ("c" "Appointment Calendar" agenda ""
           ((org-agenda-overriding-header "Appointment Calendar")
            (org-agenda-sorting-strategy '(time-up))
            (org-agenda-span 14)
            (org-agenda-regexp-filter-preset '("+APPT"))))

          ;; ("c" todo "DONE|DEFERRED|CANCELLED") ; not include scheduled TODO entiries

          ;; ("d" todo "DELEGATED")
          ("D" "Done Tasks" tags "TODO=\"DONE\""
           ((org-agenda-overriding-header "Done Tasks")))

          ("d" "Daily agenda and all TODOs"
           ((agenda "" ((org-agenda-span 'day)
                        (org-agenda-sorting-strategy '(habit-down
                                                       time-up
                                                       priority-down
                                                       todo-state-down
                                                       effort-up
                                                       category-keep))))
            (tags "+PRIORITY=\"A\"+CATEGORY={Task\\|Project}"
                  (
                   (org-agenda-skip-function
                    '(org-agenda-skip-entry-if 'todo 'done))

                   (org-agenda-sorting-strategy '(tag-up priority-down))
                   ;; (org-agenda-todo-keyword-format "")
                   (org-agenda-overriding-header "High Priority Unfinished Tasks:")))

            (alltodo ""
                     ((org-agenda-skip-function '(or (paloryemacs/org-agenda-skip-subtree-if-habit)
                                                     (paloryemacs/org-agenda-skip-subtree-if-priority ?A)
                                                     (org-agenda-skip-entry-if 'scheduled 'deadline)))
                      (org-agenda-overriding-header "ALL Normal Priority Tasks:"))))
           ;; ((org-agenda-compact-blocks t))
           ((org-agenda-block-separator ?▰)))

          ;; ("d" "Deadlined tasks" tags "TODO<>\"\"&TODO<>{DONE\\|CANCELED\\|NOTE\\|PROJECT}"
          ;;  ((org-agenda-overriding-header "Deadlined tasks: ")
          ;;   (org-agenda-skip-function '(org-agenda-skip-entry-if 'notdeadline))
          ;;   (org-agenda-sorting-strategy '(category-up))))

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
                                           (paloryemacs/org-agenda-skip-subtree-if-not-habit)))
            (org-agenda-sorting-strategy
             '(todo-state-down effort-up category-keep))))
          ("O" "All TODOs" tags "TODO<>\"\"" ((org-agenda-overriding-header "All TODOs")))
          ;; ("p" tags "+project-TODO=\"DONE\"-TODO=\"CANCELLED\"")

          ("r" "Uncategorized items" tags "CATEGORY=\"Inbox\"&LEVEL=2"
           ((org-agenda-overriding-header "Uncategorized items")))
          ("S" "Scheduled tasks" tags "TODO<>\"\"&TODO<>{APPT\\|DONE\\|CANCELED\\|NOTE}"
           ((org-agenda-overriding-header "Scheduled tasks: ")
            (org-agenda-skip-function '(or (paloryemacs/org-agenda-skip-subtree-if-habit)
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
            (org-agenda-prefix-format "%-11c%5(paloryemacs/org-todo-age) ")))

          ;; ("u" alltodo "Unscheduled TODO entries"
          ;;  ((org-agenda-skip-function
          ;;    '(org-agenda-skip-entry-if 'scheduled 'deadline 'regexp "<[^>\n]+>"))
          ;;   (org-agenda-overriding-header "Unscheduled TODO entries: ")))

          ("U" "Deferred tasks" tags "TODO=\"DEFERRED\""
           ((org-agenda-overriding-header "Deferred tasks:")
            (org-agenda-sorting-strategy '(user-defined-up))
            (org-agenda-prefix-format "%-11c%5(paloryemacs/org-todo-age) ")))


          ;; ("w" todo "WAITING" )
          ;; ("W" todo-tree "WAITING")
          ("w" "Unscheduled work-related tasks" tags "TODO<>\"\"&TODO<>{DONE\\|CANCELED\\|NOTE}"
           ((org-agenda-overriding-header "Unscheduled work-related tasks")
            (org-agenda-sorting-strategy '(todo-state-up priority-down category-up))
            (org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled 'deadline 'timestamp))
            (org-agenda-prefix-format "%-11c%5(paloryemacs/org-todo-age) ")))

          ("W" "Waiting/delegated tasks" tags "TODO=\"WAITING\"|TODO=\"DELEGATED\""
           ((org-agenda-overriding-header "Waiting/delegated tasks:")
            (org-agenda-sorting-strategy '(todo-state-up priority-down category-up))))

          ("Y" "Someday tasks" tags "TODO=\"SOMEDAY\""
           ((org-agenda-overriding-header "Someday tasks:")
            (org-agenda-sorting-strategy '(user-defined-up))
            (org-agenda-prefix-format "%-11c%5(paloryemacs/org-todo-age) "))))))


;; https://www.reddit.com/r/orgmode/comments/6ybjjw/aligned_agenda_view_anyway_to_make_this_more/
(use-package org-agenda
  :defer t
  :init
  (progn
    (setq org-agenda-files (list (concat org-directory "/GTD.org")
                                 (concat org-directory "/from-mobile.org"))  ; (directory-files org-directory t ".*\\.org$")
          ;; speedup and optimization
          ;; https://punchagan.muse-amuse.in/posts/how-i-learnt-to-use-emacs-profiler.html
          ;; http://orgmode.org/worg/agenda-optimization.html
          org-agenda-inhibit-startup t
          ;; org-agenda-use-tag-inheritance nil ; set this on a per-command in org-agenda-custom-commands
          org-agenda-show-all-dates t
          org-agenda-span 'day ; 'week
          org-agenda-start-with-log-mode nil
          org-agenda-start-with-clockreport-mode nil
          org-agenda-view-columns-initially nil
          ;; exclude scheduled items from the global TODO list.
          org-agenda-todo-ignore-scheduled 'all
          org-agenda-todo-ignore-with-date t
          org-agenda-skip-scheduled-if-done t
          org-agenda-skip-deadline-if-done t
          org-agenda-skip-timestamp-if-done t
          org-agenda-todo-list-sublevels t
          ;; I use the diary only for sexp entries and holidays, and Ι have put them into org file.
          org-agenda-include-diary nil
          ;; agenda view always starts out by showing me the next seven days.
          org-agenda-start-on-weekday nil
          org-adapt-indentation t
          org-agenda-restore-windows-after-quit t
          org-agenda-repeating-timestamp-show-all nil
          org-agenda-use-time-grid nil
          org-agenda-block-separator ?▰
          org-agenda-show-future-repeats 'next
          org-agenda-prefer-last-repeat nil)

    (setq org-agenda-time-grid '((daily today) ; remove-match
                                 (700 800 1000 1200 1400 1600 1800 2000)
                                 "......"
                                 "----------------"))

    ;; (setq org-agenda-deadline-leaders '("!D!: " "D%2d: " "")) ; default ("Deadline:  " "In %3d d.: " "%2d d. ago: ")
    ;; (setq org-agenda-scheduled-leaders '("Scheduled: " "Sched.%2dx: ")) ; default '("Scheduled: " "Sched.%2dx: ")
    ;; (org-agenda-todo-keyword-format "")
    (setq org-agenda-prefix-format
          '(
            ;; (agenda . " %i %-12:c%-12t% s") ; agenda
            (agenda  . "  %-12:c%?-12t% s")
			;; (timeline . "%-9:T%?-2t% s")   ; timeline
            (todo . " %i %-12:c")           ; todo, alltodo
            (tags . " %i %-12:c")           ; tags, tags-todo, stuck
            (search . " %i %-12:c")))       ; search

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
    (paloryemacs/set-org-agenda-custom-commands))
  :config
  (progn
    (defun paloryemacs/org-agenda-mode-init ()
      (setq truncate-lines t)
      (define-key org-agenda-mode-map " " 'org-agenda-cycle-show))

    (add-hook 'org-agenda-mode-hook 'paloryemacs/org-agenda-mode-init)

    ;; Agenda clock report
    ;;  v R to toggle it. In evilify state, use C-v R
    (setq org-agenda-clockreport-parameter-plist
          '(:link t :maxlevel 6 :fileskip0 t :compact t :narrow 60 :score 0))
    (setq org-agenda-clock-consistency-checks
          '(:max-duration "4:00" :min-duration 0 :max-gap 0 :gap-ok-around ("4:00")))

    ;; Place tags close to the right-hand side of the window
    (add-hook 'org-agenda-finalize-hook 'paloryemacs/org-agenda-adjust-tags-column)
    (defun paloryemacs/org-agenda-adjust-tags-column ()
      "Put the agenda tags by the right border of the agenda window."
      (setq org-agenda-tags-column (- 10 (window-text-width)))
      (org-agenda-align-tags))

    ;; https://blog.aaronbieber.com/2016/09/25/agenda-interactions-primer.html

    ;; Pressing c by itself to open my default capture command, which is a TODO
    ;; entry (the shortcut key is “a” and that is passed to org-capture). If I
    ;; use a prefix (by pressing C-u c), it will open the default (“vanilla”)
    ;; Org Mode capture dialog, prompting me to pick a capture type, where I can
    ;; choose my “note” type or others I have developed.
    (defun paloryemacs/org-agenda-capture (&optional vanilla)
      "Capture a task in agenda mode, using the date at point.

If VANILLA is non-nil, run the standard `org-capture'."
      (interactive "P")
      (if vanilla
          (org-capture)
        (let ((org-overriding-default-time (org-get-cursor-date)))
          (org-capture nil "a"))))

    ;; "c" default bind to 'org-agenda-goto-calendar
    (define-key org-agenda-mode-map "c" 'paloryemacs/org-agenda-capture)


    ;; evilify agenda mode
    (org-defkey org-agenda-mode-map "|" nil) ;'org-agenda-filter-remove-all
    (org-defkey org-agenda-mode-map "\\" nil) ;'org-agenda-query-not-cmd
    (org-defkey org-agenda-mode-map (kbd "C-n") nil)
    (org-defkey org-agenda-mode-map (kbd "G") nil) ;'org-agenda-toggle-time-grid

    ;; https://blog.aaronbieber.com/2016/09/25/agenda-interactions-primer.html
    (defun paloryemacs/org-agenda-next-header ()
      "Jump to the next header in an agenda series."
      (interactive)
      (paloryemacs/-org-agenda-goto-header))

    (defun paloryemacs/org-agenda-previous-header ()
      "Jump to the previous header in an agenda series."
      (interactive)
      (paloryemacs/-org-agenda-goto-header t))

    (defun paloryemacs/-org-agenda-goto-header (&optional backwards)
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

    (with-eval-after-load "evil-evilified-state"
      (evilified-state-evilify-map org-agenda-mode-map
        :mode org-agenda-mode
        :bindings
        (kbd "C-h") nil
        "j" 'org-agenda-next-line
        "k" 'org-agenda-previous-line

        "J" 'paloryemacs/org-agenda-next-header  ; org-agenda-goto-date
        "K" 'paloruemacs/org-agenda-previous-header ; org-agenda-capture

        (kbd "M-j") 'org-agenda-next-item
        (kbd "M-k") 'org-agenda-previous-item
        (kbd "M-h") 'org-agenda-earlier
        (kbd "M-l") 'org-agenda-later
        (kbd "gd")  'org-agenda-toggle-time-grid
        (kbd "gr")  'org-agenda-redo
        (kbd "M-RET") 'org-agenda-show-and-scroll-up
        (kbd "M-SPC") 'paloryemacs/org-agenda/body))

    (dolist (prefix '(("mC" . "clocks")
                      ("md" . "dates")
                      ("mi" . "insert")
                      ("ms" . "trees/subtrees")))
      (paloryemacs/declare-prefix-for-mode 'org-agenda-mode
                                           (car prefix) (cdr prefix)))

    (paloryemacs/set-leader-keys-for-major-mode 'org-agenda-mode
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

;;;
(defun paloryemacs/update-org-agenda-window-setup ()
  "When there is only one monitor, set `org-agenda-window-setup'
to `reorganize-frame', otherwise set to `other-frame'."
  (let* ((monitor-count (length (display-monitor-attributes-list)))
         (window-config (if (> monitor-count 1)
                            'other-frame
                          'reorganize-frame)))
    (setq org-agenda-window-setup window-config)
    (message "Update `org-agenda-window-setup' to %s" window-config)))

;; (paloryemacs/update-org-agenda-window-setup)

;;; archive
;; Tip: find all 'DONE' items older than 2 months and archive
;; Here's how to find all 'DONE' items older than 60 days in org-mode so they can be archived:
;; C-c a m
;; This brings up the agenda dialogue. Then at the 'Match:' prompt type:
;; CLOSED<"<-60d>"
;; Then, in the agenda window, to archive them all:
;; Press 'm' to mark current items
;; Press '%' to mark regexp.
;; Press '.' to select all.
;; Press 'B' for 'bulk action'.
;; Press '$' for archive.
;; find all 'CANCELLED' items. C-c < t then N r
(setq org-archive-location "%s_archive::")

;; http://ivanmalison.github.io/dotfiles/#org
(defun paloryemacs/org-archive-if (condition-function)
  (if (funcall condition-function)
      (let ((next-point-marker
             (save-excursion (org-forward-heading-same-level 1) (point-marker))))
        (org-archive-subtree)
        (setq org-map-continue-from (marker-position next-point-marker)))))

(defun paloryemacs/org-archive-if-completed ()
  (interactive)
  (paloryemacs/org-archive-if 'org-entry-is-done-p))

(defun paloryemacs/org-archive-completed-in-buffer ()
  (interactive)
  (org-map-entries 'paloryemacs/org-archive-if-completed))


;;; Priority
;; Note: `org-priority-faces' default to `nil' and if it is `nil' when mouse
;; over the agenda item, it cause tons of Invalid face reference: nil in
;; *Message* buffer. see also `org-agenda-fontify-priorities'
;; The entry default to B when no priority specified
(setq org-priority-faces
      '((?A . (:foreground "#ff40ff" :weight bold))
        (?B . (:foreground "#00f900" :weight bold))
        (?C . (:foreground "#fefb00" :weight bold))))


;;; org-habit
(setq org-habit-preceding-days 21
      org-habit-following-days 7
      org-habit-graph-column 90)

;;; refile
(setq org-refile-use-outline-path 'file ; use full outline paths for refile targets
      ;; don't complete in steps, use org to generate all of the possible
      ;; completions and present them at once.
      org-outline-path-complete-in-steps nil
      ;; Allow refile to create parent tasks with confirmation
      org-refile-allow-creating-parent-nodes 'confirm
      ;; targets include this file and any file contributing to the agenda - up to 9 levels deep
      org-refile-targets '((nil :maxlevel . 9)
                           (org-agenda-files :maxlevel . 9))
      org-refile-use-cache nil)


;; Exclude DONE state tasks from refile targets
;; http://doc.norang.ca/org-mode.html
;; (defun paloryemacs/verify-refile-target ()
;;   "Exclude todo keywords with a done state from refile targets"
;;   (not (member (nth 2 (org-heading-components)) org-done-keywords)))

;; (setq org-refile-target-verify-function 'paloryemacs/verify-refile-target)

;; Refile settings
;; Exclude DONE state tasks from refile targets
(defun paloryemacs/verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets"
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))

(setq org-refile-target-verify-function 'paloryemacs/verify-refile-target)


;;; for MobileOrg
;; Set to the name of the file where new notes will be stored
(with-eval-after-load "org"
  (setq org-mobile-inbox-for-pull (concat org-directory  "/from-mobile.org")
        org-mobile-directory "~/Dropbox/Apps/MobileOrg" ; Set to <your Dropbox root directory>/MobileOrg.
        org-mobile-use-encryption nil
        org-mobile-encryption-password ""))

;; (setq org-stuck-projects '("+LEVEL=2/-DONE"
;;                            ("TODO" "NEXT" "NEXTACTION")
;;                            nil))


;;;; Capture
(use-package org-capture
  :defer t
  :init
  (progn
    (define-key global-map (kbd "C-c c") 'org-capture)
    ;; https://lists.gnu.org/archive/html/emacs-orgmode/2010-08/msg00469.html
    (defun paloryemacs/find-today-trading-journal ()
      "Find today's trading journal."
      (let ((p (concat org-directory
                       (format-time-string
                        "/trading-journal/%Y-%m-%d.org"))))
        (find-file p)
        (goto-char (point-min))))

    (defun paloryemacs/find-memo-file ()
      "Find today's trading journal."
      (let ((p (concat org-directory
                       (format-time-string
                        "/Memo/%Y%m%d.org"))))
        (find-file p)
        (goto-char (point-min))))


    (setq org-capture-templates
          `(("a" "Add Task" entry
             (file+headline "~/org/GTD.org" "Tasks")
             "* TODO %?\n SCHEDULED: %t \n  :PROPERTIES:\n :ID: %(org-id-new)\n  :CREATED:  %U\n  :END:"
             :prepend t
             :empty-lines-after 1)

            ("t" "Todo" entry (file+headline "~/org/GTD.org" "Inbox")
             "* TODO %?\n  :PROPERTIES:\n :ID: %(org-id-new)\n  :CREATED:  %U\n  :END:"
             :prepend t
             :empty-lines-after 1)
            ("T" "Trading Journal" plain (function paloryemacs/find-today-trading-journal)
             "* %U\n  %i%?"
             :prepend t
             :unnarrowed nil
             :kill-buffer t
             :empty-lines-after 1)
            ;; use org-journal now
            ;; http://www.howardism.org/Technical/Emacs/journaling-org.html
            ;; ("j" "Journal" entry (file+olp+datetree "~/org/journal.org")
            ;;  "* %?\n  %i\n  %U"
            ;;  :kill-buffer t
            ;;  :empty-lines-after 1)
            ;; ("J" "Journal with Annotation" entry (file+olp+datetree "~/org/journal.org")
            ;;  "* %?\n  %U\n  %i\n  %a"
            ;;  :kill-buffer t
            ;;  :empty-lines-after 1)
            ("m" "Memo" plain #'paloryemacs/find-memo-file
             "* %T\n  %?\n  %a\n\n"
             :prepend t
             :unnarrowed t
             :kill-buffer t
             :empty-lines-after 1)
            ("d" "Drill" entry (file+headline "~/org/drill/playground.org" "Pond")
             "* Q: %?       :drill:\n\n** A:\n"
             :kill-buffer t
             :empty-lines-after 1)
            ("p" "Phone call" entry (file+headline "~/org/GTD.org" "Inbox")
             "* PHONE %? :PHONE:\n  :PROPERTIES:\n :ID: %(org-id-new)\n :CREATED:  %U\n  :END:"
             :clock-in t
             :clock-resume t
             :empty-lines-after 1)
            ;; work with org-protocol and emacs-mac port have default register to macOS
            ;; https://github.com/sprig/org-capture-extension
            ("P" "Protocol Clip" entry (file+headline ,(concat org-directory "/Notes.org") "Inbox")
             "* %?\n  :PROPERTIES:\n :ID: %(org-id-new)\n  :CREATED:  %U\n :END:\n  Source: %:annotation\n\n  #+BEGIN_QUOTE\n  %i\n  #+END_QUOTE"
             :empty-lines-after 1)

	        ("L" "Protocol Link" entry (file+headline ,(concat org-directory "/GTD.org") "Inbox")
             "* TODO Review %?%:annotation\n  :PROPERTIES:\n :ID: %(org-id-new)\n :CREATED:  %U\n :END:"
             :prepend t
             :empty-lines-after 1)
            ("r" "Remind" entry (file+headline "~/org/GTD.org" "Remind")
             "* %?\n  SCHEDULED: %(format-time-string \"<%Y-%m-%d .+1d/3d>\")\n  :PROPERTIES:\n :ID: %(org-id-new)\n :CREATED:  %U\n  :END:"
             :empty-lines-after 1)
            ("h" "Habit" entry (file+headline "~/org/GTD.org" "Habit")
             "* %?\n  SCHEDULED: %(format-time-string \"<%Y-%m-%d .+1d/3d>\")\n  :PROPERTIES:\n :ID: %(org-id-new)\n :CREATED:  %U\n  :STYLE: habit\n  :REPEAT_TO_STATE: NEXT\n  :END:"
             :empty-lines-after 1))))
  :config
  (progn
    (paloryemacs/set-leader-keys-for-minor-mode 'org-capture-mode
      dotpaloryemacs-major-mode-leader-key 'org-capture-finalize
      "a" 'org-capture-kill
      "c" 'org-capture-finalize
      "k" 'org-capture-kill
      "r" 'org-capture-refile)))


(use-package org-expiry
  :defer t
  :init
  (progn
    (setq org-expiry-created-property-name "CREATED"
          org-expiry-inactive-timestamps   t)))

;; http://www.ideaio.ch/posts/my-gtd-system-with-org-mode.html
(defun paloryemacs/insert-created-timestamp()
  "Insert a CREATED property using org-expiry.el for TODO entries"
  (org-expiry-insert-created)
  (org-back-to-heading)
  (org-end-of-line)
  (insert " "))

;;; work with appt
;; If you delete an appointment from your Org agenda file, the corresponding
;; alert is not deleted.
;; (defadvice org-agenda-to-appt (before wickedcool activate)
;;   "Clear the appt-time-msg-list."
;;   (setq appt-time-msg-list nil))

(defun paloryemacs//quiet-no-event-message (orig-fun &rest args)
  "Do annoy me when no event"
  (paloryemacs/with-suppress-message "No event to add"
    (apply orig-fun args)))

(advice-add 'org-agenda-to-appt :around #'paloryemacs//quiet-no-event-message)
;; (advice-remove 'org-agenda-to-appt #'paloryemacs//quiet-no-event-message)


(defun paloryemacs/org-agenda-to-appt ()
  (setq appt-time-msg-list nil)
  ;; Dangerous!!! do not use `appt-add', this might remove entries added by `appt-add' manually.
  (org-agenda-to-appt t "TODO"))
;; update appt
(run-at-time "24:01" (* 0.5 60 60) 'paloryemacs/org-agenda-to-appt) ;; update every half an hour

;; update appt each time agenda opened
(add-hook 'org-agenda-finalize-hook 'paloryemacs/org-agenda-to-appt)

(use-package org-protocol
  :config
  (progn
    ;; open -g 'org-protocol://hammerspoon?action=org-clock-goto'
    (defun org-protocol-hammerspoon (data)
      "Handle event from Hammerspoon"
      (let* ((action (plist-get data :action)))
        (cond ((string= action "org-clock-goto")
               (call-interactively 'org-clock-goto) ))))

    (add-to-list 'org-protocol-protocol-alist
                 '("handle action from hammerspoon"
                   :protocol "hammerspoon"
                   :function org-protocol-hammerspoon))))

;;; clock
(use-package org-clock
  :defer t
  :init
  ;; TODO: call `org-resolve-clocks' when emacs startup
  (progn
    ;; To save the clock history across Emacs sessions
    (global-set-key (kbd "C-S-g") 'org-clock-goto) ; jump to current task from anywhere
    (setq org-clock-clocked-in-display nil)
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
    (setq org-clock-idle-time 30)
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
    (defun paloryemacs/update-hammerspoon-org-clock-bar ()
      (let ((text (if (and (boundp 'org-mode-line-string)
                           org-mode-line-string
                           (fboundp' org-clocking-p)
                           (org-clocking-p))
                      (substring-no-properties org-mode-line-string)
                    "Nothing !!!")))
        (paloryemacs/open-hammerspoon-url
         "org_clock_update"
         "agenda_next_info"
         text)))

    (defun paloryemacs/org-clock-force-mode-line-update ()
      (setq org-mode-line-string nil)
      (org-clock-update-mode-line)
      (paloryemacs/update-hammerspoon-org-clock-bar))

    (add-hook 'org-clock-in-hook 'paloryemacs/org-clock-force-mode-line-update)
    (add-hook 'org-clock-cancel-hook 'paloryemacs/org-clock-force-mode-line-update)
    (add-hook 'org-clock-out-hook 'paloryemacs/org-clock-force-mode-line-update)))

;;; org-publish
(setq org-publish-project-alist
      '(("orgfiles"
         :base-directory "~/proj/homepage/"
         :base-extension "org"
         :publishing-directory "~/proj/homepage/"
         ;; :publishing-function org-publish-org-to-html
         :publishing-function (org-publish-org-to-html org-publish-attachment)
         :index-filename "PageIndex.org"
         :auto-index t
         :exclude "PrivatePage.org"   ;; regexp
         ;; :exclude "DayPlan.org\\|Monitoring.org\\|ConsultingPlans.org\\|HealthCare.org\\|JobHunting.org"
         :headline-levels 3
         :inline-images t
         :section-numbers nil
         :table-of-contents nil
         :style "<link rel=\"stylesheet\"
         href=\"common.css\" type=\"text/css\"/>"
         :preamble "<div class=\"menu\" style=\"float: right;\"> <a href=\"./index.html\">Home</a> - <a href=\"./PageIndex.html\">Index</a> </div>"
         :postamble "<font color=\"white\">&copy; 2005 - 2009 Shihpin Tseng. All Rights Reserved.</font>"

         :auto-postamble nil
         :auto-preamble t)
        ("reference-card"
         :base-directory "~/org/reference-card/"
         :base-extension "org"
         :publishing-function org-latex-publish-to-pdf
         :publishing-directory "~/org/reference-card/")
        ("other"
         :base-directory "~/Lab/notebook/"
         :base-extension "css\\|el"
         :publishing-directory "/ssh:user@host:~/html/other/")
        ("homepage" :components ("orgfiles"))))


(setq org-export-html-style-default "")
(setq org-publish-timestamp-directory "~/.org-timestamps/")

;; https://github.com/tumashu/ox-latex-chinese
(use-package ox-latex
  :defer t
  :init
  (setq org-latex-pdf-process
        '("xelatex -interaction nonstopmode -output-directory %o %f"
          "bibtex %b"
          "xelatex -interaction nonstopmode -output-directory %o %f"
          "xelatex -interaction nonstopmode -output-directory %o %f"))
  (setq org-latex-listings t)
  (setq org-export-latex-listings t)
  :config
  (add-to-list 'org-latex-packages-alist
               '(("AUTO" "inputenc" t)))

  ;; https://github.com/tsdye/org-article
  ;; $ cp org-article.cls ~/Library/texmf/tex/latex/
  ;; $ kpsewhich org-article.cls
  (add-to-list 'org-latex-classes
               '("org-article"
                 "\\documentclass{org-article}
                  [NO-DEFAULT-PACKAGES]
                  [PACKAGES]
                  [EXTRA]"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

;;; link
(setq org-link-abbrev-alist
      '(("google"   . "http://www.google.com/search?q=")
        ("baidu"    . "http://www.baidu.com/s?wd=")))

;; https://emacs.stackexchange.com/questions/33064/fontify-broken-links-in-org-mode
(with-eval-after-load "org"
  (org-link-set-parameters
   "file"
   :face (lambda (path) (if (file-exists-p path) 'org-link 'org-warning))))

;; keybinding conflicts with icicles keys
;; (org-defkey org-mode-map (kbd "C-c C-'") 'org-edit-special)
;; (define-key org-exit-edit-mode-map (kbd "C-c C-'") 'org-edit-src-exit)

;;; jump to agenda buffer when idle
;; get the idea from http://www.dbrunner.de/it/org-mode.html
;; I give a little update. execute recursive edit before pop a new window
;; see also `org-agenda-restore-windows-after-quit'
(defun paloryemacs/jump-to-org-agenda ()
  (interactive)
  (let* ((buf-name (if (boundp 'org-agenda-buffer-name)
                       org-agenda-buffer-name
                     "*Org Agenda*"))
         (buf (get-buffer buf-name))
         (wind (and buf (get-buffer-window buf))))
    (unless (window-minibuffer-p (selected-window))
      (if wind
          (with-selected-window wind
            (org-agenda-redo)
            (org-fit-window-to-buffer))
        ;; (call-interactively 'org-agenda-list)
        (org-agenda nil "d")))))

(with-eval-after-load 'evil
  (define-key evil-normal-state-map (kbd "S-SPC") 'paloryemacs/jump-to-org-agenda))

;; every 20 minutes
(run-with-idle-timer (* 20 60) t 'paloryemacs/jump-to-org-agenda)

;; (defun paloryemacs/delay-jump-to-org-agenda ()
;;   (run-at-time 13 nil #'paloryemacs/jump-to-org-agenda))

;; (add-hook 'after-init-hook
;;           #'paloryemacs/delay-jump-to-org-agenda
;;           ;; Note that 3-rd argument of this `add-hook' should be `t'
;;           ;; to append the call of the `dired' after other hooked functions,
;;           ;; most importantly after `desktop-read'.
;;           t)

;;; Embed source code and babel
;; fontify code in code blocks
(with-eval-after-load "org"
  (setq org-src-fontify-natively t)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (java . t)
     (dot . t)
     (ditaa . t)
     (R . t)
     (python . t)
     (ruby . t)
     (gnuplot . t)
     (clojure . t)
     (shell . t)
     (org . t)
     (plantuml . t)
     (latex . t)))
  ;; give us some hint we are running
  (defadvice org-babel-execute-src-block (around progress nil activate)
    (set-face-attribute
     'org-block nil :background "LightSteelBlue")
    (message "Running your code block")
    ad-do-it
    (set-face-attribute 'org-block nil :background "gray")
    (message "Done with code block")))

;; stop emacs asking for confirmation
;; (setq org-confirm-babel-evaluate nil)
(defun paloryemacs/org-confirm-babel-evaluate (lang body)
  (cond ((string= lang "ditaa") nil) ; don't ask for ditaa
        ((string= lang "emacs-lisp") nil)))
(setq org-confirm-babel-evaluate 'paloryemacs/org-confirm-babel-evaluate)


;;; Info directory
(eval-after-load "info"
  '(add-to-list 'Info-directory-list "~/.emacs.d/site-lisp/org-mode/doc"))

;;; org-mac-protocol
(with-eval-after-load "org"
  (when window-system
    (require 'org-mac-protocol nil t)))

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
;; (defadvice org-drill (before paloryemacs/org-drill-switch-to-evil-emacs-state activate)
;;   "Switch to evil-emacs-state before org-drill begin."
;;   (paloryemacs/evil-state-cycle 'insert))

;; (defadvice org-drill (after paloryemacs/org-drill-recover-evil-state activate)
;;   "Recover the evil state which saved before org-drill begin."
;;   (paloryemacs/evil-state-cycle))

;; use evil-save-state to wrap it
(defun paloryemacs/evil-org-drill ()
  "Switch to evil insert state, execute `org-drill' then restore the state."
  (interactive)
  (evil-save-state
    (evil-change-state 'insert)
    (org-drill)))

(defun paloryemacs/evil-org-drill-directory ()
  "Switch to evil insert state, execute `org-drill-directory' then restore the state."
  (interactive)
  (evil-save-state
    (evil-change-state 'insert)
    (org-drill-directory)))

(defun paloryemacs/evil-org-drill-resume ()
  "Switch to evil insert state, execute `org-drill-resume' then restore the state."
  (interactive)
  (evil-save-state
    (evil-change-state 'insert)
    (org-drill-resume)))

(defun paloryemacs/evil-org-drill-again ()
  "Switch to evil insert state, execute `org-drill-again' then restore the state."
  (interactive)
  (evil-save-state
    (evil-change-state 'insert)
    (org-drill-again)))

(defun paloryemacs/evil-org-drill-cram ()
  "Switch to evil insert state, execute `org-drill-cram' then restore the state."
  (interactive)
  (evil-save-state
    (evil-change-state 'insert)
    (org-drill-cram)))

(defhydra paloryemacs/org-agenda (:color teal)
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


(defun paloryemacs/org-insert-image ()
  "Insert a image link, when C-u take a screenshot."
  (interactive)
  (let* ((buf-file-name (buffer-file-name))
         (current-directory (and buf-file-name
                                 (file-name-directory buf-file-name)))
         (image-file-name))
    (cond ((and current-prefix-arg current-directory)
           (let* ((screenshot-directory (concat current-directory "images/"))
                  (screenshot-file-name
                   (concat (make-temp-name (format-time-string
                                            "%Y-%m-%d-" (current-time))) ".png"))
                  (screenshot-full-name
                   (concat screenshot-directory screenshot-file-name)))
             (unless (file-accessible-directory-p screenshot-directory)
               (make-directory "images/" t))
             (call-process-shell-command "screencapture" nil nil nil nil "-i"
                                         screenshot-full-name)
             (setq image-file-name (concat "./images/" screenshot-file-name))))
          ((not current-prefix-arg)
           (let ((selected-file-full-name (read-file-name "Image: ")))
             (when selected-file-full-name
               (let ((sub-image-dir-path (concat current-directory "images/")))
                 (if (string-prefix-p sub-image-dir-path
                                      (file-name-directory selected-file-full-name))
                     (setq image-file-name
                           (concat "./images/"
                                   (string-remove-prefix sub-image-dir-path
                                                         selected-file-full-name)))
                   (setq image-file-name selected-file-full-name))))))
          (t
           (message "Unable to guess where to save screenshot.")))
    (when image-file-name
      (insert (concat "[[" image-file-name "]]"))
      (org-display-inline-images))))


;;; get the summary of a today by tags
;; called by C-u sum last day
;; called by C-u sum specify date
(defun paloryemacs/org-clock-summary-today-by-tags (timerange &optional tstart tend noinsert)
  (interactive "P")
  (let* ((timerange-numeric-value (prefix-numeric-value timerange))
         (files (org-add-archive-files (org-agenda-files)))
         (include-tags '("academic" "english" "learning" "daily" "emacs" "other" "exercise"))
         (tags-time-alist (mapcar (lambda (tag) `(,tag . 0)) include-tags))
         (output-string "")
         (seconds-of-day 86400)
         (tstart (or tstart
                     (and timerange
                          (equal timerange-numeric-value 4)
                          (- (org-time-today) seconds-of-day))
                     (and timerange
                          (equal timerange-numeric-value 16)
                          (org-read-date nil nil nil "Start Date/Time:"))
                     (org-time-today)))
         (tend (or tend
                   (and timerange
                        (equal timerange-numeric-value 16)
                        (org-read-date nil nil nil "End Date/Time:"))
                   (+ tstart seconds-of-day)))
         h m file item prompt done-something)
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

;; http://kitchingroup.cheme.cmu.edu/blog/2017/04/09/A-better-return-in-org-mode
;; (define-key org-mode-map (kbd "RET") 'paloryemacs/org-return)
(defun paloryemacs/org-return ()
  "Add new list or headline "
  (interactive)
  (cond
   ((org-in-item-p)
    (if (org-element-property :contents-begin (org-element-context))
        (org-insert-heading)
      (beginning-of-line)
      (setf (buffer-substring
             (line-beginning-position) (line-end-position)) "")
      (org-return)))
   ((org-at-heading-p)
    (if (not (string= "" (org-element-property :title (org-element-context))))
        (progn (org-end-of-meta-data)
               (org-insert-heading))
      (beginning-of-line)
      (setf (buffer-substring
             (line-beginning-position) (line-end-position)) "")))
   ((org-at-table-p)
    (if (-any?
         (lambda (x) (not (string= "" x)))
         (nth
          (- (org-table-current-dline) 1)
          (org-table-to-lisp)))
        (org-return)
      ;; empty row
      (beginning-of-line)
      (setf (buffer-substring
             (line-beginning-position) (line-end-position)) "")
      (org-return)))
   (t
    (org-return))))



;; Insert key for org-mode and markdown a la C-h k
;; from SE endless http://emacs.stackexchange.com/questions/2206/i-want-to-have-the-kbd-tags-for-my-blog-written-in-org-mode/2208#2208
(defun paloryemacs/insert-keybinding-org (key)
  "Ask for a key then insert its description.
Will work on both org-mode and any mode that accepts plain html."
  (interactive "kType key sequence: ")
  (let* ((tag "@@html:<kbd>@@ %s @@html:</kbd>@@"))
    (if (null (equal key "\r"))
        (insert
         (format tag (help-key-description key nil)))
      (insert (format tag ""))
      (forward-char -8))))

(use-package org-src
  :defer t
  :init
  (progn
    (setq org-src-preserve-indentation nil)
    (setq org-edit-src-content-indentation 0))
  :config
  (progn
    (paloryemacs/set-leader-keys-for-minor-mode 'org-src-mode
      dotpaloryemacs-major-mode-leader-key 'org-edit-src-exit
      "c" 'org-edit-src-exit
      "a" 'org-edit-src-abort
      "k" 'org-edit-src-abort)))


(with-eval-after-load "org"
  ;; hydra org template
  (with-eval-after-load "hydra"
    (defhydra hydra-org-template (:color blue :hint nil)
      "
_c_enter  _q_uote    _L_aTeX:
_l_atex   _e_xample  _i_ndex:
_a_scii   _v_erse    _I_NCLUDE:
_s_rc     ^ ^        _H_TML:
_h_tml    ^ ^        _A_SCII:
"
      ("s" (paloryemacs/hot-expand "<s"))
      ("e" (paloryemacs/hot-expand "<e"))
      ("q" (paloryemacs/hot-expand "<q"))
      ("v" (paloryemacs/hot-expand "<v"))
      ("c" (paloryemacs/hot-expand "<c"))
      ("l" (paloryemacs/hot-expand "<l"))
      ("h" (paloryemacs/hot-expand "<h"))
      ("a" (paloryemacs/hot-expand "<a"))
      ("L" (paloryemacs/hot-expand "<L"))
      ("i" (paloryemacs/hot-expand "<i"))
      ("I" (paloryemacs/hot-expand "<I"))
      ("H" (paloryemacs/hot-expand "<H"))
      ("A" (paloryemacs/hot-expand "<A"))
      ("<" self-insert-command "ins")
      ("<escape>" nil :exit t)
      ("o" nil "quit")))

  (defun paloryemacs/hot-expand (str)
    "Expand org template."
    (insert str)
    (org-try-structure-completion))

  (define-key org-mode-map "<"
    (lambda () (interactive)
      (if (looking-back "^")
          (hydra-org-template/body)
        (self-insert-command 1)))))

;;; finding all tags
(defun paloryemacs/counsel-org-tags ()
  (interactive)
  (let* ((tag-l (org-map-entries (lambda () org-scanner-tags) t 'agenda))
         (tags (sort (delete-dups (apply 'append (delete-dups tag-l))) 'string<)))
    (ivy-read "Org Tag View: "
              tags
              :action (lambda (tag) (org-tags-view nil tag))
              :caller 'paloryemacs/org-tags)))

;;; indent
(defun paloryemacs/reflash-indentation ()
  "Fix org-indent issues, center line."
  (interactive)
  (org-indent-mode +1)
  (recenter-top-bottom))

;; C-x C-i or C-x Tab to call indent-rigidly
;; (define-key indent-rigidly-map (kbd "H-h") 'indent-rigidly-left)
;; (define-key indent-rigidly-map (kbd "H-l") 'indent-rigidly-right)

;;; org-journal
;; https://github.com/bastibe/org-journal
(use-package org-journal
  :defer t
  :init
  (progn
    (setq org-journal-dir (concat org-directory "/journal/")
          org-journal-file-format "%Y-%m-%d"
          org-journal-date-prefix "#+TITLE: "
          org-journal-date-format "%A, %B %d %Y"
          org-journal-time-prefix "* "
          org-journal-time-format "%R ")
    (paloryemacs/declare-prefix "aoj" "org-journal")
    (paloryemacs/set-leader-keys
      "aojj" 'org-journal-new-entry
      "aojs" 'org-journal-search-forever))
  :config
  (progn
    (paloryemacs/set-leader-keys-for-major-mode 'org-journal-mode
      "j" 'org-journal-new-entry
      "n" 'org-journal-open-next-entry
      "p" 'org-journal-open-previous-entry)

    ;; set the parent keymap, so we can use the keybinding of ", *" binded in org-mode
    (set-keymap-parent paloryemacs-org-journal-mode-map paloryemacs-org-mode-map)

    (paloryemacs/set-leader-keys-for-major-mode 'calendar-mode
      "r" 'org-journal-read-entry
      "i" 'org-journal-new-date-entry
      "n" 'org-journal-next-entry
      "p" 'org-journal-previous-entry
      "s" 'org-journal-search-forever
      "w" 'org-journal-search-calendar-week
      "m" 'org-journal-search-calendar-month
      "y" 'org-journal-search-calendar-year)))

;;; evil surround
(defun paloryemacs//surround-drawer ()
  (let ((dname (read-from-minibuffer "" "")))
    (cons (format ":%s:" (upcase (or dname ""))) ":END:")))

(defun paloryemacs//surround-code ()
  (let ((dname (read-from-minibuffer "" "")))
    (cons (format "#+BEGIN_SRC %s" (or dname "")) "#+END_SRC")))

;;; evil-org-mode
(defun paloryemacs//evil-org-mode ()
  (evil-org-mode)
  (evil-normalize-keymaps))

(use-package org-bullets
  :defer t
  :init
  (setq org-bullets-face-name nil)
  ;; http://nadeausoftware.com/articles/2007/11/latency_friendly_customized_bullets_using_unicode_characters
  ;; https://zhangda.wordpress.com/2016/02/15/configurations-for-beautifying-emacs-org-mode/
  ;; "⬢" "⭓" "■"
  ;; "◉" "◎" "⚫" "○" "►" "◇"
  ;; "✺" "✹" "✸" "✷" "✶" "✭" "✦" "■" "▼" "●"
  ;; "⊢" "⋮" "⋱" "⋱" "⋱"
  ;; "☯"
  (setq org-bullets-bullet-list
        '("☰" "☱" "☲" "☳" "☴" "☵" "☶" "☷"))
  (add-hook 'org-mode-hook 'org-bullets-mode))

(use-package org-brain
  :defer t
  :init
  (setq org-brain-path (expand-file-name "brain" org-directory))
  (paloryemacs/set-leader-keys "aob" 'org-brain-visualize)
  (with-eval-after-load 'evil
    (evil-set-initial-state 'org-brain-visualize-mode 'emacs))
  :config
  (setq org-id-track-globally t)
  (setq org-id-locations-file "~/.emacs.d/.org-id-locations")
  (push '("b" "Brain" plain (function org-brain-goto-end)
          "* %i%?" :empty-lines 1)
        org-capture-templates)
  (setq org-brain-visualize-default-choices 'all)
  (setq org-brain-title-max-length 12))

;; https://github.com/kiwanami/emacs-calfw
(defun paloryemacs/calfw-calendar ()
  (interactive)
  (let ((buf (get-buffer "*cfw-calendar*")))
    (if buf
        (pop-to-buffer buf nil)
      (cfw:open-calendar-buffer
       :contents-sources
       (list (cfw:org-create-source "Dark Blue")
             (cfw:cal-create-source "Dark Orange"))
       :view 'two-weeks))))

(use-package calfw
  :defer t
  :bind (("C-c A" . paloryemacs/calfw-calendar))
  :init
  (progn
    (use-package calfw-cal
      :defer t)
    (use-package calfw-org
      :defer t)
    ;; (bind-key "M-n" 'cfw:navi-next-month-command cfw:calendar-mode-map)
    ;; (bind-key "M-p" 'cfw:navi-previous-month-command cfw:calendar-mode-map)
    )
  :config
  (progn
    (setq cfw:fchar-junction ?╋
          cfw:fchar-vertical-line ?┃
          cfw:fchar-horizontal-line ?━
          cfw:fchar-left-junction ?┣
          cfw:fchar-right-junction ?┫
          cfw:fchar-top-junction ?┯
          cfw:fchar-top-left-corner ?┏
          cfw:fchar-top-right-corner ?┓)
    ;; (bind-key "j" 'cfw:navi-goto-date-command cfw:calendar-mode-map)
    ;; (bind-key "g" 'cfw:refresh-calendar-buffer cfw:calendar-mode-map)
    ))

(defun paloryemacs/add-created-pro-from-subtree ()
  (let* ((heading (org-heading-components))
         (level (nth 0 heading))
         timestr)
    (when (> level 1)
      (save-restriction
        (org-narrow-to-subtree)
        (when (re-search-forward (concat
                                  "^ * \\("
                                  org-ts-regexp-inactive
                                  "\\)$")
                                 nil t)
          (setq timestr (match-string-no-properties 1))
          (goto-char (point-at-bol))
          (kill-line 1)
          (org-set-property "CREATED" timestr))))))

;; my previous capture template put a timestamp in to the subtree, move it to
;; property
(defun paloryemacs/add-created-pro-from-subtree-whole-file ()
  (interactive)
  (let ((MATCH t)
        (SCOPE 'file)
        (SKIP nil)
        (spacing nil))
    (org-map-entries 'paloryemacs/add-created-pro-from-subtree
                     MATCH SCOPE SKIP)))



(provide '13org-mode)

;; Local Variables:
;; outline-regexp: ";;; "
;; mode: outline-minor
;; byte-compile-warnings: (not noruntime free-vars) **
;; End:
