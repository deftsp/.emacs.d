;;; 07org-mode.el ---

;; Copyright (C) 2007  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>
;; Keywords:

;;; Install
(add-to-list 'load-path "~/.emacs.d/site-lisp/org-mode/lisp")
(add-to-list 'load-path "~/.emacs.d/site-lisp/org-mode/contrib/lisp" t)
;; $ git pull
;; $ make autoloads EMACS=/Applications/Emacs.app/Contents/MacOS/Emacs
;; $ make EMACS=/Applications/Emacs.app/Contents/MacOS/Emacs
;; $ make doc EMACS=/Applications/Emacs.app/Contents/MacOS/Emacs

(require 'org-loaddefs)

(eval-after-load "org"
  '(progn
     (require '50calendar)
     (when (eq system-type 'darwin)
       (add-to-list 'org-modules 'org-mac-link))
     (add-to-list 'org-modules 'org-habit)
     (add-to-list 'org-modules 'org-expiry)
     (add-to-list 'org-modules 'org-mouse)
     (add-to-list 'org-modules 'org-annotate-file)
     (add-to-list 'org-modules 'org-interactive-query)
     (add-to-list 'org-modules 'org-info)
     (add-to-list 'org-modules 'org-man)
     (add-to-list 'org-modules 'org-eval)
     (add-to-list 'org-modules 'org-panel)
     (add-to-list 'org-modules 'org-toc)
     (add-to-list 'org-modules 'org-drill)))


(eval-after-load "evil"
  '(progn
     (require 'evil-org-mode)
     (add-hook 'org-mode-hook 'evil-org-mode)))

;;; global key binding
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c L") 'org-insert-link-global)
;; (define-key org-mode-map (kbd "C-c g") 'org-mac-grab-link)
;; C-c C-o Open link at or after point.
;; if no appropriate application, it will use mailcap's config to set `org-file-apps'
;; (global-set-key "\C-c o" 'org-open-at-point-global)
;; (global-set-key "\C-cb" 'org-iswitchb)

(setq org-todo-interpretation 'sequence ; or 'type
      org-use-fast-todo-selection t
      ;; `!' for a timestamp, `@' for a note with timestamp
      ;; `/!' means that in addition to the note taken when entering the state,
      ;; a timestamp should be recorded when leaving the WAIT state, if and only if the
      ;; target state does not configure logging for entering it.
      org-todo-keywords '((sequence "TODO(t)" "STARTED(s)" "DELEGATED(l)" "APPT(a)" "|" "DONE(d)" "DEFERRED(f)" "CANCELLED(c@)")
                          (sequence "WAITING(w@/!)" "HOLD(h@/!)" "SOMEDAY(S!)" "PROJECT(P@)" "OPEN(O@)" "|" "CANCELLED(c@/!)")
                          (sequence "REPORT(r)" "BUG(b)" "KNOWNCAUSE(k)" "|" "FIXED(f)")
                          (sequence "QUOTE(q!)" "QUOTED(Q!)" "|" "APPROVED(A@)" "EXPIRED(E@)" "REJECTED(R@)"))
      org-todo-keyword-faces (quote (("TODO"      . (:foreground "red"          :weight bold))
                                     ("STARTED"   . (:foreground "hot pink"     :weight bold))
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
                                 ("exercise". ?e)
                                 ("mail"    . ?M)
                                 ("movie"   . nil)
                                 ("misc"    . ?m)
                                 ("reading" . ?r))
      org-tag-alist '(("project" . ?p)))

(eval-after-load "org"
  '(setq org-agenda-custom-commands
         `(("A" agenda "Today's Priority #A tasks"
            ((org-agenda-skip-function
              '(org-agenda-skip-entry-if 'notregexp "\\=.*\\[#A\\]"))
             (org-agenda-span 'day)
             (org-agenda-overriding-header "Today's Priority #A tasks: ")))

           ("B" agenda "Today's Priority #B tasks"
            ((org-agenda-skip-function
              '(org-agenda-skip-entry-if 'notregexp "\\=.*\\[#B\\]"))
             (org-agenda-span 'day)
             (org-agenda-overriding-header "Today's Priority #B tasks: ")))

           ("C" agenda "Today's Priority #C tasks"
            ((org-agenda-skip-function
              '(org-agenda-skip-entry-if 'notregexp "\\=.*\\[#C\\]"))
             (org-agenda-span 'day)
             (org-agenda-overriding-header "Today's Priority #B tasks: ")))

           ("c" todo "DONE|DEFERRED|CANCELLED") ; not include scheduled TODO entiries

           ("d" todo "DELEGATED")

           ;; to create a sparse tree (again: current buffer only) with all entries containing the word `FIXME'.
           ("f" occur-tree "\\<FIXME\\>")

           ("g" "GeekTool Agenda"
            ((agenda ""))
            ((org-agenda-todo-keyword-format "%-11s")
             (org-agenda-prefix-format "  %-10T%?-16t% s")
             (org-agenda-show-inherited-tags nil)
             (org-agenda-remove-tags 'prefix)
             (org-agenda-tags-column 70))
            (,(concat org-directory  "/Agenda.txt")))

           ("h" "Habits" tags-todo "STYLE=\"habit\""
            ((org-agenda-overriding-header "Habits")
             (org-agenda-sorting-strategy
              '(todo-state-down effort-up category-keep))))

           ("p" tags "+project-TODO=\"DONE\"-TODO=\"CANCELLED\"")

           ("u" alltodo "Unscheduled TODO entries"
            ((org-agenda-skip-function
              '(org-agenda-skip-entry-if 'scheduled 'deadline 'regexp "<[^>\n]+>"))
             (org-agenda-overriding-header "Unscheduled TODO entries: ")))
           ("w" todo "WAITING" )
           ("W" todo-tree "WAITING"))))

(setq org-special-ctrl-a/e t
      org-cycle-separator-lines 2
      org-cycle-include-plain-lists t
      org-directory "~/org"
      org-hide-leading-stars t
      org-log-done 'time
      ;; Default target for storing notes. Used as a fall back file for org-capture.el, for templates that do not
      ;; specify a target file.
      org-default-notes-file (concat org-directory "/notes.org")
      org-agenda-files (list (concat org-directory "/GTD.org")
                             (concat org-directory "/from-mobile.org"))  ; (directory-files org-directory t ".*\\.org$")
      org-agenda-show-all-dates t
      org-agenda-span 'week
      org-agenda-include-all-todo nil
      ;; exclude scheduled items from the global TODO list.
      org-agenda-todo-ignore-scheduled t
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
      org-fast-tag-selection-single-key (quote expert)
      org-reverse-note-order t
      org-deadline-warning-days 7
      org-return-follows-link t
      org-startup-folded t
      org-startup-truncated t
      org-display-internal-link-with-indirect-buffer nil)

(with-eval-after-load "org"
  (org-defkey org-mode-map (kbd "C-c C-x t") 'pl/org-clock-summary-today-by-tags)
  ;; Undefine C-c [ and C-c ] since this breaks my
  ;; org-agenda files when directories are include It
  ;; expands the files in the directories individually
  (org-defkey org-mode-map (kbd "C-c [") 'undefined)
  (org-defkey org-mode-map (kbd "C-c ]") 'undefined))


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

;;; Priority
;; Note: `org-priority-faces' default to `nil' and if it is `nil' when mouse
;; over the agenda item, it cause tons of Invalid face reference: nil in
;; *Message* buffer. see also `org-agenda-fontify-priorities'
(setq org-priority-faces
      '((?A . (:foreground "#ff40ff" :weight bold))
        (?B . (:foreground "#00f900" :weight bold))
        (?C . (:foreground "#fefb00" :weight bold))))


;;; org-habit
(setq org-habit-preceding-days 21
      org-habit-following-days 7
      org-habit-graph-column 90)

;;; org-expiry
(setq org-expiry-inactive-timestamps t)

;;; refile
(setq org-refile-use-outline-path t ; use full outline paths for refile targets
      org-outline-path-complete-in-steps nil ; targets complete directly with IDO
      ;; Allow refile to create parent tasks with confirmation
      org-refile-allow-creating-parent-nodes 'confirm
      ;; targets include this file and any file contributing to the agenda - up to 9 levels deep
      org-refile-targets '((nil :maxlevel . 9)
                           (org-agenda-files :maxlevel . 9))
      org-blank-before-new-entry nil
      org-refile-use-cache nil)

;; Refile settings
;; Exclude DONE state tasks from refile targets
(defun pl/verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets"
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))

(setq org-refile-target-verify-function 'pl/verify-refile-target)


;;; for MobileOrg
;; Set to the name of the file where new notes will be stored
(eval-after-load "org"
  '(progn
     (setq org-mobile-inbox-for-pull (concat org-directory  "/from-mobile.org")
           org-mobile-directory "~/Dropbox/Apps/MobileOrg" ; Set to <your Dropbox root directory>/MobileOrg.
           org-mobile-use-encryption nil
           org-mobile-encryption-password "")))

;; (setq org-stuck-projects '("+LEVEL=2/-DONE"
;;                            ("TODO" "NEXT" "NEXTACTION")
;;                            nil))


;;;; Capture
(define-key global-map (kbd "C-c c") 'org-capture)

;; https://lists.gnu.org/archive/html/emacs-orgmode/2010-08/msg00469.html
(defun pl/find-today-trading-journal ()
  "Find today's trading journal."
  (let ((p (concat org-directory
                   (format-time-string
                    "/trading-journal/%Y-%m-%d.org"))))
    (find-file p)
    (goto-char (point-min))))

(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/org/GTD.org" "Inbox")
         "* TODO %?\n  %i%u"
         :kill-buffer t)
        ("T" "Trading Journal" plain (function pl/find-today-trading-journal)
         "* %U\n  %i%?"
         :prepend t
         :unnarrowed nil
         :kill-buffer t)
        ("j" "Journal" entry (file+datetree "~/org/journal.org")
         "* %?\n  %i\n  %U\n"
         :kill-buffer t)
        ("J" "Journal with Annotation" entry (file+datetree "~/org/journal.org")
         "* %?\n  %U\n  %i\n  %a"
         :kill-buffer t)
        ("m" "Memo" plain (file (concat org-directory (format-time-string "/%Y%m%d-%H%M%S.org")))
         "* MEMO <%<%Y-%m-%d>> %?\n   %i\n  %a\n\n"
         :prepend t
         :unnarrowed t
         :kill-buffer t)
        ("d" "Drill" entry (file+headline "~/org/drill/playground.org" "Pond")
         "* Q: %?       :drill:\n\n** A:\n"
         :kill-buffer t)
        ("p" "Phone call" entry (file+headline "~/org/GTD.org" "Inbox")
         "* PHONE %? :PHONE:\n  %U" :clock-in t :clock-resume t)
        ("h" "Habit" entry (file "~/org/GTD.org" "Inbox")
         "* %?\n\n  %U\n\n  SCHEDULED: %(format-time-string \"<%Y-%m-%d .+1d/3d>\")\n  :PROPERTIES:\n  :STYLE: habit \n  :REPEAT_TO_STATE: NEXT\n  :END:\n")))

;;; work with appt
(defun pl/org-agenda-to-appt ()
  (setq appt-time-msg-list nil)
  ;; Dangerous!!! do not use `appt-add', this might remove entries added by `appt-add' manually.
  (org-agenda-to-appt t "TODO"))
;; update appt
(run-at-time "24:01" (* 0.5 60 60) 'pl/org-agenda-to-appt) ;; update every half an hour

;; update appt each time agenda opened
(add-hook 'org-agenda-finalize-hook 'pl/org-agenda-to-appt)

;;;
(setq org-fontify-emphasized-text t
      org-fontify-done-headline t)

;;; clock
;; To save the clock history across Emacs sessions
(global-set-key (kbd "C-S-g") 'org-clock-goto) ; jump to current task from anywhere
(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)
(setq org-clock-idle-time 15)

(setq org-clock-into-drawer t)

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

        ("other"
         :base-directory "~/Lab/notebook/"
         :base-extension "css\\|el"
         :publishing-directory "/ssh:user@host:~/html/other/")
        ("homepage" :components ("orgfiles"))))


(setq org-export-html-style-default "")
(setq org-publish-timestamp-directory "~/.org-timestamps/")

;;; link
(setq org-link-abbrev-alist
      '(("google"   . "http://www.google.com/search?q=")
        ("baidu"    . "http://www.baidu.com/s?wd=")))

;;; org-mac-link


;; keybinding conflicts with icicles keys
;; (org-defkey org-mode-map (kbd "C-c C-'") 'org-edit-special)
;; (define-key org-exit-edit-mode-map (kbd "C-c C-'") 'org-edit-src-exit)

;; (org-defkey org-mode-map (kbd "H-,") 'org-shiftup)
;; (org-defkey org-mode-map (kbd "H-.") 'org-shiftdown)

;;; Structure editing key bindind
;; Ι have enabled `org-replace-disputed-keys'
;; M-p <S-up>
;; M-n <S-down>
;; M-- <S-left>
;; M-= <S-right>
;; M-- <C-S-left>
;; M-+ <C-s-right>

(eval-after-load "org"
  '(progn
     (org-defkey org-mode-map (kbd "M-H") 'org-metaleft)
     (org-defkey org-mode-map (kbd "M-L") 'org-metaright)
     (org-defkey org-mode-map (kbd "M-K") 'org-metaup)
     (org-defkey org-mode-map (kbd "M-J") 'org-metadown)

     (org-defkey org-mode-map (kbd "H-M-h") 'org-shiftmetaleft)
     (org-defkey org-mode-map (kbd "H-M-l") 'org-shiftmetaright)
     (org-defkey org-mode-map (kbd "H-M-k") 'org-shiftmetaup)
     (org-defkey org-mode-map (kbd "H-M-j") 'org-shiftmetadown)

     (org-defkey org-mode-map (kbd "H-K") 'org-shiftup)
     (org-defkey org-mode-map (kbd "H-J") 'org-shiftdown)
     (org-defkey org-mode-map (kbd "H-H") 'org-shiftleft)
     (org-defkey org-mode-map (kbd "H-L") 'org-shiftright)

     (org-defkey org-mode-map (kbd "C-{") 'org-shiftcontrolleft)
     (org-defkey org-mode-map (kbd "C-}") 'org-shiftcontrolright)))



;;; jump to agenda buffer when idle
;; get the idea from http://www.dbrunner.de/it/org-mode.html
;; I give a little update. execute recursive edit before pop a new window
;; see also `org-agenda-restore-windows-after-quit'
(defun pl/jump-to-org-agenda ()
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
        (call-interactively 'org-agenda-list)))))

;; every 20 minutes
(run-with-idle-timer (* 20 60) t 'pl/jump-to-org-agenda)

(defun pl/delay-jump-to-org-agenda ()
  (run-at-time 3 nil #'pl/jump-to-org-agenda))

(add-hook 'after-init-hook
          #'pl/delay-jump-to-org-agenda
          ;; Note that 3-rd argument of this `add-hook' should be `t'
          ;; to append the call of the `dired' after other hooked functions,
          ;; most importantly after `desktop-read'.
          t)

;;; Embed source code and babel
;; fontify code in code blocks
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
   (sh . t)
   (org . t)
   (plantuml . t)
   (latex . t)))


;; stop emacs asking for confirmation
;; (setq org-confirm-babel-evaluate nil)
(defun pl/org-confirm-babel-evaluate (lang body)
  (cond ((string= lang "ditaa") nil) ; don't ask for ditaa
        ((string= lang "emacs-lisp") nil)))
(setq org-confirm-babel-evaluate 'pl/org-confirm-babel-evaluate)

;; give us some hint we are running
(defadvice org-babel-execute-src-block (around progress nil activate)
  (set-face-attribute
   'org-block nil :background "LightSteelBlue")
  (message "Running your code block")
  ad-do-it
  (set-face-attribute 'org-block nil :background "gray")
  (message "Done with code block"))

;;; Info directory
(eval-after-load "info"
  '(add-to-list 'Info-directory-list "~/.emacs.d/site-lisp/org-mode/doc"))

;;; org-mac-protocol
(when window-system
  (require 'org-mac-protocol nil t))

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


;;; agenda mode
(defun pl/org-agenda-mode-init ()
  (define-key org-agenda-mode-map " " 'org-agenda-cycle-show))

(add-hook 'org-agenda-mode-hook 'pl/org-agenda-mode-init)

(defun pl/org-insert-image ()
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
               (if (string-equal (concat current-directory "images/")
                                 (file-name-directory selected-file-full-name))
                   (setq image-file-name
                         (concat "./images/"
                                 (file-name-nondirectory selected-file-full-name)))
                 (setq image-file-name selected-file-full-name)))))
          (t
           (message "Unable to guess where to save screenshot.")))
    (when image-file-name
      (insert (concat "[[" image-file-name "]]"))
      (org-display-inline-images))))


;;; get the summary of a today by tags
;; called by C-u sum last day
;; called by C-u sum specify date
(defun pl/org-clock-summary-today-by-tags (timerange &optional tstart tend noinsert)
  (interactive "P")
  (let* ((timerange-numeric-value (prefix-numeric-value timerange))
         (files (org-add-archive-files (org-agenda-files)))
         (include-tags '("academic" "english" "learning" "other" "exercise"))
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

(provide '07org-mode)

;; Local Variables:
;; outline-regexp: ";;; "
;; mode: outline-minor
;; End:
