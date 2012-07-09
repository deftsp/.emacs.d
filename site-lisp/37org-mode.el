;;; 37org-mode.el ---

;; Copyright (C) 2007  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>
;; Keywords:

(require 'org)
(require 'org-publish nil t)
;; (require 'org-mtags nil t)

;; org-mode with GTD
;; (require 'org-gtd)
;; (require 'org-mouse)
;; (require 'org-blog)
;; (setq org-blog-directory "~/blog/")

;; C-c C-o Open link at or after point.
;; if no appropriate application, it will use mailcap's config.
;; to set `org-file-apps'

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
;; (global-set-key "\C-c o" 'org-open-at-point-global)
;; (global-set-key "\C-cb" 'org-iswitchb)

;; keybinding conflicts with icicles keys
;; (org-defkey org-mode-map (kbd "C-c C-'") 'org-edit-special)
;; (define-key org-exit-edit-mode-map (kbd "C-c C-'") 'org-edit-src-exit)

;; (org-defkey org-mode-map (kbd "H-,") 'org-shiftup)
;; (org-defkey org-mode-map (kbd "H-.") 'org-shiftdown)

;; Structure editing

;; Cursor keys with modifiers
;; (org-defkey org-mode-map (kbd "H-o H") 'org-metaleft)
;; (org-defkey org-mode-map (kbd "H-o L") 'org-metaright)
;; (org-defkey org-mode-map (kbd "H-o K") 'org-metaup)
;; (org-defkey org-mode-map (kbd "H-o J") 'org-metadown)

;; (org-defkey org-mode-map (kbd "H-o M-h") 'org-shiftmetaleft)
;; (org-defkey org-mode-map (kbd "H-o M-l") 'org-shiftmetaright)
;; (org-defkey org-mode-map (kbd "H-o M-k") 'org-shiftmetaup)
;; (org-defkey org-mode-map (kbd "H-o M-j") 'org-shiftmetadown)

;; (org-defkey org-mode-map (kbd "H-o k") 'org-shiftup)
;; (org-defkey org-mode-map (kbd "H-o j") 'org-shiftdown)
;; (org-defkey org-mode-map (kbd "H-o h") 'org-shiftleft)
;; (org-defkey org-mode-map (kbd "H-o l") 'org-shiftright)

;; (org-defkey org-mode-map (kbd "H-o C-l") 'org-shiftcontrolright)
;; (org-defkey org-mode-map (kbd "H-o C-h") 'org-shiftcontrolleft)



(setq org-todo-interpretation 'sequence ; or 'type
      org-use-fast-todo-selection t)

;; (setq org-todo-keywords '((sequence "TODO(t)" "STARTED(s)" "WAITING(w@/!)" "DELEGATED(l)" "APPT(a)" "|" "DONE(d)" "DEFERRED(f)" "CANCELLED(c@)")
;;                           (sequence "REPORT(r)" "BUG(b)" "KNOWNCAUSE(k)" "|" "FIXED(f)")
;;                           (sequence "|" "CANCELED(c)")))


(setq org-todo-keywords (quote ((sequence "TODO(t)" "STARTED(s)" "|" "DONE(d!/!)")
                                (sequence "WAITING(w@/!)" "SOMEDAY(S!)" "PROJECT(P@)" "OPEN(O@)" "|" "CANCELLED(c@/!)")
                                (sequence "QUOTE(q!)" "QUOTED(Q!)" "|" "APPROVED(A@)" "EXPIRED(E@)" "REJECTED(R@)"))))


(setq org-todo-keyword-faces (quote (("TODO" :foreground "red" :weight bold)
                                     ("STARTED" :foreground "blue" :weight bold)
                                     ("DONE" :foreground "forest green" :weight bold)
                                     ("WAITING" :foreground "orange" :weight bold)
                                     ("SOMEDAY" :foreground "magenta" :weight bold)
                                     ("CANCELLED" :foreground "forest green" :weight bold)
                                     ("QUOTE" :foreground "red" :weight bold)
                                     ("QUOTED" :foreground "magenta" :weight bold)
                                     ("APPROVED" :foreground "forest green" :weight bold)
                                     ("EXPIRED" :foreground "forest green" :weight bold)
                                     ("REJECTED" :foreground "forest green" :weight bold)
                                     ("OPEN" :foreground "blue" :weight bold)
                                     ("PROJECT" :foreground "red" :weight bold))))




(setq org-agenda-custom-commands
      '(("A" agenda "Today's Priority #A tasks"
         ((org-agenda-skip-function
           (lambda nil
             (org-agenda-skip-entry-if (quote notregexp) "\\=.*\\[#A\\]")))
          (org-agenda-ndays 1)
          (org-agenda-overriding-header "Today's Priority #A tasks: ")))
        ("c" todo "DONE|DEFERRED|CANCELLED" nil)
        ("d" todo "DELEGATED" nil)
        ;; to create a sparse tree (again: current buffer only) with all entries containing the word `FIXME'.
        ("f" occur-tree "\\<FIXME\\>")
        ("m" tags "PROJECT&MAYBE" nil)
        ("p" tags "PROJECT-MAYBE-DONE" nil)
        ("P" todo "PROJECT-MAYBE-DONE" nil)
        ("u" alltodo "Unscheduled TODO entries"
         ((org-agenda-skip-function
           (lambda nil
             (org-agenda-skip-entry-if (quote scheduled) (quote deadline)
                                       (quote regexp) "<[^>\n]+>")))
          (org-agenda-overriding-header "Unscheduled TODO entries: ")))
        ("w" todo "WAITING" nil)
        ;; ("W" todo-tree "WAITING")
        ("W" agenda "agenda for 21 days" ((org-agenda-ndays 21)))
        ;; ("G" "Geektool agenda" ((agenda "") (alltodo))
        ;;  ((org-agenda-ndays 1) (org-deadline-warning-days 7))
        ;;  ("~/proj/org/Agenda.txt"))

        ("g" "GeekTool Agenda" ((agenda ""))
         ((org-agenda-todo-keyword-format "%-11s")
          (org-agenda-prefix-format "  %-10T%?-16t% s")
          (org-agenda-show-inherited-tags nil)
          (org-agenda-remove-tags 'prefix)
          (org-agenda-tags-column 70))
         ("~/proj/org/Agenda.txt"))))

(setq org-special-ctrl-a/e t
      org-cycle-separator-lines 2
      org-cycle-include-plain-lists t
      org-directory "~/proj/org"
      org-archive-location "%s_archive::"
      org-hide-leading-stars t
      org-log-done 'time
      ;; org-default-notes-files is used by remember.el in certain situations
      ;; where it needs a path to store simple notes in.
      org-default-notes-file (concat org-directory "/notes.org")
      org-agenda-files (directory-files org-directory t ".*\\.org$")
      org-agenda-show-all-dates t
      org-agenda-ndays 7
      org-agenda-skip-deadline-if-done t
      org-agenda-include-all-todo nil
      ;; exclude scheduled items from the global TODO list.
      org-agenda-todo-ignore-scheduled t
      org-agenda-skip-scheduled-if-done nil
      org-agenda-todo-list-sublevels t
      org-agenda-include-diary nil
      ;; agenda view always starts out by showing me the next seven days.
      org-agenda-start-on-weekday nil
      org-fast-tag-selection-single-key (quote expert)
      org-reverse-note-order t
      org-deadline-warning-days 7
      org-display-internal-link-with-indirect-buffer nil)

;;; for MobileOrg
;; Set to the name of the file where new notes will be stored
(setq org-mobile-inbox-for-pull "~/proj/org/flagged.org")
;; Set to <your Dropbox root directory>/MobileOrg.
(setq org-mobile-directory "~/Dropbox/MobileOrg")
;; Enable encryption
(setq org-mobile-use-encryption t)
;; Set a password
(setq org-mobile-encryption-password "op'et6m")

;; (setq org-stuck-projects '("+LEVEL=2/-DONE"
;;                            ("TODO" "NEXT" "NEXTACTION")
;;                            nil))

(setq org-tag-alist '((:startgroup . nil) ("@work" . ?w) ("@home" . ?h) ("@tennisclub" . ?t) (:endgroup . nil)
                      (:startgroup . nil) ("Online" . ?O) ("Offline" . ?F) (:endgroup . nil)
                      (:startgroup . nil) ("Business" . ?B) ("Personal" . ?P) (:endgroup . nil)
                      ("PROJECT" . ?p)
                      ("READING" . ?R)
                      ("MAIL" . ?M)))


;; (defun org-jump-to-project-todo ()
;;   (interactive)
;;   (cond (local-project-root
;;          (find-file-other-window
;;           (expand-file-name (concat "~/org/" local-project-name ".org"))))
;;         (t (message "No project set!"))))

;;;-------------------------------------------------------------------------------------------------------------------
;;; remember
;;;-------------------------------------------------------------------------------------------------------------------

;; (org-remember-insinuate)
;; (define-key global-map "\C-cr" 'org-remember)


;; If you think of something, rather than searching for the right project/day page, just type M-x remember. A buffer
;; will pop up allowing you to jot your thoughts, then insert them into whatever page you specify, with a link to
;; today's page!

;; Remember, with description DESC, the given TEXT.
;;(setq remember-handler-functions '(remember-append-to-file))
;; Remember this text to PAGE or today's page.
;; (setq remember-handler-functions '(remember-planner-append))
;;  To take advantage of PlannerMode's annotation functions
;; (setq remember-annotation-functions planner-annotation-functions)

;;; copy the entire note to the daily page, not just the header (t)
;; (setq remember-planner-copy-on-xref-flag t
;;       remember-data-file "~/.emacs.d/notes"
;;       remember-filter-functions (quote ((lambda nil (delete-trailing-whitespace))))
;;       remember-annotation-functions '(org-remember-annotation)
;;       org-remember-store-without-prompt t
;;       remember-handler-functions '(org-remember-handler))
;; (add-hook 'remember-mode-hook 'org-remember-apply-template)
;; org-remember-templates defines two templates I use with remember-mode. Because I bind the remember function to C-c R,
;; with these templates I can quickly type either C-c R t to joy a new task, or to create a brief note.
;; (setq org-remember-templates
;;       (quote ((?t "* TODO %?\n  %u" "~/proj/org/ToDo.org" "Tasks")
;;               (?m "* TODO %?\n  %u" "~/proj/org/Misc.org" "Tasks")
;;               (?n "* %u %?" "~/proj/org/Notes" "Notes"))))
;; (global-set-key (kbd "C-c R") 'remember)

;; (setq org-remember-templates
;;       '((?t "* TODO %?\n  %i\n  %a" "~/org/TODO.org")
;;         (?j "* %U %?\n\n  %i\n  %a" "~/org/JOURNAL.org")
;;         (?i "* %^{Title}\n  %i\n  %a" "~/org/JOURNAL.org" "New Ideas")))

;;; if called with a prefix, create another remember buffer.
;;; From: Jim Ottaway <j.ottaway@lse.ac.uk>
;;; To: planner-el-discuss@gna.org
;;; Subject: Re: [Planner-el-discuss] proposition: multiple remember buffers
;; (defun remember (&optional new)
;;   "Remember an arbitrary piece of data.
;; With a prefix, create a new remember buffer."
;;   (interactive "P")
;;   (window-configuration-to-register remember-register)
;;   (let* ((initial (and transient-mark-mode
;;                        mark-active
;;                        (prog1
;;                            (buffer-substring (point) (mark))
;;                          (deactivate-mark))))
;;          (annotation
;;           (if remember-run-all-annotation-functions-flag
;;               (mapconcat 'identity
;;                          (delq nil (mapcar 'funcall remember-annotation-functions))
;;                          "\n")
;;             (run-hook-with-args-until-success
;;              'remember-annotation-functions)))
;;          (buf (if new
;;                   (generate-new-buffer remember-buffer)
;;                 (get-buffer-create remember-buffer))))
;;     (run-hooks 'remember-before-remember-hook)
;;     (switch-to-buffer-other-window buf)
;;     (remember-mode)
;;     (when (= (point-max) (point-min))
;;       (when initial (insert initial))
;;       (setq remember-annotation annotation)
;;       (when remember-initial-contents (insert remember-initial-contents))
;;       (when (and (stringp annotation)
;;                  (not (equal annotation "")))
;;         (insert "\n\n" annotation))
;;       (setq remember-initial-contents nil)
;;       (goto-char (point-min)))
;;     (message "Use C-c C-c to remember the data.")))
;;remember ends there---------------------------------------------------------------------------

;;;; Capture
(define-key global-map "\C-cC" 'org-capture)
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/proj/org/GTD.org" "Tasks")
         "* TODO %?\n  %i%u")
        ("j" "Journal" entry (file+datetree "~/proj/org/journal.org")
         "* %?\nEntered on %U\n  %i\n  %a")))


;;(org-agenda-to-appt)

(setq org-fontify-emphasized-text t
      org-fontify-done-headline nil)

(setq org-adapt-indentation t)

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
         :base-directory "~/proj/org/notebook/"
         :base-extension "css\\|el"
         :publishing-directory "/ssh:user@host:~/html/other/")
        ("homepage" :components ("orgfiles"))))


(setq org-export-html-style-default "")


(setq org-publish-timestamp-directory "~/.org-timestamps/")

;; (add-hook 'emacs-lisp-mode-hook #'turn-on-orgstruct)
;; (add-hook 'lisp-mode-hook #'turn-on-orgstruct)


(setq org-link-abbrev-alist
      '(("google"   . "http://www.google.com/search?q=")
        ("baidu"    . "http://www.baidu.com/s?wd=")))


;;; work with calendar
(defun th-calendar-open-agenda ()
  (interactive)
  (let* ((calendar-date (or
                         ;; the date at point in the calendar buffer
                         (calendar-cursor-to-date)
                         ;; if there's none, use the curren date
                         (calendar-current-date)))
         (day (time-to-days (encode-time 1 1 1
                                         (second calendar-date)
                                         (first calendar-date)
                                         (third calendar-date))))
         (calendar-buffer (current-buffer)))
    (org-agenda-list nil day)
    (select-window (get-buffer-window calendar-buffer))))

(eval-after-load "calendar"
  '(progn
     (define-key calendar-mode-map (kbd "RET") 'th-calendar-open-agenda)))


;; And here’s a small minor mode which uses the function above to refresh the agenda buffer when you move point in the
;; calendar buffer, so calendar and agenda stay in sync.

;; (define-minor-mode th-org-agenda-follow-calendar-mode
;;     "If enabled, each calendar movement will refresh the org agenda
;; buffer."
;;   :lighter " OrgAgendaFollow"
;;   (if (not (eq major-mode 'calendar-mode))
;;       (message "Cannot activate th-org-agenda-follow-calendar-mode in %s." major-mode)
;;       (if th-org-agenda-follow-calendar-mode
;;           (add-hook 'calendar-move-hook 'th-calendar-open-agenda)
;;           (remove-hook 'calendar-move-hook 'th-calendar-open-agenda))))

;; (add-hook 'calendar-mode-hook 'th-org-agenda-follow-calendar-mode)

;;; Another thing I added to calendar is the display of the week-of-year in the mode-line.
;; (add-to-list 'calendar-mode-line-format
;;              '(let ((day (nth 1 date))
;;                     (month (nth 0 date))
;;                     (year (nth 2 date)))
;;                (format-time-string "Week of year: %V"
;;                 (encode-time 1 1 1 day month year))))

;;; Using other modes in org-mode --------------------------------------------------------------
;; (load "~/.emacs.d/nxhtml/autostart.el")
;; (eval-after-load "mumamo"
;;   '(progn
;;     (set-face-background 'mumamo-background-chunk-major nil)
;;     (set-face-background 'mumamo-background-chunk-submode "#222222")

;;     (defvar mumamo-org-modes
;;       '(ruby-mode emacs-lisp-mode c-mode)
;;       "Modes to include in org-files")

;;     (defvar mumamo-org-chunk-functions
;;       (mapcar
;;        (lambda (mode)
;;          (eval `(defun ,(intern (format "mumamo-chunk-org-%S" mode)) (pos min max)
;;                   ,(format "%s support inside org BEGIN END blocks" mode)
;;                   (mumamo-quick-static-chunk
;;                    pos min max ,(format "#+BEGIN %S" mode) ,(format "#+END %S" mode)
;;                    t (quote ,mode) nil))))
;;        mumamo-org-modes)
;;       "The automatically defined mumamo-chunk-org-* functions for use
;; cramming other modes into org-mode.  See `mumamo-org-modes'
;; `mumamo-quick-static-chunk'.")

;;     (eval `(define-mumamo-multi-major-mode org-mumamo-mode
;;             ,(format "Turn on multiple major modes with main major mode org-mode.\n\n%s"
;;                      (mapconcat (lambda (el) (format "- %S" el))
;;                                 mumamo-org-modes "\n"))
;;             ("Org Source Blocks Family" org-mode ,mumamo-org-chunk-functions)))))
;;;-------------------------------------------------------------------------------------------




;; To be remove
;; (defadvice org-indent-line-function (around stop-indent-in-src-block first act)
;;   "Stop indent when current line is in SRC block"
;;   (unless (org-edit-src-find-region-and-lang)
;;     ad-do-it))

;; (setq message-mode-hook
;;       (quote (orgstruct++-mode
;;               (lambda nil (setq fill-column 72) (flyspell-mode 1))
;;               turn-on-auto-fill
;;               bbdb-define-all-aliases)))

(provide '37org-mode)

;; Local Variables:
;; outline-regexp: ";;; "
;; mode: outline-minor
;; End:
