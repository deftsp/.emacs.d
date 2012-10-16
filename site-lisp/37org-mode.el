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

(setq org-todo-interpretation 'sequence ; or 'type
      org-use-fast-todo-selection t)

(setq org-todo-keywords '((sequence "TODO(t)" "STARTED(s)" "DELEGATED(l)" "APPT(a)" "|"
                           "DONE(d)" "DEFERRED(f)" "CANCELLED(c@)")
                          (sequence "WAITING(w@/!)" "SOMEDAY(S!)" "PROJECT(P@)" "OPEN(O@)" "|" "CANCELLED(c@/!)")
                          (sequence "REPORT(r)" "BUG(b)" "KNOWNCAUSE(k)" "|" "FIXED(f)")
                          (sequence "QUOTE(q!)" "QUOTED(Q!)" "|" "APPROVED(A@)" "EXPIRED(E@)" "REJECTED(R@)")))


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
(setq org-mobile-inbox-for-pull "~/proj/org/from-mobile.org")
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

;;;; Capture
(define-key global-map "\C-cc" 'org-capture)
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/proj/org/GTD.org" "Tasks")
         "* TODO %?\n  %i%u")
        ("j" "Journal" entry (file+datetree "~/proj/org/journal.org")
         "* %?\nEntered on %U\n  %i\n  %a")))

;;; work with appt
;; (org-agenda-to-appt)
(defun pl/org-agenda-to-appt ()
  ;; Dangerous!!! do not use `appt-add', this might remove entries added by `appt-add' manually.
  (org-agenda-to-appt t "TODO"))

;; update appt each time agenda opened
(add-hook 'org-agenda-finalize-hook 'org-agenda-to-appt)

;; use grow to notification
(defun pl/grow-appt-display (min-to-app new-time msg)
  (growl (format "Appointment in %s minute(s)" min-to-app) msg t))

(if (eq system-type 'darwin)
    (setq appt-disp-window-function (function pl/grow-appt-display))
    (setq appt-disp-window-function (function appt-disp-window)))



;;;
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

;;; link
(setq org-link-abbrev-alist
      '(("google"   . "http://www.google.com/search?q=")
        ("baidu"    . "http://www.baidu.com/s?wd=")))

;; keybinding conflicts with icicles keys
;; (org-defkey org-mode-map (kbd "C-c C-'") 'org-edit-special)
;; (define-key org-exit-edit-mode-map (kbd "C-c C-'") 'org-edit-src-exit)

;; (org-defkey org-mode-map (kbd "H-,") 'org-shiftup)
;; (org-defkey org-mode-map (kbd "H-.") 'org-shiftdown)

;;; Structure editing key bindind
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


(provide '37org-mode)

;; Local Variables:
;; outline-regexp: ";;; "
;; mode: outline-minor
;; End:
