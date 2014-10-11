;;; 37org-mode.el ---

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
      org-todo-keywords '((sequence "TODO(t)" "STARTED(s)" "DELEGATED(l)" "APPT(a)" "|" "DONE(d)" "DEFERRED(f)" "CANCELLED(c@)")
                          (sequence "WAITING(w@/!)" "SOMEDAY(S!)" "PROJECT(P@)" "OPEN(O@)" "|" "CANCELLED(c@/!)")
                          (sequence "REPORT(r)" "BUG(b)" "KNOWNCAUSE(k)" "|" "FIXED(f)")
                          (sequence "QUOTE(q!)" "QUOTED(Q!)" "|" "APPROVED(A@)" "EXPIRED(E@)" "REJECTED(R@)"))
      org-todo-keyword-faces (quote (("TODO"      . (:foreground "red"          :weight bold))
                                     ("STARTED"   . (:foreground "hot pink"     :weight bold))
                                     ("DONE"      . (:foreground "forest green" :weight bold :strike-through t))
                                     ("WAITING"   . (:foreground "orange"       :weight bold))
                                     ("SOMEDAY"   . (:foreground "magenta"      :weight bold))
                                     ("CANCELLED" . (:foreground "forest green" :weight bold))
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

        ("g" "GeekTool Agenda" ((agenda ""))
         ((org-agenda-todo-keyword-format "%-11s")
          (org-agenda-prefix-format "  %-10T%?-16t% s")
          (org-agenda-show-inherited-tags nil)
          (org-agenda-remove-tags 'prefix)
          (org-agenda-tags-column 70))
         (,(concat org-directory  "/Agenda.txt")))

        ("h" "Daily habits"
         ((agenda ""))
         ((org-agenda-show-log t)
          (org-agenda-span 'week)
          (org-agenda-log-mode-items '(state))
          (org-agenda-skip-function '(org-agenda-skip-entry-if 'notregexp ":daily:"))))

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
      org-agenda-todo-list-sublevels t
      ;; I use the diary only for sexp entries and holidays, and Ι have put them into org file.
      org-agenda-include-diary nil
      ;; agenda view always starts out by showing me the next seven days.
      org-agenda-start-on-weekday nil
      org-adapt-indentation t
      org-fast-tag-selection-single-key (quote expert)
      org-reverse-note-order t
      org-deadline-warning-days 7
      org-return-follows-link t
      org-startup-folded t
      org-startup-truncated t
      org-display-internal-link-with-indirect-buffer nil)

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
(setq org-archive-location "%s_archive::")


;;; org-habit
(setq org-habit-preceding-days 21
      org-habit-following-days 7
      org-habit-graph-column 72)

;;; org-expiry
(setq org-expiry-inactive-timestamps t)

;;; refile
(setq org-refile-use-outline-path nil
      org-refile-allow-creating-parent-nodes 'confirm
      ;; org-refile-targets '((org-agenda-files . (:maxlevel . 6)))
      org-blank-before-new-entry nil
      org-refile-use-cache nil)

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
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/org/GTD.org" "Inbox")
         "* TODO %?\n  %i%u"
         :kill-buffer t)
        ("j" "Journal" entry (file+datetree "~/org/journal.org")
         "* %?\n  %U\n"
         :kill-buffer t)
        ("J" "Journal with Annotation" entry (file+datetree "~/org/journal.org")
         "* %?\n  %U\n  %i\n  %a"
         :kill-buffer t)
        ("m" "Memo" plain (file (concat org-directory (format-time-string "%Y%m%d-%H%M%S.org")))
         "* MEMO <%<%Y-%m-%d>> %?\n   %i\n  %a\n\n"
         :prepend t
         :unnarrowed t
         :kill-buffer t)))


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
        (if (called-interactively-p 'interactive)
            (call-interactively 'org-agenda-list)
          (save-window-excursion
            (call-interactively 'org-agenda-list) (recursive-edit)))))))

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

;;; give us some hint we are running
(defadvice org-babel-execute-src-block (around progress nil activate)
  (set-face-attribute
   'org-block-background nil :background "LightSteelBlue")
  (message "Running your code block")
  ad-do-it
  (set-face-attribute 'org-block-background nil :background "gray")
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
      org-drill-maximum-items-per-session 40
      org-drill-maximum-duration 30   ; 30 minutes
      org-drill-scope 'file ; use `org-drill-directory' to drill whole directory
      org-drill-leech-method 'warn
      org-drill-sm5-initial-interval 4.0
      org-drill-adjust-intervals-for-early-and-late-repetitions-p t
      org-drill-add-random-noise-to-intervals-p t)


(provide '37org-mode)

;; Local Variables:
;; outline-regexp: ";;; "
;; mode: outline-minor
;; End:
