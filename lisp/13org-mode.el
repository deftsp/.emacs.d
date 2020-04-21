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
          org-hide-leading-stars nil
          org-hide-emphasis-markers nil ; hide the emphasis markup (e.g. /.../ for italics, *...* for bold, etc.)
          ;; indentation does not play well with version control system,
          ;; moreover, it force me to press TAB key too much.
          org-adapt-indentation nil
          org-log-done 'time
          org-log-reschedule 'time
          org-log-redeadline 'time
          org-log-refile nil ; per-file-basis "#+STARTUP: logrefile"
          org-log-into-drawer "LOGBOOK"
          org-startup-with-inline-images t
          org-image-actual-width nil
          org-ctrl-k-protect-subtree t ; give a query for delete
          ;; add more org level face
          ;; org-n-level-faces at most 8, if less than 8, then level-1 face gets
          ;; re-used level N+1 etc.
          ;; (add-to-list 'org-level-faces 'tl/org-level-9)
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
          org-imenu-depth 8
          org-blank-before-new-entry '((heading . auto) (plain-list-item . auto))
          org-time-stamp-rounding-minutes (quote (0 5))
          org-pretty-entities nil ; use pretty things for the clocktable
          org-pretty-entities-include-sub-superscripts t
          org-src-fontify-natively t
          org-enforce-todo-dependencies t
          org-enforce-todo-checkbox-dependencies t
          ;; If there's a region, do some commands it is I'm trying to do to ALL
          ;; headlines in region, provided that these headlines are of the same
          ;; level than the first one
          org-loop-over-headlines-in-active-region 'start-level
          org-display-internal-link-with-indirect-buffer nil)
    (setq org-goto-interface 'outline-path-completion
          org-goto-max-level 10)
    (setq org-script-display
          '(((raise -0.3)
             (height 0.7)
             (:foreground "yellow"))
            ((raise 0.3)
             (height 0.7)
             (:foreground "yellow"))
            ((raise -0.5))
            ((raise 0.5))))
    (setq org-stuck-projects
          '("+LEVEL=2+CATEGORY={Task\\|Project}/-DONE-CANCELLED"
            ("TODO" "NEXT")
            nil))
    (when window-system
      ;; … ↴, ⬎, ⤷, ⤵, ▼ and ⋱.
      (setq org-ellipsis " ◦◦◦ "))

    (when (fboundp 'tl/terminal-notification)
      (setq org-show-notification-handler
            (lambda (msg) (tl/terminal-notification nil msg))))

    ;; global Effort estimate values
    (setq org-global-properties
          '(("Effort_ALL" .
             "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 0:00")))
    ;;        1    2    3    4    5    6    7    8    9    0
    ;; These are the hotkeys ^^

    ;; Set default column view headings: Task Priority Effort Clock_Summary
    (setq org-columns-default-format "%50ITEM(Task) %2PRIORITY %10Effort(Effort){:} %10CLOCKSUM")
    (setq org-use-fast-todo-selection t
          org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "DELEGATED(l)" "APPT(a)" "|" "DONE(d)" "DEFERRED(f)" "CANCELLED(c@)" "FAILED(X@/!)")
                              (sequence "WAITING(w@/!)" "HOLD(h@/!)" "Urgent(u!)" "SOMEDAY(S!)" "PROJECT(P@)" "OPEN(O@)" "|" "CANCELLED(c@/!)")
                              ;; if 'q' is not used as shortcut, press 'q' like 'C-g'
                              ;; (sequence "QUOTE(q!)" "QUOTED(Q!)" "|" "APPROVED(A@)" "EXPIRED(E@)" "REJECTED(R@)")
                              (sequence "REPORT(r)" "BUG(b!)" "ISSUE(i!)" "ERROR(e!)" "FEATURE(f!)" "KNOWNCAUSE(k)" "|" "FIXED(f)"))
          org-todo-keyword-faces (quote (("TODO"      . (:foreground "red"          :weight bold))
                                         ("NEXT"      . (:foreground "#d33682"      :weight bold))
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
          org-group-tags t
          org-tag-persistent-alist '((:startgroup . nil) ("@office" . ?o) ("@home" . ?h) ("@shopping" . ?s) ("@tennisclub" . ?t) (:endgroup . nil)
                                     (:startgroup . nil) ("online" . ?O) ("offline" . ?F) (:endgroup . nil)
                                     (:startgroup . nil) ("business" . ?B) ("personal" . ?P) (:endgroup . nil)

                                     ;; Knowledge aspects
                                     (:startgroup . nil)
                                     ("knowledge" . nil)
                                     (:grouptags . nil)
                                     ("thought" . nil)
                                     ("strategies")
                                     ("science") ("engineering") ("finance") ("business") ("economy")
                                     ("math") ("history") ("politics") ("society") ("philosophy")
                                     ("psychology") ("literature") ("medicine")
                                     (:endgroup . nil)

                                     (:startgroup . nil)
                                     ("programming")
                                     (:grouptags . nil)
                                     ("script")
                                     ("linux") ("macos") ("bsd") ("windows")
                                     ("emacs" . ?e)
                                     ("regexp")
                                     ("Git" . ?g)
                                     (:endgroup . nil)

                                     ("project" . ?p)
                                     ("hacking" . ?H)
                                     ("reading")
                                     ("exercise". ?E)
                                     ("mail"    . ?M)
                                     ("movie"   . nil)
                                     ("misc"    . ?m)
                                     ("reading" . ?r)

                                     ;; Programming Languages
                                     (:startgroup . nil)
                                     ("programming-languages" . ?L)
                                     (:grouptags . nil)
                                     ("shell") ("bash") ("zsh")
                                     ("lisp") ("common-lisp") ("emacs-lisp") ("guile") ("scheme")
                                     ("haskell" . ?L)
                                     ("ruby") ("python") ("perl") ("php") ("erlang")
                                     ("c") ("c++") ("go") ("lua") ("rust")
                                     ("assembly") ("gas") ("nasm") ("intel") ("att")
                                     ("r") ("processing")
                                     ("database") ("sql") ("nosql") ("newsql") ("postgresql")
                                     ("xml") ("json") ("mathml")
                                     ("octave") ("matlab")
                                     ("html") ("html5") ("css") ("css3")
                                     ("javascript") ("coffeescript") ("dart")
                                     ("ocaml") ("scala") ("verilog") ("julia")
                                     (:endgroup . nil))
          org-tag-alist '((:startgroup . nil)
                          ("types")
                          (:grouptags . nil)
                          ;; types
                          ("wiki") ("org-mode") ("idea")
                          ("appointment" . ?a) ("meeting" . ?m)
                          ;; time
                          ("tomorrow" . ?t) ("future" . ?f)
                          ;; places
                          ("company" . ?N) ("home" . ?H) ("computer" . ?C) ("phone" . ?P)
                          (:endgroup . nil)

                          ;; Work
                          (:startgroup . nil)
                          ("work" . ?w)
                          (:grouptags . nil)
                          ;; Green Town
                          ("company" . nil)
                          (:endgroup . nil))

          org-tag-faces '(("wiki" :foreground "green yellow")
                          ("org" :foreground "green yellow")
                          ("computer" :foreground "green" :background "black")
                          ("life" :foreground "black" :background "DimGray")
                          ("program" :foreground "lawn green" :weight bold)
                          ("linux" :foreground "yellow" :weight bold)
                          ("mac" :foreground "#444444" :background "black" :weight bold)
                          ("emacs" :foreground "orange" :weight bold)
                          ("vim" :foreground "green" :weight bold)
                          ("reading" :foreground "green" :weight bold)
                          ("haskell" :foreground "violet" :weight bold)
                          ("lisp" :foreground "deep pink" :weight bold)
                          ("ruby" :foreground "red" :weight bold)
                          ("python" :foreground "yellow" :weight bold)
                          ("c" :foreground "gold" :weight bold)
                          ("c++" :foreground "gold" :weight bold)
                          ("go" :foreground "gold" :weight bold)
                          ("bash" :foreground "sea green")
                          ("zsh" :foreground "sea green" :weight bold))))
  :config
  (progn
    (use-package 50calendar)
    (when (eq system-type 'darwin)
      (add-to-list 'org-modules 'org-mac-link))
    (add-to-list 'org-modules 'org-habit)
    (add-to-list 'org-modules 'org-expiry)
    (add-to-list 'org-modules 'org-mouse)
    (add-to-list 'org-modules 'org-annotate-file)
    (add-to-list 'org-modules 'org-info)
    (add-to-list 'org-modules 'org-id)
    (add-to-list 'org-modules 'ol-man)
    (add-to-list 'org-modules 'org-eval)
    (add-to-list 'org-modules 'org-panel)
    (add-to-list 'org-modules 'org-expiry)
    (add-to-list 'org-modules 'org-toc)
    (add-to-list 'org-modules 'org-tempo) ;; activate old template expansion mechanism like <s
    ;; A repeating task with subitems as checkboxes. Set property
    ;; RESET_CHECK_BOXES on the task to t, When the task is completed, all the
    ;; checkboxes on the subitems should be cleared - so the task can be done at
    ;; next cyclic time.
    (add-to-list 'org-modules 'org-checklist)
    ;; comment out org-depend, which will cause the org-mode buffer to modified
    ;; status after open/refresh org-agenda, even no changes happen
    ;; (add-to-list 'org-modules 'org-depend)
    ;; https://github.com/Somelauw/evil-org-mode/blob/master/doc/keythemes.org

    (use-package evil-org
      :diminish evil-org-mode
      :after org
      :init
      (progn
        (add-hook 'org-mode-hook 'tl//evil-org-mode))
      :config
      (progn
        (evil-org-set-key-theme '(navigation textobjects todo additional))))

    (use-package org-download
      :config
      (org-download-enable))

    (use-package org-num
      :diminish org-num-mode)

    (with-eval-after-load 'counsel
      ;; C-m (ivy-done) exit with the currently selected candidate
      ;; C-M-m (ivy-call)  select the current candidate and remain in the completion session.
      ;; Typing C-M-m (ivy-call) over an already selected candidate removes it from the list of selected tags.
      ;; At any point,you can exit completion with the current list of selected
      ;; tags by typing C-M-j (ivy-immediate-done).
      (global-set-key [remap org-agenda-set-tags] #'counsel-org-tag-agenda)
      (global-set-key [remap org-set-tags-command] #'counsel-org-tag))

    (defun tl/org-mode-init ()
      (semantic-mode -1)
      (org-num-mode  +1)
      (display-line-numbers-mode -1))

    (add-to-list 'org-mode-hook 'tl/org-mode-init)

    (org-defkey org-mode-map (kbd "C-c C-x t") 'tl/org-clock-summary-today-by-tags)
    ;; Undefine C-c [ and C-c ] since this breaks my
    ;; org-agenda files when directories are include It
    ;; expands the files in the directories individually
    (org-defkey org-mode-map (kbd "C-c [") 'undefined)
    (org-defkey org-mode-map (kbd "C-c ]") 'undefined)

    (define-key org-mode-map (kbd "s-j") #'org-babel-next-src-block)
    (define-key org-mode-map (kbd "s-k") #'org-babel-previous-src-block)

    (org-defkey org-mode-map (kbd "s-b") org-babel-map)

    ;; TODO: update it, in Org 9.2 the format was changed from something like
    ;; ("s" "#+BEGIN_SRC ?\n#+END_SRC") to something like ("s" . "src")
    ;; (add-to-list 'org-structure-template-alist
    ;;              '("p" ":PROPERTIES:\n?\n:END:"))
    ;; (add-to-list 'org-structure-template-alist
    ;;              `("eh" ,(concat ":EXPORT_FILE_NAME: ?\n"
    ;;                              ":EXPORT_TITLE:\n"
    ;;                              ":EXPORT_OPTIONS: toc:nil html-postamble:nil num:nil")))


    (defmacro tl|org-emphasize (fname char)
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
      (tl/declare-prefix-for-mode 'org-mode (car prefix) (cdr prefix)))
    (tl/set-leader-keys-for-major-mode 'org-mode
      "C-l" 'tl/reflash-indentation

      "'" 'org-edit-special
      "c" 'org-capture
      "Cc" 'org-clock-cancel
      "Ci" 'org-clock-in
      "Co" 'org-clock-out
      "Cr" 'org-resolve-clocks
      "Cd" 'org-clock-display           ; C-c C-x C-d
      "Cz" 'tl/org-insert-clock-range

      "dd" 'org-deadline
      "ds" 'org-schedule
      "dt" 'org-time-stamp
      "dT" 'org-time-stamp-inactive
      "ee" 'org-export-dispatch

      "a" 'org-agenda

      "C-c C-t" 'org-todo-yesterday

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

      ;; other
      "ori" 'org-redisplay-inline-images

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
      (or dottl-major-mode-leader-key ",") 'org-ctrl-c-ctrl-c
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
      "ii" 'tl/org-insert-image
      "iK" 'tl/insert-keybinding-org
      "il" 'org-insert-link
      "ip" 'org-set-property
      "is" 'org-insert-subheading
      "it" 'org-set-tags

      ;; region manipulation
      "xb" (tl|org-emphasize tl/org-bold ?*)
      "xc" (tl|org-emphasize tl/org-code ?~)
      "xi" (tl|org-emphasize tl/org-italic ?/)
      "xo" 'org-open-at-point
      "xr" (tl|org-emphasize tl/org-clear ? )
      "xs" (tl|org-emphasize tl/org-strike-through ?+)
      "xu" (tl|org-emphasize tl/org-underline ?_)
      "xv" (tl|org-emphasize tl/org-verbose ?=)

      "px" 'org-publish
      "pp" 'org-publish-current-project
      "pf" 'org-publish-current-file
      "pa" 'org-publish-all)

    ;; Add global evil-leader mappings. Used to access org-agenda
    ;; functionalities – and a few others commands – from any other mode.
    (tl/declare-prefix "ao" "org")
    (tl/declare-prefix "aok" "clock")
    (tl/set-leader-keys
      ;; org-agenda
      "ao#" 'org-agenda-list-stuck-projects
      "ao/" 'org-occur-in-agenda-files
      "aoa" 'org-agenda-list
      "aoc" 'org-capture
      "aoe" 'org-store-agenda-views
      "aoi" 'org-insert-link-global
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
    ;; `dottl-major-mode-emacs-leader-key' to `C-c' and the key binding
    ;; C-c ' is shadowed by `spacemacs/default-pop-shell', effectively making
    ;; the Emacs user unable to exit src block editing.
    (define-key org-src-mode-map
      (kbd (concat dottl-major-mode-emacs-leader-key " '"))
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


(defun tl/org-insert-clock-range ()
  (interactive)
  (org-clock-in nil (apply 'encode-time (org-parse-time-string (org-read-date))))
  (org-resolve-clocks))

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

(defun tl/set-org-agenda-custom-commands ()
  ;; Use tags-todo like this (tags-todo "+PRIORITY={A}") will be very slow,
  ;; however add more restricts, like CATEGORY, it works fine

  ;; http://orgmode.org/manual/Matching-tags-and-properties.html
  ;; https://blog.aaronbieber.com/2016/09/24/an-agenda-for-life-with-org-mode.html
  (setq org-agenda-custom-commands
        `((" " "Daily agenda and all TODOs"
           ((agenda "" ((org-agenda-span 'day)
                        (org-agenda-sorting-strategy '(habit-down
                                                       time-up
                                                       priority-down
                                                       todo-state-down
                                                       effort-up
                                                       category-keep))))
            (tags-todo "TODO=\"NEXT\""
                       ((org-agenda-sorting-strategy '(priority-down tag-up))
                        (org-agenda-overriding-header "NEXT Tasks:")))
            (tags "+PRIORITY=\"A\"+CATEGORY={Inbox\\|Task\\|Project}"
                  ((org-agenda-skip-function
                    '(org-agenda-skip-entry-if 'todo 'done))

                   (org-agenda-sorting-strategy '(tag-up priority-down))
                   ;; (org-agenda-todo-keyword-format "")
                   (org-agenda-overriding-header "High Priority Unfinished Tasks:")))

            (alltodo ""
                     ((org-agenda-skip-function '(or (tl/org-agenda-skip-subtree-if-habit)
                                                     (tl/org-agenda-skip-subtree-if-priority ?A)
                                                     (tl/org-agenda-skip-if-blocked)
                                                     (org-agenda-skip-entry-if 'regexp "\\* NEXT")
                                                     (org-agenda-skip-entry-if 'scheduled 'deadline)))
                      (org-agenda-overriding-header "ALL Normal Priority Tasks:")
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


;; https://www.reddit.com/r/orgmode/comments/6ybjjw/aligned_agenda_view_anyway_to_make_this_more/
(use-package org-agenda
  :after (org)
  :init
  (progn
    (setq org-agenda-files `(,(concat org-directory "/agenda"))
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
          org-agenda-use-time-grid nil
          org-agenda-block-separator ?▰
          org-agenda-dim-blocked-tasks t ; 'invisible
          org-agenda-window-frame-fractions '(0.6 . 0.85) ; the min and max height of the agenda window as a fraction of frame height.
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
            (agenda  . "  %-13:c%?-12t% s")
		    ;; (timeline . "%-9:T%?-2t% s")   ; timeline
            (todo . " %i %-13:c")           ; todo, alltodo
            (tags . " %i %-13:c")           ; tags, tags-todo, stuck
            (search . " %i %-13:c")))       ; search

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
    (tl/set-org-agenda-custom-commands))
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
    (org-defkey org-agenda-mode-map "|" nil) ;'org-agenda-filter-remove-all
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

    (with-eval-after-load "evil-evilified-state"
      (evilified-state-evilify-map org-agenda-mode-map
        :mode org-agenda-mode
        :bindings
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
        (kbd "[") 'org-agenda-earlier ; default to 'org-agenda-manipulate-query-add
        (kbd "]") 'org-agenda-later ; default to 'org-agenda-manipulate-query-subtract

        ;; manipulation
        ;; We follow standard org-mode bindings (not org-agenda bindings):
        ;; <HJKL> change todo items and priorities.
        ;; M-<jk> drag lines.
        ;; M-<hl> cannot demote/promote, we use it for "do-date".
        "J" 'org-agenda-priority-down ; default bind to 'org-agenda-clock-goto
        "K" 'org-agenda-priority-up
        "H" 'org-agenda-do-date-earlier
        "L" 'org-agenda-do-date-later
        (kbd "M-j") 'org-agenda-drag-line-forward
        (kbd "M-k") 'org-agenda-drag-line-backward
        (kbd "C-S-h") 'org-agenda-todo-previousset ; Original binding "C-S-<left>"
        (kbd "C-S-l") 'org-agenda-todo-nextset ; Original binding "C-S-<right>"

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
        "C" 'tl/org-agenda-capture ; default to 'org-agenda-convert-date

        ;; mark
        "m" 'org-agenda-bulk-toggle
        "~" 'org-agenda-bulk-toggle-all ; default bind to 'org-agenda-limit-interactively
        "*" 'org-agenda-bulk-mark-all
        "%" 'org-agenda-bulk-mark-regexp
        "M" 'org-agenda-bulk-unmark-all  ; default bind to 'org-agenda-phases-of-moon
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
        "I" 'org-agenda-clock-in ; Original binding
        "O" 'org-agenda-clock-out ; Original binding
        "cg" 'org-agenda-clock-goto
        "cc" 'org-agenda-clock-cancel
        "cr" 'org-agenda-clockreport-mode

        ;; go and show
        "." 'org-agenda-goto-today ; TODO: What about evil-repeat?
        "gc" 'org-agenda-goto-calendar
        "gC" 'org-agenda-convert-date
        "gd" 'org-agenda-goto-date
        "gh" 'org-agenda-holidays
        "gm" 'org-agenda-phases-of-moon
        "gs" 'org-agenda-sunrise-sunset
        "gt" 'org-agenda-show-tags

        "p" 'org-agenda-date-prompt
        "P" 'org-agenda-show-the-flagging-note

        ;; 'org-save-all-org-buffers ; Original binding "C-x C-s"

        ;; Others
        "+" 'org-agenda-manipulate-query-add
        "-" 'org-agenda-manipulate-query-subtract
        (kbd "M-SPC") 'tl/org-agenda/body))

    (dolist (prefix '(("mC" . "clocks")
                      ("md" . "dates")
                      ("mi" . "insert")
                      ("ms" . "trees/subtrees")))
      (tl/declare-prefix-for-mode 'org-agenda-mode
        (car prefix) (cdr prefix)))

    (tl/set-leader-keys-for-major-mode 'org-agenda-mode
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

;;; org-id
;; https://stackoverflow.com/questions/13340616/assign-ids-to-every-entry-in-org-mode
;; (add-hook 'org-capture-prepare-finalize-hook 'org-id-get-create)

(setq org-id-search-archives nil
      org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)

;; Base on https://writequit.org/articles/emacs-org-mode-generate-ids.html
(defun tl/org-id-get-create-in-buffer (arg)
  "Add ID properties to all headlines in the current
buffer which do not already have one. When `arg' nil only adds ids if the
`auto-id' option is set to `t' in the file somewhere. ie,
#+OPTIONS: auto-id:t"
  (interactive "P")
  (if (and arg (>= arg 4))
      (org-map-entries 'org-id-get-create)
    (save-excursion
      (widen)
      (goto-char (point-min))
      (when (re-search-forward "^#\\+OPTIONS:.*auto-id:t\\b" (point-max) t)
        (org-map-entries 'org-id-get-create)))))

(defun tl//org-mode-save-try-add-id-hook ()
  (when (and (eq major-mode 'org-mode)
             (eq buffer-read-only nil))
    (tl/org-id-get-create-in-buffer nil)))

(add-hook 'before-save-hook 'tl//org-mode-save-try-add-id-hook)

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
(defun tl/org-archive-if (condition-function)
  (if (funcall condition-function)
      (let ((next-point-marker
             (save-excursion (org-forward-heading-same-level 1) (point-marker))))
        (org-archive-subtree)
        (setq org-map-continue-from (marker-position next-point-marker)))))

(defun tl//org-archive-if-completed ()
  (tl/org-archive-if 'org-entry-is-done-p))

(defun tl/org-archive-completed-in-buffer ()
  "Move all of the done subtrees in current buffer to the archive."
  (interactive)
  (org-map-entries 'tl//org-archive-if-completed))


;;; Priority
;; Note: `org-priority-faces' default to `nil' and if it is `nil' when mouse
;; over the agenda item, it cause tons of Invalid face reference: nil in
;; *Message* buffer. see also `org-agenda-fontify-priorities'
;; The entry default to B when no priority specified
(setq org-priority-faces
      '((?A . (:foreground "#ff40ff" :weight bold))
        (?B . (:foreground "#00f900" :weight bold))
        (?C . (:foreground "#fefb00" :weight bold))))

(use-package org-fancy-priorities
  :diminish
  :after org
  :defer t
  :defines org-fancy-priorities-list
  :hook (org-mode . org-fancy-priorities-mode)
  :config
  ;; (char-displayable-p ?❗)
  (setq org-fancy-priorities-list '((?A . "⬆")
                                    (?B . "·")
                                    (?C . "⬇")
                                    (?D . "☕")
                                    (?1 . "·")
                                    (?2 . "⮬")
                                    (?3 . "⮮")
                                    (?4 . "☕")
                                    (?I . "Important"))))


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
      org-refile-targets `((nil :maxlevel . 9) ; nil means consider headings in the current buffer
                           (org-agenda-files :maxlevel . 9)
                           (org-brain-files :maxlevel . 12)
                           (,(concat org-directory "/SomeDay.org") :maxlevel . 9))
      org-refile-use-cache nil)


;; Exclude DONE state tasks from refile targets
;; http://doc.norang.ca/org-mode.html
;; (defun tl/verify-refile-target ()
;;   "Exclude todo keywords with a done state from refile targets"
;;   (not (member (nth 2 (org-heading-components)) org-done-keywords)))

;; (setq org-refile-target-verify-function 'tl/verify-refile-target)

;; Refile settings
;; Exclude DONE state tasks from refile targets
(defun tl/verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets"
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))

(setq org-refile-target-verify-function 'tl/verify-refile-target)


;;; for MobileOrg
;; Set to the name of the file where new notes will be stored
(with-eval-after-load "org"
  (setq org-mobile-inbox-for-pull (concat org-directory  "/from-mobile.org")
        org-mobile-directory "~/Dropbox/Apps/MobileOrg" ; Set to <your Dropbox root directory>/MobileOrg.
        org-mobile-use-encryption nil
        org-mobile-encryption-password ""))

;;;; Capture
(use-package org-capture
  :defer 5
  :init
  (progn
    (define-key global-map (kbd "C-c c") 'org-capture)
    ;; https://lists.gnu.org/archive/html/emacs-orgmode/2010-08/msg00469.html
    (defun tl/find-today-trading-journal ()
      "Find today's trading journal."
      (let ((p (concat org-directory
                       (format-time-string
                        "/trading-journal/%Y-%m-%d.org"))))
        (find-file p)
        (goto-char (point-min))))

    (defun tl/find-memo-file ()
      "Find today's trading journal."
      (let ((p (concat org-directory
                       (format-time-string
                        "/Memo/%Y%m%d.org"))))
        (find-file p)
        (goto-char (point-min))))

    (defun org-hugo-new-subtree-post-capture-template ()
      "Returns `org-capture' template string for new Hugo post.
     See `org-capture-templates' for more information."
      (let* ((title (read-from-minibuffer "Post Title: ")) ;Prompt to enter the post title
             (date (format-time-string (org-time-stamp-format :long :inactive) (org-current-time)))
             (fname (org-hugo-slug title)))
        (mapconcat #'identity
                   `(
                     ,(concat "* TODO " title "     :@essay:")
                     ":PROPERTIES:"
                     ,(concat ":EXPORT_FILE_NAME: " fname)
                     ":END:"
                     "%?\n")          ;Place the cursor here finally
                   "\n")))

    (defun org-journal-find-location ()
      ;; Open today's journal, but specify a non-nil prefix argument in order to
      ;; inhibit inserting the heading; org-capture will insert the heading.
      (org-journal-new-entry t)
      ;; Position point on the journal's top-level heading so that org-capture
      ;; will add the new entry as a child entry.
      (goto-char (point-min)))

    (setq org-capture-templates
          `(("a" "Add Task" entry
             (file+headline "~/org/agenda/GTD.org" "Tasks")
             "* TODO %?\nSCHEDULED: %t \n:PROPERTIES:\n:ID: %(org-id-new)\n:CREATED:  %U\n:END:"
             :prepend t
             :empty-lines-after 1)

            ("t" "Todo" entry (file+headline "~/org/agenda/GTD.org" "Inbox")
             "* TODO %?\n:PROPERTIES:\n:ID: %(org-id-new)\n:CREATED:  %U\n:END:"
             :prepend t
             :empty-lines-after 1)
            ("T" "Trading Journal" plain (function tl/find-today-trading-journal)
             "* %U\n%i%?"
             :prepend t
             :unnarrowed nil
             :kill-buffer t
             :empty-lines-after 1)
            ;; use org-journal now
            ;; http://www.howardism.org/Technical/Emacs/journaling-org.html
            ;; ("j" "Journal" entry (file+olp+datetree "~/org/journal.org")
            ;;  "* %?\n%i\n%U"
            ;;  :kill-buffer t
            ;;  :empty-lines-after 1)
            ;; ("J" "Journal with Annotation" entry (file+olp+datetree "~/org/journal.org")
            ;;  "* %?\n%U\n%i\n%a"
            ;;  :kill-buffer t
            ;;  :empty-lines-after 1)
            ("j" "Journal entry" entry (function org-journal-find-location)
             "* %(format-time-string org-journal-time-format)%^{Title}\n%i%?")
            ("m" "Memo" plain #'tl/find-memo-file
             "* %T\n%?\n%a\n\n"
             :prepend t
             :unnarrowed t
             :kill-buffer t
             :empty-lines-after 1)
            ;; ("p" "Phone call" entry (file+headline "~/org/agenda/GTD.org" "Inbox")
            ;;  "* PHONE %? :PHONE:\n:PROPERTIES:\n:ID: %(org-id-new)\n:CREATED:  %U\n:END:"
            ;;  :clock-in t
            ;;  :clock-resume t
            ;;  :empty-lines-after 1)
            ;; work with org-protocol and emacs-mac port have default register to macOS
            ;; https://github.com/sprig/org-capture-extension
            ("p" "Hugo post" entry (file+olp "posts/micro.org" "Posts") (function org-hugo-new-subtree-post-capture-template))
            ("P" "Protocol Clip" entry (file+headline ,(concat org-directory "/Notes.org") "Inbox")
             "* %?\n:PROPERTIES:\n:ID: %(org-id-new)\n:CREATED:  %U\n:END:\nSource: %:annotation\n\n#+BEGIN_QUOTE\n%i\n#+END_QUOTE"
             :empty-lines-after 1)

	        ("L" "Protocol Link" entry (file+headline ,(concat org-directory "/agenda/GTD.org") "Inbox")
             "* TODO Review %?%:annotation\n:PROPERTIES:\n:ID: %(org-id-new)\n:CREATED:  %U\n:END:"
             :prepend t
             :empty-lines-after 1)
            ("r" "Remind" entry (file+headline "~/org/agenda/GTD.org" "Remind")
             "* %?\nSCHEDULED: %(format-time-string \"<%Y-%m-%d .+1d/3d>\")\n:PROPERTIES:\n:ID: %(org-id-new)\n:CREATED:  %U\n:END:"
             :empty-lines-after 1)
            ("h" "Habit" entry (file+headline "~/org/agenda/GTD.org" "Habit")
             "* %?\nSCHEDULED: %(format-time-string \"<%Y-%m-%d .+1d/3d>\")\n:PROPERTIES:\n:ID: %(org-id-new)\n:CREATED:  %U\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:"
             :empty-lines-after 1)
            ("w" "Web page" entry
             (file "~/org/Pages.org")
             "* %a :website:\n:PROPERTIES:\n:ID: %(org-id-new)\n:CREATED:  %U\n:END:\n%?\n\n%:initial")
            ("W" "Review: Weekly Review" entry (file+olp+datetree "~/org/WeeklyReview.org")
             (file "~/org/templates/weekly-review-template.org")))))
  :config
  (progn
    ;; ",k" not work some time, call `evil-normalize-keymaps' to force refresh
    (defun tl//org-capture-mode-init ()
      (evil-insert-state)
      (evil-normalize-keymaps))

    (add-hook 'org-capture-mode-hook 'tl//org-capture-mode-init)

    (defun tl//org-capture-after-finalize-h ()
      (tl/jump-to-org-agenda nil))

    (add-hook 'org-capture-after-finalize-hook 'tl//org-capture-after-finalize-h)

    (tl/set-leader-keys-for-minor-mode 'org-capture-mode
      dottl-major-mode-leader-key 'org-capture-finalize
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
(defun tl/insert-created-timestamp()
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

(use-package org-protocol
  :after (org)
  :config
  (progn
    ;; https://github.com/alphapapa/org-protocol-capture-html
    (use-package org-protocol-capture-html)

    (defun tl/on-select-previous-input-source (data)
      "Swith to insert when switch to Rime input method"
      (let ((source-id (plist-get data :source-id)))
        (when (string-equal source-id "im.rime.inputmethod.Squirrel.Rime")
          (let ((state (bound-and-true-p evil-state)))
            (when (and state
                       (eq state 'normal)
                       (not (minibufferp))
                       (not isearch-mode))
              (call-interactively 'evil-insert))))))


    (defun tl/on-activate ()
      "Switch to other frame, if selected window is org agenda and is the only window"
      (when (> (length (frame-list)) 1)
        (let* ((frame (selected-frame))
               (lst (window-list frame)))
          (when (and (= (length lst) 1)
                     (string= org-agenda-buffer-name
                              (buffer-name (window-buffer (car lst) ))))

            (call-interactively 'other-frame)))))

    ;; open -g 'org-protocol://hammerspoon?action=org-clock-goto'
    (defun org-protocol-hammerspoon (data)
      "Handle event from Hammerspoon"
      (let* ((action (plist-get data :action)))
        (cond ((string= action "org-clock-bar-goto")
               (call-interactively 'org-clock-goto))
              ((string= action "org-clock-bar-refresh")
               (call-interactively 'tl/update-hammerspoon-org-clock-bar))
              ((string= action "org-clock-bar-clock-in-last")
               (call-interactively 'org-clock-in-last))
              ((string= action "org-clock-bar-clock-out")
               (call-interactively 'org-clock-out))
              ((string= action "activate")
               (tl/on-activate))
              ((string= action "select-previous-input-source")
               (tl/on-select-previous-input-source data)))))

    (add-to-list 'org-protocol-protocol-alist
                 '("Handle action from hammerspoon"
                   :protocol "hammerspoon"
                   :function org-protocol-hammerspoon))

    (defun org-protocol-org-anki (data)
      (let* ((body (plist-get data :body)))
        (if (> (length body) 0)
            (org-anki-capture-protocol data)
          (error "The capture body is empty"))))

    (add-to-list 'org-protocol-protocol-alist
                 '("org-anki"
                   :protocol "org-anki"
                   :function org-protocol-org-anki))))


;; Since macOS 10.14, can not send keystroke to Firefox with system event. Use
;; hammerspoon instead
(defun pl/org-as-mac-firefox-get-frontmost-url ()
  (let ((result
	     (do-applescript
	      (concat
	       "set oldClipboard to the clipboard\n"
	       "set frontmostApplication to path to frontmost application\n"
           "do shell script \"/usr/local/bin/hs -c 'ff_url.cp_firefox_frontmost_url_to_clipboard()'\"\n"
           "delay 0.4\n"
	       "set links to the clipboard\n"
	       "set the clipboard to oldClipboard\n"
	       "activate application (frontmostApplication as text)\n"
	       "return links as string\n"))))
    (replace-regexp-in-string
     "^\"\\| - Mozilla Firefox\"$\\|\"$" ""
     (car (split-string result "[\r\n]+" t)))))

(advice-add 'org-as-mac-firefox-get-frontmost-url :override 'pl/org-as-mac-firefox-get-frontmost-url)

;;; clock
(use-package org-clock
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
    (setq org-clock-idle-time nil) ;; 30
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
    (add-hook 'org-clock-before-select-task-hook
              'tl//add-extra-empty-lines-to-clock-task-select-buffer)
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

    (advice-add 'org-clock-update-mode-line :after #'tl/update-hammerspoon-org-clock-bar)
    (advice-remove 'org-clock-update-mode-line #'tl//update-hammerspoon-org-clock-bar)

    (defun tl/org-clock-force-mode-line-update ()
      (setq org-mode-line-string nil)
      (org-clock-update-mode-line)
      (tl/update-hammerspoon-org-clock-bar))

    ;; org-clock-in will call `org-clock-update-mode-line'
    ;; (add-hook 'org-clock-in-hook 'tl/org-clock-force-mode-line-update)

    ;; `org-clock-cancel' and `org-clock-out' remove the `org-mode-line-string'
    ;; from `global-mode-string' then call `force-mode-line-update' instead of
    ;; `org-clock-update-mode-line' which have been advice to update hammerspoon
    ;; org_clock_bar
    (add-hook 'org-clock-cancel-hook 'tl/org-clock-force-mode-line-update)
    (add-hook 'org-clock-out-hook 'tl/org-clock-force-mode-line-update)))

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

(use-package org-contacts
  :after org
  :config
  (setq org-contacts-files '("~/org/agenda/Contacts.org")))

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
        (org-agenda nil " ")))))

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

;;; Embed source code and babel
;; fontify code in code blocks
(with-eval-after-load "org"
  ;; give us some hint we are running
  ;; (defadvice org-babel-execute-src-block (around progress nil activate)
  ;;   (set-face-attribute 'org-block nil :background "LightSteelBlue")
  ;;   (message "Running your code block")
  ;;   ad-do-it
  ;;   (set-face-attribute 'org-block nil :background "#002b36")
  ;;   (message "Done with code block"))

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
     (http . t)
     (latex . t)
     (sql . t))))

;; stop emacs asking for confirmation
;; (setq org-confirm-babel-evaluate nil)
(defun tl/org-confirm-babel-evaluate (lang body)
  (cond ((string= lang "ditaa") nil) ; don't ask for ditaa
        ((string= lang "emacs-lisp") nil)))
(setq org-confirm-babel-evaluate 'tl/org-confirm-babel-evaluate)


;;; Info directory
(eval-after-load "info"
  '(add-to-list 'Info-directory-list "~/.emacs.d/site-lisp/org-mode/doc"))

;;; org-mac-protocol
(with-eval-after-load "org"
  (when window-system
    (require 'org-mac-protocol nil t)))

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


(defun tl/org-insert-image ()
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

;; http://kitchingroup.cheme.cmu.edu/blog/2017/04/09/A-better-return-in-org-mode
;; (define-key org-mode-map (kbd "RET") 'tl/org-return)
(defun tl/org-return ()
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
(defun tl/insert-keybinding-org (key)
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
  :hook
  (org-src-mode . (lambda() (flycheck-mode -1)))
  :config
  (progn
    (tl/set-leader-keys-for-minor-mode 'org-src-mode
      dottl-major-mode-leader-key 'org-edit-src-exit
      "c" 'org-edit-src-exit
      "a" 'org-edit-src-abort
      "k" 'org-edit-src-abort)))


;; https://github.com/abo-abo/hydra/wiki/Org-mode-block-templates
(with-eval-after-load "org"
  (cl-pushnew '("not" . "note") org-structure-template-alist)

  (with-eval-after-load "hydra"
    (defhydra hydra-org-template (:color blue :hint nil)
      "
 _c_enter  _q_uote     _e_macs-lisp    _L_aTeX:
 _l_atex   _E_xample   _p_erl          _i_ndex:
 _a_scii   _v_erse     _P_erl tangled  _I_NCLUDE:
 _s_rc     _n_ote      plant_u_ml      _H_TML:
 _h_tml    ^ ^         ^ ^             _A_SCII:
"
      ("s" (tl/hot-expand "<s"))
      ("E" (tl/hot-expand "<e"))
      ("q" (tl/hot-expand "<q"))
      ("v" (tl/hot-expand "<v"))
      ("n" (tl/hot-expand "<not"))
      ("c" (tl/hot-expand "<c"))
      ("l" (tl/hot-expand "<l"))
      ("h" (tl/hot-expand "<h"))
      ("a" (tl/hot-expand "<a"))
      ("L" (tl/hot-expand "<L"))
      ("i" (tl/hot-expand "<i"))
      ("e" (tl/hot-expand "<s" "emacs-lisp"))
      ("p" (tl/hot-expand "<s" "perl"))
      ("u" (tl/hot-expand "<s" "plantuml :file CHANGE.png"))
      ("w" (tl/hot-expand "<s" "web"))
      ("P" (tl/hot-expand "<s" "perl" ":results output :exports both :shebang \"#!/usr/bin/env perl\"\n"))
      ("I" (tl/hot-expand "<I"))
      ("H" (tl/hot-expand "<H"))
      ("A" (tl/hot-expand "<A"))
      ("<" self-insert-command "ins")
      ("o" nil "quit"))

    (require 'org-tempo) ; Required from org 9 onwards for old template expansion
    ;; Reset the org-template expnsion system, this is need after upgrading to org 9 for some reason
    (setq org-structure-template-alist (eval (car (get 'org-structure-template-alist 'standard-value))))
    (defun tl/hot-expand (str &optional mod header)
      "Expand org template.

STR is a structure template string recognised by org like <s. MOD is a
string with additional parameters to add the begin line of the
structure element. HEADER string includes more parameters that are
prepended to the element after the #+HEADER: tag."
      (let (text)
        (when (region-active-p)
          (setq text (buffer-substring (region-beginning) (region-end)))
          (delete-region (region-beginning) (region-end))
          (deactivate-mark))
        (when header (insert "#+HEADER: " header) (forward-line))
        (insert str)
        (org-tempo-complete-tag)
        (when mod (insert mod) (forward-line))
        (when text (insert text))))

    (define-key org-mode-map "<"
      (lambda () (interactive)
        (if (or (region-active-p) (looking-back "^"))
            (hydra-org-template/body)
          (self-insert-command 1))))))

;;; finding all tags
(defun tl/counsel-org-tags ()
  (interactive)
  (let* ((tag-l (org-map-entries (lambda () org-scanner-tags) t 'agenda))
         (tags (sort (delete-dups (apply 'append (delete-dups tag-l))) 'string<)))
    (ivy-read "Org Tag View: "
              tags
              :action (lambda (tag) (org-tags-view nil tag))
              :caller 'tl/org-tags)))

;;; indent
(defun tl/reflash-indentation ()
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
          org-journal-time-format "%R "
          org-journal-carryover-items "TODO=\"TODO\"|TODO=\"NEXT\"|TODO=\"WAITING\"|TODO=\"HOLD\""
          org-journal-enable-agenda-integration t)
    (tl/declare-prefix "oj" "org-journal")
    (tl/declare-prefix "aoj" "org-journal")
    (tl/set-leader-keys
      "ojj" 'org-journal-new-entry
      "ojs" 'org-journal-search-forever
      "aojj" 'org-journal-new-entry
      "aojs" 'org-journal-search-forever))
  :config
  (progn
    (tl/set-leader-keys-for-major-mode 'org-journal-mode
      "j" 'org-journal-new-entry
      "n" 'org-journal-open-next-entry
      "p" 'org-journal-open-previous-entry)

    ;; set the parent keymap, so we can use the keybinding of ", *" binded in org-mode
    (set-keymap-parent tl-org-journal-mode-map tl-org-mode-map)

    (tl/set-leader-keys-for-major-mode 'calendar-mode
      "r" 'org-journal-read-entry
      "i" 'org-journal-new-date-entry
      "n" 'org-journal-next-entry
      "p" 'org-journal-previous-entry
      "s" 'org-journal-search-forever
      "w" 'org-journal-search-calendar-week
      "m" 'org-journal-search-calendar-month
      "y" 'org-journal-search-calendar-year)))

(with-eval-after-load 'org
  (use-package org-anki
    :config
    (progn
      (bind-key "s-c" 'org-anki-capture-region)
      (with-eval-after-load 'evil
        (evil-define-key 'normal org-anki-mode-map "gr" 'org-anki-refresh-buffer))
      (tl/set-leader-keys-for-major-mode 'org-anki-mode
        dottl-major-mode-leader-key 'org-anki-send-capture-and-quit
        "r" 'org-anki-refresh
        "k" 'org-anki-quit
        "a" 'org-anki-add-word)
      (set-keymap-parent tl-org-anki-mode-map tl-org-mode-map))))

;;; evil surround
(defun tl//surround-drawer ()
  (let ((dname (read-from-minibuffer "" "")))
    (cons (format ":%s:" (upcase (or dname ""))) ":END:")))

(defun tl//surround-code ()
  (let ((dname (read-from-minibuffer "" "")))
    (cons (format "#+BEGIN_SRC %s" (or dname "")) "#+END_SRC")))

;;; evil-org-mode
(defun tl//evil-org-mode ()
  (evil-org-mode +1)
  (evil-normalize-keymaps))

(use-package org-bullets
  :defer t
  :init
  (progn
    (setq org-bullets-face-name nil
          org-bullets-invisible-leading-stars t)
    ;; http://nadeausoftware.com/articles/2007/11/latency_friendly_customized_bullets_using_unicode_characters
    ;; https://zhangda.wordpress.com/2016/02/15/configurations-for-beautifying-emacs-org-mode/
    ;; "⬢" "⭓" "■"
    ;; "◉" "◎" "⚫" "○" "►" "◇"
    ;; "✺" "✹" "✸" "✷" "✶" "✭" "✦" "■" "▼" "●"
    ;; "⊢" "⋮" "⋱" "⋱" "⋱"
    ;; "☯" "☰" "☱" "☲" "☳" "☴" "☵" "☶" "☷"
    (setq org-bullets-bullet-list
          '("☱" "☲" "☳" "☴" "☵" "☶" "☷"))
    (add-hook 'org-mode-hook 'org-bullets-mode)))

;; https://github.com/kiwanami/emacs-calfw
(defun tl/calfw-calendar ()
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
  :bind (("C-c A" . tl/calfw-calendar))
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

(defun tl/add-created-pro-from-subtree ()
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
(defun tl/add-created-pro-from-subtree-whole-file ()
  (interactive)
  (let ((MATCH t)
        (SCOPE 'file)
        (SKIP nil)
        (spacing nil))
    (org-map-entries 'tl/add-created-pro-from-subtree
                     MATCH SCOPE SKIP)))


;; inserting a function definition
;; http://sachachua.com/blog/2018/01/org-mode-inserting-a-function-definition/
(defun tl/org-insert-defun (function)
  "Inserts an Org source block with the definition for FUNCTION."
  (interactive (find-function-read))
  (let* ((buffer-point (condition-case nil (find-definition-noselect function nil) (error nil)))
         (new-buf (car buffer-point))
         (new-point (cdr buffer-point))
         definition)
    (if buffer-point
        (with-current-buffer new-buf ;; Try to get original definition
          (save-excursion
            (goto-char new-point)
            (setq definition (buffer-substring-no-properties (point) (save-excursion (end-of-defun) (point))))))
      ;; Fallback: Print function definition
      (setq definition (concat (prin1-to-string (symbol-function function)) "\n")))
    (insert "#+begin_src emacs-lisp\n" definition "#+end_src\n")))

;;; prompt date, accurate than org-todo-yesterday
;; https://emacs.stackexchange.com/questions/9433/how-to-make-org-prompt-for-a-timestamp-when-changing-state-of-a-todo/9451

;; The line in org.el that sets the CLOSED timestamp is (org-add-planning-info
;; 'closed (org-current-effective-time)) and the LOGBOOK notes are added by
;; org-add-log-setup, which in turn calls org-effective-current-time.
;; org-effective-current-time does what it sounds like and returns the effective
;; time. The obvious solution is to temporarily change
;; org-effective-current-time to something that prompts for a date.
;; TODO: replace org-todo-yesterday
(defun tl/org-todo-with-date (&optional arg)
  (interactive "P")
  (cl-letf* ((org-read-date-prefer-future nil)
             (new-time (org-read-date t t nil "when:" nil nil nil))
             ((symbol-function #'org-current-effective-time)
              #'(lambda () new-time)))
    (org-todo arg)))

;; org-link-minor-mode
(use-package org-link-minor-mode
  :defer t
  :diminish org-link-minor-mode
  :hook ((emacs-lisp-mode . org-link-minor-mode)
         (sql-mode . org-link-minor-mode)))


;;; HUGO
(use-package ox-hugo
  :after ox
  :init
  (progn
    (setq org-hugo-default-section-directory "posts")))

(use-package easy-hugo
  :defer t
  :init
  (setq easy-hugo-basedir "~/Lab/quick-hugo/"
        easy-hugo-postdir "content/posts"))


(use-package ob-ditaa
  :defer t
  :config
  (let ((path
         "/usr/local/Cellar/ditaa/0.11.0_1/libexec/ditaa-0.11.0-standalone.jar"))
    (when (file-exists-p path)
      (setq org-ditaa-jar-path path))))


(provide '13org-mode)

;; Local Variables:
;; outline-regexp: ";;; "
;; mode: outline-minor
;; byte-compile-warnings: (not noruntime free-vars) **
;; End:
