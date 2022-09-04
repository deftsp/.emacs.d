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
  :hook ((before-save . tl/org-set-last-modified))
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
          ;; org-image-actual-width '(300)
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

    ;; local bound inhibit-message to t works when be called by other function, but not C-x C-e
    (defun tl//org-save-all-org-buffers-quietly ()
      (let ((inhibit-message t))
        (org-save-all-org-buffers)))

    ;; (defun tl//after-focus-out-save-all-org-buffers ()
    ;;   (let ((focus-out t))
    ;;     (with-no-warnings
    ;;       (dolist (f (frame-list) focus-out)
    ;;         (setq focus-out (and focus-out (not (frame-focus-state f))))))
    ;;     (when focus-out
    ;;       (tl//org-save-all-org-buffers-quietly))))

    ;; (add-function :after after-focus-change-function 'tl//after-focus-out-save-all-org-buffers) ; Sacha Chua
    ;; (remove-function after-focus-change-function 'tl//after-focus-out-save-all-org-buffers)
    (add-hook 'tl/emacs-deactivate-hook 'tl//org-save-all-org-buffers-quietly)

    (use-package evil-org
      :diminish evil-org-mode
      :after org
      :init
      (add-hook 'org-mode-hook 'tl//evil-org-mode)
      :config
      (evil-org-set-key-theme '(navigation return textobjects additional todo calendar))
      (general-define-key
       :states '(normal)
       :keymaps 'org-mode-map
       "RET" 'evil-org-return))

    (general-define-key
     :states '(visual)
     :keymaps 'org-mode-map
     "t" 'org-todo)

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
      (display-line-numbers-mode +1))

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
    (tl/set-leader-keys-for-mode 'org-mode
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
      "sA" 'tl/org-archive-completed-in-buffer
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
      "id"  'org-insert-drawer
      "iDr" 'tl/org-insert-resources-drawer-at-point
      "ie"  'org-set-effort
      "if"  'org-footnote-new
      "ih"  'org-insert-heading
      "iH"  'org-insert-heading-after-current
      "ii"  'tl/org-insert-image
      "iK"  'tl/insert-keybinding-org
      "il"  'org-insert-link
      "ip"  'org-set-property
      "is"  'org-insert-subheading
      "it"  'org-set-tags

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

;; save target buffer after archiving a node.
;; FIXME: it is not exist any more?
(setq org-archive-subtree-save-file-p t)

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
  :diminish org-fancy-priorities-mode
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
          ("c" "Add Task with Clock In" entry
           (file+headline "~/org/agenda/GTD.org" "Tasks")
           "* TODO %?\nSCHEDULED: %t \n:PROPERTIES:\n:ID: %(org-id-new)\n:CREATED:  %U\n:END:"
           :prepend t
           :clock-in t
           :clock-keep t
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
           "* TODO %?%:annotation\n:PROPERTIES:\n:ID: %(org-id-new)\n:CREATED:  %U\n:END:"
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
           (file "~/org/templates/weekly-review-template.org"))))
  (require 'private-org-capture-templates)

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

    (tl/set-leader-keys-for-mode 'org-capture-mode
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

(use-package org-protocol
  :after (org)
  :config
  (progn
    ;; https://github.com/alphapapa/org-protocol-capture-html
    (use-package org-protocol-capture-html)

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
               (call-interactively 'org-clock-out)))))

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

;; (use-package org-contacts
;;   :after org
;;   :config
;;   (setq org-contacts-files '("~/org/agenda/Contacts.org")))

;;; link
(setq org-link-abbrev-alist
      '(("google"   . "http://www.google.com/search?q=")
        ("baidu"    . "http://www.baidu.com/s?wd=")))

(use-package org-link-beautify
  :after (org)
  :hook (org-mode . org-link-beautify-enable))

(with-eval-after-load "org"
  ;; https://emacs.stackexchange.com/questions/33064/fontify-broken-links-in-org-mode
  (defun tl//org-file-link-face (path)
    (if (file-exists-p path) 'org-link 'org-warning))

  (org-link-set-parameters
   "file"
   :face #'tl//org-file-link-face)

  (defface tl/org-link-http-empty-face
    '((t (:inherit default :foreground unspecified :background unspecified)))
    "Empty face for http/https link in non org-mode")

  (defun tl//org-link-face (path)
    (if (and (not (eq major-mode 'org-mode)) (string-prefix-p ":" path))
        'tl/org-link-http-empty-face
      'org-link))

  ;; https://kitchingroup.cheme.cmu.edu/blog/2016/11/04/New-link-features-in-org-9/
  (org-link-set-parameters "x-devonthink-item" :face '(:foreground "#99dd55" :underline t))

  (org-link-set-parameters
   "http"
   :face #'tl//org-link-face)

  (org-link-set-parameters
   "https"
   :face #'tl//org-link-face))


;; keybinding conflicts with icicles keys
;; (org-defkey org-mode-map (kbd "C-c C-'") 'org-edit-special)
;; (define-key org-exit-edit-mode-map (kbd "C-c C-'") 'org-edit-src-exit)

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

(defun tl//org-redisplay-inline-images-h ()
  (when org-inline-image-overlays
    (org-redisplay-inline-images)))

(with-eval-after-load 'org
  (add-hook 'org-babel-after-execute-hook 'tl//org-redisplay-inline-images-h))


;;; Info directory
(eval-after-load "info"
  '(add-to-list 'Info-directory-list "~/.emacs.d/site-lisp/org-mode/doc"))

;;; org-mac-protocol
(with-eval-after-load "org"
  (when window-system
    (require 'org-mac-protocol nil t)))

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
    (add-to-list 'org-src-lang-modes (quote ("dot" . graphviz-dot)))
    (tl/set-leader-keys-for-mode 'org-src-mode
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
  :after (org)
  :init
  (progn
    (setq org-journal-dir (concat org-directory "/roam/daily/")
          org-journal-file-format "%Y-%m-%d.org"
          org-journal-date-prefix "#+TITLE: "
          org-journal-date-format "%A, %B %d %Y"
          org-journal-time-prefix "* "
          org-journal-time-format "%R "
          org-journal-carryover-items "TODO=\"TODO\"|TODO=\"NEXT\"|TODO=\"WAITING\"|TODO=\"HOLD\""
          ;; add the current and all future journal entries to org-agenda-files
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
    (tl/set-leader-keys-for-mode 'org-journal-mode
      "j" 'org-journal-new-entry
      "n" 'org-journal-open-next-entry
      "p" 'org-journal-open-previous-entry)

    ;; set the parent keymap, so we can use the keybinding of ", *" binded in org-mode
    ;; (set-keymap-parent tl-org-journal-mode-map tl-org-mode-map)

    (tl/set-leader-keys-for-mode 'calendar-mode
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
      (tl/set-leader-keys-for-mode 'org-anki-mode
        dottl-major-mode-leader-key 'org-anki-send-capture-and-quit
        "r" 'org-anki-refresh
        "k" 'org-anki-quit
        "a" 'org-anki-add-word)
      ;; (set-keymap-parent tl-org-anki-mode-map tl-org-mode-map)
      )))

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


(use-package org-superstar
  :defer t
  :init
  ;; http://nadeausoftware.com/articles/2007/11/latency_friendly_customized_bullets_using_unicode_characters
  ;; https://zhangda.wordpress.com/2016/02/15/configurations-for-beautifying-emacs-org-mode/
  ;; "⬢" "⭓" "■"
  ;; "◉" "◎" "⚫" "○" "►" "◇"
  ;; "✺" "✹" "✸" "✷" "✶" "✭" "✦" "■" "▼" "●"
  ;; "⊢" "⋮" "⋱" "⋱" "⋱"
  ;; "☯" "☰" "☱" "☲" "☳" "☴" "☵" "☶" "☷"
  (setq org-superstar-headline-bullets-list '("☱" "☲" "☳" "☴" "☵" "☶" "☷"))
  ;; (setq org-superstar-leading-bullet ?\s)
  ;; (setq org-superstar-leading-bullet " ․")
  (setq org-superstar-remove-leading-stars t)
  ;; If you use Org Indent you also need to add this, otherwise the
  ;; above has no effect while Indent is enabled.
  ;; (setq org-indent-mode-turns-on-hiding-stars nil)
  (setq inhibit-compacting-font-caches t)
  (add-hook 'org-mode-hook 'org-superstar-mode))

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

(defun tl/print-org-element-at-point  ()
  (interactive)
  (message "current org-element-type: %s"
           (org-element-type (org-element-at-point))))

;; Handling file properties for ‘CREATED’ & ‘LAST_MODIFIED’
[[https://github.com/zaeph/.emacs.d/blob/master/init.el][.emacs.d/init.el at master · zaeph/.emacs.d]]
(defun tl/org-find-time-file-property (property &optional anywhere)
  "Return the position of the time file PROPERTY if it exists.
When ANYWHERE is non-nil, search beyond the preamble."
  (save-excursion
    (goto-char (point-min))
    (let ((first-heading
           (save-excursion
             (re-search-forward org-outline-regexp-bol nil t))))
      (when (re-search-forward (format "^#\\+%s:" property)
                               (if anywhere nil first-heading)
                               t)
        (point)))))

(defun tl/org-has-time-file-property-p (property &optional anywhere)
  "Return the position of time file PROPERTY if it is defined.
As a special case, return -1 if the time file PROPERTY exists but
is not defined."
  (when-let ((pos (tl/org-find-time-file-property property anywhere)))
    (save-excursion
      (goto-char pos)
      (if (and (looking-at-p " ")
               (progn (forward-char)
                      (org-at-timestamp-p 'lax)))
          pos
        -1))))

(defun tl/org-set-time-file-property (property &optional anywhere pos)
  "Set the time file PROPERTY in the preamble.
When ANYWHERE is non-nil, search beyond the preamble.
If the position of the file PROPERTY has already been computed,
it can be passed in POS."
  (when-let ((pos (or pos (tl/org-find-time-file-property property))))
    (save-excursion
      (goto-char pos)
      (if (looking-at-p " ")
          (forward-char)
        (insert " "))
      (delete-region (point) (line-end-position))
      (let* ((now (format-time-string "[%Y-%m-%d %a %H:%M]")))
        (insert now)))))

(defun tl/org-set-last-modified ()
  "Update the LAST_MODIFIED file property in the preamble."
  (when (derived-mode-p 'org-mode)
    (tl/org-set-time-file-property "LAST_MODIFIED")))

;; org-link-minor-mode seems not works with 0.9.3 for now (2022-05-05)
;; (use-package org-link-minor-mode
;;   :defer t
;;   :diminish org-link-minor-mode
;;   :hook ((emacs-lisp-mode . org-link-minor-mode)
;;          (sql-mode . org-link-minor-mode)))

(use-package orglink
  :after org
  :diminish orglink-mode
  :config
  (setq orglink-activate-in-modes '(emacs-lisp-mode rust-mode))
  (global-orglink-mode +1))

(use-package org-transclusion
  :after org
  :config
  (general-define-key
   :states '(normal)
   :keymaps 'org-mode-map
   :prefix ","
   "na" 'org-transclusion-add
   "nt" 'org-transclusion-mode
   "nn" 'org-transclusion-mode))

;; TODO: not work as expect
;; enforce the number of blanks lines after elements in an org-mode document
;; `tl/print-org-element-at-point' to print element type
;; (use-package org-spacer
;;   :after org
;;   :commands (org-spacer-enforce)
;;   :init
;;   (setq org-spacer-element-blanks
;;         '((0 headline plain-list)
;;           (1 src-block table property-drawer)))

;;   (defun tl/turn-on-org-spacer ()
;;     (interactive)
;;     (add-hook 'before-save-hook 'org-spacer-enforce nil 'make-it-local))

;;   (defun tl/turn-off-org-spacer ()
;;     (interactive)
;;     (remove-hook 'before-save-hook 'org-spacer-enforce 'make-it-local))

;;   (add-hook 'org-mode-hook 'tl/turn-on-org-spacer))

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

;; TODO: update it to auto add link in RESOURCES drawer.
;; like https://github.com/Kungsgeten/org-brain/blob/master/org-brain.el
(defun tl/org-insert-resources-drawer-at-point ()
  (interactive)
  (org-insert-drawer nil "RESOURCES"))

(use-package org-mac-link
  :commands (org-mac-link-get-link))

(provide '13org-mode)

;; Local Variables:
;; outline-regexp: ";;; "
;; mode: outline-minor
;; byte-compile-warnings: (not noruntime free-vars) **
;; End:
