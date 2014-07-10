;;; 50buffer.el ---
;; Author: Shihpin Tsing <deftsp@gmail.com>

;;; ibuffer.el
(global-set-key (kbd "C-x C-b") 'ibuffer)
(setq ibuffer-shrink-to-minimum-size t
      ibuffer-always-show-last-buffer nil
      ibuffer-sorting-mode 'recency
      ibuffer-use-header-line t
      ibuffer-formats '((mark modified read-only " "
                              (name 18 18 :left :elide) " "
                              (size 9 -1 :left) " "
                              (mode 16 16 :left :elide)
                              " " filename-and-process)
                        (mark " " (name 16 -1) " " filename))
      ibuffer-elide-long-columns t
      ibuffer-eliding-string "...")

;; Gnus-style grouping
;; Ibuffer has an excellent implementation of Gnus-style grouping.
(setq ibuffer-show-empty-filter-groups nil)
(setq ibuffer-saved-filter-groups
      '(("default"
         ("Dired" (or (mode . dired-mode)))
         ("erc" (mode . erc-mode))
         ("svn" (or (mode . svn-status-mode)
                    (mode . svn-log-edit-mode)
                    (name . "^\\*svn-")
                    (name . "^\\*vc\\*$")
                    (name . "^\\*Annotate")
                    (name . "^\\*vc-")))
         ("Haskell" (or (mode . haskell-mode )
                        (mode . haskell-interactive-mode)
                        (mode . hamlet-mode)
                        (mode . haskell-cabal-mode)
                        (name . "*haskell-process-log*")))
         ("Scheme" (or (mode . scheme-mode )
                       (mode . inferior-scheme-mode)))
         ("CC Mode" (or (mode . c-mode)
                        (mode . c++-mode)
                        (mode . objc-mode)))
         ("Lua" (or (mode . lua-mode)
                    (name . "^\\*lua\\*$")))
         ("Emacs" (or (name . "^\\*scratch\\*$")
                      (name . "^\\*Messages\\*$")
                      (name . "^\\*Help\\*$")
                      (name . "^\\*info\\*$")
                      (name . "^\\*Occur\\*$")
                      (name . "^\\*grep\\*$")
                      (name . "^\\*Compile-Log\\*$")
                      (name . "^\\*Backtrace\\*$")
                      (name . "^\\*Process List\\*$")
                      (name . "^\\*gud\\*$")
                      (name . "^\\*Man")
                      (name . "^\\*WoMan")
                      (name . "^\\*Kill Ring\\*$")
                      (name . "^\\*Completions\\*$")
                      (name . "^\\*tramp")
                      (name . "^\\*shell\\*$")
                      (name . "^\\*compilation\\*$")))
         ("Elisp" (or (mode . emacs-lisp-mode)
                      (filename . "/Applications/Emacs.app")
                      (filename . "/bin/emacs")))
         ("Lisp source" (mode . lisp-mode))
         ("Agenda" (or (name . "^\\*Calendar\\*$")
                       (name . "^diary$")
                       (name . "^\\*Agenda")
                       (name . "^\\*org-")
                       (name . "^\\*Org")
                       (mode . org-mode)))
         ("Muse" (mode . muse-mode))
         ("Sawfish" (mode . sawfish-mode))
         ("Gnus" (or (mode . message-mode)
                     (mode . bbdb-mode)
                     (mode . mail-mode)
                     (mode . gnus-group-mode)
                     (mode . gnus-summary-mode)
                     (mode . gnus-article-mode)
                     (name . "^\\.bbdb$")
                     (name . "^\\.newsrc-dribble")))
         ("Latex" (or (mode . latex-mode)
                      (mode . LaTeX-mode)
                      (mode . bibtex-mode)
                      (mode . reftex-mode))))))

(add-hook 'ibuffer-mode-hook (lambda ()
                               (ibuffer-switch-to-saved-filter-groups
                                "default")))

;; reverse the order of groups:
(defadvice ibuffer-generate-filter-groups (after reverse-ibuffer-groups ()
                                                 activate)
  (setq ad-return-value (nreverse ad-return-value)))

;;; ace-jump-buffer
(setq ajb-max-window-height 37
      ajb-reuse-windows t)

;;; clean buffer list
;;; midnight
;; What would life be like if there were only saving, and never killing? These two settings tell Emacs to, every
;; midnight, kill regular buffers which haven't been used in a while.
;; <http://www.gnu.org/software/emacs/manual/html_node/Kill-Buffer.html>
(require 'midnight)

;; By default, `clean-buffer-list' will kill buffers that haven't been visited in 3 days, or in the last
;; hour in the case of special buffer that are specified by `clean-buffer-list-kill-buffer-names'
(setq clean-buffer-list-delay-general 2)
;; special buffers are cleaned every 6 hours
(setq clean-buffer-list-delay-special (* 6 3600))

(setq midnight-period (* 3 60 60))      ; occur every 3 hours default 86400 (24 hours)
;; note period timer will occuer, even the time when set the timer (boot the emacs) is after
;; "09:30am". see more..  run-at-time.
;; (midnight-delay-set 'midnight-delay "09:30am")
;; set midnight-delay after 3 hours
(midnight-delay-set 'midnight-delay (pl/future-time-string (* 3 60 60)))



;; As of 2009-10-02, DeskTop mode does not preserve the value of 'buffer-display-time' for buffers,
;; so the buffer's "age" is effectively restarted. This means that buffers restored by a Desktop
;; sessions are considered "new" by CleanBufferList, even though they may be considered"old".

;; http://www.emacswiki.org/emacs/CleanBufferList
;; The following variables can be used to customize the behavior of the
;; clean-buffer-list (which is run daily at midnight).
;; clean-buffer-list-kill-buffer-names
;; clean-buffer-list-kill-regexps
;; clean-buffer-list-kill-never-buffer-names
;; clean-buffer-list-kill-never-regexps
;; clean-buffer-list-delay-general
;; clean-buffer-list-delay-special

(mapcar (lambda (str) (add-to-list 'clean-buffer-list-kill-buffer-names str))
        '("*buffer-selection*"
          "*Finder*"
          "*Finder Category*"
          "*Finder-package*"
          "*RE-Builder*"
          "*vc-change-log*"))


(mapcar (lambda (str) (add-to-list 'clean-buffer-list-kill-regexps str))
        '("\\`\\*Customize .*\\*\\'"
          "\\`\\*\\(Wo\\)?Man .*\\*\\'"))

(mapcar (lambda (str) (add-to-list 'clean-buffer-list-kill-never-buffer-names str))
        '("*eshell*"
          "*ielm*"
          "*mail*"
          "*w3m*"
          "*w3m-cache*"))

(mapcar (lambda (str) (add-to-list 'clean-buffer-list-kill-never-regexps str))
        '("\\`\\*tramp/.*\\*\\`"
          "\\`\\*ftp .*\\*\\`"))


(provide '50buffer)
