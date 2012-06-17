;;;
;; created by Shihpin Tseng

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

;;; Gnus-style grouping
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
         ("Scheme" (or (mode . scheme-mode )
                       (mode . inferior-scheme-mode)))
         ("CC Mode" (or (mode . c-mode)
                        (mode . c++-mode)
                        (mode . objc-mode)))
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

;;; reverse the order of groups:

(defadvice ibuffer-generate-filter-groups (after reverse-ibuffer-groups ()
                                                 activate)
  (setq ad-return-value (nreverse ad-return-value)))

(provide '50switching-buffers)
