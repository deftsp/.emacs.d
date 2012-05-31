;;;
;; created by Shihpin Tseng


;;; ibuffer.el
;; (load "/usr/share/emacs/site-lisp/emacs-goodies-el/ibuffer.el")
;; (require 'ibuf-ext)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(autoload 'ibuffer "ibuffer" "List buffers." t)
(setq ibuffer-shrink-to-minimum-size t)
(setq ibuffer-always-show-last-buffer nil)
(setq ibuffer-sorting-mode 'recency)
(setq ibuffer-use-header-line t)
(setq ibuffer-formats '((mark modified read-only " " (name 16 16) " "
                         (size 6 -1 :right) " " (mode 16 16 :center)
                         " " (process 8 -1) " " filename)
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

;; Use this to reverse the order of groups:

(defadvice ibuffer-generate-filter-groups (after reverse-ibuffer-groups ()
                                                 activate)
  (setq ad-return-value (nreverse ad-return-value)))

;; Sort by pathname
;; (defun my-ibuffer-hook ()
;;   ;; add another sorting method for ibuffer (allow the grouping of
;;   ;; filenames and dired buffers
;;   (ibuffer-define-sorter pathname
;;                          (:documentation
;;                           "Sort the buffers by their pathname."
;;                           :description "path")
;;                          (string-lessp (with-current-buffer (car a)
;;                                          (or buffer-file-name
;;                                             (if (eq major-mode 'dired-mode)
;;                                                 (expand-file-name dired-directory))
;;                                             ;; so that all non pathnames are at the end
;;                                             "~"))
;;                                        (with-current-buffer (car b)
;;                                          (or buffer-file-name
;;                                             (if (eq major-mode 'dired-mode)
;;                                                 (expand-file-name dired-directory))
;;                                             ;; so that all non pathnames are at the end
;;                                             "~"))))
;;   ;; add key binding
;;   (define-key ibuffer-mode-map (kbd "s p") 'ibuffer-do-sort-by-pathname))
;; (add-hook 'ibuffer-mode-hooks 'my-ibuffer-hook)

;; I modified the above, works with GNU Emacs 22.0.92.1, should this work? --DavidBoon
;; (define-ibuffer-sorter filename-or-dired
;;     "Sort the buffers by their pathname."
;;   (:description "filenames plus dired")
;;   (string-lessp
;;    (with-current-buffer (car a)
;;      (or buffer-file-name
;;         (if (eq major-mode 'dired-mode)
;;             (expand-file-name dired-directory))
;;         ;; so that all non pathnames are at the end
;;         "~"))
;;    (with-current-buffer (car b)
;;      (or buffer-file-name
;;         (if (eq major-mode 'dired-mode)
;;             (expand-file-name dired-directory))
;;         ;; so that all non pathnames are at the end
;;         "~"))))
;; (define-key ibuffer-mode-map (kbd "s p") 'ibuffer-do-sort-by-filename-or-dired)

;; hide buffers in ibuffer
;; (setq ibuffer-never-show-predicates
;;       (list "\\*Completions\\*"
;;             "\\*vc\\*"))

;; Here’s how to hide all buffers starting with an asterisk.
;; (add-to-list 'ibuffer-never-show-regexps "^\\*")
;; View subsets of buffers
;; Also try ibuffer's "limiting" feature ('/'), which allows you to just view a subset of your buffers.
;; Diff
;; Ibuffer can show you the differences between an unsaved buffer and the file on disk with `=’.


;;; iswitchb

;;使用ido代替iswitch
;; (require 'filecache)
;;(require 'iswitchb)
;;(iswitchb-mode 1)
;;(setq read-buffer-function 'iswitchb-read-buffer)
;;(setq iswitchb-default-method 'samewindow)

;; (defun iswitchb-local-keys ()
;;   (mapc (lambda (K)
;;           (let* ((key (car K)) (fun (cdr K)))
;;             (define-key iswitchb-mode-map (edmacro-parse-keys key) fun)))
;;         '(("<right>" . iswitchb-next-match)
;;           ("<left>"  . iswitchb-prev-match)
;;           ("<up>"    . ignore             )
;;           ("<down>"  . ignore             ))))

;; (add-hook 'iswitchb-define-mode-map-hook 'iswitchb-local-keys)


;;you can kill a buffer from within iswitchb by typing C-k.

;; iswitchb ignores
;; (add-to-list 'iswitchb-buffer-ignore "^ ")
;; (add-to-list 'iswitchb-buffer-ignore "*Messages*")
;; (add-to-list 'iswitchb-buffer-ignore "*ECB")
;; (add-to-list 'iswitchb-buffer-ignore "*Buffer")
;; (add-to-list 'iswitchb-buffer-ignore "*Bookmark List*")
;; (add-to-list 'iswitchb-buffer-ignore "*Completions")
;; (add-to-list 'iswitchb-buffer-ignore "*ftp ")
;; (add-to-list 'iswitchb-buffer-ignore "*bsh")
;; (add-to-list 'iswitchb-buffer-ignore "*jde-log")
;; (add-to-list 'iswitchb-buffer-ignore "^[tT][aA][gG][sS]$")

;; (setq iswitchb-buffer-ignore '("^\\ "
;;                                "^\\*Help"
;;                                "^\\*scratch"
;;                                "^\\*Compile-Log"
;;                                "^\\*Woman-Log"
;;                                "^\\*Completions"
;;                                "^\\*Bookmark"
;;                                "^\\*Messages"
;;                                "^\\.newsrc-dribble"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(provide '50switching-buffers)
