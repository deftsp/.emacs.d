;;; 60session.el ---

;; Copyright (C) 2008  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>
;; Keywords:



;;; when open a file, point goes to the last place
(setq-default save-place t)
(require 'saveplace)

;;; automatic saving of minibuffer history.
(savehist-mode 1)
;; (add-hook 'savehist-save-hook
;;           (lambda ()
;;             (setq savehist-minibuffer-history-variables
;;                   '(regexp-history
;;                     search-ring
;;                     file-name-history
;;                     shell-command-history
;;                     minibuffer-history
;;                     extended-command-history
;;                     query-replace-history))))

;;; Desktop
(eval-after-load "desktop"
  '(progn
     (setq desktop-files-not-to-save
           (concat "\\("
                   "^/[^/:]*:\\|(ftp)$"
                   "\\|\\.gpg$"
                   "\\|\\.m$"
                   "\\|\\.mm$"
                   "\\|\\.h$"
                   "\\|\\.gz$"
                   "\\|^NEWS$"
                   ;; "\\|\\.el$"
                   "\\)"))
     ;; See also desktop-locals-to-save
     ;; Regexp identifying *buffers* that are to be excluded from saving.
     (setq desktop-buffers-not-to-save
           (concat "\\("
                   "^nn\\.a[0-9]+\\|\\.log\\|(ftp)\\|^tags\\|^TAGS"
                   "\\|\\.emacs.*\\|\\.diary\\|\\.newsrc-dribble\\|\\.bbdb"
                   "\\)$"))

     (mapc #'(lambda (major-mode)
               (add-to-list 'desktop-modes-not-to-save major-mode))
           '(dired-by-name-mode
             fundamental-mode
             org-mode
             ;; Info-mode
             ;; info-lookup-mode
             ;; tramp-cleanup-all-buffers
             erc-mode
             tags-table-mode))

     (mapc #'(lambda (lst)
               (add-to-list 'desktop-minor-mode-table lst))
           '((icicle-mode nil)
             (javascript-mode nil)
             (nxhtml-mumamo-mode nil)
             (autoinfo-mode nil)
             (ecb-minor-mode nil)
             ;; (key-chord-mode nil)
             (senator-minor-mode nil)
             (semantic-show-unmatched-syntax-mode nil)
             (semantic-stickyfunc-mode nil)
             (semantic-decoration-mode nil)
             (semantic-idle-summary-mode nil)
             (semantic-idle-scheduler-mode nil)))

     ;; save a bunch of variables to the desktop file for lists specify the len of the maximal saved data also.
     (mapcar
      (lambda (symbol)
        (add-to-list 'desktop-globals-to-save symbol))
      '((extended-command-history       . 30)
        (file-name-history              . 100)
        (kill-ring                      . 60)
        (grep-history                   . 30)
        (compile-history                . 30)
        (minibuffer-history             . 50)
        (query-replace-history          . 60)
        (read-expression-history        . 60)
        (regexp-history                 . 60)
        (regexp-search-ring             . 20)
        (search-ring                    . 20)
        ;; Feature: Saving `kill-ring' implies saving `kill-ring-yank-pointer'.
        (delq 'register-alist desktop-globals-to-save)
        (buffer-name-history            . 60)
        (Info-search-history            . 60)
        (command-history                . 60)
        (dired-shell-command-history    . 60) ;; TODO: join with shell-command-history
        (dired-regexp-history           . 20)
        (shell-command-history          . 50)

        find-args-history
        tags-file-name
        locate-history-list))))

(setq desktop-load-locked-desktop t) ; 'ask
(setq desktop-missing-file-warning nil)

;; Use M-x desktop-save once to save the desktop.When it exists, Emacs updates it on every exit.
(when (fboundp 'desktop-save-mode)
  (setq desktop-save 'if-exists)
  (desktop-save-mode 1))

;;; Let desktop work with daemon
;; (command-line) starts the server process, but only "after loading the user's init file and after
;; processing all command line arguments".
(defadvice desktop-restore-file-buffer
  (around desktop-restore-file-buffer-advice)
  "Be non-interactive while starting a daemon."
  (if (and (daemonp)
           (not server-process))
      (let ((noninteractive t))
        ad-do-it)
    ad-do-it))
(ad-activate 'desktop-restore-file-buffer)

;; Prepare Emacs desktop after loading Emacs
;; (when (fboundp 'pl/jump-to-org-agenda)
;;   (add-hook 'after-init-hook
;;             ;; delay 5 seconds
;;             (run-at-time (pl/future-time-string 5) nil #'pl/jump-to-org-agenda)
;;             ;; Note that 3-rd argument of this `add-hook' should be `t'
;;             ;; to append the call of the `dired' after other hooked functions,
;;             ;; most importantly after `desktop-read'.
;;             t))

(provide '60session)
