;;; 60session.el ---

;; Copyright (C) 2008  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>
;; Keywords:



;;; when open a file, point goes to the last place
(setq-default save-place t)
(require 'saveplace)


;; hisotry save file
(setq history-length 250
      history-delete-duplicates t
      savehist-save-minibuffer-history t
      savehist-additional-variables '(kill-ring
                                      search-ring
                                      regexp-search-ring))

(when (boundp 'pl/cache-directory)
  (setq savehist-file (concat pl/cache-directory "history")))
;;; automatic saving of minibuffer history.
(savehist-mode +1)

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
             haskell-mode ; very slow
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
             (structured-haskell-mode nil)
             (senator-minor-mode nil)
             (semantic-show-unmatched-syntax-mode nil)
             (semantic-stickyfunc-mode nil)
             (semantic-decoration-mode nil)
             (semantic-idle-summary-mode nil)
             (semantic-idle-scheduler-mode nil)))

     ;; save a bunch of variables to the desktop file for lists specify the len of the maximal saved data also.
     ;; (delq 'register-alist desktop-globals-to-save)
     (setq desktop-globals-to-save
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
             (buffer-name-history            . 60)
             (Info-search-history            . 60)
             (command-history                . 60)
             (dired-shell-command-history    . 60) ; TODO: join with shell-command-history
             (dired-regexp-history           . 20)
             (shell-command-history          . 50)
             ;; desktop-missing-file-warning
             tags-table-list
             ;; register-alist
             find-args-history
             tags-file-name
             locate-history-list))))

(setq desktop-load-locked-desktop t
      desktop-missing-file-warning nil
      desktop-save 'if-exists
      desktop-restore-frames nil)

;; Use M-x desktop-save once to save the desktop.When it exists, Emacs updates it on every exit.
;; (desktop-save-mode 1)

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

;;; workgroups2
(require 'workgroups2 nil t)
;; autoload/autosave:
(setq wg-default-session-file "~/.emacs.d/workgroups"
      wg-use-default-session-file t  ; set to nil, if you emacs started as daemon
      wg-prefix-key (kbd "C-c w")
      wg-mode-line-decor-left-brace "♯"
      wg-mode-line-decor-right-brace "")

(eval-after-load "workgroups2"
  '(when (fboundp 'key-chord-define-global)
     (key-chord-define-global ".w" wg-prefixed-map)))

;; put this one at the bottom of .emacs
(defun pl/turn-on-workgroups-mode ()
  (when (fboundp 'workgroups-mode)
    (workgroups-mode 1)))

;; make sure workgroups2 runs bofore desktop
(add-hook 'after-init-hook #'pl/turn-on-workgroups-mode)

(provide '60session)

;; Local Variables:
;; coding: utf-8-unix
;; End:
