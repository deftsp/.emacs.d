;;; 60session.el ---

;; Copyright (C) 2008  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>
;; Keywords:

(require 'saveplace)
(setq-default save-place t)

;;----------------------------------------------------------------------------------------------------
;;automatic saving of minibuffer history.
(savehist-mode 1)
(add-hook 'savehist-save-hook
          (lambda ()
            (setq savehist-minibuffer-history-variables
                  '(regexp-history
                    search-ring
                    file-name-history
                    shell-command-history
                    minibuffer-history
                    extended-command-history
                    query-replace-history))))

;;; Session Management
;; Histories of user input. For example, strings used in a find/replace command, names of files you have visited, etc.
;; Contents of registers, whether they are texts or buffer/file positions. Buffer positions are automatically converted to file positions.
;; List of recently copied/cut text blocks to paste, global markers to jump to, and other so-called rings.
;; List of recently changed files with their places and some buffer-local variables.

(when (require 'session nil t)
  (setq session-save-file (expand-file-name "~/.emacs.session"))
  (setq session-save-file-coding-system 'utf-8)
  ;; https://github.com/emacs-helm/helm/issues/94
  (setq session-save-print-spec '(t nil nil)) ; default '(t 2 1024)
  ;; exclude org-mark-ring which is a circular object. otherwise, emacs can not be closed if open org file.
  (add-to-list 'session-globals-exclude 'org-mark-ring)
  (add-hook 'after-init-hook 'session-initialize))

;;; Desktop
(eval-after-load "desktop"
  '(mapc #'(lambda (lst)
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
           (semantic-idle-scheduler-mode nil))))

(setq desktop-load-locked-desktop t) ; 'ask
(setq desktop-missing-file-warning nil)

(when (fboundp 'desktop-save-mode)
  (setq desktop-save 'if-exists)        ; save the desktop file automatically if it already exists
  (desktop-save-mode 1)
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
     locate-history-list)))

;; See also desktop-locals-to-save
;; Regexp identifying *buffers* that are to be excluded from saving.
(setq desktop-buffers-not-to-save
      (concat "\\("
              "^nn\\.a[0-9]+\\|\\.log\\|(ftp)\\|^tags\\|^TAGS"
              "\\|\\.emacs.*\\|\\.diary\\|\\.newsrc-dribble\\|\\.bbdb"
              "\\)$"))

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

(eval-after-load "desktop"
  '(mapc #'(lambda (major-mode)
             (add-to-list 'desktop-modes-not-to-save major-mode))
    '(dired-by-name-mode
      fundamental-mode
      ;; Info-mode
      ;; info-lookup-mode
      ;; tramp-cleanup-all-buffers
      erc-mode
      tags-table-mode)))

;; (add-hook 'desktop-after-read-hook 'majmodpri-apply)

;; Prepare Emacs desktop after loading Emacs
;; (add-hook 'after-init-hook
;;           (lambda ()
;;             ;; Show home directory on left pane, and last visited file on right
;;             (split-window-horizontally)
;;             (dired (my-home ?h)))
;;           ;; Note that 3-rd argument of this `add-hook' should be `t'
;;           ;; to append the call of the `dired' after other hooked functions,
;;           ;; most importantly after `desktop-read'.
;;           t)

;; (add-hook 'desktop-after-read-hook 'plan)
;;Use M-x desktop-save once to save the desktop.When it exists, Emacs updates it on every exit.

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



(provide '60session)
