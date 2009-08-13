;;; 50eshell.el ---

;; Copyright (C) 2008  S.P.Tseng

;; Author: S.P.Tseng <deftsp@gmail.com>

;; (setq eshell-save-history-on-exit t)

;; you can evaluate elisp expressions at the eshell prompt, in the same way you’d do at a Common Lisp REPL.

;; (add-hook 'eshell-first-time-mode-hook
;;           (lambda ()
;;             (setq eshell-visual-commands
;;                   (append '("mutt" "vim" "screen" "lftp" "ipython" "telnet")
;;                           eshell-visual-commands))))
(add-hook 'eshell-mode-hook '(lambda ()
                              (local-set-key (kbd "C-c C-n") (lambda ()
                                                               (interactive)
                                                               (goto-char (point-max))
                                                               (insert "ls")
                                                               (eshell-send-input)))
                              (local-set-key (kbd "<up>") 'previous-line)
                              (local-set-key (kbd "<down>") 'next-line)
                              (local-set-key (kbd "C-<up>") 'eshell-previous-matching-input-from-input)
                              (local-set-key (kbd "C-<down>") 'eshell-next-matching-input-from-input)
                              (local-set-key [home] 'eshell-backward-argument)
                              (eshell/export "EPOCROOT=\\Paragon\\")
                              (define-key eshell-mode-map "\C-d" 'my-eshell-delchar-or-exit)
                              (or (getenv "CDPATH") (setenv "CDPATH" ".:~:/usr/local"))
                              ;; (let ((path))
                              ;;   (setq path ".;/usr/local/bin:/usr/bin:/bin:/usr/bin/X11:/usr/games")
                              ;;   (setenv "PATH" path))
                              (local-set-key "\C-u" 'eshell-kill-input)
                              (local-set-key "\C-a" 'eshell-maybe-bol)))

;; (let ((path (split-string (getenv "PATH") ":")))
;;   (add-to-list 'path "/home/tsp/bin")
;;   (setenv "PATH" (mapconcat 'identity path ":")))

;;type the command clear within the eshell to clear the entire buffer
(defun eshell/clear ()
  "04Dec2001 - sailor, to clear the eshell buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)))


;;; Option Variables:
(setq eshell-directory-name  "~/.eshell/")
(and (eq window-system 'w32)
     (setq eshell-windows-shell-file "c:\\windows\\command.com"))

;; eshell-basic
(setq eshell-plain-echo-behavior t)

;; lastdir & history
;; (setq eshell-last-dir-ring-file-name
;;       (concat eshell-directory-name "lastdir." my-host-name))
;; (setq eshell-ask-to-save-last-dir 'always)
;; (setq eshell-history-file-name
;;       (concat eshell-directory-name "history." my-host-name))

;; eshell-glob
(setq eshell-ask-to-save-history 'always)
(setq eshell-error-if-no-glob t)

;; eshell-prompt
;; (setq eshell-prompt-function
;;       (lambda()
;;         (concat (getenv "USER") "@" (getenv "HOST") ":"
;;                 ((lambda (p-lst)
;;                    (if (> (length p-lst) 3)
;;                        (concat
;;                         (mapconcat (lambda (elm) (substring elm 0 1))
;;                                    (butlast p-lst (- (length p-lst) 3))
;;                                    "/")
;;                         "/"
;;                         (mapconcat (lambda (elm) elm)
;;                                    (last p-lst (- (length p-lst) 3))
;;                                    "/"))
;;                        (mapconcat (lambda (elm) elm)
;;                                   p-lst
;;                                   "/")))
;;                  (split-string (eshell/pwd) "/"))
;;                 (if (= (user-uid) 0) "# " "$ "))))

;; (setq eshell-prompt-regexp "^[^#$\n]+@[^#$\n]+:*[#$%] ")

;; private variable
(setq my-eshell-ignore-eof nil)
(defun my-eshell-delchar-or-exit (arg)
  (interactive "P")
  (let ((cur-point (point))
        (delchar-flag t))
    (save-excursion
      (eshell-bol)
      (when (and (not my-eshell-ignore-eof)
                 (= cur-point (point))
                 (= cur-point (point-max)))
        (setq delchar-flag nil)))
    (if delchar-flag
        (delete-char (if (null arg) 1 arg) (if (null arg) nil t))
        (eshell-bol)
        (insert "exit")
        (eshell-send-input))))




(setq eshell-history-size 500)
;; (global-set-key (kbd "C-c s") 'eshell-here)
;; (global-set-key (kbd "C-z") 'eshell)

(defalias 'eshell/fg 'bury-buffer)

(defun eshell-here ()
  "Run eshell in the current directory."
  (interactive)
  (let ((dir default-directory))
    (eshell)
    (unless (string= default-directory dir)
      (message "Switching to %s" dir)
      (eshell/cd (list dir))
      (eshell-emit-prompt))))

(defun eshell/info (&rest args)
  (Info-directory)
  (Info-menu (car args)))

(defun eshell/cvs-examine (&rest ignore)
  (cvs-examine "." nil))

;; Launch a shell command in the background, with its output in its own buffer (if you have to run several instances of
;; the same program, e.g. xterm, add some spaces to the end of the command, so the buffer name will be different and you
;; can run the program withoud killing the other instances.)
;; (defun m-shell-command ()
;;   "Launch a shell command."
;;   (interactive)
;;   (let ((command (read-string "Command: ")))
;;     (shell-command (concat command " &") (concat "*" command "*"))))

;; Specialized interface for playing media files
;; (defun mplayer ()
;;   "An interface to mplayer."
;;   (interactive)
;;   (shell-command (concat "xterm -e mplayer "
;;                          (shell-quote-argument
;;                           (expand-file-name
;;                            (read-file-name "Filename: ")))
;;                          " & ")))


;; C-a to beginning of command line or beginning of line?

;; I use the following code. It makes C-a go to the beginning of the command line, unless it is already there, in which
;; case it goes to the beginning of the line. So if you are at the end of the command line and want to go to the real
;; beginning of line, hit C-a twice:

(defun eshell-maybe-bol ()
  (interactive)
  (let ((p (point)))
    (eshell-bol)
    (if (= p (point))
        (beginning-of-line))))


;; repeat last shell command:
(defun repeat-last-shell-command ()
  "Repeat last command passed to shell-command.
      From <http://www.dotemacs.de/dotfiles/KilianAFoth.emacs.html>."
  (interactive)
  (save-buffer)
  (or shell-command-history (error "Nothing to repeat."))
  (shell-command (car shell-command-history)))
(global-set-key (kbd "C-c r s") 'repeat-last-shell-command)


;; Interfacing with the Debian System

;; Here is a cool function by MilanZamazal? that brings lots of Debian commands together. Note how options are defined
;; and documented using eshell-eval-using-options.
(defun eshell/deb (&rest args)
  (eshell-eval-using-options
   "deb" args
   '((?f "find" t find "list available packages matching a pattern")
     (?i "installed" t installed "list installed debs matching a pattern")
     (?l "list-files" t list-files "list files of a package")
     (?s "show" t show "show an available package")
     (?v "version" t version "show the version of an installed package")
     (?w "where" t where "find the package containing the given file")
     (nil "help" nil nil "show this usage information")
     :show-usage)
   (eshell-do-eval
    (eshell-parse-command
     (cond
       (find
        (format "apt-cache search %s" find))
       (installed
        (format "dlocate -l %s | grep '^.i'" installed))
       (list-files
        (format "dlocate -L %s | sort" list-files))
       (show
        (format "apt-cache show %s" show))
       (version
        (format "dlocate -s %s | egrep '^(Package|Status|Version):'" version))
       (where
        (format "dlocate %s" where))))
    t)))

(defalias 'eshell/emacs 'find-file)
(defun eshell/dired () (dired (eshell/pwd)))
(defalias 'eshell/man 'woman)
