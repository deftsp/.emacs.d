;;; 50ftp.el ---

;; Copyright (C) 2008  S.P.Tseng

;; Author: S.P.Tseng <deftsp@gmail.com>

;; url: http://juangarcia.890m.com/files/ftp-wrappers.el
;; ---------------------------------------------------------------------- ;;

(require 'ange-ftp)

(defconst *ftp-max-timeout* 100)

(defvar *ftp-process* nil)
(defvar *ftp-last-action* 0)

(defvar *ftp-user* "deftsp")
(defvar *ftp-host* "ftp.some-host.com")
(defvar *ftp-password* "xxxxxx")

(setq ange-ftp-ftp-program-name "ftp")

(ange-ftp-set-passwd *ftp-host* *ftp-user* *ftp-password*)

;; (ftp-connect *ftp-host* *ftp-user*)

;; ---------------------------------------------------------------------- ;;

(defun ftp-has-timed-out-p ()
  (> (- (time-to-seconds (current-time)) *ftp-last-action*)
       *ftp-max-timeout*))

(defun ftp-set-last-action ()
  (setq *ftp-last-action* (time-to-seconds (current-time))))

(defun ftp-connected-p (proc)
  (and proc
       (equal (process-status proc) 'run)
       (ftp-has-timed-out-p)))

(defun ftp-kill-process (host user proc)
  (when (ftp-connected-p proc)
    (kill-process proc)
    (setq proc nil)
    (set-buffer (get-buffer (ange-ftp-ftp-process-buffer host user)))
    (insert "\nTerminated"))
  nil)

(defun ftp-connect (host user)
  (when (ftp-has-timed-out-p)
    (ftp-kill-process host user *ftp-process*)
    (sit-for 0)
    (sleep-for 1))
  (when (not (ftp-connected-p *ftp-process*))
    (setq *ftp-process*
          (ange-ftp-get-process host user))
    (ange-ftp-set-binary-mode host user)
    (ftp-set-last-action))
  *ftp-process*)

(defun ftp-raw-send-cmd-wrapper (cmd)
  (let ((proc (ftp-connect *ftp-host* *ftp-user*)))
    (ange-ftp-raw-send-cmd proc cmd)
    (ftp-set-last-action))
  t)

;; (ftp-raw-send-cmd-wrapper "dir .emacs")

(defun ftp-copy-file (from to)
  (ftp-raw-send-cmd-wrapper
   (format "put %s %s" (expand-file-name from) to)))

(defun ftp-mkdir (dir)
  (ftp-raw-send-cmd-wrapper (format "mkdir %s" dir)))

(defun ftp-get-filename ()
  (let* ((f (expand-file-name (buffer-file-name (current-buffer))))
         (p (split-string f "/")))
    (nthcdr (- (length p) 2) p)))

;; ---------------------------------------------------------------------- ;;

(provide 'ftp-wrappers)

;; ---------------------------------------------------------------------- ;;
