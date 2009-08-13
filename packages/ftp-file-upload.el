;;; ftp-file-upload.el ---

;; url: http://juangarcia.890m.com/files/ftp-file-upload.el


;; ---------------------------------------------------------------------- ;;

(require 'ftp-wrappers)

;; ---------------------------------------------------------------------- ;;

(defconst *ftp-HOME* "c:/home/juang/")
(defconst *ftp-WEB* (concat *ftp-HOME* "website/"))

(defconst *ftp-location-map*
  (list
   (list (concat *ftp-WEB* "wordpress") "/www/blog")
   (list *ftp-WEB* "/www/")
   (list (concat *ftp-HOME* "emacs-files") "/www/files")))

(defcustom *ftp-upload-skip-prompt* nil
  "Skip the ftp prompt")

;; ---------------------------------------------------------------------- ;;

(defun ftp-upload-file (from)
  (let ((to from))
    (dolist (tuple *ftp-location-map*)
      (let ((regex (car tuple))
	    (replace (cadr tuple)))
	(when (string-match (concat "^" regex) to)
	  (setq to (replace-match replace t t to)))))
    (if (or *ftp-upload-skip-prompt*
	    (y-or-n-p (format "Query: Upload %s to %s ? " from to)))
	(ftp-copy-file from to)
      (message "Info: upload %s to %s aborted" from to))))

(defun ftp-upload-current-file ()
  (interactive)
  (ftp-upload-file (expand-file-name (buffer-file-name))))

;; (global-set-key [f9] 'ftp-upload-current-file)

;; ---------------------------------------------------------------------- ;;

(provide 'ftp-file-upload)

;; ---------------------------------------------------------------------- ;;

