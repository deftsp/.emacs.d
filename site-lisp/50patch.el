;;; tsp-patch.el ---

;; Copyright (C) 2008  S.P.Tseng

;; Author: S.P.Tseng <mendouer@163.com>


;;; help.el
;; Emacs 22.2  help.el
;;; It seems have been fixed
;; (defun describe-gnu-project ()
;;   "Display info on the GNU project."
;;   (interactive)
;;   (view-file (debian-expand-file-name-dfsg "THE-GNU-PROJECT"))
;;   (goto-char (point-min)))

;; (defun debian-expand-file-name-dfsg (filename)
;;   "Apply expand-file-name to FILENAME.
;; If expand-file-name does not find a file, append `.dfsg' and try again."
;;   (let ((file (expand-file-name filename data-directory)))
;;     (if (file-exists-p file)
;;         file
;;         (expand-file-name (concat file ".dfsg") data-directory))))


(provide '50patch)