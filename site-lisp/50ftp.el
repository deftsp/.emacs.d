;;; 50ftp.el ---

;; Copyright (C) 2008  S.P.Tseng


;; (require 'ftp-file-upload)
;; (defun ftp-dired-upload-file ()
;;   (interactive)
;;   (ftp-upload-file (expand-file-name (dired-get-file-for-visit))))

;; (define-key dired-mode-map "\C-c\C-u" 'ftp-dired-upload-file)

;; I also added a keybinding to open my images directory. Who knows, maybe one day I'll even use it :)

;; (defconst *ftp-images-directory* (concat *ftp-WEB* "blog/images"))

;; (defun ftp-dired-get-images-dir ()
;;   (interactive)
;;   (dired *ftp-images-directory*))

;; (global-set-key "\C-Ci" 'ftp-dired-get-images-dir)
