;;; emms-lyrics-download.el --- Download lyrics automatically

;; Copyright (C) 2008 Austin

;; Author: Austin <austiny.cn@gmail.com>
;; Keywords: emms music lyrics

;;; Commentary:

;; This package enables you to download lyrics automatically.

;; Put this file into your load-path and the following into your
;; ~/.emacs:
;;              (require 'emms-lyrics-download)
;;              (ad-activate 'emms-lyrics-find-lyric)

;; Then lyrics will be downloaded into the folder that contains your music files when lyrics is enabled (either by 'M-x emms-lyrics-enable' or add (emms-lyrics 1) in your .emacs)
;; Note that lyrics downloading depends the ID3 tag of the mp3. So correct ID3 tag may help you download the correct lyrics.
;; You may re-download the lyrics by 'M-x emms-lyrics-redownload-lyric'.

;;; Code:
;; Changlog:

;; 26.05.2009 S.P.Tseng <deftsp@gmail.com>
;; Try download lyrics on winampcn.com only once.

(defvar emms-lyrics-download nil
  "If `non-nil' auto downlaod lyrics, otherwise not.")


(defvar emms-tracks-have-no-lyrics nil
  "Hold the tracks that have no lyrics on winampcn.com")

(defvar emms-tracks-have-no-lyrics-file "~/.emacs.d/.emms/emms-tracks-no-lyrics"
  "The file to hold tracks have no lyrics on winampcn.com")


(defun emms-tracks-have-no-lyrics-save ()
  "Save tracks have no lyrics on winampcn.com to a file"
  (interactive)
  (set-buffer (get-buffer-create " emms-tracks-no-lyrics "))
  (erase-buffer)
  (insert
   (concat ";;; .emms-cache -*- mode: emacs-lisp; coding: "
           (symbol-name emms-cache-file-coding-system)
           "; -*-\n"))
  (print `(setq emms-tracks-have-no-lyrics (quote ,emms-tracks-have-no-lyrics)) (current-buffer))
  (write-region (point-min) (point-max) emms-tracks-have-no-lyrics-file))


(defun emms-tracks-have-no-lyrics-restore ()
    "Restore the track have no lyrics from a file."
    (interactive)
    (load emms-tracks-have-no-lyrics-file t nil t))

(add-hook 'after-init-hook 'emms-tracks-have-no-lyrics-restore)
(add-hook 'kill-emacs-hook 'emms-tracks-have-no-lyrics-save)



(defun http-url-encode (str content-type)
  "URL encode STR using CONTENT-TYPE as the coding system."
  (apply 'concat
         (mapcar (lambda (c)
                   (if (or (and (>= c ?a) (<= c ?z))
                          (and (>= c ?A) (<= c ?Z))
                          (and (>= c ?0) (<= c ?9)))
                       (string c)
                       (format "%%%02x" c)))
                 (encode-coding-string str content-type))))

(defun emms-lyrics-download-winampcn (file idx)
  "Download lyrics from winampcn.com according to what's playing."
  (when (not (member (emms-track-get (emms-playlist-current-selected-track) 'name) emms-tracks-have-no-lyrics))
    (let* ((itrack (emms-playlist-current-selected-track))
           (artist (emms-track-get itrack 'info-artist))
           (title (emms-track-get itrack 'info-title))
           (iartist (replace-regexp-in-string "[ ,]" "" artist))
           (ititle (replace-regexp-in-string "[ ,]" "" title))
           (uartist (http-url-encode iartist 'gb2312))
           (utitle (http-url-encode ititle 'gb2312))
           (lurl (format "http://www.winampcn.com/lyrictransfer/get.aspx?song=%s&artist=%s&lsong=%s&Datetime=20060901" utitle uartist utitle))
           (lfile (url-file-local-copy lurl))
           (llist (with-temp-buffer
                    (insert-file-contents lfile)
                    (let* ((urllist ())
                           (st 0)
                           (ed 0))
                      (while (setq st (re-search-forward "!\\[CDATA\\[" nil t))
                        (when (setq ed (re-search-forward "\\]\\]" nil t))
                          (let* ((url (buffer-substring st (- ed 2))))
                            (setq urllist (nconc urllist (list url))))))
                      urllist)))
           (lrcurl (nth (- idx 1) llist)))
      (delete-file lfile)
      (if (and (not (eq nil lrcurl)))
          (progn
            (let* ((lfile2 (url-file-local-copy lrcurl))
                   (st 0))
              (with-temp-buffer
                (insert-file-contents lfile2)
                (setq st (re-search-forward "</head>" nil t))
                (write-region st (point-max) file nil nil nil nil))
              (delete-file lfile2)
              (message (format "Lyrics: %s - %s downloaded (%d of %d)." artist title idx (length llist)))))
          (message (format "Lyrics: %s - %s not found." artist title))
          (pushnew (emms-track-get (emms-playlist-current-selected-track) 'name) emms-tracks-have-no-lyrics)))))

(defadvice emms-lyrics-find-lyric (before emms-lyrics-find-download-lyric (file))
  "Adding lyrics downloading to emms-lyrics-find-lyric"
  (let* ((track (emms-playlist-current-selected-track))
         (dir (file-name-directory (emms-track-get track 'name))))
    (when (eq 'file (emms-track-get track 'type))
      ;; If find two or more lyric files, only return the first one. Good luck! :-)
      (unless (file-exists-p (concat dir file)) ; same directory?
        (emms-lyrics-download-winampcn (concat dir file) 1)))))

(defun emms-lyrics-redownload-lyric (&optional idx)
  "Interactively redownload lyrics for current playing. A prefix
can be taken to specify which lyrics to download if there is
multiple. (idx starts from 1)"
  (interactive "P")
  (let* ((track (emms-playlist-current-selected-track))
         (name (emms-track-get track 'name))
         (lrc (funcall emms-lyrics-find-lyric-function
                       (emms-replace-regexp-in-string
                        (concat "\\." (file-name-extension name) "\\'")
                        ".lrc"
                        (file-name-nondirectory name)))))
    (if (eq idx nil)
	(setq idx 1))
    (when (member name emms-tracks-have-no-lyrics)
      (setq emms-tracks-have-no-lyrics (remove name emms-tracks-have-no-lyrics)))
    (emms-lyrics-download-winampcn lrc idx)))


(defun emms-lyrics-download-on ()
  (interactive)
  (setq emms-lyrics-download t)
  (ad-activate 'emms-lyrics-find-lyric))

(defun emms-lyrics-download-off nil
  (interactive)
  (setq emms-lyrics-download nil)
  (ad-deactivate 'emms-lyrics-find-lyric))

(defun emms-lyrics-download-toggle ()
  "toggle auto download lyrics from winampcn."
  (interactive)
  (if emms-lyrics-download
      (progn
        (emms-lyrics-download-off)
        (message "emms lyrics auto download off."))
      (emms-lyrics-download-on)
      (message "emms lyrics auto download on.")))


(provide 'emms-lyrics-download)

;;; emms-lyrics-download.el ends here
