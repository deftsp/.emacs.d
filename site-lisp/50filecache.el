;;; 50filecache.el ---

;; Copyright (C) 2008  S.P.Tseng

;; Author: S.P.Tseng <deftsp@gmail.com>
;; Keywords:

;; filecache makes looksups very fast

;; You can start the file-cache by either using 'C-TAB' when entering a file name in the MiniBuffer ('C-x C-f C-TAB C-g'
;; works)

(require 'filecache)
;; define dirs for cacheing file dirs
;; see http://www.emacswiki.org/cgi-bin/wiki/FileNameCache for more tricks with this...

(setq file-cache-completion-ignore-case t
      file-cache-ignore-case t
      file-cache-filter-regexps '("~$" "\\.o$" "\\.exe$" "\\.a$" "\\.elc$"
                                  ",v$" "\\.output$" "\\.$" "#$" "\\.class$"
                                  "\\.svn-base$" "\\.svn" "\\.jar$" "\\.git$"
                                  "\\.gz$" "\\.tar$" "\\.rar$"))


(eval-after-load "filecache"
  '(progn
     (message "Loading file cache...")
     (file-cache-add-directory-using-find "~/proj")
     (file-cache-add-directory-list load-path)
     ;; (file-cache-add-file-list (list "~/foo/bar" "~/baz/bar"))
     (let ((cache-dirs '("~/" "/etc/")))
       (dolist (dir cache-dirs) (file-cache-add-directory dir)))))

;; (add-hook 'midnight-hook 'file-cache-update)
(add-hook 'midnight-hook 'save-merge-file-cache)

;; Save cache to a file
(defun file-cache-save-cache-to-file (file)
  "Save contents of `file-cache-alist' to FILE.
For later retrieval using `file-cache-read-cache-from-file'"
  (interactive "FFile: ")
  (with-temp-file (expand-file-name file)
    (prin1 file-cache-alist (current-buffer))))

(defun file-cache-read-cache-from-file (file)
  "Clear `file-cache-alist' and read cache from FILE.
  The file cache can be saved to a file using
  `file-cache-save-cache-to-file'."
  (interactive "fFile: ")
  (file-cache-clear-cache)
  (save-excursion
    (set-buffer (find-file-noselect file))
    (beginning-of-buffer)
    (setq file-cache-alist (read (current-buffer)))))

(defun save-merge-file-cache ()
  "save the file cache for merge tree"
  (interactive)
  (file-cache-save-cache-to-file "~/.filecache"))

(defun read-merge-file-cache ()
  "save the file cache for merge tree"
  (interactive)
  (file-cache-read-cache-from-file "~/.filecache"))


;;; Add to file-cache when `kill-buffer'
(defun file-cache-add-this-file ()
  (and buffer-file-name
       (file-exists-p buffer-file-name)
       (file-cache-add-file buffer-file-name)))

(add-hook 'kill-buffer-hook 'file-cache-add-this-file)



(if (file-exists-p (expand-file-name "~/.filecache"))
    (file-cache-read-cache-from-file "~/.filecache"))


(provide '50filecache)