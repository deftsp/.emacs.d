;;; 50filecache.el ---

;; Copyright (C) 2008  Shihpin Tseng

;; Author: Shihin Tseng <deftsp@gmail.com>
;; Keywords:

;; filecache makes looksups very fast

;; You can start the file-cache by either using 'C-TAB' when entering a file name in the MiniBuffer ('C-x C-f C-TAB C-g'
;; works)

;; if ido-everywhere is on, `C-Tab' file-cache-minibuffer-complete can not replace the previous
;; path.`minibuffer-prompt-end' can not work correctly.

;;;; Code:
(require 'filecache)
;; define dirs for cacheing file dirs
;; see http://www.emacswiki.org/cgi-bin/wiki/FileNameCache for more tricks with this...

(defvar paloryemacs/file-cache-dirs '("~/" "/etc/" "~/.emacs.d/lisp/"))
(defvar paloryemacs/file-cache-recursive-dirs '("~/org"))

(defun paloryemacs/filecache-add-files ()
  (ignore-errors (file-cache-add-directory-list paloryemacs/file-cache-dirs)
   ;; (file-cache-add-directory-list load-path)
   (let ((cache-dirs paloryemacs/file-cache-recursive-dirs))
     (dolist (dir cache-dirs) (file-cache-add-directory-recursively dir)))))

(eval-after-load "filecache"
  '(progn
     (setq file-cache-completion-ignore-case t
           file-cache-ignore-case t)
     ;; works after 60 seconds and repeat every half day
     (run-at-time (paloryemacs/future-time-string 60) (* 12 60 60) 'paloryemacs/filecache-add-files)
     (mapcar (lambda (str) (add-to-list 'file-cache-filter-regexps str))
             '("\\.svn-base$" "\\.svn" "\\.jar$" "\\.git$" "\\.gz$" "\\.tar$" "\\.rar$" "\\.exe$" "resolv.conf$"))))

;; When the file "~/bin/datafiller" exists, (file-exists-p "~/bin/datafiller/")
;; => t, but (file-exists-p (file-truename "~/bin/datafiller/")) => nil
(defun paloryemacs/file-cache-add-this-file ()
  (and buffer-file-name
       (file-exists-p (file-truename buffer-file-name))
       (file-cache-add-file buffer-file-name)))

(add-hook 'kill-buffer-hook 'paloryemacs/file-cache-add-this-file)


(provide '50filecache)
;;; 50filecache.el ends here
