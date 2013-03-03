;;; 50filecache.el ---

;; Copyright (C) 2008  Shihpin Tseng

;; Author: Shihin Tseng <deftsp@gmail.com>
;; Keywords:

;; filecache makes looksups very fast

;; You can start the file-cache by either using 'C-TAB' when entering a file name in the MiniBuffer ('C-x C-f C-TAB C-g'
;; works)

;; if ido-everywhere is on, `C-Tab' file-cache-minibuffer-complete can not replace the previous
;; path.`minibuffer-prompt-end' can not work correctly.


(require 'filecache)
;; define dirs for cacheing file dirs
;; see http://www.emacswiki.org/cgi-bin/wiki/FileNameCache for more tricks with this...

(defvar pl/file-cache-dirs '("~/" "/etc/" "~/.emacs.d/site-lisp/" "~/Downloads/"))
(defvar pl/file-cache-recursive-dirs '("~/proj"))

(defun pl/filecache-add-files ()
  (ignore-errors (file-cache-add-directory-list pl/file-cache-dirs)
   ;; (file-cache-add-directory-list load-path)
   (let ((cache-dirs pl/file-cache-recursive-dirs))
     (dolist (dir cache-dirs) (file-cache-add-directory-recursively dir)))))

(eval-after-load "filecache"
  '(progn
     (setq file-cache-completion-ignore-case t
           file-cache-ignore-case t)
     (mapcar (lambda (str) (add-to-list 'file-cache-filter-regexps str))
             '("\\.svn-base$" "\\.svn" "\\.jar$" "\\.git$" "\\.gz$" "\\.tar$" "\\.rar$" "\\.exe$" "resolv.conf$"))
     ;; works after 60 seconds and repeat every half hour
     (run-at-time (pl/future-time-string 60) (* 0.5 60 60) 'pl/filecache-add-files)))

(provide '50filecache)
