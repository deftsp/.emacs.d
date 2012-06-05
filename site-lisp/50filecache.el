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


(eval-after-load "filecache"
  '(progn
     (setq file-cache-completion-ignore-case t
           file-cache-ignore-case t)
     (mapcar (lambda (str) (add-to-list 'file-cache-filter-regexps str))
             '("\\.svn-base$" "\\.svn" "\\.jar$" "\\.git$" "\\.gz$" "\\.tar$" "\\.rar$"))
     (file-cache-add-directory-using-find "~/proj/notebook")
     ;; (file-cache-add-directory-list load-path)
     (let ((cache-dirs '("~/" "/etc/" "~/.emacs.d/site-lisp/")))
       (dolist (dir cache-dirs) (file-cache-add-directory dir)))))

(provide '50filecache)
