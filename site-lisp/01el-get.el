;;; 01el-get.el ---

;; Copyright (C) 2011  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>


(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil t)
  (url-retrieve
   "https://github.com/dimitri/el-get/raw/master/el-get-install.el"
   (lambda (s)
     (end-of-buffer)
     (eval-print-last-sexp))))



(setq el-get-sources
      '(cssh el-get emacs-w3m switch-window vkill xcscope yasnippet package magit dired+ bbdb
             auto-complete
             (:name ecb
                    :type cvs
                    :module "ecb"
                    :url ":pserver:anonymous@ecb.cvs.sourceforge.net:/cvsroot/ecb"
                    :build `(,(concat  "make CEDET=" " EMACS=" el-get-emacs)))
             (:name cedet
                    :type bzr
                    :url "bzr://cedet.bzr.sourceforge.net/bzrroot/cedet/code/trunk"
                    :build ("touch `find . -name Makefile`" "make")
                    :build/windows-nt ("echo #!/bin/sh > tmp.sh & echo touch `/usr/bin/find . -name Makefile` >> tmp.sh & echo make FIND=/usr/bin/find >> tmp.sh"
                                       "sed 's/^M$//' tmp.sh  > tmp2.sh"
                                       "sh ./tmp2.sh" "rm ./tmp.sh ./tmp2.sh")
                    :load-path ("./common")
                    :post-init (lambda ()
                                 (require 'cedet)))
             ;; etags-table in emacs-wiki seems have bug
             (:name etags-table
                    :type http
                    :url "http://bitbucket.org/sakito/dot.emacs.d/raw/tip/local-lisp/etags-table.el"
                    :features etags-table)
             (:name etags-select
                    :type emacswiki
                    :features etags-select)
             (:name emacs-xcode-document-viewer
                    :type git
                    :url "git://github.com/sakito/emacs-xcode-document-viewer.git"
                    :features el-get
                    :load    "xcode-document-viewer.el"
                    :compile "xcode-document-viewer.el")
             (:name ac-company
                    :type http
                    :url "https://raw.github.com/buzztaiki/auto-complete/master/ac-company.el")))
(el-get)                          ; 'sync 'wait






