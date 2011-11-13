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
      '((:name org-mac-protocol
               :type git
               :url "git://github.com/claviclaws/org-mac-protocol.git"
               :features org-mac-protocol)
        ;; (:name ecb
        ;;        :type cvs
        ;;        :module "ecb"
        ;;        :url ":pserver:anonymous@ecb.cvs.sourceforge.net:/cvsroot/ecb"
        ;;        :build `(,(concat  "make CEDET=" " EMACS=" el-get-emacs)))
        ;; (:name cedet
        ;;        :type bzr
        ;;        :url "bzr://cedet.bzr.sourceforge.net/bzrroot/cedet/code/trunk"
        ;;        :build ("touch `find . -name Makefile`" "make")
        ;;        :build/windows-nt ("echo #!/bin/sh > tmp.sh & echo touch `/usr/bin/find . -name Makefile` >> tmp.sh & echo make FIND=/usr/bin/find >> tmp.sh"
        ;;                           "sed 's/^M$//' tmp.sh  > tmp2.sh"
        ;;                           "sh ./tmp2.sh" "rm ./tmp.sh ./tmp2.sh")
        ;;        :load-path ("./common")
        ;;        :post-init (lambda ()
        ;;                     (require 'cedet)))
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


(setq site-packages
      (append '(org-mode cssh el-get emacs-w3m switch-window
                         vkill lua-mode xcscope yasnippet package
                         magit dired+ bbdb auto-complete undo-tree anything
                         git-emacs emacs-goodies-el emms haskell-mode
                         haskell-mode-exts predictive icomplete+ redshank)
              (mapcar 'el-get-source-name el-get-sources)))

(el-get 'sync site-packages)

;; (el-get)                          ; 'sync 'wait




