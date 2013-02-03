;;; 12el-get.el ---

;; Copyright (C) 2011  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>


(add-to-list 'load-path "~/.emacs.d/el-get/el-get")


(unless (require 'el-get nil t)
  (url-retrieve
   "https://raw.github.com/dimitri/el-get/master/el-get-install.el"
   (lambda (s)
     (let (el-get-master-branch)
       (goto-char (point-max))
       (eval-print-last-sexp)))))

(setq el-get-github-default-url-type 'git)

;; el-get-sources is another source location for recipes, adding to your el-get-recipe-path.
(setq el-get-sources
      '((:name org-mac-protocol
               :type git
               :url "git://github.com/claviclaws/org-mac-protocol.git"
               :features org-mac-protocol)
        (:name etags-select
               :type emacswiki
               :features etags-select)

        (:name cursor-chg
               :description "Change cursor dynamically, depending on the context."
               :type emacswiki
               :features cursor-chg)

        (:name window-extension
               :description "Some extension functions manipulate window."
               :type emacswiki
               :features window-extension)


        (:name hide-comnt
               :description "Hide/show comments in code."
               :type emacswiki
               :features hide-comnt)

        (:name thing-cmds
               :description "Commands that use things, as defined by `thingatpt.el'."
               :type emacswiki
               :depends hide-comnt
               :features thing-cmds)

        (:name thingatpt+
               :description "Extensions to `thingatpt.el'."
               :type emacswiki
               :features thingatpt+)

        (:name emacs-xcode-document-viewer
               :type git
               :url "git://github.com/sakito/emacs-xcode-document-viewer.git"
               :features el-get
               :load    "xcode-document-viewer.el"
               :compile "xcode-document-viewer.el")
        (:name ac-company
               :type http
               :url "https://raw.github.com/buzztaiki/auto-complete/master/ac-company.el")
        (:name docsetutil-el
               :type github
               :description "Emacs Interface to `docsetutil'"
               :pkgname "leoliu/docsetutil-el"
               :features docsetutil)

        (:name glsl-mode
               :type github
               :description "major mode for Open GLSL shader files"
               :pkgname "jimhourihan/glsl-mode"
               :features glsl-mode)
        (:name bookmark-plus
               :pkgname "emacsmirror/bookmark-plus"
               :website "http://www.emacswiki.org/emacs/BookmarkPlus"
               :type github
               :description "Extensions to standard library `bookmark.el'"
               :features bookmark+)))

;; (el-get-save-package-status "package-name-here" "removed")

;; (el-get 'sync 'org-mode)                ; init org-mode first
(el-get 'sync 'auto-complete)           ; auto-complete should init before ac-nrepl
(el-get 'wait)                          ; 'sync 'wait


(provide '12el-get)
