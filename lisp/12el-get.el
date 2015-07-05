;;; 12el-get.el ---

;; Copyright (C) 2011  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>

;;; manually download and install
;; clone the package and place it to ~/.emacs.d/el-get, then execute
;; (el-get-save-package-status 'helm "required")
;; (el-get-post-install 'helm)

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(unless (require 'el-get nil t)
  (url-retrieve
   "https://raw.github.com/dimitri/el-get/master/el-get-install.el"
   (lambda (s)
     (let (el-get-master-branch)
       (goto-char (point-max))
       (eval-print-last-sexp)))))

(setq el-get-github-default-url-type 'git)
(setq el-get-verbose t)


;; el-get-sources is another source location for recipes, adding to your el-get-recipe-path.
(setq el-get-sources
      '((:name org-mac-protocol
               :type git
               :url "git://github.com/claviclaws/org-mac-protocol.git")
        (:name etags-select
               :type emacswiki
               :features etags-select)
        (:name cursor-chg
               :description "Change cursor dynamically, depending on the context."
               :type emacswiki
               :features cursor-chg)
        (:name col-highlight
               :description "Highlight the current column."
               :type emacswiki
               :depends vline)
        (:name crosshairs
               :description "Highlight the current line and column."
               :type emacswiki
               :depends col-highlight)
        (:name eldoc-extension
               :description "Some extension for eldoc."
               :type emacswiki)
        (:name etags-table
               :description "Set tags table(s) based on current file"
               :type emacswiki)
        (:name highlight
               :description "Highlighting commands."
               :type emacswiki)
        (:name window-extension
               :description "Some extension functions manipulate window."
               :type emacswiki
               :features window-extension)
        (:name hide-comnt
               :description "Hide/show comments in code."
               :type emacswiki
               :features hide-comnt)
        (:name hl-line+
               :description "Extensions to hl-line.el."
               :type emacswiki)
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
        (:name git-emacs
               :description "Yet another git emacs mode for newbies"
               :type github
               :pkgname "deftsp/git-emacs"
               :features git-emacs)
        (:name info+
               :description "Extensions to `info.el'."
               :type emacswiki)
        (:name bookmark+
               :pkgname "deftsp/bookmark-plus"
               :website "http://www.emacswiki.org/emacs/BookmarkPlus"
               :type github
               :description "Extensions to standard library `bookmark.el'"
               :features bookmark+)
        (:name evil-lispy
               :website "https://github.com/deftsp/evil-lispy"
               :description "Minor mode to integrate Lispy and Evil"
               :type github
               :url "git@github.com:deftsp/evil-lispy.git"
               :branch "develop"
               :depends (evil lispy))
        (:name linum-relative
               :type github
               :description "display relative line number in emacs."
               :pkgname "coldnew/linum-relative"
               :features linum-relative)
        (:name chinese-fonts-setup
               :website "https://github.com/tumashu/chinese-fonts-setup"
               :type github
               :description "A fonts config tool enforcing double-width Chinese character display."
               :pkgname "tumashu/chinese-fonts-setup"
               :features chinese-fonts-setup)
        (:name reveal-in-finder
               :website "https://github.com/kaz-yos/elisp"
               :description "Reveal file associated with buffer in OS X Finder"
               :type http
               :url "https://github.com/kaz-yos/elisp/raw/master/reveal-in-finder.el")
        (:name helm-dash
               :type github
               :description "Helm extension to search dash docsets"
               :pkgname "areina/helm-dash"
               :depends esqlite)
        (:name iedit
               :description "Edit multiple regions simultaneously in a buffer or a region."
               :type github
               :pkgname "victorhge/iedit"
               :depends (cl-lib))
        (:name evil-iedit-state
               :type github
               :description "Slick Evil states for iedit."
               :pkgname "syl20bnr/evil-iedit-state"
               :depends (evil iedit))
        (:name esqlite
               :type github
               :description "Manipulate sqlite file from Emacs"
               :pkgname "mhayashi1120/Emacs-esqlite"
               :load-path ("." "./Emacs-pcsv"))
        (:name ido-ubiquitous
               :type github
               :description "Fancy completion all over Emacs, not just for buffers and files."
               :pkgname "DarwinAwardWinner/ido-ubiquitous")
        (:name pinyin-search
               :type github
               :description "Search Chinese by the first letter of Chinese pinyin."
               :pkgname "xuchunyang/pinyin-search.el")
        (:name minimap
               :type github
               :description "Sidebar showing a "mini-map" of a buffer"
               :pkgname "dengste/minimap")))

;; (el-get-save-package-status "package-name-here" "removed")

(el-get 'sync 'org-mode) ; init org-mode first
(el-get 'sync 'exec-path-from-shell)
(require 'exec-path-from-shell nil t) ; https://github.com/purcell/exec-path-from-shell
;; (el-get 'sync 'auto-complete)           ; auto-complete should init before ac-nrepl
(el-get 'sync 'clojure-mode)
(el-get 'sync 'evil)
(el-get 'sync)                          ; 'sync 'wait


(provide '12el-get)
;;; 12el-get.el ends here
