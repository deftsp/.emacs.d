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
(setq el-get-verbose t)


;; el-get-sources is another source location for recipes, adding to your el-get-recipe-path.
(setq el-get-sources
      '((:name el-get :branch "shihpin/dev")
        (:name org-mac-protocol
               :type git
               :url "git://github.com/claviclaws/org-mac-protocol.git")
        (:name undo-tree
               :type git
               :url "http://www.dr-qubit.org/git/undo-tree.git"
               :features undo-tree)
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
        (:name git-emacs
               :description "Yet another git emacs mode for newbies"
               :type github
               :pkgname "deftsp/git-emacs"
               :features git-emacs)
        (:name hamlet-mode
               :description "An emacs mode for editing files written in Hamlet, Yesod's HTML-like templating language."
               :type github
               :pkgname "lightquake/hamlet-mode"
               :features hamlet-mode)
        (:name wcheck-mode
               :description "General interface for text checkers."
               :type github
               :pkgname "tlikonen/wcheck-mode"
               :features wcheck-mode)
        (:name glsl-mode
               :type github
               :description "major mode for Open GLSL shader files"
               :pkgname "jimhourihan/glsl-mode"
               :features glsl-mode)
        (:name vimrc-mode
               :description "Enables syntax highlighting for .vimrc/_vimrc files"
               :type github
               :pkgname "mcandre/vimrc-mode"
               :prepare (progn
                          (add-to-list 'auto-mode-alist
                                       '(".vim\\(rc\\|peratorrc\\)?$" . vimrc-mode))))
        (:name bookmark+
               :pkgname "deftsp/bookmark-plus"
               :website "http://www.emacswiki.org/emacs/BookmarkPlus"
               :type github
               :description "Extensions to standard library `bookmark.el'"
               :features bookmark+)
        (:name evil
               :website "http://gitorious.org/evil/pages/Home"
               :description "Evil is an extensible vi layer for Emacs. It
       emulates the main features of Vim, and provides facilities
       for writing custom extensions."
               :type git
               :url "git://gitorious.org/evil/evil.git"
               :features evil
               :depends undo-tree
               :build `(("make" ,(format "EMACS=%s" (shell-quote-argument el-get-emacs)) "all" "info"))
               :build/berkeley-unix (("gmake" "all" "info"))
               :info "doc")
        (:name evil-plugins
               :type github
               :description "Plugins for Emacs Evil."
               :pkgname "tarao/evil-plugins")
        (:name evil-indent-textobject
               :type github
               :description "evil textobjects based on indentation"
               :pkgname "cofi/evil-indent-textobject")
        (:name evil-matchit
               :type github
               :description "Vim matchit ported to Evil"
               :pkgname "redguardtoo/evil-matchit")
        (:name evil-exchange
               :type github
               :description "Exchange text more easily within Evil"
               :pkgname "Dewdrops/evil-exchange"
               :features evil-exchange
               :depends evil)
        (:name linum-relative
               :type github
               :description "display relative line number in emacs."
               :pkgname "coldnew/linum-relative"
               :features linum-relative)
        (:name keyfreq
               :type github
               :description "track command frequencies"
               :pkgname "dacap/keyfreq"
               :features keyfreq)
        (:name lua2-mode
               :website "http://www.enyo.de/fw/software/lua-emacs/lua2-mode.html"
               :description "a semantic highlighting extension for lua-mode"
               :type http
               :depends lua-mode
               :url "http://www.enyo.de/fw/software/lua-emacs/lua2-mode.el")
        (:name helm-swoop
               :type github
               :description "Efficiently hopping squeezed lines powered by helm interface"
               :pkgname "ShingoFukuyama/helm-swoop")
        (:name helm-dash
               :type github
               :description "Helm extension to search dash docsets"
               :pkgname "areina/helm-dash"
               :depends esqlite)
        (:name esqlite
               :type github
               :description "Manipulate sqlite file from Emacs"
               :pkgname "mhayashi1120/Emacs-esqlite"
               :load-path ("." "./Emacs-pcsv"))
        (:name ace-link
               :type github
               :description "Quickly follow links using `ace-jump-mode'"
               :pkgname "abo-abo/ace-link"
               :depends ace-jump-mode)
        (:name guide-key
               :type github
               :description "Guide the following key bindings automatically and dynamically"
               :pkgname "kbkbkbkb1/guide-key"
               :depends popwin)
        (:name flycheck-haskell
               :type github
               :description "Improved Haskell support for Flycheck"
               :pkgname "flycheck/flycheck-haskell"
               :depends (f dash haskell-mode flycheck))
        (:name haskell-mode
               :description "A Haskell editing mode"
               :type github
               :pkgname "deftsp/haskell-mode"
               :info "."
               :branch "develop"
               :build `(("make" ,(format "EMACS=%s" (shell-quote-argument el-get-emacs)) "all"))
               :post-init (progn
                            (require 'haskell-mode-autoloads)))))

;; (el-get-save-package-status "package-name-here" "removed")

;; (el-get 'sync 'org-mode)                ; init org-mode first
(el-get 'sync 'exec-path-from-shell)
(require 'exec-path-from-shell nil t) ; https://github.com/purcell/exec-path-from-shell
(el-get 'sync 'auto-complete)           ; auto-complete should init before ac-nrepl
(el-get 'sync 'clojure-mode)
(el-get 'sync 'evil)
(el-get 'sync)                          ; 'sync 'wait


(provide '12el-get)
