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
      '((:name org-mac-protocol
               :type git
               :url "git://github.com/claviclaws/org-mac-protocol.git")
        (:name undo-tree
               :type git
               :url "http://www.dr-qubit.org/git/undo-tree.git"
               :features undo-tree)
        (:name etags-select
               :type emacswiki
               :features etags-select)
        (:name anaphora
               :type github
               :description "Anaphoric expressions for Emacs Lisp, providing implicit temporary variables."
               :pkgname "rolandwalker/anaphora")
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

        (:name fvwm-mode
               :description "major mode for editing Fvwm configuration files."
               :type github
               :pkgname "theBlackDragon/fvwm-mode")

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
        (:name info+
               :description "Extensions to `info.el'."
               :type emacswiki)

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
               :pkgname "tarao/evil-plugins"
               :depends evil)
        (:name evil-indent-textobject
               :type github
               :description "evil textobjects based on indentation"
               :pkgname "cofi/evil-indent-textobject"
               :depends evil)
        (:name evil-matchit
               :type github
               :description "Vim matchit ported to Evil"
               :pkgname "redguardtoo/evil-matchit"
               :depends evil)
        (:name evil-exchange
               :type github
               :description "Exchange text more easily within Evil"
               :pkgname "Dewdrops/evil-exchange"
               :features evil-exchange
               :depends evil)
        (:name evil-args
               :type github
               :description "Motions and text objects for delimited arguments in Evil."
               :pkgname "wcsmith/evil-args"
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
        (:name reveal-in-finder
               :website "https://github.com/kaz-yos/elisp"
               :description "Reveal file associated with buffer in OS X Finder"
               :type http
               :url "https://github.com/kaz-yos/elisp/raw/master/reveal-in-finder.el")
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
        (:name ace-window
               :type github
               :description "Quickly switch windows using `ace-jump-mode'"
               :pkgname "abo-abo/ace-window"
               :depends ace-jump-mode
               :features ace-window)
        (:name ace-jump-buffer
               :type github
               :description "fast buffer switching extension to `ace-jump-mode'"
               :pkgname "waymondo/ace-jump-buffer"
               :depends (ace-jump-mode dash))
        (:name guide-key
               :type github
               :description "Guide the following key bindings automatically and dynamically"
               :pkgname "kbkbkbkb1/guide-key"
               :depends popwin)
        (:name ido-ubiquitous
               :type github
               :description "Fancy completion all over Emacs, not just for buffers and files."
               :pkgname "DarwinAwardWinner/ido-ubiquitous")
        (:name minimap
               :type github
               :description "Sidebar showing a "mini-map" of a buffer"
               :pkgname "dengste/minimap")
        (:name haskell-mode
               :description "A Haskell editing mode"
               :type github
               :pkgname "deftsp/haskell-mode"
               :info "."
               :branch "develop"
               :build `(("make" ,(format "EMACS=%s" (shell-quote-argument el-get-emacs)) "all"))
               :post-init (progn
                            (require 'haskell-mode-autoloads)))
        (:name company-cabal
               :description "Company-mode completion back-end for haskell-cabal-mode "
               :type github
               :pkgname "iquiw/company-cabal"
               :depends (company-mode cl-lib)
               :post-init (eval-after-load 'company
                            '(add-to-list 'company-backends 'company-cabal)))
        (:name emacs-moz-controller
               :description "Control Firefox from Emacs"
               :type github
               :pkgname "RenWenshan/emacs-moz-controller"
               :depends moz-repl)
        (:name cal-china-x
               :description "Chinese calendar extras"
               :type github
               :pkgname "xwl/cal-china-x"
               :features cal-china-x)
        (:name tuareg-mode
               :type github
               :description "an Emacs OCaml mode"
               :load-path (".")
               :pkgname "ocaml/tuareg"
               :prepare
               (progn
                 (autoload 'tuareg-mode "tuareg" "Major mode for editing Caml code" t)
                 (autoload 'camldebug "camldebug" "Run the Caml debugger" t)
                 (dolist (ext '(".cmo" ".cmx" ".cma" ".cmxa" ".cmi"))
                   (add-to-list 'completion-ignored-extensions ext))
                 (add-to-list 'auto-mode-alist '("\\.ml[iylp]?" . tuareg-mode))))))

;; (el-get-save-package-status "package-name-here" "removed")

;; (el-get 'sync 'org-mode)                ; init org-mode first
(el-get 'sync 'exec-path-from-shell)
(require 'exec-path-from-shell nil t) ; https://github.com/purcell/exec-path-from-shell
;; (el-get 'sync 'auto-complete)           ; auto-complete should init before ac-nrepl
(el-get 'sync 'clojure-mode)
(el-get 'sync 'evil)
(el-get 'sync 'ace-jump-mode)
(el-get 'sync 'ace-window)
(el-get 'sync)                          ; 'sync 'wait


(provide '12el-get)
;;; 12el-get.el ends here
