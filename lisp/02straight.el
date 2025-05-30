;;; 02straight.el ---                                -*- lexical-binding: t; coding: utf-8-unix; -*-

;; Copyright (C) 2020  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>
;; Keywords:

;; M-x straight-pull-recipe-repositories

;; (require 'straight-x)

(setq straight-repository-branch "develop"
      straight-check-for-modifications '(check-on-save)
      straight-use-package-by-default nil)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))


(with-eval-after-load 'straight
  ;; `let-alist' is built into Emacs 26 and onwards
  (add-to-list 'straight-built-in-pseudo-packages 'let-alist))

(straight-use-package 'use-package)
(straight-use-package 'use-package-chords)

;; [[https://github.com/syl20bnr/spacemacs/issues/8315][Evil commands are very slow · Issue #8315 · syl20bnr/spacemacs]]
;; emacs-mac which have the window-system as `mac' port have the correctly path
;; emacs-plus which have the window-system as `ns' have the wrong PATH unless it start from the terminal
(use-package exec-path-from-shell
  :straight exec-path-from-shell
  :if (memq window-system '(ns))
  :config
  (add-to-list 'exec-path-from-shell-variables "GOROOT")
  (add-to-list 'exec-path-from-shell-variables "LIBRARY_PATH")
  (add-to-list 'exec-path-from-shell-variables "LD_LIBRARY_PATH")
  (setq exec-path-from-shell-check-startup-files nil)
  ;; List of environment variables which are copied from the shell.
  (push "HISTFILE" exec-path-from-shell-variables)
  (exec-path-from-shell-initialize))

(straight-use-package '(asdf-vm :type git :host github :repo "delonnewman/asdf-vm.el"))
;; it will set the path of user installed python package
(use-package asdf-vm
  :config
  (asdf-vm-init))

(straight-use-package 'async)
(straight-use-package 'a)
(straight-use-package 's)
(straight-use-package '(f
                        :type git
                        :host github
                        :repo "rejeep/f.el"
                        :branch "master"
                        :files (:defaults "f.el" "f-shortdoc.el")))
(straight-use-package 'ht)
(straight-use-package 'compat)

(straight-use-package '(emacsql :files (:defaults "*.el" "sqlite")))

;; (straight-use-package '(org :build (:not native-compile)))
;; (straight-use-package '(org-contrib :build (:not native-compile)))

;; or see the `straight-built-in-pseudo-packages' variable
;; straight-use-package '(org :type built-in))

;; Pin the org versin to ca873f7
;; https://github.com/org-roam/org-roam/issues/2361
;; https://old.reddit.com/r/emacs/comments/15jyzz7/strange_orgroam_bug_with_links_and_more/
(straight-use-package 'org)
(straight-use-package 'org-contrib)
(straight-use-package 'org-mru-clock)
(straight-use-package 'org-superstar)
(straight-use-package 'org-clock-convenience)
(straight-use-package 'org-download)
(straight-use-package 'org-fancy-priorities)
(straight-use-package 'org-journal)
(straight-use-package 'org-mac-link)
;; (straight-use-package '(org-link-minor-mode :type git :host github :repo "seanohalpin/org-link-minor-mode"))
(straight-use-package 'orglink)
(straight-use-package '(org-protocol-capture-html :type git :host github :repo "alphapapa/org-protocol-capture-html"))
(straight-use-package 'org-ql)
;; (straight-use-package '(org-roam :type git :host github :repo "org-roam/org-roam" :branch "master"))
(straight-use-package '(org-roam
                        :type git
                        :host github
                        :repo "org-roam/org-roam"
                        :files (:defaults "extensions/*")))
;; (straight-use-package 'org-roam-server)
(straight-use-package '(org-spacer :type git :host github :repo "dustinlacewell/org-spacer.el"))
(straight-use-package 'org-super-agenda)
(straight-use-package 'org-trello)
(straight-use-package 'ob-http)
(straight-use-package 'org-transclusion)
;; (straight-use-package '(org-link-beautify :host nil :type git :repo "https://repo.or.cz/org-link-beautify.git"))

(straight-use-package 'sql-indent)
(straight-use-package 'sqlformat)

;; register my fork of ace-window before it's registered by the origin recipe. Otherwise, there will a warning like "Two
;; different recipes given for ..."
(straight-use-package
 '(ace-window :type git :host github :repo "abo-abo/ace-window"
              :fork (:host github :repo "deftsp/ace-window" :branch "shihpin")))

(straight-use-package '(treemacs :files (:defaults "Changelog.org"
                                         "icons"
                                         "src/elisp/treemacs*.el"
                                         "src/scripts/treemacs*.py"
                                         "treemacs-pkg.el"
                                         "src/extra/*")
                                 :includes (treemacs-all-the-icons
                                            treemacs-evil
                                            treemacs-icons-dired
                                            treemacs-magit
                                            treemacs-persp
                                            treemacs-pesrpective
                                            treemacs-projectile)))
;; (straight-use-package 'treemacs)

(straight-use-package 'smartparens)
(straight-use-package 'flymake)
(straight-use-package '(flymake-posframe :type git :host github :repo "Ladicle/flymake-posframe"))

(straight-use-package '(vline :type git :host github :repo "buzztaiki/vline"))

(straight-use-package 'evil)
(straight-use-package 'evil-org)
(straight-use-package 'evil-anzu)
(straight-use-package 'evil-args)
(straight-use-package 'evil-collection)
(straight-use-package 'evil-easymotion)
(straight-use-package 'evil-ediff)
(straight-use-package 'evil-embrace)
(straight-use-package 'evil-escape)
(straight-use-package 'evil-exchange)
(straight-use-package 'evil-find-char-pinyin)
(straight-use-package 'evil-fringe-mark)
(straight-use-package 'evil-goggles)
(straight-use-package 'evil-indent-plus)
(straight-use-package 'evil-leader)
(straight-use-package 'evil-lion)
(straight-use-package 'evil-matchit)
(straight-use-package 'evil-mc)
(straight-use-package 'evil-nerd-commenter)
(straight-use-package 'evil-numbers)
(straight-use-package 'evil-owl)
(straight-use-package '(evil-plugins :type git :host github :repo "tarao/evil-plugins"))
(straight-use-package 'evil-replace-with-register)
(straight-use-package 'evil-snipe)
;; (straight-use-package 'evil-special-modes)
(straight-use-package 'evil-surround)
(straight-use-package 'evil-textobj-anyblock)
(straight-use-package 'evil-textobj-column)
(straight-use-package 'evil-textobj-syntax)
(straight-use-package 'evil-vimish-fold)
(straight-use-package 'evil-visual-mark-mode)
(straight-use-package 'evil-visualstar)
;; (straight-use-package 'evil-string-inflection)
(straight-use-package 'string-inflection)


;; (straight-use-package 'undo-tree)
(straight-use-package 'undo-fu)
(straight-use-package 'undo-fu-session)
(straight-use-package 'vundo)

(straight-use-package 'better-jumper)

;; (straight-use-package 'flx) ; insteaded by prescient.el
(straight-use-package 'prescient)
;; (straight-use-package 'ivy-fuz)

;; tree-sitter
(straight-use-package 'treesit-auto)
;; (straight-use-package 'tree-sitter)
;; (straight-use-package 'tree-sitter-langs)
;; (straight-use-package 'evil-textobj-tree-sitter)
(straight-use-package
 '(evil-textobj-tree-sitter
   :type git
   :host github
   :repo "meain/evil-textobj-tree-sitter"
   :files (:defaults "queries" "treesit-queries")))
(straight-use-package '(fingertip :type git :host github :repo "manateelazycat/fingertip"))

(straight-use-package 'swiper)
(straight-use-package 'ivy-hydra)
(straight-use-package 'ivy-purpose)
(straight-use-package 'ivy-xref)
(straight-use-package 'ivy-rich)
(straight-use-package 'counsel-tramp)
(straight-use-package 'ivy-prescient)

(straight-use-package 'company)
(straight-use-package 'company-posframe)
(straight-use-package 'company-quickhelp)
(straight-use-package 'company-tabnine)
(straight-use-package 'company-prescient)
(straight-use-package 'company-anaconda)
(straight-use-package 'company-cabal)
;; (straight-use-package 'company-flx) ; insteaded by prescient.el
;; (straight-use-package 'company-fuzzy)
(straight-use-package 'company-ghc)
(straight-use-package 'company-ledger)
(straight-use-package 'company-ghci)

(straight-use-package 'flycheck)
(straight-use-package 'flycheck-grammarly)
(straight-use-package 'flycheck-haskell)
(straight-use-package 'flycheck-inline)
(straight-use-package 'flycheck-posframe)
(straight-use-package 'flycheck-rust)
(straight-use-package 'flycheck-flow)

(straight-use-package
 '(lsp-bridge :type git
              :host github
              :repo "manateelazycat/lsp-bridge"
              ;; :files (:defaults "*")
              :files (:defaults "*.el" "*.py" "acm" "core" "langserver" "multiserver" "resources")
              :build (:not compile)))

(straight-use-package
 `(lspce :type git :host github :repo "zbelial/lspce"
         :files (:defaults ,(pcase system-type
                              ('gnu/linux "lspce-module.so")
                              ('darwin "lspce-module.dylib")))
         :pre-build ,(pcase system-type
                       ('gnu/linux '(("cargo" "build" "--release") ("cp" "./target/release/liblspce_module.so" "./lspce-module.so")))
                       ('darwin '(("cargo" "build" "--release") ("cp" "./target/release/liblspce_module.dylib" "./lspce-module.dylib"))))))

(straight-use-package 'lsp-ivy)
(straight-use-package 'lsp-mode)
(straight-use-package 'lsp-ui)
;; https://github.com/merrickluo/lsp-tailwindcss
(straight-use-package '(lsp-tailwindcss :type git :host github :repo "merrickluo/lsp-tailwindcss"))

(straight-use-package 'dap-mode)

;; Python
(straight-use-package 'pyenv-mode)
(straight-use-package 'pyvenv)
(straight-use-package 'lsp-pyright)
(straight-use-package 'pythonic)
;; (straight-use-package 'pymacs)
(straight-use-package 'pylookup)
(straight-use-package 'poetry)
(straight-use-package 'pipenv)

(straight-use-package
 '(rime :type git
        :host github
        :repo "DogLooksGood/emacs-rime"
        :files ("*.el" "Makefile" "lib.c")))

;; (straight-use-package '(powerline :build (:not native-compile)))
(straight-use-package 'powerline)
(straight-use-package '(unicad :type git :host github :repo "ukari/unicad"))

(straight-use-package 'rainbow-mode)
;; https://github.com/DevelopmentCool2449/colorful-mode
;; rainbow-mode can not work with hl-line, might use colorful-mode
;; (straight-use-package 'colorful-mode)
;; https://github.com/alphapapa/prism.el
(straight-use-package 'prism)

(straight-use-package 'magit)
(straight-use-package 'magit-delta)

(straight-use-package 'rust-mode)
;; (straight-use-package 'rustic)
;; (straight-use-package
;;  '(rustic :type git :host github :repo "brotzeit/rustic"
;;           :fork (:host github :repo "deftsp/rustic" :branch "shihpin")))

;; brotzeit/rustic is unmaintained, use this ford instead.
(straight-use-package
 '(rustic :type git :host github :repo "emacs-rustic/rustic"))

(straight-use-package 'pdf-tools)
(straight-use-package 'saveplace-pdf-view)

(straight-use-package 'docker)
(straight-use-package 'list-utils)
(straight-use-package 'ace-isearch)
(straight-use-package 'ace-link)
(straight-use-package 'dockerfile-mode)
;; https://raw.githubusercontent.com/llvm-mirror/llvm/master/utils/emacs/llvm-mode.el
;; (straight-use-package 'llvm-mode)
(straight-use-package 'queue)
(straight-use-package 'ace-pinyin)
(straight-use-package 'docsetutil)
(straight-use-package 'loop)
(straight-use-package 'quick-peek)
(straight-use-package 'doom-modeline)
(straight-use-package 'rainbow-delimiters)
(straight-use-package 'ack-menu)
(straight-use-package 'flymake-easy)
(straight-use-package 'reformatter)
(straight-use-package 'add-node-modules-path)
(straight-use-package '(dotenv :type git :host github :repo "pkulev/dotenv.el"))
(straight-use-package 'request)
(straight-use-package 'adoc-mode)
(straight-use-package 'dumb-jump)
(straight-use-package 'frame-purpose)
(straight-use-package 'lua-mode)
(straight-use-package 'restclient)
(straight-use-package 'aggressive-indent)
(straight-use-package 'easy-hugo)
(straight-use-package 'fringe-helper)
(straight-use-package 'macrostep)
(straight-use-package 'reveal-in-osx-finder)
(straight-use-package 'all-the-icons)
(straight-use-package 'eglot)
(straight-use-package 'fuzzy)
(straight-use-package 'mag-menu)
(straight-use-package 'rg)
(straight-use-package 'all-the-icons-dired)
(straight-use-package 'ein)
(straight-use-package 'fuzzy-match)
;; (straight-use-package 'rope)
;; (straight-use-package 'ropemacs)
;; (straight-use-package 'ropemode)
(straight-use-package 'ample-regexps)
(straight-use-package 'benchmark-init)
(straight-use-package 'fvwm-mode)
(straight-use-package 'magit-popup)
(straight-use-package 'amx)
(straight-use-package 'el-patch)
(straight-use-package 'gcmh)
(straight-use-package 'magit-todos)
(straight-use-package 'anaconda-mode)

(straight-use-package 'eldoc-eval)
(straight-use-package 'eldoc-box)

(straight-use-package 'sideline)
(straight-use-package '(sideline-eldoc :type git :host github :repo "ginqi7/sideline-eldoc"))
(straight-use-package '(sideline-flymake :type git :host github :repo "emacs-sideline/sideline-flymake"))

(straight-use-package 'geiser)
;; (straight-use-package 'major-mode-icons)
(straight-use-package 'rsense)
(straight-use-package 'anaphora)
(straight-use-package 'eldoc-extension)
(straight-use-package 'general)
(straight-use-package '(marionette :type git :host github :repo "xuchunyang/marionette.el"))
(straight-use-package 'rspec-mode)
(straight-use-package 'elfeed)
;; (straight-use-package 'ghc-mod)
(straight-use-package 'markdown-mode)
(straight-use-package 'ruby-electric)
(straight-use-package 'anki-editor)
(straight-use-package 'elfeed-org)
(straight-use-package 'ghub)
(straight-use-package 'markup-faces)
(straight-use-package 'annalist)
(straight-use-package 'elisp-refs)
(straight-use-package 'ghub+)
(straight-use-package 'memoize)
(straight-use-package 'anzu)
(straight-use-package 'elisp-slime-nav)
(straight-use-package 'minimap)
(straight-use-package '(semantic-stickyfunc-enhance :type git :host github :repo "tuhdo/semantic-stickyfunc-enhance"))
(straight-use-package 'apiwrap)
(straight-use-package 'git-messenger)
(straight-use-package 'mmm-mode)
(straight-use-package 'seq)
;; (straight-use-package 'arduino-mode)
(straight-use-package 'git-modes)
(straight-use-package 'mmt)
(straight-use-package 'sesman)
(straight-use-package 'ascii-art-to-unicode)
(straight-use-package '(git-timemachine :host nil :type git :repo "https://codeberg.org/pidu/git-timemachine.git"))
;; (straight-use-package 'moz-repl)
;; (straight-use-package 'session)
(straight-use-package 'asciidoc)
(straight-use-package 'embrace)
(straight-use-package '(git-undo-el :type git :host github :repo "jwiegley/git-undo-el"))
;; There's some problem, when building emacs-libvterm in NixOS. https://github.com/akermu/emacs-libvterm/issues/115.
;; Using the vterm package from nix
(when (eq system-type 'darwin)
  (straight-use-package '(vterm :post-build ((let ((vterm-always-compile-module t)) (require 'vterm))))))
(straight-use-package 'multi-vterm)
(straight-use-package 'vterm-toggle)

(straight-use-package '(emacs-devdocs-browser :type git :host github :repo "blahgeek/emacs-devdocs-browser"))

(straight-use-package 'shell-pop)
(straight-use-package 'auto-complete)
(straight-use-package 'epc)
(straight-use-package 'glsl-mode)
(straight-use-package 'multiple-cursors)
(straight-use-package 'shrink-path)
(straight-use-package 'auto-dictionary)
(straight-use-package 'epl)
(straight-use-package 'go-mode)
(straight-use-package 'names)
(straight-use-package 'shut-up)
(straight-use-package 'auto-highlight-symbol)
(straight-use-package 'eshell-autojump)
(straight-use-package 'god-mode)
(straight-use-package 'narrow-indirect)
(straight-use-package 'simple-httpd)
(straight-use-package 'avy)
(straight-use-package 'eshell-did-you-mean)
(straight-use-package 'golden-ratio)
(straight-use-package 'nix-mode)
(straight-use-package 'skewer-mode)
(straight-use-package 'avy-menu)
(straight-use-package 'eshell-fringe-status)
(straight-use-package 'goto-chg)
(straight-use-package 'noflet)
(straight-use-package 'smart-jump)
(straight-use-package 'avy-zap)
(straight-use-package 'eshell-prompt-extras)
(straight-use-package 'grammarly)
(straight-use-package 'notify)
(straight-use-package 'babel)
(straight-use-package 'eslintd-fix)
(straight-use-package 'graphql)
(straight-use-package 'nyan-mode)
(straight-use-package 'spinner)
(straight-use-package 'esup)
(straight-use-package 'graphviz-dot-mode)
(straight-use-package 'splitter)
(straight-use-package 'etags-table)
(straight-use-package 'growl)
;; (straight-use-package 'om)
(straight-use-package '(boxquote :type git :host github :repo "davep/boxquote.el" :branch "main"))
(straight-use-package 'hamlet-mode)
(straight-use-package 'browse-kill-ring)
(straight-use-package 'haskell-emacs)
;; (straight-use-package 'swank-js)
(straight-use-package 'buffer-move)
(straight-use-package 'haskell-mode)
(straight-use-package 'swift-mode)
(straight-use-package 'bug-hunter)
(straight-use-package 'hasky-extensions)
(straight-use-package 'cal-china-x)
(straight-use-package 'hasky-stack)
(straight-use-package 'symbol-overlay)
(straight-use-package 'calfw)
(straight-use-package 'helpful)
(straight-use-package 'tablist)
(straight-use-package 'ccls)
(straight-use-package 'hide-comnt)
(straight-use-package 'thing-cmds)
(straight-use-package 'chumpy-windows)
(straight-use-package 'tidy)
(straight-use-package 'cl-lib)
(straight-use-package 'highlight-indent-guides)
(straight-use-package 'transient)
(straight-use-package 'clojure-mode)
(straight-use-package 'highlight-indentation)
(straight-use-package 'transpose-frame)
(straight-use-package 'cmake-mode)
;; (straight-use-package '(hindent :no-native-compile t))
(straight-use-package 'cnfonts)
(straight-use-package 'hl-line+)
(straight-use-package 'treepy)
(straight-use-package 'coffee-mode)
(straight-use-package 'hl-todo)
(straight-use-package 'ts)
(straight-use-package 'col-highlight)
(straight-use-package 'hlint-refactor)
(straight-use-package 'tuareg) ; OCaml mode
(straight-use-package 'typescript-mode)
(straight-use-package 'hydra)
(straight-use-package 'typit)
(straight-use-package 'ibuffer-vc)
(straight-use-package 'icomplete+)
(straight-use-package 'outline-magic)
(straight-use-package 'unicode-escape)
(straight-use-package 'ido-occasional)
(straight-use-package 'ov)
(straight-use-package 'use-package)
;; (straight-use-package 'ido-vertical-mode)
(straight-use-package 'ox-hugo)
(straight-use-package 'vc-msg)
(straight-use-package 'idomenu)
(straight-use-package '(ox-texinfo-plus :type git :host github :repo "tarsius/ox-texinfo-plus"))
(straight-use-package 'vimish-fold)
(straight-use-package 'package)
(straight-use-package 'vimrc-mode)
(straight-use-package 'imenu-list)
(straight-use-package 'pamparam)
(straight-use-package 'visual-regexp)
(straight-use-package 'pandoc-mode)
(straight-use-package 'vkill)
;; (straight-use-package 'intero)
(straight-use-package 'pangu-spacing)
;; (straight-use-package 'vline)
(straight-use-package 'counsel-etags)
(straight-use-package 'io-mode)
(straight-use-package 'parsec)
(straight-use-package 'counsel-gtags)
(straight-use-package '(irfc :type git :host github :repo "yskkin/irfc"))
(straight-use-package '(irfc-x :type git :host github :repo "kai2nenobu/irfc-x"))
(straight-use-package 'parseclj)
(straight-use-package 'wcheck-mode)
(straight-use-package 'parseedn)
(straight-use-package 'pcre2el)
(straight-use-package 'web-mode)
(straight-use-package 'crosshairs)
(straight-use-package 'websocket)
(straight-use-package 'cssh)
(straight-use-package 'peg)
(straight-use-package 'wgrep)
(straight-use-package 'ctable)
(straight-use-package 'js2-mode)
(straight-use-package 'pfuture)
;; (straight-use-package 'dart-mode)
(straight-use-package 'js2-refactor)
(straight-use-package 'pinyin-search)
;; (straight-use-package 'window-extension)
;; (straight-use-package 'dart-server)
(straight-use-package 'json-mode)
(straight-use-package 'pinyinlib)
(straight-use-package 'window-purpose)
(straight-use-package 'dash)
(straight-use-package 'json-reformat)
(straight-use-package 'pkg-info)
(straight-use-package 'winum)
(straight-use-package 'dash-at-point)
(straight-use-package 'json-rpc)
(straight-use-package 'platformio-mode)
(straight-use-package 'with-editor)
(straight-use-package 'deadgrep)
(straight-use-package 'expand-region)
(straight-use-package 'json-snatcher)
(straight-use-package 'popup)
(straight-use-package 'worf)
(straight-use-package 'deferred)
(straight-use-package 'jump-char)
(straight-use-package 'pos-tip)
;; (straight-use-package '(workgroups2 :build (:not native-compile)))
(straight-use-package 'workgroups2)
(straight-use-package 'deft)
(straight-use-package 'eyebrowse)
;; (straight-use-package 'kaleidoscope)
(straight-use-package 'posframe)
(straight-use-package 'ws-butler)
(straight-use-package 'diff-hl)
(straight-use-package 'key-chord)
(straight-use-package 'xterm-color)
(straight-use-package 'diminish)
(straight-use-package 'fill-column-indicator)
(straight-use-package 'keyfreq)
(straight-use-package 'prodigy)
(straight-use-package 'yaml-mode)
(straight-use-package 'dimmer)
(straight-use-package 'flow-minor-mode)
(straight-use-package 'prog-fill)
(straight-use-package 'yasnippet)
(straight-use-package 'dired-hacks)
(straight-use-package 'flutter)
(straight-use-package 'link-hint)
(straight-use-package 'yasnippet-snippets)
(straight-use-package 'dired-quick-sort)
(straight-use-package 'linkd)
(straight-use-package 'zoutline)
(straight-use-package '(dired-sidebar :type git :host github :repo "deftsp/dired-sidebar" :branch "shihpin"))
(straight-use-package 'diredfl)
(straight-use-package 'lispy)
(straight-use-package '(solidity-mode :files (:defaults "*.el")))



(straight-use-package 'scratch)
(straight-use-package 'persistent-scratch)


;; (straight-use-package 'projectile)
;; (straight-use-package 'counsel-projectile)
(straight-use-package 'find-file-in-project)

(straight-use-package 'flyspell-correct)
(straight-use-package 'wucuo)


(straight-use-package 'mise)

;; https://github.com/m00natic/vlfi/blob/master/vlf.el
(straight-use-package 'vlf)

(straight-use-package 'logview)

(straight-use-package '(exwm :type git :host github :repo "deftsp/exwm" :branch "shihpin"))

(straight-use-package '(aidermacs :type git :host github :repo "MatthewZMD/aidermacs"))

(provide '02straight)
