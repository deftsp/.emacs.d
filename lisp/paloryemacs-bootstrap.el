;;; paloryemacs-bootstrap.el ---

;; Copyright (C) 2010  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>
;; Keywords:

;;; Code:

(require 'prelude)
(require '01env)
(require '02base)
(require '03frame)
(require '12el-get)

(require '02utils)
(require '05cc-mode)
(require 'core-keybindings)
(require 'core-jump)
(require 'core-funcs)
(require '11cedet)
(require '13org-mode)

(require '23powerline)
(require '40company-mode)
(when (eq system-type 'gnu/linux)
  (require '42ecb.el))

(require '50abbrev)
(require '50alias)
(require '50android)
(require '50asm-mode)
;; (require '50auto-complete)
(require '50auto-fill)
(require '50auto-insert)
;; (require '50bbdb)
(require '50calendar)

(require '50clojure)
(require '50common-lisp)
(require '50completion)
(require '50dictionary)
(require '50dired)
(require '50display)
(require '50docker)
(require '50elfeed)
(require '50gnus)
(require '50ediff)
(require '50emacs-lisp-mode)
(require '50emacsclient)
(require '50epg)
(require '50erc)
(require '50eshell)
(require '50tags)
(require '50ffap)
(require '50filecache)
(require '50ftp)
(require '50font)
(require '50gdb)
(require '50go)
(require '50hammerspoon)
(require '50haskell)
(require '50haskell-emacs)
(require '50hideshow)
(require '50input-method)
(require '50ivy)
(require '50javascript)
(require '50key-chord)
(require '50key-bindings)
(require '50major-modes)
(require '50lua)
(require '50maxima)
;; (require '50minibuffer-line)
(require '50misc)
(require '50mmm-mode)
;; (require '50mode-line)
(require '50moz-controller)
(require '50narrowing)
(require '50net-utils)
;; (require '50nethack)
(require '50nxml)
(require '50ocaml)
(require '50outline-mode)
(require '50paredit)
(require '50patch)
(require '50perl)
(require '50printing)
(require '50python-mode)
(require '50restclient)
(require '50ruby)
(require '50sawfish)
(require '50scheme)
(require '50search)
(require '50shell)
(require '50slime-js)
(require '50swift-mode)
(require '50syntax-checker)
(require '50sql)
(require '50buffer)
(require '50smartparens)
(require '50tab-completion)
(require '50tempo)
(require '50tex)
(require '50tools)
(require '50tramp)
(require '50traverselisp)
(require '50unicode-input)
(require '50vc)
;; (require '50w3m)  ; helm-config will defvar w3m-command, require it before that
(require '50wcheck-mode)
(require '50web)
(require '50window)
(require '50xcode)
(require '51CommonLispTemplates)
;; (require '51helm)

;; (require '52emms) ; do not use it any more
;; (require '52icicles)
(require '52evil-mode)
;; (require '52ido)

(require '60session)
;; (require '62winring)
(require '99face)
(require '100align)

(load-file "~/.emacs.d/private-ede-projects.el")

(provide 'paloryemacs-bootstrap)
