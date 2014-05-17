;;; 00util.el ---

;; Copyright (C) 2010  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>
;; Keywords:

(require '01env)
(require '02utils)
(require '05cc-mode)
;; (require '09cedet)
(require '37org-mode)
(require '12el-get)

(require '39util)
(when (eq system-type 'gnu/linux)
  (require '42ecb.el))

(require '50abbrev)
(require '50alias)
(require '50android)
(require '50asm-mode)
(require '50auto-complete)
(require '50auto-fill)
(require '50auto-insert)
;; (require '50bbdb)
(require '50calendar)


(require '50clojure)
(require '50common-lisp)
(require '50completion)
(require '50css)
(require '50dictionary)
(require '50dired)
(require '50display)
(require '50door-gnus)
(require '50ediff)
;; (require '50eim)
(require '50emacsclient)
(require '50epg)
(require '50erc)
(require '50eshell)
(require '50etags)
(require '50ffap)
(require '50filecache)
(require '50flashcard)
(require '50flymake)
(require '50ftp)
(require '50gdb)
(require '50go)
(require '50haskell)
(require '50hideshow)
(require '50javascript)
(require '50keys)
(require '50major-modes)
(require '50lua)
(require '50maxima)
(require '50misc)
(require '50mode-line)
(require '50net-utils)
;; (require '50nethack)
(require '50ocaml)
(require '50outline-mode)
(require '50paredit)
(require '50patch)
(require '50perl)
(require '50printing)
(require '50python-mode)
(require '50ruby)
(require '50sawfish)
(require '50scheme)
(require '50search)
(require '50shell)
(require '50slime-js)
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
(require '50w3m)  ; anything-config will defvar w3m-command, require it before that
(require '50wcheck-mode)
(require '50web-mode)
(require '50window)
(require '50xcode)
(require '50yasnippet)
(require '50workgroups)
(require '51CommonLispTemplates)
;; (require '51anything)
(require '51helm)
;; (require '52emms) ; do not use it any more
;; (require '52icicles)
(require '52evil-mode)
(require '52ido)
(require '60session)
;; (require '62winring)
(require '99face)

(load-file "~/.emacs.d/private-ede-projects.el")

(provide '00site-start)
