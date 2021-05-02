;;; 50abbrev.el ---

;; Copyright (C) 2012  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>
;; Keywords: abbrev

;; http://ergoemacs.org/emacs/emacs_abbrev_mode.
;; I add “8” in front of all my abbrevs. For example, my abbrev for the word “ variable ” is “ 8var ”. This way, it is
;; easier for me to avoid expanding when i don't want to. I choose “8” because it's one of the easy reach keys for right
;; hand, and is a rare starting letter of a word. You could use another rare starting letter, such as “ z ”.
(define-abbrev-table 'global-abbrev-table
  '(("8alpha" "α")
    ("8beta" "β" )
    ("8gamma" "γ")
    ("8xi" "ξ")
    ("8theta" "θ")
    ;; math/unicode symbols
    ("8in" "∈") ;
    ("8nin" "∉")
    ("8inf" "∞")   ; infinity
    ("8luv" "♥")
    ("8smly" "☺")
    ("8ar1" "→" )
    ("8ar2" "⇒" )

    ;; computing tech
    ("8wp" "Wikipedia")
    ("8ms" "Microsoft")
    ("8g" "Google")
    ("8qt" "QuickTime")
    ("8it" "IntelliType")
    ("8msw" "Microsoft Windows")
    ("8win" "Windows")
    ("8ie" "Internet Explorer")
    ("8ahk" "AutoHotkey")
    ("8pr" "POV-Ray")
    ("8ps" "PowerShell")
    ("8mma" "Mathematica")
    ("8js" "javascript")
    ("8vb" "Visual Basic")
    ("8yt" "YouTube")
    ("8ge" "Google Earth")
    ("8ff" "Firefox")

    ;; normal english words
    ("8alt" "alternative")
    ("8char" "character")
    ("8def" "definition")
    ("8bg" "background")
    ("8kb" "keyboard")
    ("8ex" "example")
    ("8kbd" "keybinding")
    ("8env" "environment")
    ("8var" "variable")
    ("8ev" "environment variable")
    ("8cp" "computer")
    ("8btw" "by the way" )
    ("8afaict" "as far as I can tell" )
    ("8wether" "whether" )
    ("8pov" "point of view")

    ;; url
    ("8omuse" "http://www.emacswiki.org/cgi-bin/oddmuse.pl" )
    ("8ewiki" "http://www.emacswiki.org/cgi-bin/wiki.pl" )

    ;; emacs regex
    ("8num" "\\([0-9]+?\\)")
    ("8str" "\\([^\"]+?\\)\"")
    ("8curly" ""\\([^”]+?\\)"")

    ;; shell commands
    ("8im" "convert -quality 85% ")
    ("8ims" "convert -size  -quality 85% ")
    ("8im256" "convert +dither -colors 256 ")
    ("8imf" "find . -name \"*png\" | xargs -l -i basename \"{}\" \".png\" | xargs -l -i  convert -quality 85% \"{}.png\" \"{}.jpg\"")

    ("8f0" "find . -type f -empty")
    ("8f00" "find . -type f -size 0 -exec rm {} ';'")

    ("flaot" "float")))

(setq-default abbrev-mode t)

;; do not auto save abbrevs
(setq save-abbrevs nil)

(if (file-exists-p  abbrev-file-name)
    (quietly-read-abbrev-file abbrev-file-name))


;;; defun with abbrev
;; (defun define-abbrev-function (table abbrev func)
;;   (put func 'no-self-insert t)
;;   (define-abbrev table abbrev "" `(lambda () (call-interactively ',func))))

;; (defmacro defun-abbrev (funcname abbrev &rest body)
;;   "Defun a function and define an abbrev.
;; Note that `table' is abbrev table to use."
;;   `(progn
;;      (defun ,funcname ,@body)
;;      (define-abbrev-function table ,abbrev ',funcname)))


(provide '50abbrev)
;;; 50abbrev.el ends here
