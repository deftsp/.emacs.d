;;; 50display.el ---

;; Copyright (C) 2008  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>
;; Keywords:

;;; frame parameters
(setq frame-resize-pixelwise t
      window-resize-pixelwise t)

(when (eq window-system 'ns)
  (setq initial-frame-alist `((tool-bar-lines . 0)
                              (menu-bar-lines . 0)
                              (width . 202)
                              (height . 51)
                              (top . 22) ; (frame-parameter nil 'top)
                              (left . 0)
                              (alpha . (96 96)) ; first number is for the active window and the second for the inactive
                              (mouse-color . "gray80")
                              (right-fringe . 0) ; do not show right fringe
                              ;; (scroll-bar-width . 12)
                              (vertical-scroll-bars . right))

        ;; default-frame-alist is defined in terms of initial-frame-alist.  Don't
        ;; use copy-sequence here -- it doesn't copy the list elements, just the
        ;; list's cons cells.  Use copy-alist instead.
        default-frame-alist (copy-alist initial-frame-alist))
  (setq tooltip-frame-parameters  '((name . "tooltip")
                                    (internal-border-width . 1)
                                    (border-width . 0))))


;;; standard display
;; the 0x2500 page, which is where all the graphics characters live. Feel free to chose other characters if you want,
;; perhaps to support rounded corners, for example.

;; Note that these numbers are in octal. Use 'M-x calculator', switch to octal input using `i o', type the number, use
;; decimal output using `o d' and you'll find that 0222 is 146, the "right single quotation mark" mentioned on Jukka's
;; page.

;; (standard-display-ascii ?\t "^I")
(standard-display-ascii ?\15 "")        ; I hate "^M"
;; (standard-display-ascii ?\74 "‹") ;; use unicode to display this symbol
;; (standard-display-ascii ?\76 "›")

(when (fboundp 'decode-coding-string)
  ;; (standard-display-ascii ?\200 (decode-coding-string "\244" 'latin-9))
  (standard-display-ascii ?λ (decode-coding-string "\xEB" 'greek-iso-8bit)))


(standard-display-ascii ?\200 (vector (decode-char 'ucs #x253c)))
(standard-display-ascii ?\201 (vector (decode-char 'ucs #x251c)))
(standard-display-ascii ?\202 (vector (decode-char 'ucs #x252c)))
(standard-display-ascii ?\203 (vector (decode-char 'ucs #x250c)))
(standard-display-ascii ?\204 (vector (decode-char 'ucs #x2524)))
(standard-display-ascii ?\205 (vector (decode-char 'ucs #x2502)))
(standard-display-ascii ?\206 (vector (decode-char 'ucs #x2510)))
(standard-display-ascii ?\210 (vector (decode-char 'ucs #x2534)))
(standard-display-ascii ?\211 (vector (decode-char 'ucs #x2514)))
(standard-display-ascii ?\212 (vector (decode-char 'ucs #x2500)))
(standard-display-ascii ?\214 (vector (decode-char 'ucs #x2518)))

(standard-display-ascii ?\221 "\`")
(standard-display-ascii ?\222 "\'")
(standard-display-ascii ?\223 "\"")
(standard-display-ascii ?\224 "\"")
(standard-display-ascii ?\225 "\*")
(standard-display-ascii ?\226 "-")
(standard-display-ascii ?\227 "--")     ; long dash

;; (standard-display-ascii ?\240  [? ])    ;  non-breaking space
(standard-display-ascii ?\251 "©")      ; \251 copyright,  [?(?C?)]
;; (standard-display-ascii ?\256  [?(?R?)])  ; registered [?(?R?)]
;; (standard-display-ascii ?\267  [?*])    ; center dot
;; (standard-display-ascii ?\351  [?e?\']) ; \351 e'
;; (standard-display-ascii ?\200 [15])
;; (standard-display-ascii ?\201 [21])
;; (standard-display-ascii ?\202 [24])
;; (standard-display-ascii ?\203 [13])
;; (standard-display-ascii ?\204 [22])
;; (standard-display-ascii ?\205 [25])
;; (standard-display-ascii ?\206 [12])
;; (standard-display-ascii ?\210 [23])
;; (standard-display-ascii ?\211 [14])
;; (standard-display-ascii ?\212 [18])
;; (standard-display-ascii ?\214 [11])
;; (standard-display-ascii ?\222 [?\'])
;; (standard-display-ascii ?\223 [?\"])
;; (standard-display-ascii ?\224 [?\"])
;; (standard-display-ascii ?\227 " -- ")



;; A "script name symbol" is a means to associate a lisp symbol with a particular charset, a set of
;; charsets, or a range of characters. So, for example, if you look at the value of the variable
;; "charset-script-alist", you will see an Alist of charsets vs the corresponding most appropriate
;; script name symbols. If you look at the value of the variable "script-representative-chars", you
;; will see an Alist of script name symbols vs the representative characters. The variable
;; "char-script-table" shows the full mapping of characters to script name symbols (as specified by
;; "script-representative-chars"). I don't know of any "definitive" list of script names (for
;; example, "ascii" is a script name as well but isn't contained in these 2 variables); however, the
;; values that are contained in the variables "charset-script-alist" and "char-script-table" are
;; probably most of them.


;; To get a list of all the possible values of "charset" that is available to set-fontset-font, use
;; the "list-character-sets" function. You can also use the "list-charset-chars" function to see the
;; list of characters contained in a specific charset.

(defun pl/set-font ()
  (let* ((default-font-size
          (case window-system
            ('x   12)
            ('ns  12)
            ('w32 13)
            (t (error "Can not set font in this window system."))))

         (cjk-font-size 14)
         (font-pool
          '((x   . ("DejaVu Sans Mono" "Microsoft YaHei"))
            (ns  . ("Monaco" "STHeiti"))
            (w32 . ("Monaco" "Microsoft YaHei"))))
         (font-list (cdr (assoc window-system font-pool)))
         (default-font-name (format "%s:pixelsize=%d" (car font-list) default-font-size))
         (cjk-font-family (cadr font-list)))

    ;; This will decide default font size.
    (set-frame-font default-font-name)
    ;; Fallback font
    (set-fontset-font t 'unicode "Arial Unicode MS")
    ;; Font for CJK characters
    (mapc (lambda (range)
            (set-fontset-font
             "fontset-default" `(,(car range) . ,(cadr range)) (font-spec :family cjk-font-family :size cjk-font-size)))
          '((#x2E80 #x2EFF)                    ; CJK Radicals Supplement
            (#x3000 #x303F)                    ; CJK Symbols and Punctuation
            (#x31C0 #x31EF)                    ; CJK Strokes
            (#x3200 #x32FF)                    ; Enclosed CJK Letters and Months
            (#x3300 #x33FF)                    ; CJK Compatibility
            (#x3400 #x4DBF)                    ; CJK Unified Ideographs Extension A
            (#x4E00 #x9FFF)                    ; CJK Unified Ideographs
            (#xF900 #xFAFF)                    ; CJK Compatibility Ideographs
            (#xFE30 #xFE4F)                    ; CJK Compatibility Forms

            (#x2000 #x206F)                    ; General Punctuation
            (#xFF00 #xFFEF)                    ; Halfwidth and Fullwidth Form
            ))

    ;; #x03A8 ?Ψ
    ;; #x03BB Lambda
    ;; #x0192 ?ƒ
    (mapc
     (lambda (range)
       (set-fontset-font
        "fontset-default" `(,(car range) . ,(cadr range)) (font-spec :family "dejavu sans mono" :size default-font-size)))
     '((#x0180 #x024F)                    ; Latin Extended-B
       (#x0370 #x03FF)                    ; Greek
       (#x2000 #x206f)                    ; General Punctuation
       ;; (#x2200 #x22FF)                    ; Mathematical Operator
       (#x2300 #x23FF)                    ; Miscellaneous Technical
       ))

    ;; ⋂ & ⋃
    (set-fontset-font "fontset-default"
                      (cons #x22c2
                            #x22c3)
                      (font-spec :family "FreeMono" :size default-font-size))

    (when (eq window-system 'x)
      ;; IPA Extensions
      (set-fontset-font "fontset-default"
                        (cons #x0250
                              #x02AF)
                        (font-spec :family "Gentium" :size default-font-size))

      ;; Phonetic Extensions
      (set-fontset-font "fontset-default"
                        (cons #x1D00
                              #x1D7F)
                        (font-spec :family "Gentium" :size default-font-size))
      ;; ☹ & ☺
      (set-fontset-font "fontset-default"
                        (cons #x2639
                              #x263a)
                        (font-spec :family "fixed" :size default-font-size)))))

(when window-system
  (pl/set-font))

;;; visually indicate empty line
;; https://www.reddit.com/r/emacs/comments/2kdztw/emacs_in_evil_mode_show_tildes_for_blank_lines/
;; https://github.com/syl20bnr/vi-tilde-fringe
(when (fboundp 'define-fringe-bitmap)
  (define-fringe-bitmap 'tilde
    [#b00000000
     #b00000000
     #b00000000
     #b01110001
     #b11011011
     #b10001110
     #b00000000
     #b00000000]
    nil nil 'center)

  ;; (define-fringe-bitmap 'solid-block [0 0 #x3c #x3c #x3c #x3c 0 0])
  (define-fringe-bitmap 'solid-block
    [#b00000000
     #b00000000
     #b00111100
     #b00111100
     #b00111100
     #b00111100
     #b00000000
     #b00000000]
    nil nil 'center)
  (set-fringe-bitmap-face 'tilde font-lock-doc-face)
  (set-fringe-bitmap-face 'solid-block font-lock-doc-face)
  (setcdr (assq 'empty-line fringe-indicator-alist) 'solid-block)
  (setq-default indicate-empty-lines t))


(provide '50display)

;;; 50display.el ends here
