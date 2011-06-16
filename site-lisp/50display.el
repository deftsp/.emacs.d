;;; 50display.el ---

;; Copyright (C) 2008  S.P.Tseng

;; Author: S.P.Tseng <deftsp@gmail.com>
;; Keywords:

;; ----------------------------------------------------------------------------------------------------
;;; StandardDisplay
;; ----------------------------------------------------------------------------------------------------

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



(when window-system
  ;; lambda
  (set-fontset-font "fontset-default"
                    (cons (decode-char 'ucs #x03bb)
                          (decode-char 'ucs #x03bb))
                    (font-spec :family "dejavu sans mono" :weight 'normal :size 12 :width 'normal))

  ;; (set-fontset-font "fontset-global"
  ;;                   (cons (decode-char 'ucs #x03bb)
  ;;                         (decode-char 'ucs #x03bb))
  ;;                   "-schumacher-clean-medium-r-*-*-12-*-*-*-*-*-iso8859-7")
  ;; logical not
  ;; (set-fontset-font "fontset-global"
  ;;                   (cons (decode-char 'ucs #xac)
  ;;                         (decode-char 'ucs #xac))
  ;;                   "-*-terminus-medium-r-*-*-12-*-*-*-*-*-iso8859-7")
  ;; (set-fontset-font "fontset-global"
  ;;                   (cons (decode-char 'ucs #xff00)
  ;;                         (decode-char 'ucs #xffef))
  ;;                   "-*-wenquanyi bitmap song-*-*-*-*-12-*-*-*-*-*-*-*")
  ;; (set-fontset-font "fontset-global"
  ;;                   (cons (decode-char 'ucs #x3000)
  ;;                         (decode-char 'ucs #x303f))
  ;;                   "-*-wenquanyi bitmap song-*-*-*-*-12-*-*-*-*-*-*-*")
  ;; ?Ψ
  (set-fontset-font "fontset-default"
                    (cons (decode-char 'ucs #x03a8)
                          (decode-char 'ucs #x03a8))
                    (font-spec :family "dejavu sans mono" :weight 'normal :size 12 :width 'normal))
  ;; ?ƒ
  (set-fontset-font "fontset-default"
                    (cons (decode-char 'ucs #x192)
                          (decode-char 'ucs #x192))
                    (font-spec :family "dejavu sans mono" :weight 'normal :size 12 :width 'normal))


  (when (eq window-system 'x)
    ;; IPA Extensions
    (set-fontset-font "fontset-default"
                      (cons (decode-char 'ucs #x250)
                            (decode-char 'ucs #x2af))
                      (font-spec :family "Gentium" :size 14))



    ;; Phonetic Extensions
    (set-fontset-font "fontset-default"
                      (cons (decode-char 'ucs #x1d00)
                            (decode-char 'ucs #x1d7f))
                      (font-spec :family "Gentium" :size 14)))


  ;; Mathematical Operator
  (set-fontset-font "fontset-default"
                    (cons (decode-char 'ucs #x2200)
                          (decode-char 'ucs #x22ff))
                    (font-spec :family "dejavu sans mono" :weight 'medium  :size 12))
  ;; ⋂ & ⋃
  (set-fontset-font "fontset-default"
                    (cons (decode-char 'ucs #x22c2)
                          (decode-char 'ucs #x22c3))
                    (font-spec :family "FreeMono" :size 12))

  ;; ☹ & ☺
  ;; (set-fontset-font "fontset-default"
  ;;                   (cons (decode-char 'ucs #x2639)
  ;;                         (decode-char 'ucs #x263a))
  ;;                   (font-spec :family "fixed" :size 12))


  (set-fontset-font "fontset-default"
                    (cons (decode-char 'ucs #x3000)
                          (decode-char 'ucs #x303f))
                    (font-spec :family (case window-system
                                         ('x "微软雅黑")
                                         ('ns "STHeiti")
                                         ('w32 "微软雅黑")
                                         (t (error "Can not set font in this window system.")))
                               :size 12))


  ;; Halfwidth and Fullwidth Form
  (set-fontset-font "fontset-default"
                    (cons (decode-char 'ucs #xff00)
                          (decode-char 'ucs #xffef))
                    (font-spec :family (case window-system
                                         ('x "微软雅黑")
                                         ('ns "STHeiti")
                                         ('w32 "微软雅黑")
                                         (t (error "Can not set font in this window system.")))
                               :size 12))

  (set-fontset-font (frame-parameter nil 'font)
                    'han (font-spec :family (case window-system
                                              ('x "微软雅黑")
                                              ('ns "STHeiti")
                                              ('w32 "微软雅黑")
                                              (t (error "Can not set font in this window system.")))
                                    :weight 'medium :size 12 :width 'normal))



  ;; ‼ & ‽
  (set-fontset-font "fontset-default"
                    (cons (decode-char 'ucs #x203c)
                          (decode-char 'ucs #x203d))
                    (font-spec :family "FreeMono" :size 12)))



;; (set-fontset-font (frame-parameter nil 'font)
;;        'han '("cwTeXHeiBold" . "unicode-bmp"))

;; set the font for new frames in Emacs 23
;; (if (>= emacs-major-version 23)
;;     (modify-all-frames-parameters
;;      '((font . "Monospace-11"))))


;; (setq xwl-default-font "Monaco-10"
;;       xwl-chinese-font "NSimSun"
;;       xwl-japanese-font xwl-chinese-font)

;; (let ((charset-font `((japanese-jisx0208   . ,xwl-japanese-font)
;;                       (chinese-gb2312      . ,xwl-chinese-font)
;;                       (chinese-gbk         . ,xwl-chinese-font)
;;                       (gb18030             . ,xwl-chinese-font)
;;                       ;; (big5             . "Hei")
;;                       (japanese-jisx0208   . ,xwl-japanese-font)
;;                       ;; (japanese-jisx0212 . ,xwl-japanese-font)
;;                       )))
;;   (set-default-font xwl-default-font) ; this will decide font size.
;;   (mapc (lambda (charset-font)
;;           (set-fontset-font (frame-parameter nil 'font)
;;                             (car charset-font)
;;                             (font-spec :family (cdr charset-font) :size 14)))
;;         charset-font))



;;; 50display.el ends here

