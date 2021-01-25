;;; 50display.el ---

;; Copyright (C) 2008  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>
;; Keywords:

;;; frame parameters
(setq frame-resize-pixelwise t
      ;; enable `window-resize-pixelwise' will cause the help window get the
      ;; wrong height, that is to say only show part of the last line
      window-resize-pixelwise nil)

(when (memq window-system '(x w32 mac ns))
  (setq initial-frame-alist `((tool-bar-lines . 0)
                              (ns-transparent-titlebar . t)
                              (ns-appearance . dark) ;; or light - depending on your theme
                              ;; https://github.com/railwaycat/homebrew-emacsmacport/issues/124
                              ;; https://github.com/railwaycat/homebrew-emacsmacport/issues/139
                              (menu-bar-lines . ,(if (member window-system '(mac ns)) 1 0))
                              (width . 201)
                              (height . 51)
                              (top . 22) ; (frame-parameter nil 'top)
                              (left . 0)
                              (alpha . (,dottl-active-transparency . ,dottl-inactive-transparency)) ; first number is for the active window and the second for the inactive
                              (mouse-color . "gray80")
                              ;; http://emacsredux.com/blog/2015/01/18/customizing-the-fringes/
                              (left-fringe . 8) ; default width (8 pixels).
                              (right-fringe . 8)
                              ;; natural-title-bar https://github.com/railwaycat/homebrew-emacsmacport/wiki/Natural-Title-Bar
                              ;; defaults write org.gnu.Emacs TransparentTitleBar DARK
                              ;; (scroll-bar-background . "#80abb6")
                              ;; (scroll-bar-width . 12)
                              ;; (vertical-scroll-bars . right)
                              (vertical-scroll-bars . nil)))

  (when (memq window-system '(mac ns))
    (add-to-list 'default-frame-alist '(ns-appearance . dark)) ; nil for dark text
    (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t)))

  ;; default-frame-alist is defined in terms of initial-frame-alist.  Don't
  ;; use copy-sequence here -- it doesn't copy the list elements, just the
  ;; list's cons cells.  Use copy-alist instead.

  (setq default-frame-alist (copy-alist initial-frame-alist))

  (setq tooltip-frame-parameters  '((name . "tooltip")
                                    (internal-border-width . 1)
                                    (border-width . 0))))

;; Test
;; (modify-frame-parameters
;;  (selected-frame)
;;  '((left-fringe . 8)
;;    (right-fringe . 16)))

;; (make-glyph-code ?┃)
(if window-system
    (progn
      (setq window-divider-default-places 'right-only
            window-divider-default-right-width 4)
      (window-divider-mode +1))
  ;; console window split line
  (set-display-table-slot standard-display-table 'vertical-border ?│))

;;; standard display
;; the 0x2500 page, which is where all the graphics characters live. Feel free to chose other characters if you want,
;; perhaps to support rounded corners, for example.

;; Note that these numbers are in octal. Use 'M-x calculator', switch to octal input using `i o', type the number, use
;; decimal output using `o d' and you'll find that 0222 is 146, the "right single quotation mark" mentioned on Jukka's
;; page.

(when (fboundp 'decode-coding-string)
  ;; (standard-display-ascii ?\t "^I")
  (standard-display-ascii ?\15 "")        ; I hate "^M"
  ;; (standard-display-ascii ?\74 "‹") ;; use unicode to display this symbol
  ;; (standard-display-ascii ?\76 "›")


  ;; (standard-display-ascii ?\200 (decode-coding-string "\244" 'latin-9))
  (standard-display-ascii ?λ (decode-coding-string "\xEB" 'greek-iso-8bit))

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

  (standard-display-ascii ?\240  [? ])    ; non-breaking space
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
  )





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

;;; frame transparency
;; https://www.reddit.com/r/emacs/comments/5rnpsm/nice_hydra_to_set_frame_transparency/
(defun tl/set-transparency (inc &optional inc-p)
  "Increase or decrease the selected frame transparency"
  (when (display-graphic-p)
    (let* ((alphas (frame-parameter (selected-frame) 'alpha))
           (alpha-active (car alphas))
           (alpha-inactive (cdr alphas))
           (alpha-next (if inc-p
                           (cond ((> (- alpha-active inc) 100) 100)
                                 ((< (- alpha-active inc) 0) 0)
                                 (t (- alpha-active inc)))
                         inc)))

      (set-frame-parameter (selected-frame) 'alpha
                           (cons alpha-next alpha-inactive)))))

(defhydra hydra-transparency (:columns 2)
  "
ALPHA : [ %(frame-parameter nil 'alpha) ]
"
  ("j" (lambda () (interactive) (tl/set-transparency +1 t)) "+ more")
  ("k" (lambda () (interactive) (tl/set-transparency -1 t)) "- less")
  ("J" (lambda () (interactive) (tl/set-transparency +10 t)) "++ more")
  ("K" (lambda () (interactive) (tl/set-transparency -10 t)) "-- less")
  ("=" (lambda (value) (interactive "nTransparency Value 0 - 100 opaque: " )
         (tl/set-transparency value nil)) "Set to ?" :color blue)
  ("r" (lambda () (interactive)
         (tl/set-transparency dottl-active-transparency nil)) "reset" :color blue)
  ("t" (lambda () (interactive)
         (let ((alpha (frame-parameter (selected-frame) 'alpha))
               (dotfile-setting (cons dottl-active-transparency
                                      dottl-inactive-transparency)))
           (if (equal alpha dotfile-setting)
               (set-frame-parameter (selected-frame) 'alpha '(100 . 100))
             (set-frame-parameter (selected-frame) 'alpha dotfile-setting)))) "toggle" :color blue)
  ("" nil "cancel")
  ("q" nil "cancel"))

(tl/set-leader-keys "TT" 'hydra-transparency/body)


(defun tl/remove-menu-item ()
  "Remove menu item."
  ;; (define-key global-map [menu-bar edit] nil)
  ;; (define-key global-map [menu-bar tools] nil)
  (define-key global-map [menu-bar options] nil))

(add-hook 'after-init-hook 'tl/remove-menu-item)

(provide '50display)

;; ─    2500    BOX DRAWINGS LIGHT HORIZONTAL
;; ━    2501    BOX DRAWINGS HEAVY HORIZONTAL
;; │    2502    BOX DRAWINGS LIGHT VERTICAL
;; ┃    2503    BOX DRAWINGS HEAVY VERTICAL
;; ┄    2504    BOX DRAWINGS LIGHT TRIPLE DASH HORIZONTAL
;; ┅    2505    BOX DRAWINGS HEAVY TRIPLE DASH HORIZONTAL
;; ┆    2506    BOX DRAWINGS LIGHT TRIPLE DASH VERTICAL
;; ┇    2507    BOX DRAWINGS HEAVY TRIPLE DASH VERTICAL
;; ┈    2508    BOX DRAWINGS LIGHT QUADRUPLE DASH HORIZONTAL
;; ┉    2509    BOX DRAWINGS HEAVY QUADRUPLE DASH HORIZONTAL
;; ┊    250A    BOX DRAWINGS LIGHT QUADRUPLE DASH VERTICAL
;; ┋    250B    BOX DRAWINGS HEAVY QUADRUPLE DASH VERTICAL
;; ┌    250C    BOX DRAWINGS LIGHT DOWN AND RIGHT
;; ┍    250D    BOX DRAWINGS DOWN LIGHT AND RIGHT HEAVY
;; ┎    250E    BOX DRAWINGS DOWN HEAVY AND RIGHT LIGHT
;; ┏    250F    BOX DRAWINGS HEAVY DOWN AND RIGHT
;; ┐    2510    BOX DRAWINGS LIGHT DOWN AND LEFT
;; ┑    2511    BOX DRAWINGS DOWN LIGHT AND LEFT HEAVY
;; ┒    2512    BOX DRAWINGS DOWN HEAVY AND LEFT LIGHT
;; ┓    2513    BOX DRAWINGS HEAVY DOWN AND LEFT
;; └    2514    BOX DRAWINGS LIGHT UP AND RIGHT
;; ┕    2515    BOX DRAWINGS UP LIGHT AND RIGHT HEAVY
;; ┖    2516    BOX DRAWINGS UP HEAVY AND RIGHT LIGHT
;; ┗    2517    BOX DRAWINGS HEAVY UP AND RIGHT
;; ┘    2518    BOX DRAWINGS LIGHT UP AND LEFT
;; ┙    2519    BOX DRAWINGS UP LIGHT AND LEFT HEAVY
;; ┚    251A    BOX DRAWINGS UP HEAVY AND LEFT LIGHT
;; ┛    251B    BOX DRAWINGS HEAVY UP AND LEFT
;; ├    251C    BOX DRAWINGS LIGHT VERTICAL AND RIGHT
;; ┝    251D    BOX DRAWINGS VERTICAL LIGHT AND RIGHT HEAVY
;; ┞    251E    BOX DRAWINGS UP HEAVY AND RIGHT DOWN LIGHT
;; ┟    251F    BOX DRAWINGS DOWN HEAVY AND RIGHT UP LIGHT
;; ┠    2520    BOX DRAWINGS VERTICAL HEAVY AND RIGHT LIGHT
;; ┡    2521    BOX DRAWINGS DOWN LIGHT AND RIGHT UP HEAVY
;; ┢    2522    BOX DRAWINGS UP LIGHT AND RIGHT DOWN HEAVY
;; ┣    2523    BOX DRAWINGS HEAVY VERTICAL AND RIGHT
;; ┤    2524    BOX DRAWINGS LIGHT VERTICAL AND LEFT
;; ┥    2525    BOX DRAWINGS VERTICAL LIGHT AND LEFT HEAVY
;; ┦    2526    BOX DRAWINGS UP HEAVY AND LEFT DOWN LIGHT
;; ┧    2527    BOX DRAWINGS DOWN HEAVY AND LEFT UP LIGHT
;; ┨    2528    BOX DRAWINGS VERTICAL HEAVY AND LEFT LIGHT
;; ┩    2529    BOX DRAWINGS DOWN LIGHT AND LEFT UP HEAVY
;; ┪    252A    BOX DRAWINGS UP LIGHT AND LEFT DOWN HEAVY
;; ┫    252B    BOX DRAWINGS HEAVY VERTICAL AND LEFT
;; ┬    252C    BOX DRAWINGS LIGHT DOWN AND HORIZONTAL
;; ┭    252D    BOX DRAWINGS LEFT HEAVY AND RIGHT DOWN LIGHT
;; ┮    252E    BOX DRAWINGS RIGHT HEAVY AND LEFT DOWN LIGHT
;; ┯    252F    BOX DRAWINGS DOWN LIGHT AND HORIZONTAL HEAVY
;; ┰    2530    BOX DRAWINGS DOWN HEAVY AND HORIZONTAL LIGHT
;; ┱    2531    BOX DRAWINGS RIGHT LIGHT AND LEFT DOWN HEAVY
;; ┲    2532    BOX DRAWINGS LEFT LIGHT AND RIGHT DOWN HEAVY
;; ┳    2533    BOX DRAWINGS HEAVY DOWN AND HORIZONTAL
;; ┴    2534    BOX DRAWINGS LIGHT UP AND HORIZONTAL
;; ┵    2535    BOX DRAWINGS LEFT HEAVY AND RIGHT UP LIGHT
;; ┶    2536    BOX DRAWINGS RIGHT HEAVY AND LEFT UP LIGHT
;; ┷    2537    BOX DRAWINGS UP LIGHT AND HORIZONTAL HEAVY
;; ┸    2538    BOX DRAWINGS UP HEAVY AND HORIZONTAL LIGHT
;; ┹    2539    BOX DRAWINGS RIGHT LIGHT AND LEFT UP HEAVY
;; ┺    253A    BOX DRAWINGS LEFT LIGHT AND RIGHT UP HEAVY
;; ┻    253B    BOX DRAWINGS HEAVY UP AND HORIZONTAL
;; ┼    253C    BOX DRAWINGS LIGHT VERTICAL AND HORIZONTAL
;; ┽    253D    BOX DRAWINGS LEFT HEAVY AND RIGHT VERTICAL LIGHT
;; ┾    253E    BOX DRAWINGS RIGHT HEAVY AND LEFT VERTICAL LIGHT
;; ┿    253F    BOX DRAWINGS VERTICAL LIGHT AND HORIZONTAL HEAVY
;; ╀    2540    BOX DRAWINGS UP HEAVY AND DOWN HORIZONTAL LIGHT
;; ╁    2541    BOX DRAWINGS DOWN HEAVY AND UP HORIZONTAL LIGHT
;; ╂    2542    BOX DRAWINGS VERTICAL HEAVY AND HORIZONTAL LIGHT
;; ╃    2543    BOX DRAWINGS LEFT UP HEAVY AND RIGHT DOWN LIGHT
;; ╄    2544    BOX DRAWINGS RIGHT UP HEAVY AND LEFT DOWN LIGHT
;; ╅    2545    BOX DRAWINGS LEFT DOWN HEAVY AND RIGHT UP LIGHT
;; ╆    2546    BOX DRAWINGS RIGHT DOWN HEAVY AND LEFT UP LIGHT
;; ╇    2547    BOX DRAWINGS DOWN LIGHT AND UP HORIZONTAL HEAVY
;; ╈    2548    BOX DRAWINGS UP LIGHT AND DOWN HORIZONTAL HEAVY
;; ╉    2549    BOX DRAWINGS RIGHT LIGHT AND LEFT VERTICAL HEAVY
;; ╊    254A    BOX DRAWINGS LEFT LIGHT AND RIGHT VERTICAL HEAVY
;; ╋    254B    BOX DRAWINGS HEAVY VERTICAL AND HORIZONTAL
;; ╌    254C    BOX DRAWINGS LIGHT DOUBLE DASH HORIZONTAL
;; ╍    254D    BOX DRAWINGS HEAVY DOUBLE DASH HORIZONTAL
;; ╎    254E    BOX DRAWINGS LIGHT DOUBLE DASH VERTICAL
;; ╏    254F    BOX DRAWINGS HEAVY DOUBLE DASH VERTICAL
;; ═    2550    BOX DRAWINGS DOUBLE HORIZONTAL
;; ║    2551    BOX DRAWINGS DOUBLE VERTICAL
;; ╒    2552    BOX DRAWINGS DOWN SINGLE AND RIGHT DOUBLE
;; ╓    2553    BOX DRAWINGS DOWN DOUBLE AND RIGHT SINGLE
;; ╔    2554    BOX DRAWINGS DOUBLE DOWN AND RIGHT
;; ╕    2555    BOX DRAWINGS DOWN SINGLE AND LEFT DOUBLE
;; ╖    2556    BOX DRAWINGS DOWN DOUBLE AND LEFT SINGLE
;; ╗    2557    BOX DRAWINGS DOUBLE DOWN AND LEFT
;; ╘    2558    BOX DRAWINGS UP SINGLE AND RIGHT DOUBLE
;; ╙    2559    BOX DRAWINGS UP DOUBLE AND RIGHT SINGLE
;; ╚    255A    BOX DRAWINGS DOUBLE UP AND RIGHT
;; ╛    255B    BOX DRAWINGS UP SINGLE AND LEFT DOUBLE
;; ╜    255C    BOX DRAWINGS UP DOUBLE AND LEFT SINGLE
;; ╝    255D    BOX DRAWINGS DOUBLE UP AND LEFT
;; ╞    255E    BOX DRAWINGS VERTICAL SINGLE AND RIGHT DOUBLE
;; ╟    255F    BOX DRAWINGS VERTICAL DOUBLE AND RIGHT SINGLE
;; ╠    2560    BOX DRAWINGS DOUBLE VERTICAL AND RIGHT
;; ╡    2561    BOX DRAWINGS VERTICAL SINGLE AND LEFT DOUBLE
;; ╢    2562    BOX DRAWINGS VERTICAL DOUBLE AND LEFT SINGLE
;; ╣    2563    BOX DRAWINGS DOUBLE VERTICAL AND LEFT
;; ╤    2564    BOX DRAWINGS DOWN SINGLE AND HORIZONTAL DOUBLE
;; ╥    2565    BOX DRAWINGS DOWN DOUBLE AND HORIZONTAL SINGLE
;; ╦    2566    BOX DRAWINGS DOUBLE DOWN AND HORIZONTAL
;; ╧    2567    BOX DRAWINGS UP SINGLE AND HORIZONTAL DOUBLE
;; ╨    2568    BOX DRAWINGS UP DOUBLE AND HORIZONTAL SINGLE
;; ╩    2569    BOX DRAWINGS DOUBLE UP AND HORIZONTAL
;; ╪    256A    BOX DRAWINGS VERTICAL SINGLE AND HORIZONTAL DOUBLE
;; ╫    256B    BOX DRAWINGS VERTICAL DOUBLE AND HORIZONTAL SINGLE
;; ╬    256C    BOX DRAWINGS DOUBLE VERTICAL AND HORIZONTAL
;; ╭    256D    BOX DRAWINGS LIGHT ARC DOWN AND RIGHT
;; ╮    256E    BOX DRAWINGS LIGHT ARC DOWN AND LEFT
;; ╯    256F    BOX DRAWINGS LIGHT ARC UP AND LEFT
;; ╰    2570    BOX DRAWINGS LIGHT ARC UP AND RIGHT
;; ╱    2571    BOX DRAWINGS LIGHT DIAGONAL UPPER RIGHT TO LOWER LEFT
;; ╲    2572    BOX DRAWINGS LIGHT DIAGONAL UPPER LEFT TO LOWER RIGHT
;; ╳    2573    BOX DRAWINGS LIGHT DIAGONAL CROSS
;; ╴    2574    BOX DRAWINGS LIGHT LEFT
;; ╵    2575    BOX DRAWINGS LIGHT UP
;; ╶    2576    BOX DRAWINGS LIGHT RIGHT
;; ╷    2577    BOX DRAWINGS LIGHT DOWN
;; ╸    2578    BOX DRAWINGS HEAVY LEFT
;; ╹    2579    BOX DRAWINGS HEAVY UP
;; ╺    257A    BOX DRAWINGS HEAVY RIGHT
;; ╻    257B    BOX DRAWINGS HEAVY DOWN
;; ╼    257C    BOX DRAWINGS LIGHT LEFT AND HEAVY RIGHT
;; ╽    257D    BOX DRAWINGS LIGHT UP AND HEAVY DOWN
;; ╾    257E    BOX DRAWINGS HEAVY LEFT AND LIGHT RIGHT
;; ╿    257F    BOX DRAWINGS HEAVY UP AND LIGHT DOWN
;; ▀    2580    UPPER HALF BLOCK
;; ▁    2581    LOWER ONE EIGHTH BLOCK
;; ▂    2582    LOWER ONE QUARTER BLOCK
;; ▃    2583    LOWER THREE EIGHTHS BLOCK
;; ▄    2584    LOWER HALF BLOCK
;; ▅    2585    LOWER FIVE EIGHTHS BLOCK
;; ▆    2586    LOWER THREE QUARTERS BLOCK
;; ▇    2587    LOWER SEVEN EIGHTHS BLOCK
;; █    2588    FULL BLOCK
;; ▉    2589    LEFT SEVEN EIGHTHS BLOCK
;; ▊    258A    LEFT THREE QUARTERS BLOCK
;; ▋    258B    LEFT FIVE EIGHTHS BLOCK
;; ▌    258C    LEFT HALF BLOCK
;; ▍    258D    LEFT THREE EIGHTHS BLOCK
;; ▎    258E    LEFT ONE QUARTER BLOCK
;; ▏    258F    LEFT ONE EIGHTH BLOCK
;; ▐    2590    RIGHT HALF BLOCK
;; ░    2591    LIGHT SHADE
;; ▒    2592    MEDIUM SHADE
;; ▓    2593    DARK SHADE
;; ▔    2594    UPPER ONE EIGHTH BLOCK
;; ▕    2595    RIGHT ONE EIGHTH BLOCK
;; ▖    2596    QUADRANT LOWER LEFT
;; ▗    2597    QUADRANT LOWER RIGHT
;; ▘    2598    QUADRANT UPPER LEFT
;; ▙    2599    QUADRANT UPPER LEFT AND LOWER LEFT AND LOWER RIGHT
;; ▚    259A    QUADRANT UPPER LEFT AND LOWER RIGHT
;; ▛    259B    QUADRANT UPPER LEFT AND UPPER RIGHT AND LOWER LEFT
;; ▜    259C    QUADRANT UPPER LEFT AND UPPER RIGHT AND LOWER RIGHT
;; ▝    259D    QUADRANT UPPER RIGHT
;; ▞    259E    QUADRANT UPPER RIGHT AND LOWER LEFT
;; ▟    259F    QUADRANT UPPER RIGHT AND LOWER LEFT AND LOWER RIGHT

;;; 50display.el ends here
