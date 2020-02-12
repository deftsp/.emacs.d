;;; 50font.el ---                                   -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>
;; Keywords: font

;;; Code:

;;; Programming fonts
;; https://github.com/ProgrammingFonts/ProgrammingFonts

;;; tumashu/cnfonts;; http://zhuoqiang.me/torture-emacs.html
;; http://baohaojun.github.io/perfect-emacs-chinese-font.html
;; https://github.com/tumashu/cnfonts
;; Avoiding use the font with its name contain '-'.
;; https://github.com/tumashu/cnfonts/issues/11

;;; line spacing
;; (setq line-spacing nil)
;; (defun tl/toggle-line-spacing ()
;;   "Toggle line spacing between 1 and 5 pixels."
;;   (interactive)
;;   (if (eq line-spacing 1)
;;       (setq-default line-spacing 5)
;;       (setq-default line-spacing 1)))


(defun tl/toggle-operator-composition-mode ()
  (interactive)
  (when (fboundp 'mac-auto-operator-composition-mode)
    (if mac-auto-operator-composition-mode
        (mac-auto-operator-composition-mode -1)
      (mac-auto-operator-composition-mode))))
(defalias 'tl/toggle-ligature 'tl/toggle-operator-composition-mode)

(use-package mac-win
  :init
  (setq mac-auto-operator-composition-characters
        "!\"#$%&'()*+,-./:;<=>?@[]^_`{|}~"))

;; (setq x-use-underline-position-properties nil)

;; https://github.com/tumashu/cnfonts/issues/64
;; By default, Emacs will try to use the default face’s font for displaying
;; symbol and punctuation characters, disregarding the fontsets, if the default
;; font can display the character. Set this to nil to make Emacs honor the
;; fontsets instead.
(setq use-default-font-for-symbols nil) ; default is nil

;; Note: when specify the font to "PragmataPro", the ?— will take column and
;; the char-width is 1. However if the font is "PragmataPro Mono", only takes
;; one column.
;; (char-width ?—)

(use-package cnfonts
  :init
  (progn
    (setq cnfonts-verbose nil
          cnfonts-default-step tl/default-cnfonts-fontsize-step
          cnfonts-use-face-font-rescale nil ; not work on macOS
          cnfonts-profiles '("program" "org-mode" "read-book")))
  :config
  (progn
    (when window-system
      ;; https://github.com/railwaycat/emacs-mac-port
      (when (fboundp 'mac-magnify-text-scale)
        ;; instead of emacs-mac-port's mac-magnify-text-scale
        (global-set-key [magnify-up] 'cnfonts-increase-fontsize)
        (global-set-key [magnify-down] 'cnfonts-decrease-fontsize))
      (global-set-key (kbd "<M-wheel-up>") 'cnfonts-decrease-fontsize)
      (global-set-key (kbd "<M-wheel-down>") 'cnfonts-increase-fontsize))

    (defun tl/cnfonts-reset-profile-and-fontsize ()
      "Reset fontsize with chinese-fonts-setup."
      (interactive)
      (when (display-graphic-p)
        (let* ((profile-name tl/default-cnfonts-profile-name)
               (profile-step (cnfonts--get-profile-step profile-name))
               (reset-step (- tl/default-cnfonts-fontsize-step profile-step)))
          (cnfonts--select-profile profile-name)
          (cnfonts--step-fontsize reset-step))))

    ;; https://nerdfonts.com/
    ;; https://github.com/ryanoasis/nerd-fonts
    ;; https://en.wikipedia.org/wiki/Unicode_font
    ;; #x03A8 ?Ψ
    ;; #x03BB Lambda
    ;; #x0192 ?ƒ
    ;; #x22c2 ⋂
    ;; #22c3  ⋃
    ;; #x2639 ☹
    ;; #x263A ☺
    ;; (#x0250 #x02AF)                    ; IPA Extensions
    ;; (#x1d00 #x1D7F)                    ; Phonetic Extensions
    ;; (#x0180 #x024F)                    ; Latin Extended-B
    ;; (#x0370 #x03FF)                    ; Greek
    ;; (#x2000 #x206f)                    ; General Punctuation
    ;; (#x2200 #x22FF)                    ; Mathematical Operator
    ;; (#x2300 #x23FF)                    ; Miscellaneous Technical

    ;; (#x2E80 #x2EFF)                    ; CJK Radicals Supplement
    ;; (#x3000 #x303F)                    ; CJK Symbols and Punctuation
    ;; (#x31C0 #x31EF)                    ; CJK Strokes
    ;; (#x3200 #x32FF)                    ; Enclosed CJK Letters and Months
    ;; (#x3300 #x33FF)                    ; CJK Compatibility
    ;; (#x3400 #x4DBF)                    ; CJK Unified Ideographs Extension A
    ;; (#x4E00 #x9FFF)                    ; CJK Unified Ideographs
    ;; (#xF900 #xFAFF)                    ; CJK Compatibility Ideographs
    ;; (#xFE30 #xFE4F)                    ; CJK Compatibility Forms

    ;; (#x2000 #x206F)                    ; General Punctuation
    ;; (#xFF00 #xFFEF)                    ; Halfwidth and Fullwidth Form

    ;; Note: the final choosed font to display a char, decided by face or fontset

    ;; https://github.com/rolandwalker/unicode-fonts
    ;; On the assumption that an extended Latin font such as Monaco,
    ;; Consolas, or DejaVu Sans Mono is already being used for the default
    ;; face, no separate mappings are provided for the following Unicode
    ;; blocks:
    ;;
    ;;     Basic Latin
    ;;     Latin Extended Additional
    ;;     Latin Extended-A
    ;;     Latin Extended-B
    ;;     Latin-1 Supplement
    ;;     Spacing Modifier Letters
    (defun tl/cnfonts-set-extra-fonts (fontsizes-list)
      (mapc
       (lambda (l)
         (let* ((default-font-name "MesloLGS Nerd Font")
                (target (cdr (assoc 'target l)))
                (fontname (or (cdr (assoc 'fontname l))
                              default-font-name))
                (fontsize-index (or (cdr (assoc 'fontsize-index l)) 0))
                (rescale (or (cdr (assoc 'rescale l)) 1))
                (fontsize (* rescale (nth fontsize-index fontsizes-list)))
                (fontspec (font-spec :name fontname
                                     :size fontsize
                                     :weight 'normal
                                     :slant 'normal)))
           (when (and fontname (not (cnfonts--fontspec-valid-p fontspec)))
             (error "字体 %S 不存在！" fontname))

           (set-fontset-font "fontset-default"
                             target
                             fontspec nil 'prepend)))
       '(((target . symbol)
          (rescale . 0.8)) ; not works for some font size
         ;; Note: "« »" not coverd by mplus Nerd Font
         ;; ((target . (#x0080 . #x00ff))) ; Latin-1 Supplement,   see above comment
         ((target . (#xe5fa . #xe62b))) ; Seti-UI + Custom
         ((target . (#xe700 . #xe7cf))) ; Devicons
         ((target . (#xeffe . #xf2e9))) ; Font Awesome
         ((target . (#xe200 . #xe2a9))) ; Font Awesome Extension
         ((target . (#xf400 . #xf67c))) ; Octicons
         ((target . (#xe0a0 . #xe0d7))) ; Powerline Extra Symbols
         ((target . (#x23ed . #x2b5c))) ; IEC Power Symbols
         ((target . (#xf300 . #xf317))) ; Font Linux
         ((target . (#x2500 . #x257f))) ; ┌ └
         ((target . (#x25A0 . #x25ff))  ; Geometric Shapes, ▢ ◦
          (rescale . 0.8)
          (fontname . "Hack"))
         ;; "☰" "☱" "☲" "☳" "☴" "☵" "☶" "☷"
         ((target . (#x2600 . #x26ff))
          (fontname . "FreeMono")
          (fontsize-index . 0))
         ((target . (#x2700 . #x27bf))) ; Dingbats
         ((target . (#x2622 . #x2622)) ; ☢
          (fontname . "DejaVu Sans Mono")
          (rescale . 0.98)))))

    (add-hook 'cnfonts-set-font-finish-hook
              'tl/cnfonts-set-extra-fonts)

    (cnfonts-enable)))


;; (defhydra hydra-zoom ()
;;   "zoom"
;;   ("+" text-scale-increase "in")
;;   ("-" text-scale-decrease "out")
;;   ("0" (text-scale-adjust 0) "reset")
;;   ("<escape>" nil "quit" :color blue)
;;   ("q"        nil "quit" :color blue))


;;;
(use-package all-the-icons
  :defer t
  :init
  (setq all-the-icons-scale-factor 1.0))

;; ;; conflict with dired-quick-sort
;; (use-package all-the-icons-dired
;;   :after (all-the-icons dired)
;;   :init
;;   (with-eval-after-load 'dired
;;     (add-hook 'dired-mode-hook 'all-the-icons-dired-mode)))


(provide '50font)

;; Local Variables:
;; eval: (auto-fill-mode -1)
;; End:
;;; 50font.el ends here
