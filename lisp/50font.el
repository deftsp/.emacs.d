;;; 50font.el ---                                   -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>
;; Keywords: font


;;; Commentary:
;;

;;; Code:

;;; tumashu/chinese-fonts-setup
;; http://zhuoqiang.me/torture-emacs.html
;; http://baohaojun.github.io/perfect-emacs-chinese-font.html
;; https://github.com/tumashu/chinese-fonts-setup
;; Avoiding use the font with its name contain '-'.
;; https://github.com/tumashu/chinese-fonts-setup/issues/11
(setq cfs-profiles '("program" "org-mode" "read-book"))

(with-eval-after-load "chinese-fonts-setup"
  (when window-system
    ;; https://github.com/railwaycat/emacs-mac-port
    (when (fboundp 'mac-magnify-text-scale)
      ;; instead of emacs-mac-port's mac-magnify-text-scale
      (global-set-key [magnify-up] 'cfs-increase-fontsize)
      (global-set-key [magnify-down] 'cfs-decrease-fontsize))
    (global-set-key (kbd "<M-wheel-up>") 'cfs-decrease-fontsize)
    (global-set-key (kbd "<M-wheel-down>") 'cfs-increase-fontsize)))

(defun pl/cfs-reset-profile-and-fontsize ()
  "Reset fontsize with chinese-fonts-setup."
  (interactive)
  (when (display-graphic-p)
    (let* ((profile-name pl/default-cfs-profile-name)
           (current-step pl/default-cfs-fontsize-step))
      (cfs--select-profile profile-name)
      (cfs--set-font (cfs--get-fontsizes current-step))
      (cfs--save-fontsize-step profile-name current-step)
      (message cfs--minibuffer-echo-string))))

(defun pl/cfs-set-symbol-fonts (fontsizes-list)
  ;; (set-fontset-font t 'symbol "Inconsolata" nil 'append)
  ;; (set-fontset-font t 'symbol "Symbola" nil 'append)
  ;; (set-fontset-font t 'unicode "Segoe UI Emoji" nil 'append)
  ;; (set-fontset-font t 'unicode "STIX" nil 'append)
  (set-fontset-font t 'symbol "DejaVu Sans Mono"))

(add-hook 'cfs-set-font-finish-hook 'pl/cfs-set-symbol-fonts)

;; (defhydra hydra-zoom ()
;;   "zoom"
;;   ("+" text-scale-increase "in")
;;   ("-" text-scale-decrease "out")
;;   ("0" (text-scale-adjust 0) "reset")
;;   ("q" nil "quit" :color blue))


;;; old method, which will cause Chinese font can scale when English scale.
;; To get a list of all the possible values of "charset" that is available to set-fontset-font, use
;; the "list-character-sets" function. You can also use the "list-charset-chars" function to see the
;; list of characters contained in a specific charset.
;; (defun pl/set-font ()
;;   (let* ((default-font-size
;;            (case window-system
;;              ('x   12)
;;              ('ns  12)
;;              ('w32 13)
;;              (t (error "Can not set font in this window system."))))

;;          (cjk-font-size 14)
;;          (font-pool
;;           '((x   . ("DejaVu Sans Mono" "Microsoft YaHei"))
;;             (ns  . ("Monaco" "STHeiti"))
;;             (w32 . ("Monaco" "Microsoft YaHei"))))
;;          (font-list (cdr (assoc window-system font-pool)))
;;          (default-font-name (format "%s:pixelsize=%d" (car font-list) default-font-size))
;;          (cjk-font-family (cadr font-list)))

;;     ;; This will decide default font size.
;;     (set-frame-font default-font-name)
;;     ;; Fallback font
;;     (set-fontset-font t 'unicode "Arial Unicode MS")
;;     ;; Font for CJK characters
;;     (mapc (lambda (range)
;;             (set-fontset-font
;;              "fontset-default" `(,(car range) . ,(cadr range)) (font-spec :family cjk-font-family :size cjk-font-size)))
;;           '((#x2E80 #x2EFF)                    ; CJK Radicals Supplement
;;             (#x3000 #x303F)                    ; CJK Symbols and Punctuation
;;             (#x31C0 #x31EF)                    ; CJK Strokes
;;             (#x3200 #x32FF)                    ; Enclosed CJK Letters and Months
;;             (#x3300 #x33FF)                    ; CJK Compatibility
;;             (#x3400 #x4DBF)                    ; CJK Unified Ideographs Extension A
;;             (#x4E00 #x9FFF)                    ; CJK Unified Ideographs
;;             (#xF900 #xFAFF)                    ; CJK Compatibility Ideographs
;;             (#xFE30 #xFE4F)                    ; CJK Compatibility Forms

;;             (#x2000 #x206F)                    ; General Punctuation
;;             (#xFF00 #xFFEF)                    ; Halfwidth and Fullwidth Form
;;             ))

;;     ;; #x03A8 ?Ψ
;;     ;; #x03BB Lambda
;;     ;; #x0192 ?ƒ
;;     (mapc
;;      (lambda (range)
;;        (set-fontset-font
;;         "fontset-default" `(,(car range) . ,(cadr range)) (font-spec :family "dejavu sans mono" :size default-font-size)))
;;      '((#x0180 #x024F)                    ; Latin Extended-B
;;        (#x0370 #x03FF)                    ; Greek
;;        (#x2000 #x206f)                    ; General Punctuation
;;        ;; (#x2200 #x22FF)                    ; Mathematical Operator
;;        (#x2300 #x23FF)                    ; Miscellaneous Technical
;;        ))

;;     ;; ⋂ & ⋃
;;     (set-fontset-font "fontset-default"
;;                       (cons #x22c2
;;                             #x22c3)
;;                       (font-spec :family "FreeMono" :size default-font-size))

;;     (when (eq window-system 'x)
;;       ;; IPA Extensions
;;       (set-fontset-font "fontset-default"
;;                         (cons #x0250
;;                               #x02AF)
;;                         (font-spec :family "Gentium" :size default-font-size))

;;       ;; Phonetic Extensions
;;       (set-fontset-font "fontset-default"
;;                         (cons #x1D00
;;                               #x1D7F)
;;                         (font-spec :family "Gentium" :size default-font-size))
;;       ;; ☹ & ☺
;;       (set-fontset-font "fontset-default"
;;                         (cons #x2639
;;                               #x263a)
;;                         (font-spec :family "fixed" :size default-font-size)))))

;; (when window-system
;;   (pl/set-font))

(provide '50font)

;; Local Variables:
;; eval: (mmm-mode 1)
;; End:

;;; 50font.el ends here
