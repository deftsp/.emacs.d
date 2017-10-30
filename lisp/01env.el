;;; 01env.el ---

;;; Commentary:
;; Emacs will load this file first than other init files.

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(when (display-graphic-p)
  (load-theme 'paloryemacs t))

;; always load the newer one between .el and .elc
(setq load-prefer-newer t)

;;; ENV
;; (setenv "SBCL_HOME" "/usr/lib/sbcl")

;; this gives matlab access to the X11 windowing system, so I can see figures, etc.
;; (setenv "DISPLAY" ":0.0")



;;; LANG
(setenv "LANG" (or (getenv "LANG") "en_US.UTF-8"))
(setenv "LC_CTYPE" (or (getenv "LC_CTYPE") "en_US.UTF-8"))

;; http://www.unicode.org/Public/UNIDATA/EastAsianWidth.txt
;; http://d.hatena.ne.jp/khiker/20110327/emacs_cjkamb

;; There is a problem if you set "Chinese-GB" to this configuration; The
;; "Chinese-GB" configuration changes `cjk-char-width-table-list' and that
;; changes specific characters' `char-width'.(like § or λ) It might occur that
;; actual width and char-width are different and it can not be calculate length
;; of string from other extension packages.
(set-language-environment "English")

;; Test example:
;; (set-language-environment "Chinese-GB")
;; (char-width ?“)                        ; => 2 (#o2, #x2)

;; (set-language-environment "English")
;; (set-language-environment 'utf-8)
;; (char-width ?“)                        ; => 1 (#o1, #x1)

;; cjk-char-width-table-list
;; (use-cjk-char-width-table 'zh_CN)

;;; east asian ambiguous settings
;; east asian ambiguous character table
(defun east-asian-ambiguous-characters ()
  '((#x00A1 . #x00A1) (#x00A4 . #x00A4) (#x00A7 . #x00A8)
    (#x00AA . #x00AA) (#x00AD . #x00AE) (#x00B0 . #x00B4)
    (#x00B6 . #x00BA) (#x00BC . #x00BF) (#x00C6 . #x00C6)
    (#x00D0 . #x00D0) (#x00D7 . #x00D8) (#x00DE . #x00E1)
    (#x00E6 . #x00E6) (#x00E8 . #x00EA) (#x00EC . #x00ED)
    (#x00F0 . #x00F0) (#x00F2 . #x00F3) (#x00F7 . #x00FA)
    (#x00FC . #x00FC) (#x00FE . #x00FE) (#x0101 . #x0101)
    (#x0111 . #x0111) (#x0113 . #x0113) (#x011B . #x011B)
    (#x0126 . #x0127) (#x012B . #x012B) (#x0131 . #x0133)
    (#x0138 . #x0138) (#x013F . #x0142) (#x0144 . #x0144)
    (#x0148 . #x014B) (#x014D . #x014D) (#x0152 . #x0153)
    (#x0166 . #x0167) (#x016B . #x016B) (#x01CE . #x01CE)
    (#x01D0 . #x01D0) (#x01D2 . #x01D2) (#x01D4 . #x01D4)
    (#x01D6 . #x01D6) (#x01D8 . #x01D8) (#x01DA . #x01DA)
    (#x01DC . #x01DC) (#x0251 . #x0251) (#x0261 . #x0261)
    (#x02C4 . #x02C4) (#x02C7 . #x02C7) (#x02C9 . #x02CB)
    (#x02CD . #x02CD) (#x02D0 . #x02D0) (#x02D8 . #x02DB)
    (#x02DD . #x02DD) (#x02DF . #x02DF) (#x0300 . #x036F)
    (#x0391 . #x03A9) (#x03B1 . #x03C1) (#x03C3 . #x03C9)
    (#x0401 . #x0401) (#x0410 . #x044F) (#x0451 . #x0451)
    (#x2010 . #x2010) (#x2013 . #x2016) (#x2018 . #x2019)
    (#x201C . #x201D) (#x2020 . #x2022) (#x2024 . #x2027)
    (#x2030 . #x2030) (#x2032 . #x2033) (#x2035 . #x2035)
    (#x203B . #x203B) (#x203E . #x203E) (#x2074 . #x2074)
    (#x207F . #x207F) (#x2081 . #x2084) (#x20AC . #x20AC)
    (#x2103 . #x2103) (#x2105 . #x2105) (#x2109 . #x2109)
    (#x2113 . #x2113) (#x2116 . #x2116) (#x2121 . #x2122)
    (#x2126 . #x2126) (#x212B . #x212B) (#x2153 . #x2154)
    (#x215B . #x215E) (#x2160 . #x216B) (#x2170 . #x2179)
    (#x2190 . #x2199) (#x21B8 . #x21B9) (#x21D2 . #x21D2)
    (#x21D4 . #x21D4) (#x21E7 . #x21E7) (#x2200 . #x2200)
    (#x2202 . #x2203) (#x2207 . #x2208) (#x220B . #x220B)
    (#x220F . #x220F) (#x2211 . #x2211) (#x2215 . #x2215)
    (#x221A . #x221A) (#x221D . #x2220) (#x2223 . #x2223)
    (#x2225 . #x2225) (#x2227 . #x222C) (#x222E . #x222E)
    (#x2234 . #x2237) (#x223C . #x223D) (#x2248 . #x2248)
    (#x224C . #x224C) (#x2252 . #x2252) (#x2260 . #x2261)
    (#x2264 . #x2267) (#x226A . #x226B) (#x226E . #x226F)
    (#x2282 . #x2283) (#x2286 . #x2287) (#x2295 . #x2295)
    (#x2299 . #x2299) (#x22A5 . #x22A5) (#x22BF . #x22BF)
    (#x2312 . #x2312) (#x2460 . #x24E9) (#x24EB . #x254B)
    (#x2550 . #x2573) (#x2580 . #x258F) (#x2592 . #x2595)
    (#x25A0 . #x25A1) (#x25A3 . #x25A9) (#x25B2 . #x25B3)
    (#x25B6 . #x25B7) (#x25BC . #x25BD) (#x25C0 . #x25C1)
    (#x25C6 . #x25C8) (#x25CB . #x25CB) (#x25CE . #x25D1)
    (#x25E2 . #x25E5) (#x25EF . #x25EF) (#x2605 . #x2606)
    (#x2609 . #x2609) (#x260E . #x260F) (#x2614 . #x2615)
    (#x261C . #x261C) (#x261E . #x261E) (#x2640 . #x2640)
    (#x2642 . #x2642) (#x2660 . #x2661) (#x2663 . #x2665)
    (#x2667 . #x266A) (#x266C . #x266D) (#x266F . #x266F)
    (#x273D . #x273D) (#x2776 . #x277F) (#xE000 . #xF8FF)
    (#xFE00 . #xFE0F) (#xFFE0 . #xFFE6) (#xFFFD . #xFFFD)))

;; setting function
(defun set-east-asian-ambiguous-width (width)
  (cond ((= emacs-major-version 22)
         (set-east-asian-ambiguous-width-22 width))
        ((> emacs-major-version 22)
         (set-east-asian-ambiguous-width-23 width))))

;; for emacs 22
(defun set-east-asian-ambiguous-width-22 (width)
  (if (= width 2)
      (utf-translate-cjk-set-unicode-range (east-asian-ambiguous-characters))))

;; for over 23 (checked work in emacs 24)
(defun set-east-asian-ambiguous-width-23 (width)
  (while (char-table-parent char-width-table)
         (setq char-width-table (char-table-parent char-width-table)))
  (let ((table (make-char-table nil)))
    (dolist (range (east-asian-ambiguous-characters))
      (set-char-table-range table range width))
    (optimize-char-table table)
    (set-char-table-parent table char-width-table)
    (setq char-width-table table)))

(set-east-asian-ambiguous-width 2)

;;; coding system
(defun paloryemacs/set-coding-system (coding)
  (prefer-coding-system coding) ; default coding system for subprocess I/O
  (set-clipboard-coding-system coding) ; 'ctext
  (set-selection-coding-system coding)
  (set-file-name-coding-system coding)
  (set-keyboard-coding-system coding)
  (set-default-coding-systems coding)
  (set-terminal-coding-system coding)
  (set-buffer-file-coding-system coding)
  (setq locale-coding-system coding)
  (add-to-list 'process-coding-system-alist `("git" . ,coding))
  (add-to-list 'auto-coding-alist `("COMMIT_EDITMSG" . ,coding)))

(paloryemacs/set-coding-system 'utf-8)

(set-input-method nil)

;; (add-to-list 'process-environment "LOCALE=C")

;; nfo is ascii file with cp437
(modify-coding-system-alist 'file "\\.nfo\\'" 'cp437)

(when (< emacs-major-version 23)
  (define-coding-system-alias 'gb18030 'gb2312)
  (define-coding-system-alias 'x-gbk 'gb2312)
  (define-coding-system-alias 'gbk 'gb2312))

;; (when (> emacs-major-version 22)
;;   (define-coding-system-alias 'gb2312 'gbk)
;;   (define-coding-system-alias 'x-gbk 'gbk))

;;; modifier
(cl-case system-type
  (darwin
   (setq mac-function-modifier 'hyper) ; 'none, super
   (setq mac-option-modifier 'super) ; sets the Option key as Super
   (setq mac-command-modifier 'meta) ; sets the Command key as Meta
   (setq mac-control-modifier 'control))
  (windows-nt
   ;; setting the PC keyboard's various keys to
   ;; Super or Hyper, for emacs running on Windows.
   (setq w32-pass-lwindow-to-system nil
         w32-pass-rwindow-to-system nil
         w32-pass-apps-to-system nil
         ;; Left Windows key
         w32-lwindow-modifier 'super
         ;; Right Windows key
         w32-rwindow-modifier 'super
         ;; Menu key
         w32-apps-modifier 'hyper))
  (gnu/linux
   ;; do nothing. You should set Super and Hyper from your OS
   nil))

;;; Difference between "exec-path" and "PATH"
;; The value of "PATH" is used by emacs when you are running a shell in emacs, similar to when you
;; are using a shell in a terminal.
;; The "exec-path" is used by emacs itself to find programs it needs for its features, such as spell
;; checking, file compression, compiling, grep, diff, etc.
(with-eval-after-load 'exec-path-from-shell
  (when (memq window-system '(mac ns))
    (add-to-list 'exec-path-from-shell-variables "GOROOT")
    (setq exec-path-from-shell-check-startup-files nil)
    (exec-path-from-shell-initialize)))

(when (eq system-type 'darwin)
  (setenv "INFOPATH" (concat (expand-file-name "~/share/info:") (getenv "INFOPATH"))))

(defconst paloryemacs/cache-directory (concat user-emacs-directory "cache/")
  "cache files directory")

;;; preset variables
;; `org-replace-disputed-keys' must be set before org.el is loaded, it seems some pacakge install by el-get will load
;; org.el, so just preset it.
(setq org-replace-disputed-keys t)

(provide '01env)
