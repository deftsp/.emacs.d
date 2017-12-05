;;; 01env.el ---

;;; Commentary:
;; Emacs will load this file first than other init files.

;;; LANG
(setenv "LANG" (or (getenv "LANG") "en_US.UTF-8"))
(setenv "LC_CTYPE" (or (getenv "LC_CTYPE") "en_US.UTF-8"))
(set-language-environment 'UTF-8) ; Chinese-GB, English

;;; ENV
;; (setenv "SBCL_HOME" "/usr/lib/sbcl")

;; this gives matlab access to the X11 windowing system, so I can see figures, etc.
;; (setenv "DISPLAY" ":0.0")

;;; coding system
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))
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
  (setq org-export-coding-system coding)
  (set-charset-priority 'unicode)
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

;; http://www.unicode.org/Public/UNIDATA/EastAsianWidth.txt
;; http://d.hatena.ne.jp/khiker/20110327/emacs_cjkamb

;; There is a problem if you set "Chinese-GB" to this configuration; The
;; "Chinese-GB" configuration changes `cjk-char-width-table-list' and that
;; changes specific characters' `char-width'.(like § or λ) It might occur that
;; actual width and char-width are different and it can not be calculate length
;; of string from other extension packages.

;; Test example:
;; (set-language-environment "Chinese-GB")
;; (char-width ?“)                        ; => 2 (#o2, #x2)

;; (set-language-environment "English")
;; (set-language-environment 'UTF-8)
;; (char-width ?“)                        ; => 1 (#o1, #x1)

;; cjk-char-width-table-list
;; (use-cjk-char-width-table 'zh_CN)
;; (char-width ?“)

;; (char-width ?—)

;;; east asian ambiguous settings
;; (use-package east-asian-ambiguous)

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
         mac-pass-command-to-system t
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

;; see also color-theme-buffer-local from https://github.com/vic/color-theme-buffer-local
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(when (display-graphic-p)
  (load-theme 'paloryemacs t))

(provide '01env)
