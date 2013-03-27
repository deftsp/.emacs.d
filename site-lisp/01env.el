;;; 01env.el ---

(when (display-graphic-p)
  (load-theme 'palory t))

;; (setenv "LANG" "C")
;; (setenv "SBCL_HOME" "/usr/lib/sbcl")

;; coding system of a newly created buffer, terminal, keyboard,file name
;; default coding system for subprocess I/O
(prefer-coding-system 'utf-8)
(set-clipboard-coding-system 'utf-8) ; 'ctext
(set-selection-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(setq locale-coding-system 'utf-8)
(set-language-environment 'utf-8)
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
(case system-type
  (darwin
   (setq mac-function-modifier 'super) ; 'none, super
   (setq mac-option-modifier 'hyper) ; sets the Option key as Super
   (setq mac-command-modifier 'meta) ; sets the Command key as Meta
   (setq mac-control-modifier 'control))
  (wndows-nt
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
;; The value of “ PATH ” is used by emacs when you are running a shell in emacs, similar to when you
;; are using a shell in a terminal.
;; The "exec-path" is used by emacs itself to find programs it needs for its features, such as spell
;; checking, file compression, compiling, grep, diff, etc.
(defun pl/gentoo-prefix-path ()
  (if (eq system-type 'darwin)
      (let ((eprefix (expand-file-name "~/Library/Gentoo"))) ; Gentoo Prefix
        (if (file-directory-p eprefix)
            (concat eprefix "/bin:"
                    eprefix "/usr/bin:")
            ""))
      ""))

(defun pl/xcode-bin-path ()
  (if (eq system-type 'darwin)
      (let ((p "/Applications/Xcode.app/Contents/Developer/usr/bin"))
        (if (file-directory-p p) (concat p ":") ""))
      ""))


(setenv "PATH"
        (concat
         "/Applications/Emacs.app/Contents/MacOS/bin:"  ; TODO: update for multi platform
         "/Applications/Emacs.app/Contents/MacOS/libexec:"
         (expand-file-name "~/bin:")
         "/usr/texbin:"
         "/Applications/Gnuplot.app/Contents/Resources/bin:"
         (expand-file-name "~/local/bin:")
         (expand-file-name "~/.cabal/bin:")
         "/usr/local/bin:"
         "/opt/bin:"
         (pl/gentoo-prefix-path)
         (pl/xcode-bin-path)
         (getenv "PATH")))

(let ((gentoo-prefix-path (pl/gentoo-prefix-path))
      (xcode-bin-path (pl/xcode-bin-path)))
  (mapc (lambda (n) (add-to-list 'exec-path n))
        `(,(expand-file-name  "~/bin")
          ,(expand-file-name "~/.cabal/bin")
           "/usr/texbin"
           "/Applications/Gnuplot.app/Contents/Resources/bin"
           "/usr/local/bin"
           "/usr/local/opt/ruby/bin"
           "/opt/bin"
           "/usr/X11R6/bin"))

  (unless (string-equal gentoo-prefix-path "")
    (add-to-list 'exec-path gentoo-prefix-path))
  (unless (string-equal xcode-bin-path "")
    (add-to-list 'exec-path xcode-bin-path)))




;; EPREFIX="$HOME/Library/Gentoo""
(when (eq system-type 'darwin)
 (let ((eprefix (expand-file-name "~/Library/Gentoo")))
  (when (file-directory-p eprefix)
    (add-to-list 'exec-path (concat eprefix "/bin"))
    (add-to-list 'exec-path (concat eprefix "/usr/bin")))))



(when (eq system-type 'darwin)
  (setenv "INFOPATH" (concat (expand-file-name "~/share/info:") (getenv "INFOPATH"))))


;;; preset variables
;; `org-replace-disputed-keys' must be set before org.el is loaded, it seems some pacakge install by el-get will load
;; org.el, so just preset it.
(setq org-replace-disputed-keys t)



(provide '01env)
