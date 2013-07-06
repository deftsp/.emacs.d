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
;; The value of “ PATH ” is used by emacs when you are running a shell in emacs, similar to when you
;; are using a shell in a terminal.
;; The "exec-path" is used by emacs itself to find programs it needs for its features, such as spell
;; checking, file compression, compiling, grep, diff, etc.
(defvar pl/path-list nil "PATH list for darwin only")

(defun pl/env-path-init ()
  (case system-type
    (darwin
     (setq pl/path-list `(,(expand-file-name "~/bin")
                          "/usr/texbin"
                          "/Applications/Gnuplot.app/Contents/Resources/bin"
                          ,(expand-file-name "~/local/bin")
                          ,(expand-file-name "~/.cabal/bin")
                          ,(expand-file-name "~/opt/go-packages/bin")
                          "/usr/local/bin"
                          "/opt/bin"
                          "/Applications/Emacs.app/Contents/MacOS/bin"
                          "/Applications/Emacs.app/Contents/MacOS/libexec"))

     (let ((p "/Applications/Xcode.app/Contents/Developer/usr/bin"))
       (when (file-directory-p p)
         (add-to-list 'pl/path-list p))))

    (windows-nt
     nil)
    (gnu/linux
     nil))

  (setq pl/path-list (cl-remove-if-not #'file-directory-p pl/path-list))

  (setenv "PATH"
          (concat (mapconcat #'identity pl/path-list ":")
                  ":"
                  (getenv "PATH")))
  ;; set exec-path
  (mapc (lambda (p) (add-to-list 'exec-path p))
        pl/path-list))

(when (eq system-type 'darwin)
  (pl/env-path-init)
  (setenv "INFOPATH" (concat (expand-file-name "~/share/info:") (getenv "INFOPATH"))))


;;; preset variables
;; `org-replace-disputed-keys' must be set before org.el is loaded, it seems some pacakge install by el-get will load
;; org.el, so just preset it.
(setq org-replace-disputed-keys t)



(provide '01env)
