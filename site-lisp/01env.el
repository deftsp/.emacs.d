;;;;

;; (setenv "LANG" "C")
;; (setenv "SBCL_HOME" "/usr/lib/sbcl")
;; (set-language-environment 'utf-8)
;; (set-keyboard-coding-system 'utf-8)
;; (set-clipboard-coding-system 'utf-8)
;; (set-clipboard-coding-system 'ctext)
;; (set-terminal-coding-system 'utf-8)
;; (set-selection-coding-system 'utf-8)
;; (set-default-coding-systems 'utf-8)
;; (setq locale-coding-system 'utf-8)
;; (setq-default pathname-coding-system 'utf-8)
;; I want unicode
;; (setq default-buffer-file-coding-system 'utf-8)
;; (set-buffer-file-coding-system 'utf-8)
;; (prefer-coding-system 'utf-8)
;; (add-to-list 'process-environment "LOCALE=C")
;; (setq current-language-environment 'utf-8)

;; (setq font-encoding-alist
;;       (append '(("MuleTibetan-0" (tibetan . 0))
;;                 ("GB2312" (chinese-gb2312 . 0))
;;                 ("JISX0208" (japanese-jisx0208 . 0))
;;                 ("JISX0212" (japanese-jisx0212 . 0))
;;                 ("VISCII" (vietnamese-viscii-lower . 0))
;;                 ("KSC5601" (korean-ksc5601 . 0))
;;                 ("MuleArabic-0" (arabic-digit . 0))
;;                 ("MuleArabic-1" (arabic-1-column . 0))
;;                 ("MuleArabic-2" (arabic-2-column . 0))) font-encoding-alist))

;; (require 'code-pages)
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

(when (fboundp 'utf-translate-cjk-mode)
  (utf-translate-cjk-mode t)
  (utf-translate-cjk-load-tables))


;; (unless (coding-system-p 'gbk)
;;   (define-coding-system-alias 'gbk 'chinese-iso-8bit))
;; (unless (coding-system-p 'chinese-gbk)
;;   (define-coding-system-alias 'chinese-gbk 'chinese-iso-8bit))
;; (setq default-process-coding-system '(gbk . gbk))

;; Emacs23，它的 default-process-coding-system 默认是(undecided-dos . undecided-unix).如果什么都不改的话分别 grep utf-8
;; 和 gbk 编码的文件输出的中文都不是乱码，如果指定 process coding 为 gbk 的话就会有乱码了。

;; 和进程相关的编码系统是由 process-coding-system-alist 和 default-process-coding-system 决定的

;; nfo是cp437编码的ascii文件, 这样所有nfo后缀名的文件都会用cp437编码打开
(modify-coding-system-alist 'file "\\.nfo\\'" 'cp437)

(when (< emacs-major-version 23)
  (define-coding-system-alias 'gb18030 'gb2312)
  (define-coding-system-alias 'x-gbk 'gb2312)
  (define-coding-system-alias 'gbk 'gb2312))

(when (> emacs-major-version 22)
  (define-coding-system-alias 'gb2312 'gbk)
  (define-coding-system-alias 'x-gbk 'gbk))




;;; Difference between "exec-path" and "PATH"
;; The value of “ PATH ” is used by emacs when you are running a shell in emacs, similar to when you
;; are using a shell in a terminal.
;; The "exec-path" is used by emacs itself to find programs it needs for its features, such as spell
;; checking, file compression, compiling, grep, diff, etc.
(let ((darwin-path ""))
  (when (eq system-type 'darwin)
    (let ((eprefix (expand-file-name "~/Library/Gentoo")))
      (when (file-directory-p eprefix)
        (setq darwin-path
              (concat eprefix "/bin" ":"
                      eprefix "/usr/bin" ":"
                      (if (file-directory-p "/Applications/Xcode.app/Contents/Developer/usr/bin")
                          "/Applications/Xcode.app/Contents/Developer/usr/bin:" ""))))))
  (setenv "PATH"
          (concat
           (expand-file-name "~/bin") ":"
           (expand-file-name "~/local/bin") ":"
           "/usr/local/bin" ":"
           darwin-path
           (getenv "PATH"))))

(mapc (lambda (n) (add-to-list 'exec-path n))
      `(,(expand-file-name  "~/bin") "/usr/local/bin" "/usr/X11R6/bin"))

;; EPREFIX="$HOME/Library/Gentoo""
(when (eq system-type 'darwin)
 (let ((eprefix (expand-file-name "~/Library/Gentoo")))
  (when (file-directory-p eprefix)
    (add-to-list 'exec-path (concat eprefix "/bin"))
    (add-to-list 'exec-path (concat eprefix "/usr/bin")))))


(provide '01env)