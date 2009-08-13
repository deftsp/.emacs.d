;; -*- mode: Emacs-Lisp -*-
;; Time-stamp: <2009-08-09 01:09:45 S.P.Tseng>

;;; Completion

;;; dabbrev-expand
(setq dabbrev-case-fold-search 'case-fold-search)
(global-set-key (kbd "M-/") 'dabbrev-expand)
;; (define-key minibuffer-local-map (kbd "C-<tab>") 'dabbrev-expand)

;; Continuing Expansion
;; Once you have successfully used dabbrev-expand to expand a word, hitting <space> then immediately using
;; dabbrev-expand again will continue to expand from the point that the expansion was found.

;; (require 'dabbrev-expand-multiple)
;; (setq dabbrev-expand-multiple-select-keys '("a" "s" "d" "f" "q" "w" "e" "r"))
;; (global-set-key (kbd "M-/") 'dabbrev-expand-multiple)


;;; pabbrev --- I use PredictiveMode instead of.
;; (require 'pabbrev)
;; (setq pabbrev-idle-timer-verbose nil)
;; (global-pabbrev-mode -1)

;;; hippie-expand (like M-/, but more powerful)
(defun he-tag-beg ()
  (let ((p (save-excursion
             (backward-word 1)
             (point))))
    p))

(defun try-expand-tag (old)
  (unless old
    (he-init-string (he-tag-beg) (point))
    (setq he-expand-list (sort
                          (all-completions he-search-string 'tags-complete-tag) 'string-lessp)))
  (while (and he-expand-list
            (he-string-member (car he-expand-list) he-tried-table))
    (setq he-expand-list (cdr he-expand-list)))
  (if (null he-expand-list)
      (progn
        (when old (he-reset-string))
        ())
      (he-substitute-string (car he-expand-list))
      (setq he-expand-list (cdr he-expand-list))
      t))

(global-set-key (kbd "<C-tab>") 'hippie-expand)
;; (global-set-key (kbd "M-/") 'hippie-expand)

;;hippie-expand-try-functions-list是一个优先列表.
;;其中 "expand-dabbrev" 是指搜索匹配你当前输入的头部的词语进行补全。
(autoload 'senator-try-expand-semantic "senator")
(setq hippie-expand-try-functions-list
      '(try-expand-all-abbrevs             ; 匹配所有缩写词
        try-expand-dabbrev                 ; 搜索当前 buffer
        try-expand-dabbrev-visible         ; 搜索当前可见窗口
        try-expand-dabbrev-all-buffers     ; 搜索所有 buffer
        try-expand-dabbrev-from-kill       ; 从 kill-ring 中搜索
        try-complete-lisp-symbol-partially ; 部分补全 elisp symbol
        try-complete-lisp-symbol           ; 补全 lisp symbol
        try-complete-file-name-partially   ; 文件名部分匹配
        try-complete-file-name             ; 文件名匹配
        ;; senator-try-expand-semantic
        try-expand-line                    ; 补全当前行
        try-expand-line-all-buffers
        try-expand-list                    ; 补全一个列表
        try-expand-list-all-buffers
        try-expand-tag
        ispell-complete-word
        try-expand-whole-kill))

(setq-default abbrev-mode t)
;;(read-abbrev-file "~/.abbrev_defs")
(setq abbrev-file-name "~/.emacs.d/.abbrev_defs")
;; 禁用自动保存缩写词
(setq save-abbrevs nil)
(if (file-exists-p  abbrev-file-name)
    (quietly-read-abbrev-file abbrev-file-name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;输入inc 然后会提示输入文件名称.
(mapc
 (lambda (mode)
   (define-abbrev-table mode '(("inc" "" tsp-skel-include 1))))
 '(c-mode-abbrev-table c++-mode-abbrev-table))

;; 输入 inc , 可以自动提示输入文件名称,可以自动补全.
(define-skeleton tsp-skel-include
    "generate include<>" ""
    > "#include <"
    (completing-read
     "Include File:"
     (mapcar #'(lambda (f) (list f ))
             (apply 'append
                    (mapcar #'(lambda (dir)
                                (if (or (string= dir "/usr/include") (string= dir "/usr/local/include"))
                                    (directory-files dir nil "^[^.]")
                                    (flet ((last-dir (path)
                                             (string-match ".*/\\(.*\\)" path)
                                             (match-string 1 path)))
                                      (mapcar #'(lambda (file-name)
                                                  (concat (last-dir dir)
                                                          "/" file-name))
                                              (directory-files dir)))))
                            (list "/usr/include"
                                  "/usr/local/include"
                                  "/usr/include/sys"
                                  "/usr/include/netinet"
                                  "/usr/include/arpa"
                                  "/usr/include/bits")))))
    ">\n")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;当你发呆的时间超过 4 秒以上，头文件的文件名称自动加一个下划线，在上面按回车，可以打开这个文件

(defvar tsp-c/c++-hightligh-included-files-key-map nil)
(if tsp-c/c++-hightligh-included-files-key-map
    nil
    (setq tsp-c/c++-hightligh-included-files-key-map (make-sparse-keymap))
    (define-key tsp-c/c++-hightligh-included-files-key-map (kbd "<RET>") 'find-file-at-point))

(defun tsp-c/c++-hightligh-included-files ()
  (interactive)
  (when (or (eq major-mode 'c-mode)
            (eq major-mode 'c++-mode))
    (save-excursion
      (goto-char (point-min))
      ;; remove all overlay first
      (mapc (lambda (ov) (if (overlay-get ov 'tsp-c/c++-hightligh-included-files)
                             (delete-overlay ov)))
            (overlays-in (point-min) (point-max)))
      (while (re-search-forward "^#include[ \t]+[\"<]\\(.*\\)[\">]" nil t nil)
        (let* ((begin  (match-beginning 1))
               (end (match-end 1))
               (ov (make-overlay begin end)))
          (overlay-put ov 'tsp-c/c++-hightligh-included-files t)
          (overlay-put ov 'keymap tsp-c/c++-hightligh-included-files-key-map)
          (overlay-put ov 'face 'underline))))))
;; 这不是一个好办法，也可以把它加载到 c-mode-hook or c++-mode-hook 中。
;;(setq tsp-c/c++-hightligh-included-files-timer (run-with-idle-timer 4 t 'tsp-c/c++-hightligh-included-files))
(add-hook 'c-mode-hook 'tsp-c/c++-hightligh-included-files)
(add-hook 'c++-mode-hook 'tsp-c/c++-hightligh-included-files)

;;-------------------------------------------------------------------------------------
;;注意在触发abbrev时，不要用回车否则光标不会停在指定位置
(define-skeleton tsp-skeleton-c-mode-main-func
    "generate int main(int argc, char * argv[]) automatic" nil
    "int\nmain(int argc, char *argv[]) \n{\n"
    > _  "\n" > "return 0;"
    "\n}")
(define-abbrev-table 'c-mode-abbrev-table
    '(("main" "" tsp-skeleton-c-mode-main-func 1)))
(define-abbrev-table 'c++-mode-abbrev-table
    '(("main" "" tsp-skeleton-c-mode-main-func 1)))
;;-------------------------------------------------------------------------------------
(define-skeleton tsp-skel-c-for-func
    "generate for () { } automatic" nil
    "for (" _ ") { " > \n
    \n
    "}"> \n)
(define-abbrev-table 'c-mode-abbrev-table
    '(("fors" "" tsp-skel-c-for-func 1)))
(define-abbrev-table 'c++-mode-abbrev-table
    '(("fors" "" tsp-skel-c-for-func 1)))
;;-------------------------------------------------------------------------------------

(define-skeleton tsp-skel-c-ife
    "Insert a C if ... else .. block" nil
    > "if (" _ ") {" \n
    \n
    "}" > \n
    "else {" > \n
    \n
    "}" > \n)
(define-abbrev-table 'c-mode-abbrev-table
    '(("ife" "" tsp-skel-c-ife 1)))
(define-abbrev-table 'c++-mode-abbrev-table
    '(("ife" "" tsp-skel-c-ife 1)))

(define-skeleton tsp-skel-elisp-separator
    "Inserts a separator for elisp file."
  nil
  ";; ------------------------------------------------------------------------------------\n"
  ";; "_"\n"
  ";; ------------------------------------------------------------------------------------\n")

;; (define-skeleton skeleton-c-mode-comment-box
;;   "create a comment box" nil
;;   "/**********************************************\n"
;;   > " * " _ "\n"
;;   > " **********************************************/"
;;   )

;;--------------------------------------------------------------------------------------

;;--------------------------------------------------------------------------------
;;;Pair Insertion
;;--------------------------------------------------------------------------------
(setq skeleton-pair t)
;; do not insert newline after skeleton insertation
(setq skeleton-end-hook nil)

;; (setq skeleton-pair-alist  '((?` ?` _ "''") ; "?" a space
;;                              (?\( _ ")")
;;                              (?\[ _ "]")
;;                              (?{ \n > _ \n ?} >)))

(setq skeleton-pair-alist '((?( _ ?)) (?\))
                            (?[ _ ?]) (?\])
                            (?{ _ ?}) (?\})
                            (?< _ ?>) (?\>)
                            (?« _ ?») (?\»)
                            (?` _ ?')))


;; (add-hook 'emacs-lisp-mode-hook
;;           (lambda ()
;;             (make-local-variable 'skeleton-pair)
;;             (make-local-variable 'skeleton-pair-on-word)
;;             (make-local-variable 'skeleton-pair-filter-function)
;;             (make-local-variable 'skeleton-pair-alist)
;;             (setq skeleton-pair-on-word t
;;                   skeleton-pair t)
;;             (local-set-key (kbd "(") 'skeleton-pair-insert-maybe)
;;             (local-set-key (kbd "[") 'skeleton-pair-insert-maybe)
;;             (local-set-key (kbd "{") 'skeleton-pair-insert-maybe)))


(defun tsp-auto-pair ()
  (interactive)
  (make-local-variable 'skeleton-pair-alist)
  (local-set-key (kbd "\'") 'skeleton-pair-insert-maybe)
  (local-set-key (kbd "\"") 'skeleton-pair-insert-maybe)
  (local-set-key (kbd "(") 'skeleton-pair-insert-maybe)
  (local-set-key (kbd "[") 'skeleton-pair-insert-maybe)
  (local-set-key (kbd "{") 'skeleton-pair-insert-maybe)
  )

(add-hook 'c-mode-hook 'tsp-auto-pair)
(add-hook 'c++-mode-hook 'tsp-auto-pair)

;; (global-set-key (kbd "<") 'skeleton-pair-insert-maybe)
;; (global-set-key (kbd "`") 'skeleton-pair-insert-maybe)
(global-set-key (kbd "\"") 'skeleton-pair-insert-maybe)
(global-set-key (kbd "(") 'skeleton-pair-insert-maybe)
(global-set-key (kbd "[") 'skeleton-pair-insert-maybe)
(global-set-key (kbd "{") 'skeleton-pair-insert-maybe)

;; (global-set-key (kbd "(") (lambda () (interactive) (insert "()") (backward-char 1)))
;; (global-set-key (kbd "“") (lambda () (interactive) (insert "“”") (backward-char 1)))
;; (global-set-key (kbd "‘") (lambda () (interactive) (insert "‘’") (backward-char 1)))
;; (global-set-key (kbd "«") (lambda () (interactive) (insert "«»") (backward-char 1)))
;;-----------------------------------------------------------------------------------

(define-abbrev-table 'global-abbrev-table '(("alpha" "α" nil 0)
                                            ("beta" "β" nil 0)
                                            ("gamma" "γ" nil 0)
                                            ("theta" "θ" nil 0)
                                            ("Infinity" "∞" nil 0)
                                            ("ar1" "→" nil 0)
                                            ("ar2" "⇒" nil 0)
                                            ("afaict" "as far as I can tell" nil 1)
                                            ("omuse" "http://www.emacswiki.org/cgi-bin/oddmuse.pl" nil 0)
                                            ("btw" "by the way" nil 3)
                                            ("wether" "whether" nil 5)
                                            ("ewiki" "http://www.emacswiki.org/cgi-bin/wiki.pl" nil 3)
                                            ("pov" "point of view" nil 1)))

;;; defun with abbrev
;; (defun define-abbrev-function (table abbrev func)
;;   (put func 'no-self-insert t)
;;   (define-abbrev table abbrev "" `(lambda () (call-interactively ',func))))

;; (defmacro defun-abbrev (funcname abbrev &rest body)
;;   "Defun a function and define an abbrev.
;; Note that `table' is abbrev table to use."
;;   `(progn
;;      (defun ,funcname ,@body)
;;      (define-abbrev-function table ,abbrev ',funcname)))


;; turn on Partial Completion mode, for example 'M-x q r r' will equal to query-replace-regexp
;; when input 're-b', press Tab. cursor will go to the wrong place.
;; (setq PC-meta-flag nil)
;; (partial-completion-mode t)

;; auto complete function an variables in minibuffer
;; * Incremental minibuffer completion *
(icomplete-mode t)
(eval-after-load "icomplete" '(progn (require 'icomplete+)))
(setq icomplete-prospects-height 1)
