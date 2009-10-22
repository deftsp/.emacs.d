;;; 50c-mode.el ---
;; Author: S.P.Tseng <deftsp@gmail.com>
;; Time-stamp: <2009-10-22 13:18:13 S.P.Tseng>

(require 'smarter-operator)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;make a #define be left-aligned
(setq c-electric-pound-behavior (quote (alignleft)))
;; variable: comment-padding Padding string that `comment-region' puts between comment chars and text.

(defun tsp-c-mode-hook ()
  (interactive)
  (c-set-style "bsd")
  ;; (c-toggle-auto-state) ; 当你输入时自动缩进，自动换行
  (c-toggle-hungry-state)              ; 此模式下，当按Backspace时会删除最多的空格
  (c-subword-mode 1)                   ; makes some movement and text commands recognize case-change as a word boundary.
  ;; (setq c-tab-always-indent nil)
  (require 'ctypes)                     ; ctypes 可以识别你的 C 文件里的类型定义 (typedef)。自动对它们进行语法加亮。
  (ctypes-auto-parse-mode 1)
  (setq c-basic-offset 4)
  ;;(local-set-key [(control tab)] 'tempo-forward-mark)    ; move to next tempo mark
  (imenu-add-menubar-index)             ; 在菜单中加入当前Buffer的函数索引
  (define-key c-mode-base-map (kbd "s-o") 'eassist-switch-h-cpp) ; 列出当前buffer中的各个方法的列表，包括参数列表和返回类型
  (define-key c-mode-base-map (kbd "C-x , m") 'eassist-list-methods) ; 在buffer中，切换同一个目录下的头文件和源文件
  ;; for outline minor mode
  (outline-minor-mode)
  (make-local-variable 'outline-regexp)
  ;; (setq outline-regexp ".*{")
  ;; (setq outline-regexp "[:blank:]*\\(.*{\\|.*}\\)")
  ;; (setq outline-regexp "[^ #\t\n]\\|[:blank:]*\\([{}]\\|[^* \t\n\^M\^L]\\|\\*+[a-zA-Z_0-9=(]\\)")
  (setq outline-regexp "[^ #\t\n]\\|[:blank:]*\\(.*{\\|.*}\\)")
  ;; (setq outline-regexp "[ \t]*\\([^* \t\n\^M\^L]\\|\\*+[a-zA-Z_0-9=(]\\)")
  ;; (hide-sublevels 2)
  ;; (hide-body)
  (smarter-operator-mode))

;; (defun tsp-next-c-function ()
;;   "Go to start of next C function."
;;   (interactive)
;;   (c-beginning-of-defun -1))

;; (defun tsp-prev-c-function ()
;;   "Go to start of next C function."
;;   (interactive)
;;   (c-beginning-of-defun 2))

(defun my-c-mode-setup ()
  (local-set-key "\C-m" 'c-context-line-break) ; 多行注释回车在下一行行首自动添加 *
  ;;对嵌套的#ifdef上色 ,不同层次的#ifdef有不同的颜色
  (require 'ifdef)
  ;; (local-set-key (kbd "M-/") 'semantic-complete-analyze-inline)
  (local-set-key "." 'semantic-complete-self-insert)
  ;; (local-set-key ">" 'semantic-complete-self-insert)
  (define-key c-mode-base-map (kbd "<M-S-iso-lefttab>") 'mark-ifdef)
  (define-key c-mode-base-map (kbd "M-TAB") 'semantic-ia-complete-symbol)
  (define-key c-mode-base-map (kbd "C-c ?") 'semantic-ia-complete-symbol-menu)
  (define-key c-mode-base-map (kbd "C-c >") 'semantic-complete-analyze-inline)
  (define-key c-mode-base-map (kbd "C-c p") 'semantic-analyze-proto-impl-toggle)
  (define-key c-mode-base-map (kbd "C-c =") 'semantic-decoration-include-visit)
  (define-key c-mode-base-map (kbd "C-c D") 'semantic-ia-show-doc)
  (define-key c-mode-base-map (kbd "H-SPC") 'semantic-ia-show-summary)
  (define-key c-mode-base-map (kbd "C-c C-r") 'semantic-symref)


  (define-key c-mode-base-map (kbd "H-M-f") 'c-forward-into-nomenclature)
  (define-key c-mode-base-map (kbd "H-M-b") 'c-backward-into-nomenclature)
  (define-key c-mode-base-map (kbd "H-M-n") 'senator-previous-token)
  (define-key c-mode-base-map (kbd "H-M-p") 'senator-next-token)
  (define-key c-mode-base-map (kbd "H-M-j") 'tsp-move-function-down)
  (define-key c-mode-base-map (kbd "H-M-k") 'tsp-move-function-up))

(add-hook 'c-mode-common-hook 'my-c-mode-setup)



(defun tsp-cpp-mode-hook ()
  ;;  (c-toggle-auto-state)
  (c-toggle-hungry-state)
  (c-set-style "stroustrup")
  (require 'ctypes)
  (ctypes-auto-parse-mode 1)
  ;; makes some movement and text commands recognize case-change as a word boundary.
  (c-subword-mode 1)
  (setq c-basic-offset 4)
  (imenu-add-menubar-index)
  (smarter-operator-mode))
(add-hook 'c-mode-hook 'tsp-c-mode-hook)
(add-hook 'c++-mode-hook 'tsp-cpp-mode-hook)
;;============================================================

;;;
;; move current function up
(defun tsp-move-function-up ()
  (interactive)
  (save-excursion
    (c-mark-function)
    (kill-region (region-beginning) (region-end))
    (c-beginning-of-defun 1)
    (yank)))

;; move current function down
(defun tsp-move-function-down ()
  (interactive)
  (save-excursion
    (c-mark-function)
    (kill-region (region-beginning) (region-end))
    (c-beginning-of-defun -1)
    (yank)))
;; Probably you you can use 'delete-and-extract-region' instead of 'kill-region' plus 'yank'.

;;----------------------------------------------------------------------------------------------------

;; (defun mark-c-scope-beg ()
;;   "Marks the c-scope (region between {}) enclosing the point.
;;    Naive, as will be confused by { } within strings"
;;   (let
;;    ((scope-depth 1))
;;  (while (not (= scope-depth 0))
;;    (search-backward-regexp "}\\|{")
;;    (if (string= (char-to-string (char-before)) "}")
;;        (setq scope-depth (1+ scope-depth))
;;          (setq scope-depth (1- scope-depth)))))
;;   (point))

;; (defun mark-c-scope-end ()
;;   "Marks the c-scopie (region between {}) enclosing the point.
;;    Naive, as will be confused by { } within strings"
;;   (let
;;    ((scope-depth 1))
;;  (while (not (= scope-depth 0))
;;    (search-forward-regexp "}\\|{")
;;    (if (string= (char-to-string (char-before)) "}")
;;        (setq scope-depth (1- scope-depth))
;;          (setq scope-depth (1_ scope-depth)))))
;;   (point))

;; (defun kill-c-scope ()
;;   (interactive)
;;   (let
;;    ((inital-point (point)))
;;  (save-excursion
;;    (let
;;      ((beg (mark-c-scope-beg)))
;;      (goto-char inital-point)
;;      (let ((end (mark-c-scope-end))))))))

;;; Comment
;; (setq comment-start "//")
;; (setq compilation-read-command 'nil)

(setq comment-style 'extra-line)        ;default "indent"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; font-lock
;; Colorisation : C : Grands classiks
(font-lock-add-keywords 'c-mode
                        '(("\\<\\(string\\)\\>" . font-lock-type-face)
                          ("\\<\\(vector\\)\\>" . font-lock-type-face)
                          ("\\<\\(namespace\\)\\>" . font-lock-keyword-face)
                          ("\\<\\(class\\)\\>" . font-lock-keyword-face)
                          ("\\<\\(virtual\\)\\>" . font-lock-keyword-face)
                          ("\\<\\(printf\\)\\>" . font-lock-keyword-face)
                          ("\\<\\(getc\\)\\>" . font-lock-keyword-face)
                          ("\\<\\(strtok\\)\\>" . font-lock-keyword-face)
                          ("\\<\\(strcmp\\)\\>" . font-lock-keyword-face)
                          ("\\<\\(strlen\\)\\>" . font-lock-keyword-face)
                          ("\\<\\(atoi\\)\\>" . font-lock-keyword-face)
                          ("\\<\\(fgets\\)\\>" . font-lock-keyword-face)
                          ("\\<\\(fprintf\\)\\>" . font-lock-keyword-face)
                          ("\\<\\(remove\\)\\>" . font-lock-keyword-face)
                          ("\\<\\(rename\\)\\>" . font-lock-keyword-face)
                          ("\\<\\(stat\\)\\>" . font-lock-keyword-face)
                          ("\\<\\(open\\)\\>" . font-lock-keyword-face)
                          ("\\<\\(opendir\\)\\>" . font-lock-keyword-face)
                          ("\\<\\(closedir\\)\\>" . font-lock-keyword-face)
                          ("\\<\\(fstat\\)\\>" . font-lock-keyword-face)
                          ("\\<\\(fopen\\)\\>" . font-lock-keyword-face)
                          ("\\<\\(freopen\\)\\>" . font-lock-keyword-face)
                          ("\\<\\(fclose\\)\\>" . font-lock-keyword-face)
                          ("\\<\\(fflush\\)\\>" . font-lock-keyword-face)
                          ("\\<\\(fpurge\\)\\>" . font-lock-keyword-face)
                          ("\\<\\(feof\\)\\>" . font-lock-keyword-face)
                          ("\\<\\(dlopen\\)\\>" . font-lock-keyword-face)
                          ("\\<\\(dlsym\\)\\>" . font-lock-keyword-face)
                          ("\\<\\(dlerror()\\)\\>" . font-lock-keyword-face)
                          ("\\<\\(sleep;?\\)\\>" . font-lock-keyword-face)
                          ("\\<\\(setenv;?\\)\\>" . font-lock-keyword-face)
                          ("\\<\\(pthread_[-.a-z0-9_]*;?\\)\\>" . font-lock-keyword-face)
                          ("\\<\\(sem_[-.a-z0-9_]*;?\\)\\>" . font-lock-keyword-face)
                          ;; Colorisation : Glib Gdk Gtk+ GNOME
                          ("\\<\\(g_[-.a-z0-9_]*;?\\)\\>" . font-lock-keyword-face)
                          ("\\<\\(gdk_[-.a-z0-9_]*;?\\)\\>" . font-lock-constant-face)
                          ("\\<\\(gtk_[-.a-z0-9_]*;?\\)\\>" . font-lock-constant-face)
                          ("\\<\\(gnome_[-.a-z0-9_]*;?\\)\\>" . font-lock-constant-face)
                          ("\\<\\(poptGetArgs\\)\\>" . font-lock-constant-face)
                          ;; Colorisation : C : Commentaires
                          ("\\<\\(FIXME\\):" 1 font-lock-warning-face prepend)
                          ("\\<\\(TODO\\):" 1 font-lock-warning-face prepend)
                          ("\\<\\(td_[-.a-z0-9_]*;?\\)\\>" . font-lock-builtin-face)
                          ("\\<\\(WARNING\\)" 1 font-lock-warning-face t)
                          ("\\<\\(NOTE\\)" 1 font-lock-warning-face t)
                          ("\\<\\(NOTES\\)" 1 font-lock-warning-face t)
                          ("\\<\\(DEBUG\\)" 1 font-lock-warning-face t)
                          ("\\<\\(OUTPUT\\)" 1 font-lock-warning-face t)
                          ("\\<\\(IMPORTANT\\)" 1 font-lock-warning-face t)))

(eval-after-load "cc-mode"
  '(dolist (type (list "UCHAR" "USHORT" "ULONG" "BOOL" "BOOLEAN" "LPCTSTR" "C[A-Z]\\sw+" "\\sw+_t"))
    (add-to-list 'c-font-lock-extra-types type)))

;; indent the entire buffer
(defun indent-entire-c-buffer ()
  "Indent entire buffer of C source code."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (< (point) (point-max))
      (c-indent-command)
      (end-of-line)
      (forward-char 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; "funky stuff"

;; The function above is funky but useful. Having swapped the pairs ('[', '{'), ('-', '_') and (']', '}'), in order to type
;; "->", we need to type four characters ('Shift' followed by '-' followed by 'Shift' followed by '>'). With the above
;; code, all you need to type is two underscores: '__'). Automagically, they are converted into '->'). Similarly, two
;; successive dots '..' are translated into '[]' (for array indexing). I find that these combinations improve my
;; code-typing speed significantly.


(defun my-editing-function (first last len)
  (interactive)
  (if (and (boundp 'major-mode)
           (member major-mode (list 'c-mode 'c++-mode 'gud-mode))
           (= len 0)
           (> (point) 4)
           (= first (- (point) 1)))
      (cond
        ((and (string-equal (buffer-substring (point) (- (point) 2)) "__")
              (not (string-equal (buffer-substring (point) (- (point) 3)) "___")))
         (progn (delete-backward-char 2) (insert-char ?- 1) (insert-char ?> 1)))

        ((string-equal (buffer-substring (point) (- (point) 3)) "->_")
         (progn (delete-backward-char 3) (insert-char ?_ 3)))

        ((and (string-equal (buffer-substring (point) (- (point) 2)) "..")
              (not (string-equal (buffer-substring (point) (- (point) 3)) "...")))
         (progn (delete-backward-char 2) (insert-char ?[ 1) (insert-char ?] 1) (backward-char 1)))

        ((and (> (point-max) (point))
              (string-equal (buffer-substring (+ (point) 1) (- (point) 2)) "[.]"))
         (progn (forward-char 1) (delete-backward-char 3) (insert-char ?. 1) (insert-char ?. 1) )))
      nil))

(add-hook 'after-change-functions 'my-editing-function)


;;----------------------------------------------------------------------------------------------------
;; jump out from a pair(like quote, parenthesis, etc.)
(defun tsp-c-escape-pair ()
  (interactive)
  (let ((pair-regexp "[^])}\"'>]*[])}\"'>]"))
    (if (looking-at pair-regexp)
        (progn
          ;; be sure we can use C-u C-@ to jump back
          ;; if we goto the wrong place
          (push-mark)
          (goto-char (match-end 0)))
        (c-indent-command))))


 (defun do-cdecl ()
   "Compose C and C++ type declarations"
    (interactive)
    (shell-command
     (concat "cdecl explain \"" (buffer-substring (region-beginning)
                                                  (region-end)) "\"")))

