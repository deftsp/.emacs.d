;;; 40cedet.el ---

;; Copyright (C) 2008  S.P.Tseng

;;; CEDET
;; (require 'cedet)
;; (load-file "~/.emacs.d/packages/cedet/common/cedet.el")
;; (require 'eassist)                      ; cvs
;; eassist的两个功能
;; 1. 列出当前buffer中的各个方法的列表，包括参数列表和返回类型。
;; 2. 在buffer中，切换同一个目录下的头文件和源文件

;;; semantic
;; `semantic-idle-scheduler-mode' - Keeps a buffer's parse tree up to date.
;; `semanticdb-minor-mode' - Stores tags when a buffer is not in memory.
;; `semanticdb-load-ebrowse-caches' - Loads any ebrowse dbs created earlier.
;; (semantic-load-enable-minimum-features)

;; Enable some semantic features that provide basic coding assistance.
;; This includes `semantic-load-enable-minimum-features' plus:
;; `imenu' - Lists Semantic generated tags in the menubar.
;; `semantic-idle-summary-mode' - Show a summary for the tag indicated by code under point.  (intellisense)
;; `senator-minor-mode' - Semantic Navigator, and global menu for all features Semantic.
;; `semantic-mru-bookmark-mode' - Provides a `switch-to-buffer' like keybinding for tag names.
(semantic-load-enable-code-helpers)
;; (semantic-load-enable-guady-code-helpers)

;; * This enables the use of Exuberent ctags if you have it installed.
;; (semantic-load-enable-all-exuberent-ctags-support)

;; Enable SRecode (Template management) minor-mode.
;; (global-srecode-minor-mode 1)


;;; Customization of Semanticdb
(setq semanticdb-default-save-directory (expand-file-name "~/.semanticdb")
      ;; semanticdb-persistent-path (quote (never))
      semanticdb-search-system-databases t)

;; if you want to enable support for gnu global
(require 'semanticdb-system)
;; (require 'semanticdb-global)
;; (semanticdb-enable-gnu-global-databases 'c-mode)
;; (semanticdb-enable-gnu-global-databases 'c++-mode)
(require 'semanticdb-cscope)
;; enable ctags for some languages:
;;  Unix Shell, Perl, Pascal, Tcl, Fortran, Asm
;; (semantic-load-enable-primary-exuberent-ctags-support)


;; (global-semanticdb-minor-mode 1)
;; (add-to-list 'semanticdb-project-roots "~/proj")



;; (global-semantic-show-unmatched-syntax-mode -1)
;; (setq semantic-idle-scheduler-working-in-modeline-flag nil)

(require 'semantic-ia nil t)
;; (require 'semantic-complete nil t)        ; provides semantic-complete-jump, semantic-complete-jump-local
(global-set-key (kbd "C-x , j") 'semantic-ia-fast-jump)
(global-set-key (kbd "C-x , d") 'my-find-definition)
(global-set-key (kbd "C-x , g") 'my-jump-to-function)
;; 直接解析当前光标下的符号（函数或者变量），跳转到相关的定义。这个功能和senator-jump很相像，只是后者还需要回车确认一下。
(defun my-find-definition (arg)
  "Jump to the definition of the symbol, type or function at point.
  With prefix arg, find in other window."
  (interactive "P")
  (let* ((tag (or (semantic-idle-summary-current-symbol-info-context)
                  (semantic-idle-summary-current-symbol-info-brutish)
                  (error "No known tag at point")))
         (pos (or (semantic-tag-start tag)
                  (error "Tag definition not found")))
         (file (semantic-tag-file-name tag)))
    (if file
        (if arg (find-file-other-window file) (find-file file))
        (if arg (switch-to-buffer-other-window (current-buffer))))
    (push-mark)
    (goto-char pos)
    (end-of-line)))

;; (eval-after-load "semantic-c"
;;   '(dolist (d (list "~/.emacs.d/msys/mingw/include"
;;                "~/.emacs.d/msys/mingw/lib/gcc/mingw32/4.3.0/include"
;;                "~/.emacs.d/msys/mingw/lib/gcc/mingw32/4.3.0/include-fixed"
;;                "~/.emacs.d/msys/mingw/lib/gcc/mingw32/4.3.0/include/c++"
;;                "~/.emacs.d/msys/mingw/lib/gcc/mingw32/4.3.0/include/c++/backward"
;;                "~/.emacs.d/msys/mingw/lib/gcc/mingw32/4.3.0/include/c++/mingw32"))
;;     (semantic-add-system-include d)))

(eval-after-load "semantic-complete"
  '(setq semantic-complete-inline-analyzer-displayor-class
    semantic-displayor-ghost))

;; (global-semantic-idle-completions-mode 1)
;;----------------------------------------------------------------------------------------------------
;; Speedbar

(require 'sr-speedbar)
(global-set-key (kbd "s-\\") 'sr-speedbar-toggle)

(require 'speedbar)
;; (require 'sb-info "sb-info" t) ;speedbar support for info files
;; (require 'sb-html nil t)
;; (require 'sb-rmail "sb-rmail" t)
;; (setq dframe-update-speed 30)

;;; Key binding

(define-key speedbar-key-map "t" 'speedbar-toggle-updates)
(define-key speedbar-key-map "g" 'speedbar-refresh)
(define-key speedbar-key-map "q" 'bury-buffer)
(define-key speedbar-key-map "Q" '(lambda ()
                                   (interactive)
                                   (delete-window (selected-window))
                                   (kill-buffer speedbar-buffer)))

(define-key speedbar-key-map "n" 'speedbar-next)
(define-key speedbar-key-map "p" 'speedbar-prev)
(define-key speedbar-key-map "\M-n" 'speedbar-restricted-next)
(define-key speedbar-key-map "\M-p" 'speedbar-restricted-prev)
(define-key speedbar-key-map "\C-\M-n" 'speedbar-forward-list)
(define-key speedbar-key-map "\C-\M-p" 'speedbar-backward-list)
;; These commands never seemed useful.
;;  (define-key speedbar-key-map " " 'speedbar-scroll-up)
;;  (define-key speedbar-key-map [delete] 'speedbar-scroll-down)

;; Short cuts I happen to find useful
(define-key speedbar-key-map "r" (lambda () (interactive)
                                         (speedbar-change-initial-expansion-list
                                          speedbar-previously-used-expansion-list-name)))
(define-key speedbar-key-map "b" (lambda () (interactive)
                                         (speedbar-change-initial-expansion-list "quick buffers")))

(define-key speedbar-key-map "f" (lambda () (interactive)
                                         (speedbar-change-initial-expansion-list "files")))

(define-key speedbar-key-map "-" (lambda () (interactive)
                                         (speedbar-change-initial-expansion-list "Project")))


;; Basic tree features
(define-key speedbar-file-key-map "e" 'speedbar-edit-line)
(define-key speedbar-file-key-map "\C-m" 'speedbar-edit-line)
(define-key speedbar-file-key-map "+" 'speedbar-expand-line)
(define-key speedbar-file-key-map "=" 'speedbar-expand-line)
(define-key speedbar-file-key-map "-" 'speedbar-contract-line)

(define-key speedbar-file-key-map "[" 'speedbar-expand-line-descendants)
(define-key speedbar-file-key-map "]" 'speedbar-contract-line-descendants)

(define-key speedbar-file-key-map " " 'speedbar-toggle-line-expansion)

;; file based commands
(define-key speedbar-file-key-map "U" 'speedbar-up-directory)
(define-key speedbar-file-key-map "^" 'speedbar-up-directory)
(define-key speedbar-file-key-map "I" 'speedbar-item-info)
(define-key speedbar-file-key-map "B" 'speedbar-item-byte-compile)
(define-key speedbar-file-key-map "L" 'speedbar-item-load)
(define-key speedbar-file-key-map "C" 'speedbar-item-copy)
(define-key speedbar-file-key-map "D" 'speedbar-item-delete)
(define-key speedbar-file-key-map "O" 'speedbar-item-object-delete)
(define-key speedbar-file-key-map "R" 'speedbar-item-rename)
(define-key speedbar-file-key-map "M" 'speedbar-create-directory)

;; (modify-frame-parameters speedbar-frame
;;                         '((top . 3)
;;                           (left . 128)
;;                           (height . 51)
;;                           (width . 25)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ede customization
;; Enable EDE (Project Management) features
(eval-after-load "ede"
  '(global-ede-mode t))
;; Enable EDE for a pre-existing C++ project
;; (ede-cpp-root-project "NAME" :file "~/myproject/Makefile")
;; (require 'semantic-lex-spp)

;; cpp-tests project definition
;; (ede-cpp-root-project "cpp-tests" :file "~/projects/lang-exp/cpp/CMakeLists.txt"
;;                       :system-include-path '("/home/ott/exp/include"
;;                                              "/home/ott/exp/include/boost-1_37"))
;; (ede-cpp-root-project "text-categorization" :file "~/projects/text-categorization/CMakeLists.txt"
;;                       :system-include-path '("/home/ott/exp/include"
;;                                              "/home/ott/exp/include/boost-1_37"))
;; (ede-cpp-root-project "wfilter" :file "~/projects/wfilter/README"
;;                       :system-include-path '("/home/ott/exp/include"
;;                                              "/home/ott/exp/include/boost-1_37"))



;; (let ((vmap (make-sparse-keymap)))
;;   (define-key vmap "p" 'ede-speedbar)
;;   (define-key vmap "l" 'ede-load-project-file)
;;   ;; bind our submap into speedbar-key-map
;;   (define-key speedbar-key-map "v" vmap))

;; (setq senator-minor-mode-name "SN")
;; (setq semantic-imenu-auto-rebuild-directory-indexes nil)
;; (global-srecode-minor-mode 1)
;; (global-semantic-mru-bookmark-mode 1)

;; (global-set-key [(s-down-mouse-3)] 'senator-completion-menu-popup)

;;; System header files
;; To normal work with system-wide libraries, Semantic should has access to system include files,
;; that contain information about functions & data types, implemented by these libraries.

;; If you use GCC for programming in C & C++, then Semantic can automatically find path, where
;; system include files are located. To do this, you need to load semantic-gcc package with
;; following command:
(require 'semantic-gcc)
;; You can also explicitly specify additional paths for look up of include files (and these paths also could vary for specific modes).
;; (semantic-add-system-include "~/exp/include/boost_1_37" 'c++-mode)
;; (semantic-add-system-include "/usr/include/gtk-2.0/" 'c-mode)
;; (semantic-add-system-include "/usr/include/glib-2.0/" 'c-mode)

;;; smart complitions
;; (require 'semantic-ia)
;; (setq-mode-local c-mode semanticdb-find-default-throttle
;;                  '(project unloaded system recursive))
;; (setq-mode-local c++-mode semanticdb-find-default-throttle
;;                  '(project unloaded system recursive))
;; (setq-mode-local erlang-mode semanticdb-find-default-throttle
;;                  '(project unloaded system recursive))


;; (defun my-cedet-hook ()
;; ; (local-set-key "\C-c/" 'semantic-ia-complete-symbol)
;;   (local-set-key [(control return)] 'semantic-ia-complete-symbol)
;;   (local-set-key "\C-c/" 'semantic-ia-complete-symbol-menu)
;;   (local-set-key "\C-c." 'senator-complete-symbol)
;;   ;;
;;   (local-set-key "\C-c>" 'semantic-complete-analyze-inline)
;;   (local-set-key "\C-c=" 'semantic-decoration-include-visit)

;;   (local-set-key "\C-cj" 'semantic-ia-fast-jump)
;;   (local-set-key "\C-cp" 'semantic-analyze-proto-impl-toggle))
;; (add-hook 'c-mode-common-hook 'my-cedet-hook)
;; (add-hook 'lisp-mode-hook 'my-cedet-hook)

;; (defun my-c-mode-cedet-hook ()
;; ; (local-set-key "." 'semantic-complete-self-insert)
;; ; (local-set-key ">" 'semantic-complete-self-insert)
;;   (local-set-key "\C-ct" 'eassist-switch-h-cpp)
;;   (local-set-key "\C-xt" 'eassist-switch-h-cpp))
;; (add-hook 'c-mode-common-hook 'my-c-mode-cedet-hook)

;; reference: http://xtalk.msk.su/~ott/en/writings/emacs-devenv/EmacsCedet.html

(defun my-semanticdb-minor-mode-p ()
  "Query if the current buffer has Semanticdb mode enabled."
  (condition-case blah
      (and (semanticdb-minor-mode-p)
           (eq imenu-create-index-function
               'semantic-create-imenu-index))
    (error nil)))

(defun my-icompleting-read (prompt choices)
  (flet ((ido-make-buffer-list (default)
           (setq ido-temp-list choices)))
    (ido-read-buffer prompt)))

(defun my-jump-to-function ()
  "Jump to a function found by either Semantic or Imenu within the
    current buffer."
  (interactive)
  (cond
    ((my-semanticdb-minor-mode-p) (my-semantic-jump-to-function))
    ((boundp 'imenu-create-index-function) (my-imenu-jump-to-function))))

(defun my-imenu-jump-to-function ()
  "Jump to a function found by Semantic within the current buffer
    with ido-style completion."
  (interactive)
  (save-excursion
    (setq imenu--index-alist (funcall imenu-create-index-function)))
  (let ((thing (assoc
                (my-icompleting-read "Go to: "
                                     (mapcar #'car imenu--index-alist))
                imenu--index-alist)))
    (when thing
      (funcall imenu-default-goto-function (car thing) (cdr thing))
      (recenter))))

(defun my-semantic-jump-to-function ()
  "Jump to a function found by Semantic within the current buffer
    with ido-style completion."
  (interactive)
  (let ((tags
         (remove-if
          (lambda (x)
            (or (getf (semantic-tag-attributes x) :prototype-flag)
                (not (member (cadr x) '(function variable type)))))
          (semanticdb-file-stream (buffer-file-name (current-buffer)))))
        (names (make-hash-table :test 'equal)))
    (dolist (tag tags)
      (let ((sn (semantic-tag-name tag)))
        (when (gethash sn names)
          (setq sn
                (loop for i = 1 then (1+ i)
                   for name = (format "%s<%d>" sn i)
                   while (gethash name names)
                   finally return name)))
        (puthash sn tag names)))
    (goto-char (semantic-tag-start
                (gethash
                 (my-icompleting-read "Go to: " (hash-table-keys names))
                 names)))
    (recenter)))

;; Great, exactly what I was looking for :) Unfortunately this results in “Symbol’s function definition is void: hash-table-keys” on Aquamacs 0.99d - any thoughts? JanR?

;; Oops! This is one of mine:

(defun hash-table-keys (hash)
  (let ((ret nil))
    (maphash (lambda (k v) (push k ret)) hash)
    ret))






;; Local Variables:
;; outline-regexp: ";;; *"
;; End:
