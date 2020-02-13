;;; 11cedet.el ---

;; Copyright (C) 2015  Shihpin Tseng

;;; CEDET

;; using cedet from bzr
;; bzr revert
;; bzr clean-tree
;; bzr clean-tree --ignore
;; make touch-makefiles
;; make EMACS=/Applications/Emacs.app/Contents/MacOS/Emacs
;; cd contrib
;; make EMACS=/Applications/Emacs.app/Contents/MacOS/Emacs

;; Load CEDET.
;; See cedet/common/cedet.info for configuration details.
;; IMPORTANT: For Emacs >= 23.2, you must place this *before* any
;; CEDET component (including EIEIO) gets activated by another
;; package (Gnus, auth-source, ...).

;;; install develop version cedet
;; install with el-get now
;; Note: don't add cedet directory recursive, it'll cause strange problem
;; (defvar tl/cedet-root-path (file-name-as-directory "~/.emacs.d/site-lisp/cedet/"))
;; (load-file (concat tl/cedet-root-path "cedet-devel-load.el"))
;; (load-file (concat tl/cedet-root-path "contrib/cedet-contrib-load.el"))

(use-package semantic
  :defer t
  :init
  (progn
    ;; Add further minor-modes to be enabled by semantic-mode. See doc-string of `semantic-default-submodes' for other
    ;; things you can use here.
    (add-to-list 'semantic-default-submodes 'global-semantic-idle-scheduler-mode t)
    (add-to-list 'semantic-default-submodes 'global-semanticdb-minor-mode t)
    (add-to-list 'semantic-default-submodes 'global-semantic-idle-summary-mode t)
    ;; (add-to-list 'semantic-default-submodes 'global-semantic-idle-completions-mode t) ; use auto completion instead
    (when (fboundp 'global-cedet-m3-minor-mode)
      (add-to-list 'semantic-default-submodes 'global-cedet-m3-minor-mode t))
    (add-to-list 'semantic-default-submodes 'global-semantic-mru-bookmark-mode t)
    ;; (add-to-list 'semantic-default-submodes 'global-semantic-decoration-mode t) ; Additional tag decorations.
    ;; (add-to-list 'semantic-default-submodes 'global-semantic-highlight-func-mode t) ; Highlight the current tag.
    ;; (add-to-list 'semantic-default-submodes 'global-semantic-idle-local-symbol-highlight-mode t) ; Highlight references of the symbol under point.

    ;; (add-to-list 'semantic-default-submodes 'global-semantic-show-unmatched-syntax-mode t)
    (add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode t) ; show current fun in header line
    (add-to-list 'semantic-default-submodes 'global-semantic-show-parser-state-mode t))
  :config
  (define-key semantic-mode-map [menu-bar] nil))


(defun tl/lazy-load-stickyfunc-enhance ()
  "Lazy load the package."
  (require 'stickyfunc-enhance))

;;; inhibit semantic for some major modes
(defun tl/semantic-inhibit-func ()
  (cond
   ((member major-mode '(scheme-mode
                         javascript-mode
                         html-helper-mode
                         html-mode
                         org-mode))
    ;; to disable semantic, return non-nil.
    t)
   (t nil)))

(when (boundp 'semantic-inhibit-functions)
  (add-to-list 'semantic-inhibit-functions 'tl/semantic-inhibit-func))

;; (require 'semantic/ia)
;; (require 'semantic/bovine/c)
;; (require 'semantic/bovine/gcc)
;; (require 'semantic/bovine/clang nil t)
;; (require 'semantic/decorate/include)
;; (require 'semantic/lex-spp)
;; (require 'eassist nil t) ; for eassist-lists-methods, and eassist-switch-h-cpp


;;; Enable SRecode (Template management) minor-mode.
;; (global-srecode-minor-mode 1)

;; (remove-hook 'scheme-mode-hook 'semantic-default-scheme-setup)

;;; customisation of modes
(defun tl/cedet-hook ()
  ;; (semantic-mode +1)
  (local-set-key "\C-x,." 'senator-complete-symbol)
  (local-set-key "\C-x,?" 'semantic-ia-complete-symbol) ; M-TAB
  (local-set-key "\C-x,>" 'semantic-complete-analyze-inline)
  (local-set-key "\C-x,=" 'semantic-decoration-include-visit)
  (local-set-key "\C-x,d" 'tl/semantic-find-definition)
  (local-set-key "\C-x,j" 'semantic-ia-fast-jump)
  (local-set-key "\C-x,q" 'semantic-ia-show-doc) ; C-x,D
  (local-set-key "\C-x,s" 'semantic-ia-show-summary)
  (local-set-key "\C-x,p" 'semantic-analyze-proto-impl-toggle)
  (local-set-key "\C-x,F" 'semantic-tag-folding-fold-block)
  (local-set-key "\C-x,S" 'semantic-tag-folding-show-block))

;; (add-hook 'semantic-init-hooks 'tl/cedet-hook)
(add-hook 'c-mode-common-hook 'tl/cedet-hook)
;; (add-hook 'lisp-mode-hook 'tl/cedet-hook)
;; (add-hook 'scheme-mode-hook 'tl/cedet-hook)
(add-hook 'emacs-lisp-mode-hook 'tl/cedet-hook)
(add-hook 'erlang-mode-hook 'tl/cedet-hook)



(defun tl/c-mode-cedet-hook ()
  (local-set-key "." 'semantic-complete-self-insert)
  (local-set-key ">" 'semantic-complete-self-insert)
  (local-set-key "\C-x,t" 'eassist-switch-h-cpp) ; use ff-find-other-file instead
  ;; Switch header and body file according to `eassist-header-switches' var.
  (local-set-key "\C-x,m" 'eassist-list-methods))
(add-hook 'c-mode-common-hook 'tl/c-mode-cedet-hook)

;;; global support
;;; brew install global
;; (when (cedet-gnu-global-version-check t)
;;   (semanticdb-enable-gnu-global-databases 'c-mode)
;;   (semanticdb-enable-gnu-global-databases 'c++-mode))

;;; ctags
;; enable ctags for some languages:
;; Unix Shell, Perl, Pascal, Tcl, Fortran, Asm
;; brew install ctags
;; (semantic-load-enable-all-ectags-support)
;; (semantic-load-enable-primary-ectags-support)

;;; cscope
;; (require 'semantic/db-cscope)
;; (semanticdb-enable-cscope-databases t)

;; cscope
;; 使用 cscope 浏览源代码,这个 xcscope 是个改进版，为每一次查找的结果使用不同 buffer，
;; 这样就可以保存以前的结果。

;; (when (eq system-type 'gnu/linux)
;;   (require 'xcscope)
;;   (defadvice cscope-bury-buffer (after bury-buffer activate)
;;     "bury the current buffer and remove it from the selected window
;; if it is displayed there."
;;     (delete-window (get-buffer-window (get-buffer cscope-output-buffer-name)))))


;; C-c s a             设定初始化的目录，一般是你代码的根目录
;; C-s s I             对目录中的相关文件建立列表并进行索引
;; C-c s s             序找符号
;; C-c s g             寻找全局的定义
;; C-c s c             看看指定函数被哪些函数所调用
;; C-c s C             看看指定函数调用了哪些函数
;; C-c s e             寻找正则表达式
;; C-c s f             寻找文件
;; C-c s i             看看指定的文件被哪些文件 include


;;; complete
(setq semantic-complete-inline-analyzer-displayor-class
      'semantic-displayor-traditional-with-focus-highlight) ; semantic-displayor-ghost

;;; List of active decoration styles.
;; (setq semantic-decoration-styles '(("semantic-decoration-on-includes" . t)
;;                                    ("semantic-decoration-on-protected-members" . t)
;;                                    ("semantic-decoration-on-private-members" . t)
;;                                    ("semantic-tag-boundary" . t)))

;;; tag format
(setq semantic-idle-breadcrumbs-format-tag-function ; default semantic-format-tag-abbreviate
      'semantic-format-tag-prototype) ; semantic-format-tag-uml-prototype

(setq semantic-idle-work-parse-neighboring-files-flag t)
(setq semantic-idle-work-update-headers-flag t)

;;; senator
;; (add-hook 'semantic-init-hooks 'senator-minor-mode)
;; (remove-hook 'semantic-init-hooks 'senator-minor-mode)
;; (global-set-key [(s-down-mouse-3)] 'senator-completion-menu-popup)

;; (add-to-list 'semanticdb-project-roots "~/proj")


;;; EDE (Project Management)
;; FIXME: enable it will issue require(ede/project-am), if enable desktop-mode.
;; (global-ede-mode 1)

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

;;; System header files
;; To normal work with system-wide libraries, Semantic should has access to system include files,
;; that contain information about functions & data types, implemented by these libraries.

;; If you use GCC for programming in C & C++, then Semantic can automatically find path, where
;; system include files are located. To do this, you need to load semantic-gcc package with
;; following command:
;; (require 'semantic/bovine/gcc)

;; You can also explicitly specify additional paths which must be absolute path not relative path with
;; `semantic-add-system-include' for look up of include files (and these paths also could vary for
;; specific modes).
;;
;; (semantic-add-system-include "~/exp/include/boost_1_37" 'c++-mode)
;; (semantic-add-system-include "/usr/include/gtk-2.0/" 'c-mode)
;; (semantic-add-system-include "/usr/include/glib-2.0/" 'c-mode)

(when (eq system-type 'windows-nt)
  (defconst tl/cedet-win32-include-dirs
    (list "C:/MinGW/include"
          "C:/MinGW/include/c++/3.4.5"
          "C:/MinGW/include/c++/3.4.5/mingw32"
          "C:/MinGW/include/c++/3.4.5/backward"
          "C:/MinGW/lib/gcc/mingw32/3.4.5/include"
          "C:/Program Files/Microsoft Visual Studio/VC98/MFC/Include"))
  (mapc (lambda (dir)
          (semantic-add-system-include dir 'c++-mode)
          (semantic-add-system-include dir 'c-mode))
        tl/cedet-win32-include-dirs))

;;;
(defun tl/semantic-find-definition (arg)
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

;;; integration with imenu
;; (defun tl/imenu-add-to-menubar ()
;;   (imenu-add-to-menubar "TAGS"))
;; (add-hook 'semantic-init-hooks 'tl/imenu-add-to-menubar)
;; (setq semantic-imenu-auto-rebuild-directory-indexes nil)

;;; info
;; (defun cedet-settings-4-info ()
;;   "`cedet' settings for `info'."
;;   (info-initialize)
;;   (dolist (package `("cogre" "common" "ede" "eieio" "semantic/doc" "speedbar" "srecode"))
;;     (add-to-list 'Info-default-directory-list (concat "~/.emacs.d/site-lisp/cedet/" package "/"))))

;; (eval-after-load "info"
;;   `(cedet-settings-4-info))

(provide '11cedet)

;; Local Variables:
;; outline-regexp: ";;; *"
;; End:
