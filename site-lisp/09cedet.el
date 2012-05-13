;;; 09cedet.el ---

;; Copyright (C) 2012  Shihpin Tseng

;;; CEDET

;; using cedet from bzr
;; bzr revert
;; bzr clean-tree
;; bzr clean-tree --ignore
;; touch `find . -name Makefile`
;; make EMACS=/Applications/Emacs.app/Contents/MacOS/Emacs

;; Load CEDET.
;; See cedet/common/cedet.info for configuration details.
;; IMPORTANT: For Emacs >= 23.2, you must place this *before* any
;; CEDET component (including EIEIO) gets activated by another
;; package (Gnus, auth-source, ...).

;; note: don't add cedet directory recursive, it'll cause strange problem

(load-file "~/.emacs.d/lisp/cedet/cedet-devel-load.el")
(add-to-list 'load-path "~/.emacs.d/lisp/cedet/contrib/")

;; Add further minor-modes to be enabled by semantic-mode.
;; See doc-string of `semantic-default-submodes' for other things
;; you can use here.
(add-to-list 'semantic-default-submodes 'global-semantic-idle-scheduler-mode t)
(add-to-list 'semantic-default-submodes 'global-semanticdb-minor-mode t)
(add-to-list 'semantic-default-submodes 'global-semantic-idle-summary-mode t)
(add-to-list 'semantic-default-submodes 'global-semantic-idle-completions-mode t)
(add-to-list 'semantic-default-submodes 'global-cedet-m3-minor-mode t)
(add-to-list 'semantic-default-submodes 'global-semantic-mru-bookmark-mode t)
;; (add-to-list 'semantic-default-submodes 'global-semantic-decoration-mode t) ; Additional tag decorations.
;; (add-to-list 'semantic-default-submodes 'global-semantic-highlight-func-mode t) ; Highlight the current tag.
;; Highlight references of the symbol under point.
;; (add-to-list 'semantic-default-submodes 'global-semantic-idle-local-symbol-highlight-mode t)
(add-to-list 'semantic-default-submodes 'global-semantic-show-parser-state-mode t)
;; Show current fun in header line
;; (add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode t)
;; (add-to-list 'semantic-default-submodes 'global-semantic-show-unmatched-syntax-mode t)

;; Enable Semantic
(semantic-mode 1)

;; (require 'semantic/bovine/c)
;; (require 'semantic/bovine/gcc)
;; (require 'semantic/bovine/clang)
;; (require 'semantic/ia)
;; (require 'semantic/decorate/include)
;; (require 'semantic/lex-spp)
(require 'eassist) ; for eassist-lists-methods, and eassist-switch-h-cpp

;;; global support
(semanticdb-enable-gnu-global-databases 'c-mode)
(semanticdb-enable-gnu-global-databases 'c++-mode)

;;; Enable SRecode (Template management) minor-mode.
(global-srecode-minor-mode 1)

;; (remove-hook 'scheme-mode-hook 'semantic-default-scheme-setup)

;;; customisation of modes
(defun pl/cedet-hook ()
  (local-set-key "\C-x,." 'senator-complete-symbol)
  (local-set-key "\C-x,?" 'semantic-ia-complete-symbol) ; M-TAB
  (local-set-key "\C-x,>" 'semantic-complete-analyze-inline)
  (local-set-key "\C-x,=" 'semantic-decoration-include-visit)
  (local-set-key "\C-x,j" 'semantic-ia-fast-jump)
  (local-set-key "\C-x,q" 'semantic-ia-show-doc) ; C-x,D
  (local-set-key "\C-x,s" 'semantic-ia-show-summary)
  (local-set-key "\C-x,p" 'semantic-analyze-proto-impl-toggle)
  (local-set-key "\C-x,F" 'semantic-tag-folding-fold-block)
  (local-set-key "\C-x,S" 'semantic-tag-folding-show-block)
  (add-to-list 'ac-sources 'ac-source-semantic))

;; (add-hook 'semantic-init-hooks 'pl/cedet-hook)
(add-hook 'c-mode-common-hook 'pl/cedet-hook)
(add-hook 'lisp-mode-hook 'pl/cedet-hook)
(add-hook 'scheme-mode-hook 'pl/cedet-hook)
(add-hook 'emacs-lisp-mode-hook 'pl/cedet-hook)
(add-hook 'erlang-mode-hook 'pl/cedet-hook)



(defun pl/c-mode-cedet-hook ()
  (local-set-key "." 'semantic-complete-self-insert)
  (local-set-key ">" 'semantic-complete-self-insert)
  (local-set-key "\C-x,t" 'eassist-switch-h-cpp) ; use ff-find-other-file instead
  ;; Switch header and body file according to `eassist-header-switches' var.
  (local-set-key "\C-x,m" 'eassist-list-methods))
(add-hook 'c-mode-common-hook 'pl/c-mode-cedet-hook)


;;; ctags
(when (cedet-ectag-version-check t)
  ;; (semantic-load-enable-all-ectags-support)
  (semantic-load-enable-primary-ectags-support))

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

;; (setq semantic-imenu-auto-rebuild-directory-indexes nil)
;; (global-set-key [(s-down-mouse-3)] 'senator-completion-menu-popup)


;;; cscope
;; (require 'semanticdb-cscope)
;; (add-to-list 'semanticdb-project-roots "~/proj")


;;; EDE (Project Management)
(global-ede-mode 1)
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

;;; System header files
;; To normal work with system-wide libraries, Semantic should has access to system include files,
;; that contain information about functions & data types, implemented by these libraries.

;; If you use GCC for programming in C & C++, then Semantic can automatically find path, where
;; system include files are located. To do this, you need to load semantic-gcc package with
;; following command:
;; (when (string= emacs-version "23.1.1")
;; (require 'semantic-gcc))
;; You can also explicitly specify additional paths for look up of include files (and these paths also could vary for specific modes).
;; (semantic-add-system-include "~/exp/include/boost_1_37" 'c++-mode)
;; (semantic-add-system-include "/usr/include/gtk-2.0/" 'c-mode)
;; (semantic-add-system-include "/usr/include/glib-2.0/" 'c-mode)

(defconst cedet-user-include-dirs
  (list ".." "../include" "../inc" "../common" "../public"
        "../.." "../../include" "../../inc" "../../common" "../../public"
        ))

(defconst cedet-cocos2d-include-dir
  (list "../libs"
        "../libs/cocos2dx/include"
        "../libs/cocos2dx/platform"
        "../cocos2dx/include"
        "../cocos2dx/platform"
        "../../Classes"
        "../Box2D"
        "/Applications/Xcode.app/Contents/Developer\
/Platforms/iPhoneOS.platform/Developer/SDKs/iPhoneOS5.1.sdk/usr/include/c++/4.2.1"))

(defconst cedet-win32-include-dirs
  (list "C:/MinGW/include"
        "C:/MinGW/include/c++/3.4.5"
        "C:/MinGW/include/c++/3.4.5/mingw32"
        "C:/MinGW/include/c++/3.4.5/backward"
        "C:/MinGW/lib/gcc/mingw32/3.4.5/include"
        "C:/Program Files/Microsoft Visual Studio/VC98/MFC/Include"))

(let ((include-dirs cedet-user-include-dirs))
  (setq include-dirs (append include-dirs cedet-cocos2d-include-dir))
  (when (eq system-type 'windows-nt)
    (setq include-dirs (append include-dirs
                               cedet-win32-include-dirs)))

  (mapc (lambda (dir)
          (semantic-add-system-include dir 'c++-mode)
          (semantic-add-system-include dir 'c-mode))
        include-dirs))



;;; smart complitions
;; (setq-mode-local c-mode semanticdb-find-default-throttle
;;                  '(project unloaded system recursive))
;; (setq-mode-local c++-mode semanticdb-find-default-throttle
;;                  '(project unloaded system recursive))
;; (setq-mode-local erlang-mode semanticdb-find-default-throttle
;;                  '(project unloaded system recursive))



;;; face
(eval-after-load "pulse"
  '(progn
    (set-face-attribute 'pulse-highlight-start-face nil :background "#222222")))


(eval-after-load 'semantic/decorate/mode
  '(progn
     (set-face-attribute 'semantic-tag-boundary-face nil :overline "#227777")
     (set-face-attribute 'semantic-decoration-on-private-members-face nil :background "#333333")))

(eval-after-load 'semantic/decorate/include
  '(progn
     (set-face-attribute 'semantic-decoration-on-unparsed-includes nil :background "#555555")))

(defun cedet-settings-4-info ()
  "`cedet' settings for `info'."
  (info-initialize)
  (dolist (package `("cogre" "common" "ede" "eieio" "semantic/doc" "speedbar" "srecode"))
    (add-to-list 'Info-default-directory-list (concat "~/.emacs.d/lisp/cedet/" package "/"))))

(eval-after-load "info"
  `(cedet-settings-4-info))


(provide '09cedet)

;; Local Variables:
;; outline-regexp: ";;; *"
;; End:
