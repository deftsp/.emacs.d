;;; 51ivy.el ---                                     -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>

(defvar tl/ignore-buffer-or-file-regexp
  '("^\\ " "^\\*Completions*" "^\\*Article\\*" "^\\*Apropos*"
    "^\\*Ibuffer*" "^\\*Backtrace*"  "^\\*Help"  "^\\*Bookmark"
    "^\\*Messages" "^\\.newsrc-dribble"  "^\\*Woman-Log"
    "^\\*Compilation" "^\\*Compile-Log" "^\\*Calendar"
    "^\\*cscope"  "^\\*grep" "*BBDB*" "*Tree*"  "*Group*"
    "*Helm Swoop*"  "*EMMS Playlist*"  "^\\*Summary\\ n" "Map_Sym.txt"
    "^\\*w3m*" "^\\#" "^\\irc.*:" "localhost:6668" "^\\*TeX\\ Help\\*"
    "\\`auto/" "\\.prv/" "_region_" "^.DS_Store$" "\\.hi\\'"))

(defun tl/counsel-jump-in-buffer ()
  "Jump in buffer with `counsel-imenu' or `counsel-org-goto' if in org-mode"
  (interactive)
  (call-interactively
   (cond
    ((eq major-mode 'org-mode) 'counsel-org-goto)
    (t 'counsel-imenu))))


(use-package counsel
  :diminish counsel-mode
  :commands (counsel-describe-function counsel-describe-variable)
  :bind (("C-h f" . counsel-describe-function)
         ("C-h v" . counsel-describe-variable))
  :init
  (progn
    (setq counsel-outline-face-style 'verbatim)
    (global-set-key (kbd "s-g") 'counsel-rg)
    (tl/set-leader-keys
      dottl-emacs-command-key 'counsel-M-x ; 'execute-extended-command
      ;; files
      "ff"  'counsel-find-file
      "fel" 'counsel-find-library
      "fL"  'counsel-locate
      ;; help
      "?"   'counsel-descbinds
      "hdf" 'counsel-describe-function
      "hdF" 'counsel-describe-face
      "hdm" 'describe-mode
      "hdv" 'counsel-describe-variable
      "hi"  'counsel-info-lookup-symbol
      ;; "hR"  'tl/counsel-search-docs
      ;; insert
      "iu"  'counsel-unicode-char
      ;; jump

      "ji"  'tl/counsel-jump-in-buffer
      ;; register/ring
      "ry"  'counsel-yank-pop
      "rm"  'counsel-mark-ring
      ;; jumping
      "sj"  'tl/counsel-jump-in-buffer
      ;; themes
      "Ts"  'counsel-load-theme
      "sgp" 'counsel-git-grep))
  :config
  (progn
    (with-eval-after-load 'shell
      (define-key shell-mode-map (kbd "C-r") 'counsel-shell-history))
    (setq counsel-find-file-ignore-regexp
          (string-join tl/ignore-buffer-or-file-regexp "\\|"))
    (define-key counsel-find-file-map (kbd "C-h") 'counsel-up-directory)
    ;; remaps built-in commands that have a counsel replacement
    ;; do not use counsel-M-x for now
    ;; (define-key counsel-mode-map (vector 'remap 'execute-extended-command) nil)
    (counsel-mode +1)))

;; Support pinyin in Ivy
;; Input prefix ';' to match pinyin. For example: 你好 can be match by type ;nh.
;; Refer to  https://github.com/abo-abo/swiper/issues/919 and
;; https://github.com/pengpengxp/swiper/wiki/ivy-support-chinese-pinyin
;; https://github.com/seagle0128/.emacs.d/blob/master/lisp/init-ivy.el
(use-package pinyinlib
  :after ivy
  :commands pinyinlib-build-regexp-string
  :init
  (with-no-warnings
    (defun tl/ivy--regex-pinyin (str)
      "The regex builder wrapper to support pinyin."
      (or (tl/pinyin-to-utf8 str)
          (and (fboundp 'ivy-prescient-non-fuzzy)
               (ivy-prescient-non-fuzzy str))
          (ivy--regex-plus str)))

    ;; (mapcar
    ;;  (lambda (item)
    ;;    (let ((key (car item))
    ;;          (value (cdr item)))
    ;;      (when (member value '(ivy-prescient-non-fuzzy
    ;;                            ivy--regex-plus))
    ;;        (setf (alist-get key ivy-re-builders-alist)
    ;;              #'tl/ivy--regex-pinyin))))
    ;;  ivy-re-builders-alist)


    (defun tl/pinyinlib-build-regexp-string (str)
      "Build a pinyin regexp sequence from STR."
      (cond ((equal str ".*") ".*")
            (t (pinyinlib-build-regexp-string str t))))

    (defun tl/pinyin-regexp-helper (str)
      "Construct pinyin regexp for STR."
      (cond ((equal str " ") ".*")
            ((equal str "") nil)
            (t str)))

    (defun tl/pinyin-to-utf8 (str)
      "Convert STR to UTF-8."
      (cond ((equal 0 (length str)) nil)
            ((equal (substring str 0 1) ";")
             (mapconcat
              #'tl/pinyinlib-build-regexp-string
              (remove nil (mapcar
                           #'tl/pinyin-regexp-helper
                           (split-string
                            (replace-regexp-in-string ";" "" str )
                            "")))
              ""))
            (t nil)))))

(use-package ivy
  :defer t
  :diminish ivy-mode
  :chords (("jv" . hydra-view/body))
  :init
  (setq ivy-display-style 'fancy)
  ;; include recent(recentf) files and/or bookmarks, recentf-cleanup
  (setq ivy-use-virtual-buffers t)
  ;; Better fuzzy matching support: http://oremacs.com/2016/01/06/ivy-flx/
  ;; ivy--regex-plus use a .* regex wild card in place of each single space in the input.
  (setq ivy-re-builders-alist
        '((ivy-switch-buffer . ivy--regex-plus)
          (counsel-projectile-find-file . ivy--regex-plus)
          (counsel-rg . ivy--regex-ignore-order)
          (counsel-outline . tl/ivy--regex-pinyin)
          (counsel-org-goto . tl/ivy--regex-pinyin)
          (t . ivy--regex-fuzzy)))

  (setq ivy-initial-inputs-alist nil)

  (setq ivy-count-format "[%d/%d] ")
  (setq ivy-use-selectable-prompt t)
  (setq ivy-height 15)
  (setq confirm-nonexistent-file-or-buffer t)
  ;; https://oremacs.com/2016/06/27/ivy-push-view/
  ;; (global-set-key (kbd "s-v") 'ivy-push-view)
  ;; delete view, delete many views at once by pressing C-M-m[M-RET] (ivy-call)
  ;; (global-set-key (kbd "s-V") 'ivy-pop-view)

  ;; (with-eval-after-load 'desktop
  ;;   (add-to-list 'desktop-globals-to-save 'ivy-views))

  (defun tl/ivy-views-clean ()
    (interactive)
    (setq ivy-views nil))

  (tl/set-leader-keys
    "fr" 'counsel-recentf
    "rl" 'ivy-resume)
  :config
  (mapcar (lambda (str) (add-to-list 'ivy-ignore-buffers str)) tl/ignore-buffer-or-file-regexp)

  (define-key ivy-minibuffer-map (kbd "C-w") 'ivy-occur)
  (define-key ivy-minibuffer-map [escape] 'minibuffer-keyboard-quit)
  (define-key ivy-minibuffer-map (kbd "M-j") 'ivy-next-line)
  (define-key ivy-minibuffer-map (kbd "M-k") 'ivy-previous-line)
  (global-set-key (kbd "C-c C-r") 'ivy-resume)
  (global-set-key [f6] 'ivy-resume)

  (defun tl/ivy-switch-view ()
    (interactive)
    (let ((ivy-initial-inputs-alist
           '((ivy-switch-buffer . "{} "))))
      (ivy-switch-buffer)))

  (global-set-key (kbd "s-v") 'hydra-view/body)
  (defhydra hydra-view (:color blue :hint nil)
    "view control panel"
    ("v" ivy-push-view "save layout" )
    ;; ("V" ivy-pop-view "delete a layout")
    ("d" ivy-pop-view "delete a layout")
    ("l" tl/ivy-switch-view "select a layout")
    ("<escape>" nil nil)
    ("q" nil))


  (with-eval-after-load 'evil
    (evil-make-overriding-map ivy-occur-mode-map 'normal))
  (tl/set-leader-keys-for-major-mode 'ivy-occur-grep-mode
    "w" 'ivy-wgrep-change-to-wgrep-mode)

  (use-package ivy-hydra
    :config
    (define-key hydra-ivy/keymap [escape] 'hydra-ivy/keyboard-escape-quit-and-exit))

  (use-package ivy-rich
    :after (:all ivy counsel)
    :init
    (setq ivy-virtual-abbreviate 'full
          ivy-rich-path-style 'abbrev
          ivy-rich-parse-remote-file-path t)
    :config
    (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
    (setq ivy-rich-display-transformers-list
          '(ivy-switch-buffer
            (:columns
             ((ivy-switch-buffer-transformer (:width 30))
              (ivy-rich-switch-buffer-size (:width 7))
              (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
              (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
              (ivy-rich-switch-buffer-project (:width 15 :face success))
              (ivy-rich-switch-buffer-path (:width
                                            (lambda (x)
                                              (ivy-rich-switch-buffer-shorten-path
                                               x (ivy-rich-minibuffer-width 0.3))))))
             :predicate
             (lambda (cand) (get-buffer cand)))
            counsel-find-file
            (:columns
             ((ivy-read-file-transformer)
              (ivy-rich-counsel-find-file-truename (:face font-lock-doc-face))))
            counsel-M-x
            (:columns
             ((counsel-M-x-transformer (:width 40))
              (ivy-rich-counsel-function-docstring (:face font-lock-doc-face))))
            counsel-describe-function
            (:columns
             ((counsel-describe-function-transformer (:width 40))
              (ivy-rich-counsel-function-docstring (:face font-lock-doc-face))))
            counsel-describe-variable
            (:columns
             ((counsel-describe-variable-transformer (:width 40))
              (ivy-rich-counsel-variable-docstring (:face font-lock-doc-face))))
            counsel-recentf
            (:columns
             ((ivy-rich-candidate (:width 0.8))
              (ivy-rich-file-last-modified-time (:face font-lock-comment-face))))
            counsel-bookmark
            (:columns
             ((ivy-rich-candidate (:width 32))
              (ivy-rich-bookmark-type :wdith 15)
              (ivy-rich-bookmark-info)))))
    (ivy-mode +1)
    (ivy-rich-mode +1)))

(use-package ivy-xref
  :defer t
  :init
  (progn
    (setq xref-show-xrefs-function #'ivy-xref-show-xrefs)))

;;; swiper
(use-package swiper
  :init
  (progn
    (tl/set-leader-keys
      "ss" 'swiper
      "sb" 'swiper-all)
    (global-set-key "\C-r" 'swiper)
    (global-set-key "\C-s" 'swiper)))

(defun tl/swiper-dwim (arg)
  "Start swiper with input as the selected region or symbol at point by default.
C-u     -> `ivy-resume' (resume from where you last left off swiper)
C-u C-u -> Start swiper without any arguments (stock behavior)"
  (interactive "P")
  (cl-case (car arg)
    (4  (ivy-resume)) ; C-u
    (16 (swiper)) ; C-u C-u
    (t  (swiper (modi/get-symbol-at-point)))))

(use-package doom-todo-ivy
  :defer t
  :commands (doom/ivy-tasks))

(provide '51ivy)

;; Local Variables: **
;; outline-regexp: ";;; " **
;; byte-compile-warnings: (not noruntime free-vars) **
;; End: **
