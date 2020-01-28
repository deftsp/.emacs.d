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

(use-package ivy
  :defer t
  :diminish ivy-mode
  :chords (("jv" . hydra-view/body))
  :init
  (progn
    (setq ivy-display-style 'fancy)
    ;; include recent(recentf) files and/or bookmarks, recentf-cleanup
    (setq ivy-use-virtual-buffers t)
    ;; Better fuzzy matching support: http://oremacs.com/2016/01/06/ivy-flx/
    ;; ivy--regex-plus use a .* regex wild card in place of each single space in the input.
    (setq ivy-re-builders-alist
          '((ivy-switch-buffer . ivy--regex-plus)
            (counsel-rg . ivy--regex-plus)
            (t . ivy--regex-fuzzy)))

    (setq ivy-initial-inputs-alist nil)
    (push '(counsel-ag . "--file-search-regex '' -- ") ivy-initial-inputs-alist)
    (push '(counsel-rg . "--glob '**' -- ") ivy-initial-inputs-alist)

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
      "rl" 'ivy-resume))
  :config
  (progn
    (mapcar (lambda (str) (add-to-list 'ivy-ignore-buffers str))
            tl/ignore-buffer-or-file-regexp)

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
      ("V" ivy-pop-view "delete a layout")
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
      (ivy-rich-mode +1)
      (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line))
    (ivy-mode +1)))

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
