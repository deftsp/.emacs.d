;;; 50ivy.el ---                                     -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>

(defvar paloryemacs/ignore-buffer-or-file-regexp
  '("^\\ " "^\\*Completions*" "^\\*Article\\*" "^\\*Apropos*"
    "^\\*Ibuffer*" "^\\*Backtrace*"  "^\\*Help"  "^\\*Bookmark"
    "^\\*Messages" "^\\.newsrc-dribble"  "^\\*Woman-Log"
    "^\\*Compilation" "^\\*Compile-Log" "^\\*Calendar"
    "^\\*cscope"  "^\\*grep" "*BBDB*" "*Tree*"  "*Group*"
    "*Helm Swoop*"  "*EMMS Playlist*"  "^\\*Summary\\ n" "Map_Sym.txt"
    "^\\*w3m*" "^\\#" "^\\irc.*:" "localhost:6668" "^\\*TeX\\ Help\\*"
    "\\`auto/" "\\.prv/" "_region_" "^.DS_Store$" "\\.hi\\'"))


(use-package counsel
  :init
  (progn
    (global-set-key (kbd "s-g") 'counsel-rg)
    (paloryemacs/set-leader-keys
      dotpaloryemacs-emacs-command-key 'execute-extended-command
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
      ;; "hR"  'paloryemacs/counsel-search-docs
      ;; insert
      "iu"  'counsel-unicode-char
      ;; jump
      ;; register/ring
      "ry"  'counsel-yank-pop
      "rm"  'counsel-mark-ring
      ;; jumping
      "sj"  'counsel-imenu
      ;; themes
      "Ts"  'counsel-load-theme
      "sgp" 'counsel-git-grep))
  :config
  (progn
    (setq counsel-find-file-ignore-regexp
          (string-join paloryemacs/ignore-buffer-or-file-regexp "\\|"))
    (define-key counsel-find-file-map (kbd "C-h") 'counsel-up-directory)
    ;; remaps built-in commands that have a counsel replacement
    ;; do not use counsel-M-x for now
    (define-key counsel-mode-map (vector 'remap 'execute-extended-command) nil)
    (counsel-mode +1)))

(use-package ivy
  :defer t
  :init
  (progn
    (setq ivy-display-style 'fancy)
    (setq ivy-use-virtual-buffers t)
    ;; http://oremacs.com/2016/01/06/ivy-flx/
    ;; let flx (hopefully) sort the matches in a nice way
    (setq ivy-initial-inputs-alist nil)
    (setq ivy-count-format "[%d/%d] ")
    (setq ivy-use-selectable-prompt t)
    (setq ivy-height 12)
    (setq confirm-nonexistent-file-or-buffer t)
    (setq ivy-re-builders-alist
          '((t . ivy--regex-fuzzy)))

    (mapcar (lambda (str) (add-to-list 'ivy-ignore-buffers str))
            paloryemacs/ignore-buffer-or-file-regexp)
    ;; https://oremacs.com/2016/06/27/ivy-push-view/
    ;; (global-set-key (kbd "s-v") 'ivy-push-view)
    ;; delete view, delete many views at once by pressing C-M-m[M-RET] (ivy-call)
    ;; (global-set-key (kbd "s-V") 'ivy-pop-view)
    (paloryemacs/set-leader-keys
      "fr" 'counsel-recentf
      "rl" 'ivy-resume))
  :config
  (progn
    (define-key ivy-minibuffer-map (kbd "C-w") 'ivy-occur)
    (define-key ivy-minibuffer-map [escape] 'minibuffer-keyboard-quit)
    (define-key ivy-minibuffer-map (kbd "M-j") 'ivy-next-line)
    (define-key ivy-minibuffer-map (kbd "M-k") 'ivy-previous-line)
    (global-set-key (kbd "C-c C-r") 'ivy-resume)
    (global-set-key [f6] 'ivy-resume)

    (with-eval-after-load 'evil
      (evil-make-overriding-map ivy-occur-mode-map 'normal))
    (paloryemacs/set-leader-keys-for-major-mode 'ivy-occur-grep-mode
      "w" 'ivy-wgrep-change-to-wgrep-mode)

    (use-package ivy-hydra
      :config
      (define-key hydra-ivy/keymap [escape] 'hydra-ivy/keyboard-escape-quit-and-exit))

    (use-package ivy-rich
      :init
      (setq ivy-virtual-abbreviate 'full
            ivy-rich-abbreviate-paths t
            ivy-rich-switch-buffer-align-virtual-buffer t)
      :config
      (ivy-set-display-transformer 'ivy-switch-buffer 'ivy-rich-switch-buffer-transformer))))


(global-set-key (kbd "s-v") 'hydra-view/body)
(defhydra hydra-view (:color blue :hint nil)
  "view control panel"
  ("v" ivy-push-view "save layout" )
  ("V" ivy-pop-view "delete a layout")
  ("d" ivy-pop-view "delete a layout")
  ("l" paloryemacs/ivy-switch-view "select a layout")
  ("<escape>" nil nil)
  ("q" nil))


(defun paloryemacs/ivy-switch-view ()
  (interactive)
  (let ((ivy-initial-inputs-alist
         '((ivy-switch-buffer . "{}"))))
    (ivy-switch-buffer)))

(when (fboundp 'ivy-mode)
  (ivy-mode +1))

(autoload 'counsel-describe-function "counsel" "Forward to (`describe-function' FUNCTION) with ivy completion." t)
(autoload 'counsel-describe-variable "counsel" "Forward to (`describe-variable' VARIABLE BUFFER FRAME)." t)
(autoload 'counsel-ag "counsel" "Grep for a string in the current directory using ag. INITIAL-INPUT can be given as the initial minibuffer input." t)
(global-set-key (kbd "C-h f") 'counsel-describe-function)
(global-set-key (kbd "C-h v") 'counsel-describe-variable)


;;; swiper
(use-package swiper
  :init
  (progn
    (paloryemacs/set-leader-keys
      "ss" 'swiper
      "sb" 'swiper-all)
    (global-set-key "\C-r" 'swiper)
    (global-set-key "\C-s" 'swiper)))

(defun paloryemacs/swiper-dwim (arg)
  "Start swiper with input as the selected region or symbol at point by default.
C-u     -> `ivy-resume' (resume from where you last left off swiper)
C-u C-u -> Start swiper without any arguments (stock behavior)"
  (interactive "P")
  (cl-case (car arg)
    (4  (ivy-resume)) ; C-u
    (16 (swiper)) ; C-u C-u
    (t  (swiper (modi/get-symbol-at-point)))))

(provide '50ivy)

;; Local Variables: **
;; outline-regexp: ";;; " **
;; byte-compile-warnings: (not noruntime free-vars) **
;; End: **
