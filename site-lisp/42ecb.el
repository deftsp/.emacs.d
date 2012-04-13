;;; 42ecb.el ---

;; Copyright (C) 2008  S.P.Tseng

;; Author: S.P.Tseng <deftsp@gmail.com>
;; Keywords:
;; (require 'ecb)
(require 'ecb-autoloads)                ; use `ecb-update-autoloads' to create ecb-autoloads.el

;; (ecb-byte-compile)
(setq ecb-auto-activate nil)

(eval-after-load "ecb"
  '(progn
    (setq ecb-winman-winring-name "ECB"
     ecb-tree-buffer-style 'ascii-guides ; 'image
     ecb-windows-width 0.26
     ecb-compile-window-height 0.3
     ecb-auto-expand-tag-tree-collapse-other 'only-if-on-tag
     ecb-tip-of-the-day nil
     ;; ecb-layout-name 'left8          ; defautl 'left8
     ecb-tree-incremental-search 'substring ; 'prefix
     ecb-use-speedbar-for-directories nil)
    (set-face-background 'ecb-default-highlight-face "#666666")))
;; (setq ecb-other-window-jump-behavior 'edit-and-compile)

(eval-after-load "ecb"
  '(add-to-list 'ecb-source-path '("~/proj/homepage/" "homepage")))

;; With activated ECB `calendar' does not shrink it´s window to the small size but splits the window equally. But if you
;; add this to your `.emacs' it works:
(eval-after-load "ecb"
 '(add-hook 'calendar-initial-window-hook
           (function (lambda ()
             (when (and ecb-minor-mode
                      (ecb-point-in-edit-window-number))
               ;; if no horizontal split then nothing
               ;; special to do
               (or (= (frame-width) (window-width))
                  (shrink-window (- (window-height) 9))))))))
;;; pretty layout
;; (require 'ecb-layout)
;; (defvar ecb-emms-buffer-name "*EMMS Playlist*")
;; (defvar ecb-w3m-buffer-name "*w3m*")

;; (defecb-window-dedicator ecb-set-emms-buffer ecb-emms-buffer-name
;;   "Display the EMMS-buffer in current window and make window dedicated."
;;   (switch-to-buffer ecb-emms-buffer-name))

;; (defecb-window-dedicator ecb-set-w3m-buffer ecb-w3m-buffer-name
;;   "Display the w3m-buffer in current window and make window dedicated."
;;   (switch-to-buffer ecb-w3m-buffer-name))

;; (defvar ecb-slime-repl-buffer-name "*slime-repl sbcl*")
;; (defecb-window-dedicator ecb-set-slime-repl-buffer ecb-slime-repl-buffer-name
;;   "Display the slime-repl-buffer in current window and make window dedicated."
;;   (switch-to-buffer ecb-slime-repl-buffer-name))

;; (ecb-layout-define "emms" left
;;   (emms)
;;   (ecb-set-emms-buffer)
;;   (ecb-split-ver 0.4)
;;   (w3m)
;;   (ecb-set-w3m-buffer)
;;   (select-window (next-window)))

;; (ecb-layout-define "hacking" left
;;   "create a layout work with slime"
;;   (ecb-set-directories-buffer)
;;   (ecb-split-ver 0.3)
;;   (ecb-set-sources-buffer)
;;   (ecb-split-ver 0.35)
;;   (ecb-set-methods-buffer)
;;   (ecb-split-ver 0.65)
;;   (ecb-set-history-buffer)
;;   (select-window (next-window))
;;   ;; (ecb-split-ver 0.82)
;;   ;; (ecb-set-slime-repl-buffer)
;;   ;; (select-window (previous-window (selected-window) 0))
;;   )

(defun winring-jump-to-ecb ()
  (interactive)
  (if (member "ECB" (winring+-names))
      (if (string= winring-name "ECB")
          (winring-prev-configuration)
          (winring+-jump-to-configuration "ECB"))
      (winring+-new-configuration "ECB")))

(global-set-key (kbd "C-x E") 'winring-jump-to-ecb)

;;; color
(eval-after-load "ecb"
  '(progn
    (set-face-foreground 'ecb-tag-header-face "gray10")
    (set-face-background 'ecb-tag-header-face "rosy brown")))

;; (defun set-winring-name-and-activate-ecb ()
;;   (cond ((string= "ECB" winring-name) (ecb-activate))
;;         ((string= "001" winring-name)
;;          (winring-set-name "ECB")
;;          (ecb-activate))
;;         (t (winring+-jump-to-configuration "ECB")
;;            (ecb-activate))))

;; (add-hook 'after-init-hook 'set-winring-name-and-activate-ecb)
;;; 42ecb.el ends here

(provide '42ecb)