;;; 50auto-complete.el ---

;; Copyright (C) 2009  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>
;; Keywords:


;;; auto-complete
;; (require 'ac-company)
;; (eval-after-load "ac-company"
;;   '(progn
;;      (ac-company-define-source ac-source-company-xcode company-xcode)
;;      (add-to-list 'ac-modes 'objc-mode)))



;; (global-set-key "\M-/" 'ac-start)

(defvar ac-source-etags
  '((candidates . (lambda ()
                    (all-completions ac-target (tags-completion-table))))
    (candidate-face . ac-candidate-face)
    (selection-face . ac-selection-face)
    (requires . 3)) ; at least 3 cha
  "etags source")


(defun pl/auto-complete-settings ()
  "Settings for `auto-complete'."
  (add-to-list 'ac-modes 'objc-mode) ; add objc-mode to ac-modes
  (global-auto-complete-mode 1)
  ;; after editing and adding dictionary, you should do M-x ac-clear-dictionary-cache to apply changes
  (add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")

  (set-face-background 'ac-selection-face "steelblue")
  (set-face-foreground 'ac-selection-face "white")
  (set-face-underline  'ac-candidate-face "lightgray")
  (set-face-background 'ac-candidate-face "lightgray")
  (set-face-foreground 'ac-candidate-face "black")
  (set-face-background 'ac-completion-face "darkblue")
  (set-face-foreground 'ac-completion-face "white")


  (set-face-background 'popup-scroll-bar-foreground-face "#222222")
  (set-face-foreground 'popup-scroll-bar-background-face "lightgray")
  (set-face-foreground 'popup-menu-selection-face "white")
  (set-face-background 'popup-menu-selection-face "#0000ff")
  (set-face-foreground 'ac-completion-face "black")
  (set-face-background 'ac-completion-face "violet")

  ;; start completion when entered 2 characters
  (setq ac-auto-start nil)                ; do not start automatically
  (setq ac-dwim t)
  (setq ac-use-quick-help nil)          ; TODO: tooltip positon is not correct

  (setq ac-candidate-max 20)

  (setq ac-clang-flags `(,(concat "-I" (expand-file-name "~/src/cocos2d-iphone/cocos2d"))
                         ,(concat "-I" (expand-file-name "~/src/cocos2d-iphone/external/Box2d/Box2D"))
                         "-I/Developer/Platforms/iPhoneOS.platform/Developer/SDKs/iPhoneOS5.0.sdk/System/Library/Frameworks"))


  ;; Completion by TAB
  (ac-set-trigger-key "TAB")
  (define-key ac-mode-map (kbd "<backtab>") 'auto-complete)
  ;; (define-key ac-mode-map (kbd "<C-tab>") 'auto-complete)
  (define-key ac-complete-mode-map  (kbd "<backtab>") 'ac-previous)
  (define-key ac-complete-mode-map "\t" 'ac-complete)
  (define-key ac-complete-mode-map "\C-n" 'ac-next)
  (define-key ac-complete-mode-map "\C-p" 'ac-previous))


(defun pl/ac-c-mode-common-setup ()
  ;; (add-to-list 'ac-sources 'ac-source-company-xcode)
  (add-to-list 'ac-sources 'ac-source-clang)
  (add-to-list 'ac-sources 'ac-source-gtags)
  (add-to-list 'ac-sources 'ac-source-etags))


(add-hook 'lisp-interaction-mode 'ac-emacs-lisp-mode-setup)
(add-hook 'objc-mode-hook 'pl/ac-objc-mode-setup)
(add-hook 'c-mode-common-hook 'pl/ac-c-mode-common-setup)

(eval-after-load "popup"
  '(progn
     (set-face-attribute 'popup-scroll-bar-foreground-face nil :foreground "white" :background "#999999")
     (set-face-attribute 'popup-scroll-bar-background-face nil :foreground "yellow" :background "#cccccc")))


(eval-after-load "auto-complete"
  '(progn
     (require 'auto-complete-config)
     (require 'auto-complete-clang)
     (pl/auto-complete-settings)
     (ac-config-default)))


(provide '50auto-complete)
