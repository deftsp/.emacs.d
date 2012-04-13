;;; 50auto-complete.el ---

;; Copyright (C) 2009  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>
;; Keywords:


;;; auto-complete
(require 'auto-complete)
(require 'auto-complete-config)
(require 'auto-complete-clang)


(autoload 'auto-complete-mode "auto-complete"
  "This extension provides a way to select a completion with popup menu." t)

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


(defun auto-complete-settings ()
  "Settings for `auto-complete'."


  ;; add objc-mode to ac-modes
  (add-to-list 'ac-modes 'objc-mode)
  (global-auto-complete-mode 1)

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
  (ac-set-trigger-key "TAB")

  (setq ac-candidate-max 20)

  (setq ac-clang-flags `(,(concat "-I" (expand-file-name "~/src/cocos2d-iphone/cocos2d"))
                         ,(concat "-I" (expand-file-name "~/src/cocos2d-iphone/external/Box2d/Box2D"))
                         "-I/Developer/Platforms/iPhoneOS.platform/Developer/SDKs/iPhoneOS5.0.sdk/System/Library/Frameworks"))


  ;; Completion by TAB
  ;; (define-key ac-complete-mode-map "\t" 'ac-complete)
  ;; (define-key ac-complete-mode-map  (kbd "<backtab>") 'ac-previous)
  ;; Use M-n/M-p to select candidates
  (define-key ac-complete-mode-map "\C-n" 'ac-next)
  (define-key ac-complete-mode-map "\C-p" 'ac-previous)

  ;; Stop completion by pressing M-/.

  ;; (define-key ac-mode-map (kbd "<backtab>") 'auto-complete)
  (define-key ac-complete-mode-map "\M-/" 'ac-stop))

(eval-after-load "auto-complete-config"
  '(auto-complete-settings))

(defun ac-objc-mode-setup ()
  (setq ac-sources (append '(ac-source-clang ac-source-etags) ; ac-source-company-xcode
                           ac-sources)))

(add-hook 'lisp-interaction-mode 'ac-emacs-lisp-mode-setup)
(add-hook 'objc-mode-hook 'ac-objc-mode-setup)
(ac-config-default)

(provide '50auto-complete)