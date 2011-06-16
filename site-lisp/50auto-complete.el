;;; 50auto-complete.el ---

;; Copyright (C) 2009  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>
;; Keywords:


;;; auto-complete
(require 'auto-complete)
(require 'auto-complete-config)

(require 'ac-company)

(autoload 'auto-complete-mode "auto-complete"
  "This extension provides a way to select a completion with popup menu." t)
(global-auto-complete-mode t)

(eval-after-load "ac-company"
  '(progn
     (ac-company-define-source ac-source-company-xcode company-xcode)
     (add-to-list 'ac-modes 'objc-mode)))




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

;; Don't start completion automatically
;; Add following code to your .emacs.
;; (setq ac-auto-start nil)
;; (global-set-key "\M-/" 'ac-start)

;; start completion when entered 3 characters
(setq ac-auto-start nil)                ; 2
(setq ac-dwim t)
(ac-set-trigger-key "TAB")

(setq ac-candidate-max 20)


;; Completion by TAB
;; (define-key ac-complete-mode-map "\t" 'ac-complete)
(define-key ac-complete-mode-map "\t" 'ac-expand)
(define-key ac-complete-mode-map "\r" 'ac-complete)
(define-key ac-complete-mode-map  (kbd "<backtab>") 'ac-previous)
;; Use M-n/M-p to select candidates
(define-key ac-complete-mode-map "\M-n" 'ac-next)
(define-key ac-complete-mode-map "\M-p" 'ac-previous)

;; Stop completion by pressing M-/.
(define-key ac-complete-mode-map "\M-/" 'ac-stop)

;; (define-key ac-mode-map (kbd "<backtab>") 'auto-complete)

(defvar ac-source-etags
  '((candidates . (lambda ()
                    (all-completions ac-target (tags-completion-table))))
    (candidate-face . ac-candidate-face)
    (selection-face . ac-selection-face)
    (requires . 3)) ; at least 3 cha
  "etags source")


(dolist (hook (list 'emacs-lisp-mode-hook 'lisp-interaction-mode))
  (add-hook hook '(lambda ()
                   (add-to-list 'ac-sources 'ac-source-symbols))))

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (make-local-variable 'ac-sources)
            (add-to-list 'ac-sources 'ac-source-symbols)))

;; C++-mode
;; Keywords.
(add-hook 'c++-mode-hook '(lambda ()
                           (add-to-list 'ac-sources 'ac-c++-sources)))



;; TODO: ac-source-company-xcode can not work
(add-hook 'objc-mode-hook
          (lambda ()
            ;; (define-key objc-mode-map (kbd "\t") 'ac-complete)
            ;; (push'ac-source-c++-keywords ac-sources)
            ;; use xcode's complete
            (push ac-source-etags ac-sources)))



