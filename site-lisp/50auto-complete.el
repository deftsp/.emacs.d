;;; 50auto-complete.el ---

;; Copyright (C) 2009  S.P.Tseng

;; Author: S.P.Tseng <deftsp@gmail.com>
;; Keywords:


;;; auto-complete
(require 'auto-complete)
(autoload 'auto-complete-mode "auto-complete"
  "This extension provides a way to select a completion with popup menu." t)
(global-auto-complete-mode -1)
(setq ac-override-local-map nil)        ;don't override local map
(setq ac-dwim t)


(require 'auto-complete-extension nil t) ;optional
(require 'auto-complete-yasnippet nil t) ;optional
(require 'auto-complete-semantic nil t)  ;optional
(require 'auto-complete-gtags nil t)     ;optional
(require ' auto-complete-cpp nil t)


(set-face-background 'ac-selection-face "blue")
(set-face-foreground 'ac-selection-face "white")
(set-face-background 'ac-candidate-face "dark gray")
(set-face-foreground 'ac-candidate-face "black")
(set-face-background 'ac-completion-face "darkblue")
(set-face-foreground 'ac-completion-face "white")

;; Use M-n/M-p to select candidates
(define-key ac-complete-mode-map "\M-n" 'ac-next)
(define-key ac-complete-mode-map "\M-p" 'ac-previous)

;; Don't start completion automatically
;; Add following code to your .emacs.
;; (setq ac-auto-start nil)
;; (global-set-key "\M-/" 'ac-start)

;; Stop completion by pressing M-/.
(define-key ac-complete-mode-map "\M-/" 'ac-stop)

;; start completion when entered 3 characters
;; (setq ac-auto-start 3)

;; Completion by TAB
;; (define-key ac-complete-mode-map "\t" 'ac-expand)
(define-key ac-complete-mode-map "\t" 'ac-complete)
(define-key ac-complete-mode-map "\r" nil)



;; The sources for common all mode.
(setq ac-sources '(;; ac-source-yasnippet ;this source need file `auto-complete-yasnippet.el'
                   ;; ac-source-semantic    ;this source need file `auto-complete-semantic.el'
                   ac-source-imenu
                   ac-source-abbrev
                   ac-source-words-in-buffer
                   ac-source-files-in-current-dir
                   ac-source-filename))


(dolist (hook (list 'emacs-lisp-mode-hook 'lisp-interaction-mode))
  (add-hook hook '(lambda ()
                   (add-to-list 'ac-sources 'ac-source-symbols))))



(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (make-local-variable 'ac-sources)
            (setq ac-sources '(;; ac-source-yasnippet
                               ac-source-abbrev
                               ac-source-words-in-buffer
                               ac-source-symbols))))

;; C-common-mode
;; Enables omnicompletion with `c-mode-common'.
(add-hook 'c-mode-common-hook
          '(lambda ()
            (add-to-list 'ac-omni-completion-sources
             (cons "\\." '(ac-source-semantic)))
            (add-to-list 'ac-omni-completion-sources
             (cons "->" '(ac-source-semantic)))
            (add-to-list 'ac-sources 'ac-source-gtags)))

;; C++-mode
;; Keywords.
(add-hook 'c++-mode-hook '(lambda ()
                           (add-to-list 'ac-sources 'ac-c++-sources)))



;; (add-hook 'eshell-mode-hook
;;    (lambda ()
;;      (make-local-variable 'ac-sources)
;;      (setq ac-sources '(ac-source-yasnippet ac-source-abbrev ac-source-files-in-current-dir ac-source-words-in-buffer))))

