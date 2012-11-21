;;; 50auto-complete.el ---

;; Copyright (C) 2009  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>
;; Keywords:


(require 'auto-complete)

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
  (add-to-list 'ac-modes 'org-mode)
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
  (setq ac-auto-start nil)         ; do not start automatically
  (setq ac-dwim t)
  (setq ac-use-quick-help t)

  (setq ac-candidate-max 20)
  (setq ac-use-menu-map t)

  ;; find out the include file search pathes of your g++:
  ;; echo "" | g++ -v -x c++ -E -
  (setq ac-clang-flags
        (mapcar (lambda (item)
                  (concat "-I" item))
                (split-string
                 "
 /usr/llvm-gcc-4.2/bin/../lib/gcc/i686-apple-darwin11/4.2.1/include
 /usr/include/c++/4.2.1
 /usr/include/c++/4.2.1/backward
 /usr/local/include
 /Applications/Xcode.app/Contents/Developer/usr/llvm-gcc-4.2/lib/gcc/i686-apple-darwin11/4.2.1/include
 /usr/include
 /System/Library/Frameworks
 /Library/Frameworks
"
                 )))

  ;; Completion by TAB
  (ac-set-trigger-key "C-M-/") ; "TAB"
  (define-key ac-mode-map (kbd "C-M-/") 'auto-complete)
  (define-key ac-completing-map  (kbd "<backtab>") 'ac-previous)
  (define-key ac-completing-map "\C-n" 'ac-next)
  (define-key ac-completing-map "\C-p" 'ac-previous))


(defun pl/ac-c-mode-common-setup ()
  ;; (add-to-list 'ac-sources 'ac-source-company-xcode)
  ;; (add-to-list 'ac-sources 'ac-source-clang)
  ;; (add-to-list 'ac-sources 'ac-source-gtags)
  ;; (add-to-list 'ac-sources 'ac-source-etags)
  (add-to-list 'ac-sources 'ac-source-yasnippet)
  (add-to-list 'ac-sources 'ac-source-semantic))

(add-hook 'c-mode-common-hook 'pl/ac-c-mode-common-setup)
(add-hook 'org-mode-hook 'pl/ac-c-mode-common-setup)

(add-hook 'lisp-interaction-mode 'ac-emacs-lisp-mode-setup)


;;; Scheme
(require 'scheme-complete nil t)
(eval-after-load "scheme-complete"
  '(progn
    (defvar ac-source-scheme
      '((candidates . (lambda ()
                        (all-completions ac-target (car (scheme-current-env))))))
      "Source for scheme keywords.")))

(defun pl/ac-scheme-mode-common-setup ()
  (if (fboundp 'scheme-current-env)
      (add-to-list 'ac-sources 'ac-source-scheme))
  (add-to-list 'ac-sources 'ac-source-yasnippet))
(add-hook 'scheme-mode-hook 'pl/ac-scheme-mode-common-setup)


(eval-after-load "popup"
  '(progn
    (set-face-attribute 'popup-scroll-bar-foreground-face nil :foreground "white" :background "#bbbbbb")
    (set-face-attribute 'popup-scroll-bar-background-face nil :foreground "yellow" :background "#dddddd")))


(eval-after-load "auto-complete"
  '(progn
     (require 'auto-complete-config)
     (require 'auto-complete-clang)
     (pl/auto-complete-settings)
     (ac-config-default)))


(provide '50auto-complete)
