;;; 50yasnippet.el ---

;; Copyright (C) 2009  S.P.Tseng

;; Author: S.P.Tseng <deftsp@gmail.com>
;; Keywords:


(require 'yasnippet)
(yas/initialize)
(yas/load-directory "~/.emacs.d/packages/yasnippet/snippets")
(yas/load-directory "~/.emacs.d/yasnippet-snippets")

(setq yas/prompt-functions '(yas/ido-prompt yas/dropdown-prompt  yas/completing-prompt yas/x-prompt yas/no-prompt))

(eval-after-load "yasnippet"
  '(yas/define-snippets 'org-mode
   '(("elisp" "#+BEGIN_SRC emacs-lisp
  $0
#+END_SRC" "#+BEGIN_SRC emacs-lisp ... #+END_SRC"))))


;; (eval-after-load "yasnippet"
;;   '(add-hook 'org-mode-hook
;;     (lambda ()
;;       (org-set-local 'yas/trigger-key [tab])
;;       (define-key yas/keymap [tab] 'yas/next-field-group))))


(add-hook 'org-mode-hook
          #'(lambda ()
              (local-set-key [tab] 'yas/expand)))
