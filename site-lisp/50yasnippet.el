;;; 50yasnippet.el ---

;; Copyright (C) 2009  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>
;; Keywords:


(eval-after-load "yasnippet"
  '(progn
     (yas/initialize)
     (yas/load-directory "~/.emacs.d/yasnippet-snippets")
     (setq yas/prompt-functions
           '(yas/ido-prompt yas/dropdown-prompt  yas/completing-prompt yas/x-prompt yas/no-prompt))))

(defun yas/advise-indent-function (function-symbol)
  (eval `(defadvice ,function-symbol (around yas/try-expand-first activate)
           ,(format
             "Try to expand a snippet before point, then call `%s' as usual"
             function-symbol)
           (let ((yas/fallback-behavior nil))
             (unless (and (interactive-p)
                          (yas/expand))
               ad-do-it)))))

(yas/advise-indent-function 'org-cycle)

