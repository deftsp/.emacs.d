;;; 50yasnippet.el ---

;; Copyright (C) 2009  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>
;; Keywords:


(eval-after-load "yasnippet"
  '(progn
     (setq yas/snippet-dirs `(,(expand-file-name "~/.emacs.d/snippets")
                              ,(expand-file-name "~/.emacs.d/site-lisp/yasnippet-go")))
     ;; Initialize Yasnippet
     ;; Don't map TAB to yasnippet
     ;; In fact, set it to something we'll never use because
     ;; we'll only ever trigger it indirectly.
    (setq yas/trigger-key (kbd "C-c <kp-multiply>"))

    (yas/global-mode 1)
    (setq yas/prompt-functions
     '(yas/ido-prompt yas/dropdown-prompt  yas/completing-prompt yas/x-prompt yas/no-prompt))))

;; (defun yas/advise-indent-function (function-symbol)
;;   (eval `(defadvice ,function-symbol (around yas/try-expand-first activate)
;;            ,(format
;;              "Try to expand a snippet before point, then call `%s' as usual"
;;              function-symbol)
;;            (let ((yas/fallback-behavior nil))
;;              (unless (and (interactive-p)
;;                           (yas/expand))
;;                ad-do-it)))))

;; (yas/advise-indent-function 'org-cycle)


(provide '50yasnippet)
