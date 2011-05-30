;;; 01el-get.el ---

;; Copyright (C) 2011  S.P. Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>


(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil t)
  (url-retrieve
   "https://github.com/dimitri/el-get/raw/master/el-get-install.el"
   (lambda (s)
     (end-of-buffer)
     (eval-print-last-sexp))))


(eval-after-load "el-get"
  '(progn
     (setq el-get-sources
           '(cssh el-get switch-window vkill google-maps nxhtml xcscope yasnippet
                  ;; (:name magit
                  ;;        :after (lambda () (global-set-key (kbd "C-x C-z") 'magit-status)))

                  (:name asciidoc
                         :type elpa
                         :after (lambda ()
                                  (autoload 'doc-mode "doc-mode" nil t)
                                  (add-to-list 'auto-mode-alist '("\\.adoc$" . doc-mode))
                                  (add-hook 'doc-mode-hook '(lambda ()
                                                              (turn-on-auto-fill)
                                                              (require 'asciidoc)))))

                  (:name lisppaste        :type elpa)
                  (:name dictionary-el    :type apt-get)
                  ))))
;; (el-get 'sync)
;; (el-get 'wait)
;; (el-get)