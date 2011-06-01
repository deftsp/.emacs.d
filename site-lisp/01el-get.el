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



(setq el-get-sources
      '(cssh el-get emacs-w3m switch-window vkill
             xcscope yasnippet package magit dired+
             bbdb))
(el-get)                          ; 'sync 'wait
