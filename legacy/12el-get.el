;;; 12el-get.el ---

;; Copyright (C) 2011  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>

;;; manually download and install
;; clone the package and place it to ~/.emacs.d/el-get, then execute
;; (el-get-save-package-status 'helm "required")
;; (el-get-post-install 'helm)

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(unless (require 'el-get nil t)
  (url-retrieve
   "https://raw.github.com/dimitri/el-get/master/el-get-install.el"
   (lambda (s)
     (let (el-get-master-branch)
       (goto-char (point-max))
       (eval-print-last-sexp)))))

(add-to-list 'el-get-recipe-path (expand-file-name "~/.emacs.d/el-get-user/recipes"))
(setq el-get-github-default-url-type 'git
      el-get-verbose nil
      el-get-is-lazy t)

;; (el-get-save-package-status "package-name-here" "removed")

(el-get-bundle ace-window :type github :pkgname "deftsp/ace-window" :branch "shihpin")
(el-get-bundle org-link-minor-mode :type github :pkgname "deftsp/org-link-minor-mode" :branch "shihpin")
(el-get-bundle ibuffer-vc :type github :pkgname "purcell/ibuffer-vc")
(el-get-bundle vterm-toggle :type github :pkgname "jixiuf/vterm-toggle")
(el-get-bundle company-ledger
  :type github
  :description "Transaction Auto-Completion for Beancount & other Ledger-Likes with Company Mode"
  :pkgname "debanjum/company-ledger")
(el-get-bundle reveal-in-osx-finder :type github :pkgname "kaz-yos/reveal-in-osx-finder")
(el-get-bundle om :type github :pkgname "ndwarshuis/om.el" :description "A functional library for org-mode")
(el-get-bundle org-fancy-priorities :type github :pkgname "harrybournis/org-fancy-priorities")

;; (el-get-bundle diredp :description "Extensions to Dired" :type http :url "https://www.emacswiki.org/emacs/download/dired+.el")

;; (el-get-bundle ibuffer-projectile :type github :pkgname "purcell/ibuffer-projectile")


;; (el-get 'sync 'cedet)
(el-get 'sync 'names) ; reinstall org-mode require names
(el-get 'sync 'org-mode) ; init org-mode first
;; (el-get 'sync 'org-drill) ; init after org-mode, don't use build-in org-drill
;; (el-get 'sync 'exec-path-from-shell)
;; (require 'exec-path-from-shell nil t) ; https://github.com/purcell/exec-path-from-shell
(el-get 'sync 'clojure-mode)
(el-get 'sync 'evil)

(el-get 'sync)                          ; 'sync 'wait


(provide '12el-get)
;;; 12el-get.el ends here
