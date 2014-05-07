;;; 51helm.el ---

;; Copyright (C) 2013  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>

;;; Helm is a fork of anything.el originaly written by Tamas Patrovic and can be considered to be its successor.

;;; Note: it's better to load w3m before el-get install/update helm

;; make EMACS="/Applications/Emacs.app/Contents/MacOS/Emacs -Q -batch " all

(add-to-list 'load-path (file-name-as-directory "~/.emacs.d/site-lisp/helm/"))
(require 'helm-config)
;; (helm-mode 1) ; use ido everywhere instead

;;; helm-command-prefix-key (default to C-x c).
(setq helm-idle-delay 0.1)
(setq helm-input-idle-delay 0.1)

(eval-after-load "helm-files"
  '(progn
     (when (eq system-type 'darwin)
       (setq helm-c-locate-command "locate-with-mdfind %.0s %s")) ; helm-for-files;

     (loop for ext in '("\\.swf$" "\\.elc$" "\\.pyc$")
           do (add-to-list 'helm-c-boring-file-regexp-list ext))))
;; (define-key global-map [(alt t)] 'helm-for-files))
(define-key global-map (kbd "M-y") 'helm-show-kill-ring)


;;; ghc-mode
(defun helm-ghc-browse-document ()
  "Preconfigured `helm' lightweight version \(buffer -> recentf\)."
  (interactive)
  (require 'helm-files)
  (helm-other-buffer 'helm-c-source-ghc-mod "*helm ghc mod*"))


(defvar helm-c-source-ghc-mod
  '((name . "ghc-browse-document")
    (init . helm-c-source-ghc-mod)
    (candidates-in-buffer)
    (candidate-number-limit . 9999999)
    (action ("Open" . helm-c-source-ghc-mod-action))))


(defun helm-c-source-ghc-mod ()
  (unless (executable-find "ghc-mod")
    (error "ghc-mode not find."))
  (let ((buffer (helm-candidate-buffer 'global)))
    (with-current-buffer buffer
      (call-process "ghc-mod" nil t t "list"))))


(defun helm-c-source-ghc-mod-action (candidate)
  (interactive "P")
  (let* ((pkg (ghc-resolve-package-name candidate)))
    (helm-aif (and pkg candidate)
        (ghc-display-document pkg it nil)
      (message "No document found"))))

;;; helm-swoop
(eval-after-load "helm-swoop"
  '(require 'helm-utils nil t))
(global-set-key (kbd "H-i") 'helm-swoop)
(global-set-key (kbd "H-I") 'helm-swoop-back-to-last-point)
;; When doing isearch, hand the word over to helm-swoop
(define-key isearch-mode-map (kbd "H-i") 'helm-swoop-from-isearch)
(define-key isearch-mode-map (kbd "C-S-i") 'helm-swoop-from-isearch)

(eval-after-load "helm"
  '(progn
     (define-key helm-map (kbd "M-j") 'helm-next-line)
     (define-key helm-map (kbd "M-k") 'helm-previous-line)))


(provide '51helm)
;;; 51helm.el ends here
