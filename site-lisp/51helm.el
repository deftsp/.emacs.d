;;; 51helm.el ---

;; Copyright (C) 2013  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>

;;; Helm is a fork of anything.el originaly written by Tamas Patrovic and can be considered to be its successor.

;;; Note: it's better to load w3m before el-get install/update helm


(require 'helm-config)
;; (helm-mode 1) ; use ido everywhere instead

;;; helm-command-prefix-key (default to C-x c).


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


(provide '51helm)
;;; 51helm.el ends here
