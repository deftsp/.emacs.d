;;; 50ffap.el ---

;; Copyright (C) 2008  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>

;; ffap -- find file at point
(use-package ffap
  :init
  ;; C-u C-x C-f finds the file at point via the ffap package), relative filenames work also
  (setq ffap-require-prefix t)
  ;; browse urls at point via w3m
  (setq ffap-url-fetcher 'w3m-browse-url) ; or 'browe-url
  (setq ffap-c-path '("/usr/include" "/usr/local/include"))

  (setq ffap-newfile-prompt t)
  (setq ffap-kpathsea-depth 5)

  ;; (add-to-list 'ffap-c-path "/system/A/include")
  ;; (add-to-list 'ffap-c-path "/system/specific/include")
  ;; (setq 'ffap-c-path (parse-colon-path (getenv "INCLUDE")))
  ;; (dolist (x (split-string (getenv "INCLUDE") ";"))
  ;;  (add-to-list 'ffap-c-path x))

  ;; (global-set-key (kbd "C-x f") 'find-file-at-point)
  ;; (global-set-key (kbd "C-x f") 'find-file-root)
  ;; rebind C-x C-f and others to the ffap bindings (see variable ffap-bindings)
  (ffap-bindings))


(provide '50ffap)
