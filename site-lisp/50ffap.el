;;; 50ffap.el ---

;; Copyright (C) 2008  S.P.Tseng

;; Author: S.P.Tseng <deftsp@gmail.com>

;; ffap -- find file at point
(require 'ffap)
;; rebind C-x C-f and others to the ffap bindings (see variable ffap-bindings)
(ffap-bindings)
;; C-u C-x C-f finds the file at point via the ffap package), relative filenames work also
(setq ffap-require-prefix t)
;; browse urls at point via w3m
(setq ffap-url-fetcher 'w3m-browse-url) ; or 'browe-url
(setq ffap-c-path '("/usr/include" "/usr/local/include"))
;; (add-to-list 'ffap-c-path "/system/A/include")
;; (add-to-list 'ffap-c-path "/system/specific/include")
;; (setq 'ffap-c-path (parse-colon-path (getenv "INCLUDE")))
;; (dolist (x (split-string (getenv "INCLUDE") ";"))
;;  (add-to-list 'ffap-c-path x))

;; C-x f 原来的绑定是 set-fill-column ,不太常用, 于是绑定到 find-file-at-point 上. 这个功能很有用, 类似于 Vim 的 gf 功能,
;; 可以打开光标处的文件. 他也有普通 find-file 的功能. 这个功能在 shell 模式下和 c-mode 下特别有用.
;; (global-set-key (kbd "C-x f") 'find-file-at-point)
;; (global-set-key (kbd "C-x f") 'find-file-root)
