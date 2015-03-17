;;; 50tramp.el ---
;;
;; Description:
;; Author: Shihpin Tseng <deftsp@gmail.com>
;; Created: Tue Oct 16 20:12:51 2007
;; Version:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:


;; In order to edit a root-owned file on the local host you enter something like: `C-x C-f'
;; /sudo::<path-to-root-owned-file>. sudo:: is a shortcut for sudo's default, which is /sudo:root@localhost:.

(require 'ange-ftp)
(require 'tramp)

(setq tramp-default-user nil
      tramp-default-host "localhost"
      tramp-encoding-shell "/bin/sh"
      tramp-default-method "ssh"
      tramp-auto-save-directory "~/.tmp")
;; Tramp `debugging' options
(setq tramp-debug-buffer 1
      tramp-verbose 10)


;; Default method to use for specific host/user pairs.
;; enable edit remote root files
(add-to-list 'tramp-default-proxies-alist
             '(nil "\\`root\\'" "/ssh:%h:"))
(add-to-list 'tramp-default-proxies-alist
             '((regexp-quote (system-name)) nil nil))
(add-to-list 'tramp-default-proxies-alist
             '("192.241.209.135" "\\`root\\'" "/ssh:%h:"))

;; (add-to-list 'tramp-default-method-alist
;;              '("10.13.122.225" "" "ssh"))
;; (add-to-list 'tramp-default-method-alist
;;              '("tsp" "" "sudo"))
;; (add-to-list 'tramp-default-user-alist
;;              '("" "10.13.122.225" "root"))
;; (setq tramp-default-host "10.13.122.225")

;; (eval-after-load "tramp"
;;   '(add-to-list 'tramp-default-method-alist
;;                 '(".*\\.umich\\.edu\\'" "" "ssh")))

;; specify coding-system
;; (add-to-list 'auto-coding-alist
;;              '("^/tmp/ange-ftp" . gb2312))

;;; backup
(setq tramp-backup-directory-alist backup-directory-alist)

(provide '50tramp)
