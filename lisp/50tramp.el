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

(use-package tramp
  :defer t
  :config
  (setq tramp-default-host "localhost"
        tramp-encoding-shell "/bin/sh"
        tramp-default-method "ssh"
        tramp-auto-save-directory "~/.tmp"
        tramp-verbose 10
        tramp-backup-directory-alist backup-directory-alist)
  ;; Default method to use for specific host/user pairs. enable edit remote root
  ;; files
  (mapc (lambda (x) (add-to-list 'tramp-default-proxies-alist x))
        '((nil "\\`root\\'" "/ssh:%h:")
          ("192.241.209.135" "\\`root\\'" "/ssh:%h:"))))

(provide '50tramp)
