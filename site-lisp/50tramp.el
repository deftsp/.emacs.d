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

(setq tramp-default-user "deftsp"
      tramp-default-host "localhost"
      tramp-encoding-shell "/bin/sh"
      ;; tramp-default-method "ssh"
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


;;---------------------------------------------------------------------------------------------------------
;; reopen current file with tramp by the method of sudo
(defun find-alternative-file-with-sudo ()
  (interactive)
  (let ((fname (or buffer-file-name
                   dired-directory)))
    (when fname
      (find-alternate-file
       (if (string-match "^/su:root@localhost:" fname)
           (replace-regexp-in-string "^/su:root@localhost:" "" fname)
           (concat "/su:root@localhost:" fname))))))

(global-set-key (kbd "C-c f s") 'find-alternative-file-with-sudo) ; "C-x C-r"
;; find-alternative-file-with-sudo ------------------------------------------------------------------------

;; if we try to save a file owned by someone else, use sudo
;; http://www.emacswiki.org/cgi-bin/wiki/SudoSave
(when (require 'sudo nil t)
  (defun sudo-before-save-hook ()
    (set (make-local-variable 'sudo:file) (buffer-file-name))
    (when sudo:file
      (unless (file-writable-p sudo:file)
        (set (make-local-variable 'sudo:old-owner-uid)
             (nth 2 (file-attributes sudo:file)))
        (when (numberp sudo:old-owner-uid)
          (unless (= (user-uid) sudo:old-owner-uid)
            (when (y-or-n-p
                   (format "File %s is owned by %s, save it with sudo? "
                           (file-name-nondirectory sudo:file)
                           (user-login-name sudo:old-owner-uid)))
              (sudo-chown-file (int-to-string (user-uid))
                               (sudo-quoting sudo:file))
              (add-hook 'after-save-hook
                        (lambda ()
                          (sudo-chown-file (int-to-string sudo:old-owner-uid)
                                           (sudo-quoting sudo:file))
                          (if sudo-clear-password-always
                              (sudo-kill-password-timeout)))
                        nil ;; not append
                        t   ;; buffer local hook
                        )))))))
  (add-hook 'before-save-hook 'sudo-before-save-hook))



(provide '50tramp)
