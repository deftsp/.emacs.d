;;; tsp-tramp.el ---
;;
;; Filename: tsp-tramp.el
;; Description:
;; Author: S.P.Tseng <kirby1985@gmail.com>
;; Created: Tue Oct 16 20:12:51 2007
;; Version:
;; Last-Updated: Tue Oct 16 20:43:18 2007 (28800 CST)
;;           By: S.P.Tseng
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'ange-ftp)
(require 'tramp)

(setq tramp-default-user "tsp"
      tramp-default-host "localhsot"
      tramp-encoding-shell "/bin/sh"
      tramp-default-method "ssh"
      tramp-auto-save-directory "~/.tmp")
;; Tramp `debugging' options
(setq debug-on-error 1
      tramp-debug-buffer 1
      tramp-verbose 10)
;; 你也可以对特定的 用户/主机 组合设定不同的方法
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

;; 使用 TRAMP 把当前文件以 sudo 方式打开
(defun tsp-find-alternative-file-with-sudo ()
  (interactive)
  (when buffer-file-name
    (let ((point (point)))
      (find-alternate-file
       (concat "/sudo:root@localhost:"
               buffer-file-name))
      (goto-char point))))

(global-set-key (kbd "C-c f s") 'tsp-find-alternative-file-with-sudo)
;; coding-system ，通过指定 file-coding-system-alist 里面一个函数我
;; 可以看到传进去的参数大概是这个样子：
;; ((write-region "" nil "/tmp/ange-ftp8132kPY" nil silent
;; "/tmp/ange-ftp8132kPY"))
;; 终于知道为什么 "^/ftp:" 不能匹配了，应该写成 "^/tmp/ange-ftp" 。
;; (add-to-list 'auto-coding-alist
;;              '("^/tmp/ange-ftp" . gb2312))


;;---------------------------------------------------------------------------------------------------------
;;find-file-root
;;--------------------------------------------------------------------------------------------------------- This whole
;;succession of Lisp is to open a file as root, easily, and bind it to a key. It is totally stolen from Alex Schroder.
;;<http://repose.cx/conf/.elisp/de-root.el>
(defvar find-file-root-prefix "/sudo:root@localhost:"
  "*The filename prefix used to open a file with `find-file-root'.
   This should look something like \"/sudo:root@localhost:\" (new style
   TRAMP) or \"/[sudo:root@localhost]/\" (XEMacs or old style TRAMP).")

(defvar find-file-root-history nil
  "History list for files found using `find-file-root'.")

(defvar find-file-root-hook nil
  "Normal hook for functions to run after finding a \"root\" file.")

(defun find-file-root ()
  "*Open a file as the root user.
   Prepends `find-file-root-prefix' to the selected file name so that it
   maybe accessed via the corresponding tramp method."

  (interactive)
  (let* ( ;; We bind the variable `file-name-history' locally so we can
         ;; use a separate history list for "root" files.
         (file-name-history find-file-root-history)
         (name (or buffer-file-name default-directory))
         (tramp (and (tramp-tramp-file-p name)
                   (tramp-dissect-file-name name)))
         path dir file)

    (when (setq file (read-file-name "Find file (UID = 0): " dir path))
      (find-file (concat find-file-root-prefix file))
      ;; If this all succeeded save our new history list.
      (setq find-file-root-history file-name-history)
      ;; allow some user customization
      (run-hooks 'find-file-root-hook))))

(defface find-file-root-header-face
    '((t (:foreground "white" :background "red3")))
  "*Face use to display header-lines for files opened as root."
  :group 'file-root)

(defun find-file-root-header-warning ()
  "*Display a warning in header line of the current buffer.
   This function is suitable to add to `find-file-root-hook'."
  (let* ((warning "WARNING: EDITING FILE AS ROOT!")
         (space (+ 6 (- (frame-width) (length warning))))
         (bracket (make-string (/ space 2) ?-))
         (warning (concat bracket warning bracket)))
    (setq header-line-format
          (propertize  warning 'face 'find-file-root-header-face))))
(add-hook 'find-file-root-hook 'find-file-root-header-warning)
(global-set-key (kbd "C-x C-r") 'find-file-root)
;;find-file-root ends there---------------------------------------------------------------------------

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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tips
;; Using su to edit stuff as root
;; C-x C-f /su::/etc/hosts RET

;;; tsp-tramp.el ends here
