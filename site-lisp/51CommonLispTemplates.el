;;; 50CommonLispTemplates.el ---

;; Copyright (C) 2008  S.P.Tseng

;; Author: S.P.Tseng <deftsp@gmail.com>
;; Keywords:

;;; CLTemplateMakeSystem
;; How tu Use
;; M-x ska-cl-make-system

;; You will be asked for the system's name and then some magic will find whether the current directory can be used, a
;; directory has to be created or where you wan't to create the system

(defun ska-cl-make-system (sysname)
  (interactive "sSystem Name: ")
  ;; make sure we live in a reasonable directory
  (let* ((cpath (directory-file-name default-directory))
         (cwd (file-name-nondirectory cpath)))
    (unless (string= sysname cwd)
      (if (y-or-n-p
           (format "%s is not the current directory. Create it in %s? "
                   sysname cpath))
          (progn
            (make-directory sysname)
            (cd sysname))
          (cd (read-directory-name "Choose system directory: ")))))

  ;; now create files
  (flet ((create-lisp-file (name content)
           (with-temp-buffer
             (let ((author (user-full-name))
                   (copyright (or (and (boundp 'my-copyright-holder)
                                       my-copyright-holder)
                                  (user-full-name)))
                   (year (substring (current-time-string) -4)))
               (insert
                ";;; -*- Mode: LISP; -*-\n"
                "\n"
                ";;; Time-stamp: <>\n"
                ";;; $Id: $\n;;;\n"
                ";;; Copyright (C) " year " by " copyright "\n;;;\n"
                ";;; Author: " author "\n;;;\n\n"))
             (insert content)
             (unless (file-writable-p name)
               (error "%s is not writeable" name))
             (when (or (not (file-exists-p name))
                       (y-or-n-p (concat name " exists. Overwrite? ")))
               (write-region (point-min) (point-max) name)))))
    (create-lisp-file (concat sysname ".asd")
                      (concat "\n(in-package :cl-user)\n\n"
                              "(defpackage :" sysname ".system\n"
                              "  (:use :cl :asdf))\n\n"
                              "(in-package :" sysname ".system)\n\n"
                              "(defsystem :" sysname "\n"
                              "  :serial t\n"
                              "  :components ((:file \"packages\")\n"
                              "               (:file \"specials\")\n"
                              "               (:file \"" sysname "\"))\n"
                              "  :depends-on ())\n"))
    (create-lisp-file "packages.lisp"
                      (concat
                       "(in-package :cl-user)\n\n"
                       "(defpackage :" sysname "\n"
                       "  (:use :cl)\n"
                       "  (:export ))\n\n"))
    (create-lisp-file "specials.lisp"
                      (concat "(in-package :" sysname ")\n"))
    (create-lisp-file (concat sysname ".lisp")
                      (concat "(in-package :" sysname ")\n")))
  (find-file (concat sysname ".lisp"))
  (goto-char (point-max)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; A template for ASDF system files:

(push `(("\\.asd\\'" . "ASDF Skeleton")
        "System Name: "
        "
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :" str ".system)
    (defpackage :" str ".system
      (:use :common-lisp :asdf))))


(in-package :" str ".system)

(defsystem :" str "
  :description " ?\" (read-string "Description: ") ?\""
  :author \"" (user-full-name) " <" user-mail-address ">\"
  :licence \"" (read-string "License: ") "\"
  :version \"" (read-string "Version: ") "\"
  :components (())
  :depends-on ())")
  auto-insert-alist)


;; How do you use this ?
;; It should work automatically if you have auto-insert enabled:
;; (add-hook 'find-file-hooks '(lambda ()
;;                              (auto-insert)))

;; and you open an empty file ending in ".asd"
;; It will ask you about
;; * The name of your package,
;; * a short description,
;; * license you put this package under,
;; * and the version number of you newly created package.

;; Modifications

;; One might want to use TAB completion for the license and default values for the version numnber:
;; Just change two lines:

;; :licence \"" (completing-read "License: " '(("GPL" 1)
;;                                ("BSD" 2))) "\"
;; :version \"" (completing-read "Version: " nil nil nil "0.1") "\"

;; And fill in the missing licenses. Of course this makes it impossible to use a license with spaces in the name that's
;; not on the list. You can always change it later.

;; One could also change the skeleton to not read the system name as a string but with a default of the name of the
;; current directory. This is left as an exercise to the reader or next editor



(provide '51CommonLispTemplates)