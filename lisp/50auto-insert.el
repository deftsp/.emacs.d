;;; 50auto-insert.el ---


(add-hook 'find-file-hooks 'auto-insert)
;; (add-hook 'write-file-hooks 'copyright-update)
(setq auto-insert t)
(setq auto-insert-query nil)
(setq auto-insert-directory "~/.emacs.d/templates/")
;;(define-auto-insert "\\.el" lisp-template.el")
;;(define-auto-insert "\\.c" "c-template.c")
;;(define-auto-insert "\\.cpp" "cpp-template.cpp")
(define-auto-insert "\\.txt" "txt-template.txt")
;;(define-auto-insert "\\.php" "my-php-template.php")
(define-auto-insert "\\.asm" "asm-template.asm")
;;(define-auto-insert "\\.scm" "scheme-template.scm")

(define-auto-insert 'sh-mode '(nil "#!/bin/sh\n\n"))

;; (define-auto-insert
;;     '("\\.\\([Hh]\\|hh\\|hxx\\|hpp\\)\\'" . "C / C++ header")
;;     '((upcase (concat "_"
;;                (replace-regexp-in-string
;;                 "[^a-zA-Z0-9]" "_"
;;                 (format "%s_%d_" (file-name-nondirectory buffer-file-name) (random)))))
;;       "#ifndef " str \n
;;       "#define " str "\n\n"
;;       _ "\n\n#endif"))

(defun paloryemacs/auto-inset-copyright ()
  (concat "Copyright (C) "
          (format-time-string "%Y ")
          (user-full-name)
          ". All rights reserved.\n"))

;;; Header files
(define-auto-insert
    (cons "\\.\\([Hh]\\|hpp\\)$" "C/C++ Header File Template")
    '(nil
      "/* "(buffer-name) " ---\n"
      ;; " * $Id$\n"
      " * Author: " (format "%s <%s>" (user-full-name) user-mail-address) "\n"
      " * Created: " (current-time-string) "\n"
      " * Description: \n"
      " * " (paloryemacs/auto-inset-copyright)
      " */\n\n"
      (let* ((nopath (file-name-sans-extension (file-name-nondirectory buffer-file-name)))
             (indent (concat "__" (upcase nopath)
                             "_" (upcase (file-name-extension
                                          buffer-file-name)))))
        (concat "#ifndef " indent "\n"
                "#define " indent "    1" "\n\n\n"
                "#endif /* " indent " */"))
      (previous-line 8)
      (end-of-line)))

;;;C/C++ files
(define-auto-insert
    (cons "\\.\\(c\\|cpp\\)$" "C/C++ Source File Template")
    '(nil
      "/* "(buffer-name) " ---\n"
      ;; " * $Id$\n"
      " * Author: " (format "%s <%s>" (user-full-name) user-mail-address) "\n"
      " * Created: "(current-time-string) "\n"
      " * Description: \n"
      " * " (paloryemacs/auto-inset-copyright)
      " */\n\n"
      ;; (let* ((nopath (file-name-nondirectory buffer-file-name))
      ;;        (noext  (file-name-sans-extension nopath)))
      ;;   (concat  "#include \"" noext ".h\"\n\n"))
      (previous-line 4)
      (end-of-line)))

;;; Lisp files
(define-auto-insert
    (cons "\\.\\(lisp\\)$" "Lisp File Template")
    '(nil
      ";;;; "(buffer-name) " ---\n"
      ;; ";; $Id$\n"
      ";; Author: " (format "%s <%s>" (user-full-name) user-mail-address) "\n"
      ";; Created: "(current-time-string) "\n"
      ";; Description: \n"
      ";; " (paloryemacs/auto-inset-copyright)
      "\n\n"
      (previous-line 4)
      (end-of-line)))

(define-auto-insert
    (cons "\\.\\(scm\\|ss\\)$" "Scheme File Template")
    '(nil
      ";;;; "(buffer-name) " ---\n"
      ;; ";; $Id$\n"
      ";; Author: " (format "%s <%s>" (user-full-name) user-mail-address) "\n"
      ";; Created: "(current-time-string) "\n"
      ";; Description: \n"
      ";; " (paloryemacs/auto-inset-copyright)
      "\n\n"
      (previous-line 4)
      (end-of-line)))



;;; AWK files
;; (define-auto-insert
;;     (cons "\\.\\(awk\\)" "AWK File Template")
;;     '(nil
;;       "#!/bin/bash\n"
;;       "#\n"
;;       "# $Header: $\n"
;;       "# $Log: $\n"
;;       "#\n"
;;       "# \n"
;;       "#\n"
;;       "BEGIN{}\n"
;;       "{\n"
;;       "\n"
;;       "}\n"
;;       (previous-line 6)
;;       (end-of-line)))

;;; HTML files
;; (define-auto-insert
;;     (cons "\\.\\(htm\\)" "HTML File Template")
;;     '(nil
;;       "<!DOCTYPE XHTML PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\">\n"
;;       "<html>\n"
;;       "  <head>\n"
;;       "    <title>\n"
;;       "\n"
;;       "    </title>\n"
;;       "    <meta http-equiv=\"content-type\" content=\"text/html; charset=utf-8\">\n"
;;       "    <meta name=\"generator\" content=\"GNUEmacs\">\n"
;;       "    <meta name=\"keywords\" content=\"TODO\">\n"
;;       "    <meta name=\"description\" content=\"TODO\">\n"
;;       "  </head>\n"
;;       "  <body>\n"
;;       "    \n"
;;       "    <p align=\"right\"><b>Best</b> <i>viewed</i> <a><u>with</u></a> Lynx</p>\n"
;;       "  </body>\n"
;;       "</html>\n"
;;       (previous-line 4)
;;       (end-of-line)))

(provide '50auto-insert)
