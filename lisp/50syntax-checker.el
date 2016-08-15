;;; 50syntax-checker.el ---

;; Copyright (C) 2008  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>

;;; flymake will excute check-syntax when:
;; open file
;; new line (flymake-start-syntax-check-on-newline)
;; code change after 0.5s (flymake-no-changes-timeout)
;; execute, flymake-start-syntax-check


;; What you need is only a Makefile with an extra target "check-syntax":

;; check-syntax:
;;       gcc -o nul -Wall -Wextra -fsyntax-only $(CHK_SOURCES)

;; (eval-after-load "flymake"
;;  '(progn
;;     (require 'flymake-cursor)
;;     (setq flymake-gui-warnings-enabled nil)))


;;; install el-get
;; makeinfo should use /usr/local/Cellar/texinfo/6.0/bin/makeinfo
;; /usr/local/Cellar/texinfo/6.0/bin/makeinfo -o doc/flycheck.info doc/flycheck.texi
;; brew link  --force texinfo
;; (require 'flycheck nil t)
;; (set-default 'flycheck-check-syntax-automatically nil)

(when (fboundp 'global-flycheck-mode)
  (add-hook 'after-init-hook #'global-flycheck-mode))

;; Generalized next-error system
(defun paloryemacs//error-delegate ()
  "Decide which error API to delegate to.

Delegates to flycheck if it is enabled and the next-error buffer
is not visible. Otherwise delegates to regular Emacs next-error."
  (if (and (bound-and-true-p flycheck-mode)
           (let ((buf (ignore-errors (next-error-find-buffer))))
             (not (and buf (get-buffer-window buf)))))
      'flycheck
    'emacs))

(defun paloryemacs/next-error (&optional n reset)
  "Dispatch to flycheck or standard emacs error."
  (interactive "P")
  (let ((sys (paloryemacs//error-delegate)))
    (cond
     ((eq 'flycheck sys) (call-interactively 'flycheck-next-error))
     ((eq 'emacs sys) (call-interactively 'next-error)))))

(defun paloryemacs/previous-error (&optional n reset)
  "Dispatch to flycheck or standard emacs error."
  (interactive "P")
  (let ((sys (paloryemacs//error-delegate)))
    (cond
     ((eq 'flycheck sys) (call-interactively 'flycheck-previous-error))
     ((eq 'emacs sys) (call-interactively 'previous-error)))))



;; (define-key global-map (kbd "C-c d e") 'flymake-cursor-show-errors-at-point-now)
;; (add-hook 'find-file-hook 'flymake-find-file-hook)
;; (setq flymake-gui-warnings-enabled nil)
;; (setq flymake-log-level 0) ; 1

;; flymake-allowed-file-name-masks

;;; elisp
;; (defun flymake-elisp-init ()
;;   (unless (string-match "^ " (buffer-name))
;;     (let* ((temp-file   (flymake-init-create-temp-buffer-copy
;;                          'flymake-create-temp-inplace))
;;            (local-file  (file-relative-name
;;                          temp-file
;;                          (file-name-directory buffer-file-name))))
;;       (list
;;        (expand-file-name invocation-name invocation-directory)
;;        (list
;;         "-Q" "--batch" "--eval"
;;         (prin1-to-string
;;          (quote
;;           (dolist (file command-line-args-left)
;;             (with-temp-buffer
;;               (insert-file-contents file)
;;               (emacs-lisp-mode)
;;               (let ((parse-sexp-ignore-comments t))
;;                 (condition-case data
;;                     (scan-sexps (point-min) (point-max))
;;                   (scan-error
;;                    (goto-char(nth 2 data))
;;                    (princ (format "%s:%s: error: Unmatched bracket or quote\n"
;;                                   file (line-number-at-pos))))))))))
;;         local-file)))))

;; (push '("\\.el$" flymake-elisp-init) flymake-allowed-file-name-masks)

;; (add-hook 'emacs-lisp-mode-hook
;;           ;; workaround for (eq buffer-file-name nil)
;;           (function (lambda () (if buffer-file-name (flymake-mode)))))

;;; objc
;; (defvar xcode:gccver "4.2")
;; (defvar xcode:sdkver "4.3")
;; (defvar xcode:sdkpath "/Developer/Platforms/iPhoneSimulator.platform/Developer")
;; (defvar xcode:sdk (concat xcode:sdkpath "/SDKs/iPhoneSimulator" xcode:sdkver ".sdk"))
;; (defvar flymake-objc-compiler (concat xcode:sdkpath "/usr/bin/gcc-" xcode:gccver))
;; (defvar flymake-objc-compile-default-options (list "-Wall" "-Wextra" "-fsyntax-only" "-ObjC" "-std=c99" "-isysroot" xcode:sdk))
;; (defvar flymake-last-position nil)
;; (defvar flymake-objc-compile-options '("-I."))


;; (defun flymake-objc-init ()
;;   (let* ((temp-file (flymake-init-create-temp-buffer-copy
;;                      'flymake-create-temp-inplace))
;;          (local-file (file-relative-name
;;                       temp-file
;;                       (file-name-directory buffer-file-name))))
;;     (list flymake-objc-compiler (append flymake-objc-compile-default-options flymake-objc-compile-options (list local-file)))))


;; (add-hook 'objc-mode-hook
;;           (lambda ()
;;             (push '("\\.m$" flymake-objc-init) flymake-allowed-file-name-masks)
;;             (push '("\\.mm$" flymake-objc-init) flymake-allowed-file-name-masks)
;;             (push '("\\.h$" flymake-objc-init) flymake-allowed-file-name-masks)
;;             (if (and (not (null buffer-file-name)) (file-writable-p buffer-file-name))
;;                 (flymake-mode t))))

;;; Flymake

;; (defun flymake-ruby-init ()
;;   (let* ((temp-file (flymake-init-create-temp-buffer-copy
;;                      'flymake-create-temp-inplace))
;;          (local-file (file-relative-name
;;                       temp-file
;;                       (file-name-directory buffer-file-name))))
;;     ;; Invoke ruby with '-c' to get syntax checking
;;     (list "ruby" (list "-c" local-file))))




(provide '50syntax-checker)
;;; 50syntax-checker ends here
