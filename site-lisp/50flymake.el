;;; 50flymake.el ---

;; Copyright (C) 2008  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>

;;----------------------------------------------------------------------------------------------------
;; flymake mode
;;----------------------------------------------------------------------------------------------------
;; What you need is only a Makefile with an extra target "check-syntax":

;; check-syntax:
;;       gcc -o nul -Wall -Wextra -fsyntax-only $(CHK_SOURCES)


(eval-after-load "flymake"
  '(progn
     (set-face-attribute 'flymake-warnline nil :foreground "#ccccff" :background "#333300")
     (set-face-attribute 'flymake-errline nil :foreground "#cceecc" :background "#402222")))






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



;; (define-key global-map (kbd "C-c d e") 'flyc/show-fly-error-at-point-now)


;; (add-hook 'find-file-hook 'flymake-find-file-hook)
;; (setq flymake-gui-warnings-enabled nil)
;; (setq flymake-log-level 0)


;;; flymake will excute check-syntax when:
;; open file
;; new line (flymake-start-syntax-check-on-newline)
;; code change after 0.5s (flymake-no-changes-timeout)
;; execute, flymake-start-syntax-check


;; flymake-allowed-file-name-masks




;; (setq flymake-log-level 1)




(provide '50flymake)
;; 50flymode mode ends here------------------------------------------------------------------------------
