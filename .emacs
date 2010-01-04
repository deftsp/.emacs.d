;;Time-stamp: <2010-01-05 01:46:55 S.P.Tseng>
;; $Id$

;; First, avoid the evil:
(when (featurep 'xemacs)
  (error "This .emacs file (probably) does not work with XEmacs."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; what kind of system are we using?  start with these, as it will influence
;; other stuff inspired by: http://www.xsteve.at/prg/emacs/.emacs.txt
(defconst win32-p (eq system-type 'windows-nt)
  "Are we running on a WinTel system?")
(defconst linux-p (or (eq system-type 'gnu/linux) (eq system-type 'linux))
  "Are we running on a GNU/Linux system?")
(defconst console-p (eq (symbol-value 'window-system) nil)
  "Are we running in a console (non-X) environment?")


(setq emacs-load-start-time (current-time))
(setq debug-on-error t)                 ;will be cleared at end of buffer
;; (setq stack-trace-on-error t)

;; I use the Common Lisp stuff all the time
(require 'cl)

;; Load init-settings-path
(add-to-list 'load-path "~/.emacs.d/packages")
(if (fboundp 'normal-top-level-add-subdirs-to-load-path)
    (let* ((my-lisp-dir "~/.emacs.d/packages/")
	   (default-directory my-lisp-dir))
      (setq load-path (cons my-lisp-dir load-path))
      (normal-top-level-add-subdirs-to-load-path)))
;; If you only want some of the subdirectories added you can use
;; (normal-top-level-add-to-load-path '("emms" "erc" "planner" "w3"))
;; (mapc 'load (directory-files "~/.emacs.d/site-lisp" t "\.el$"))

(load "~/.emacs.d/site-lisp/00site-start.el")


;; (let ((default-directory "~/.emacs.d/"))
;;   (add-to-list 'load-path default-directory)
;;   (normal-top-level-add-subdirs-to-load-path))

;; (defun load-directory (dir)
;;   (mapcar '(lambda (x)
;;              (load-file x))
;;           (directory-files dir t "\\.el$")))

;; (load-directory "~/.emacs.d/customizations/")
;; (load-directory "~/.emacs.d/utility/")


;;; init end ther ----------------------------------------------------------------


;; A fun startup message, somewhat reminiscent of "The Matrix: Reloaded"
(defconst animate-n-steps 3)
(defun emacs-reloaded ()
  (animate-string (concat ";; Initialization successful, welcome to "
                          (substring (emacs-version) 0 16) ".") 0 0)
  (newline-and-indent) (newline-and-indent))

(add-hook 'after-init-hook 'emacs-reloaded)

(setq debug-on-error nil)                 ; was set t at top of buffer
;; (setq stack-trace-on-error nil)

(when (require 'time-date nil t)
  (message "Emacs startup time: %d seconds." (time-to-seconds (time-since emacs-load-start-time))))


;; Tips
;; If you want to require lots of module you do like this
;; (dolist (module '(module1
;;                   module2
;;                   module3))
;;   (require module))


;; boot sequence
;; site-start.el --> .emacs --> default.el and terminal type file.
