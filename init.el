;; -*- mode: emacs-lisp; coding: utf-8 -*-

;;; boot sequence
;; site-start.el --> .emacs --> default.el and terminal type file.

(setq read-process-output-max (* 1024 1024)) ;; 1mb

(setq byte-compile-warnings '(cl-functions))
;; want to see how often GC happens
;; (setq garbage-collection-messages nil)

(setq native-comp-deferred-compilation nil
      native-comp-async-report-warnings-errors 'slient)

(require 'comp)
(with-eval-after-load 'comp
  ;; https://github.com/raxod502/straight.el/issues/680
  (add-to-list 'native-comp-deferred-compilation-deny-list "\\(?:[^z-a]*-autoloads\\.el$\\)"))

;; always load the newer one between .el and .elc
;; https://www.mattduck.com/2021-05-upgrading-to-emacs-28.html
;; https://github.com/bbatsov/prelude/issues/1134
(setq load-prefer-newer nil)
(require 'jka-compr)
(setq load-prefer-newer t)

;;; package
;; (setq package-archives
;;       '(("marmalade" . "https://marmalade-repo.org/packages/")
;;         ("gnu" . "https://elpa.gnu.org/packages/")
;;         ("melpa" . "https://melpa.org/packages/")
;;         ("elpy" . "https://jorgenschaefer.github.io/packages/")
;;         ("org" . "http://orgmode.org/elpa/")))

;; (setq package-check-signature nil)

;; (package-initialize)

(when (featurep 'xemacs)
  (error "This .emacs file (probably) does not work with XEmacs."))

(when (version< emacs-version "28.0.50")
  (error "This .emacs file (probably) work with Emacs >= 28.0.50"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; what kind of system are we using?  start with these, as it will influence
;; other stuff inspired by: http://www.xsteve.at/prg/emacs/.emacs.txt
(defconst win32-p (eq system-type 'windows-nt)
  "Are we running on a WinTel system?")
(defconst linux-p (or (eq system-type 'gnu/linux) (eq system-type 'linux))
  "Are we running on a GNU/Linux system?")
(defconst mac-p (eq system-type 'darwin) "Are we running on Macintosh system?")
(defconst console-p (eq (symbol-value 'window-system) nil)
  "Are we running in a console (non-X) environment?")


(setq emacs-load-start-time (current-time))
(setq debug-on-error t)                 ;will be cleared at end of buffer
;; http://pages.sachachua.com/.emacs.d/Sacha.html
;; While edebugging, use T to view a trace buffer (*edebug-trace*). Emacs will quickly execute the rest of your code,
;; printing out the arguments and return values for each expression it evaluates.
(setq edebug-trace t)

;;;  Load Path stuff
(unless (boundp 'user-emacs-directory)
  (defvar user-emacs-directory "~/.emacs.d/"
    "Directory beneath which additional per-user Emacs-specificfiles are placed. Various programs in
  Emacs store information in this directory. Note that this should end with a directory separator.
  See also `locate-user-emacs-file'."))

(defvar user-package-directory (concat user-emacs-directory "packages/"))

(add-to-list 'load-path user-package-directory)
(add-to-list 'load-path "~/.emacs.d/lisp/")
(add-to-list 'load-path "~/.emacs.d/private/")


(if (fboundp 'normal-top-level-add-subdirs-to-load-path)
    (let* ((default-directory (expand-file-name "~/.emacs.d/packages/")))
      ;; If you only want some of the subdirectories added you can use
      ;; (normal-top-level-add-to-load-path '("cedet"))
      (normal-top-level-add-subdirs-to-load-path)))


;;; load custom file
(setq custom-file "~/.emacs.d/custom.el")
(when (file-exists-p custom-file)
  (load custom-file 'noerror))

(require 'tl-bootstrap)

(setq debug-on-error nil)                 ; was set to t at top of buffer

(when (require 'time-date nil t)
  (message "Emacs startup time: %d seconds." (time-to-seconds (time-since emacs-load-start-time))))

;;; init end there
