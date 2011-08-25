;;; 50flymake.el ---

;; Copyright (C) 2008  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>

;;----------------------------------------------------------------------------------------------------
;; flymake mode
;;----------------------------------------------------------------------------------------------------
;; What you need is only a Makefile with an extra target "check-syntax":

;; check-syntax:
;;       gcc -o nul -Wall -Wextra -fsyntax-only $(CHK_SOURCES)

;; (autoload 'flymake-find-file-hook "flymake" "" t)

(require 'flymake)


(defvar xcode:gccver "4.2")
(defvar xcode:sdkver "4.3")
(defvar xcode:sdkpath "/Developer/Platforms/iPhoneSimulator.platform/Developer")
(defvar xcode:sdk (concat xcode:sdkpath "/SDKs/iPhoneSimulator" xcode:sdkver ".sdk"))
(defvar flymake-objc-compiler (concat xcode:sdkpath "/usr/bin/gcc-" xcode:gccver))
(defvar flymake-objc-compile-default-options (list "-Wall" "-Wextra" "-fsyntax-only" "-ObjC" "-std=c99" "-isysroot" xcode:sdk))
(defvar flymake-last-position nil)
(defvar flymake-objc-compile-options '("-I."))


(defun flymake-objc-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-inplace))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
    (list flymake-objc-compiler (append flymake-objc-compile-default-options flymake-objc-compile-options (list local-file)))))


;; (add-hook 'objc-mode-hook
;;           (lambda ()
;;             (push '("\\.m$" flymake-objc-init) flymake-allowed-file-name-masks)
;;             (push '("\\.mm$" flymake-objc-init) flymake-allowed-file-name-masks)
;;             (push '("\\.h$" flymake-objc-init) flymake-allowed-file-name-masks)
;;             (if (and (not (null buffer-file-name)) (file-writable-p buffer-file-name))
;;                 (flymake-mode t))))



(defun flymake-display-err-minibuffer ()
  (interactive)
  (let* ((line-no (flymake-current-line-no))
         (line-err-info-list (nth 0 (flymake-find-err-info flymake-err-info line-no)))
         (count (length line-err-info-list)))
    (while (> count 0)
      (when line-err-info-list
        (let* ((file (flymake-ler-file (nth (1- count) line-err-info-list)))
               (full-file (flymake-ler-full-file (nth (1- count) line-err-info-list)))
               (text (flymake-ler-text (nth (1- count) line-err-info-list)))
               (line (flymake-ler-line (nth (1- count) line-err-info-list))))
          (message "[%s] %s" line text)))
      (setq count (1- count)))))

(defadvice flymake-goto-next-error (after display-message activate compile)
  (flymake-display-err-minibuffer))

(defadvice flymake-goto-prev-error (after display-message activate compile)
  (flymake-display-err-minibuffer))

(defadvice flymake-mode (before post-command-stuff activate compile)
  (set (make-local-variable 'post-command-hook)
       (add-hook 'post-command-hook 'flymake-display-err-minibuffer)))

(define-key global-map (kbd "C-c d e") 'flymake-display-err-minibuffer)

(set-face-background 'flymake-warnline "SlateGray4")
(set-face-attribute 'flymake-errline nil :foreground "black" :background "IndianRed")


;; (add-hook 'find-file-hook 'flymake-find-file-hook)
;; (setq flymake-gui-warnings-enabled nil)
;; (setq flymake-log-level 0)


;;; flymake will excute check-syntax when:
;; open file
;; new line (flymake-start-syntax-check-on-newline)
;; code change after 0.5s (flymake-no-changes-timeout)
;; execute, flymake-start-syntax-check


;; flymake-allowed-file-name-masks


;;; align
;; (when (require 'align nil t)
;;   (setq xsteve-c-align-rules-list
;;         `((c-comment-one-line
;;            (regexp . "[^- \t]\\(\\s-*\\)/\\*.*\\*/$")
;;            (group  . 1)
;;            (repeat . nil))

;;           (c-macro-definition
;;            (regexp   . "^\\s-*#\\s-*define\\s-+\\S-+\\(\\s-+\\)"))

;;           (c-macro-line-continuation
;;            (regexp   . "\\(\\s-*\\)\\\\$")
;;            (column   . c-backslash-column))

;;           (c-variable-declaration
;;            (regexp   . ,(concat "[*&0-9A-Za-z_]>?[&*]*\\(\\s-+[*&]*\\)"
;;                                 "[A-Za-z_][0-9A-Za-z:_]*\\s-*\\(\\()\\|"
;;                                 "=[^=\n].*\\|(.*)\\|\\(\\[.*\\]\\)*\\)?"
;;                                 "\\s-*[;,]\\|)\\s-*$\\)"))
;;            (group    . 1)
;;            (justify  . t)
;;            (valid
;;             . ,(function
;;                 (lambda ()
;;                   (not (or (save-excursion
;;                              (goto-char (match-beginning 1))
;;                              (backward-word 1)
;;                              (looking-at
;;                               "\\(goto\\|return\\|new\\|delete\\|throw\\)"))
;;                            (if (and (boundp 'font-lock-mode) font-lock-mode)
;;                                (eq (cadr (memq 'face (text-properties-at (point))))
;;                                    'font-lock-comment-face)
;;                              (eq (caar (c-guess-basic-syntax)) 'c))))))))

;;           (c-assignment
;;            (regexp   . ,(concat "[^-=!^&*+<>/| \t\n]\\(\\s-*[-=!^&*+<>/|]*\\)"
;;                                 "=\\(\\s-*\\)\\([^= \t\n]\\|$\\)"))
;;            (group    . (1 2))
;;            (justify  . t)
;;            (tab-stop . nil))

;;           (c-chain-logic
;;            (regexp   . "\\(\\s-*\\)\\(&&\\|||\\|\\<and\\>\\|\\<or\\>\\)")
;;            (modes    . align-c++-modes)
;;            (valid    . ,(function
;;                          (lambda ()
;;                            (save-excursion
;;                              (goto-char (match-end 2))
;;                              (looking-at "\\s-*\\(/[*/]\\|$\\)"))))))
;;           ))

;;   (add-hook 'c-mode-hook (lambda () (setq align-mode-rules-list xsteve-c-align-rules-list))))

;;flymode mode ends here------------------------------------------------------------------------------
