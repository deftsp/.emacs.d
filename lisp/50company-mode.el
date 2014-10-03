;;; 50company-mode.el ---                            -*- lexical-binding: t; -*-

;; Copyright (C) 2014  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>
;; Keywords:

;;; Code:
;; el-get install company company-ghc company-cabal
;; when the completion candidates are shown,
;; press <f1> to display the documentation for the selected candidate,
;; or C-w to see its source. Not all back-ends support this.

(add-hook 'after-init-hook 'global-company-mode)

(eval-after-load 'company
  '(progn
     company-require-match nil ; company-explicit-action-p
     (setq company-show-numbers t
           company-clang-insert-arguments nil
           company-dabbrev-downcase nil
           ;; company-begin-commands '(self-insert-command)
           ;; company-backends (delete 'company-ropemacs company-backends)
           ;; company-backends (delete 'company-capf company-backends)
           company-idle-delay 0.2)

     (define-key company-active-map (kbd "M-j") 'company-select-next)
     (define-key company-active-map (kbd "M-k") 'company-select-previous)
     (key-chord-define company-active-map "df" 'company-abort)
     (add-to-list 'company-backends 'company-cmake)))


(provide '50company-mode)
;;; 50company-mode.el ends here
