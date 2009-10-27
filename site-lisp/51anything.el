;;; tsp-anything.el ---

;; Copyright (C) 2008  S.P.Tseng

;; Author: S.P.Tseng <deftsp@gmail.com>

;; (autoload 'anything "anything"
;;   "Select anything. In Lisp program, some optional arguments can be used.

;; Note that all the optional arguments are prefixed because of
;; dynamic scope problem, IOW argument variables may eat
;; already-bound variables. Yuck!
;; " t)

(require 'anything nil t)
(setq anything-idle-delay 0.3           ; default 0.5
      anything-input-idle-delay 0.5)    ; default 0.1

(eval-after-load "anything"
  '(progn
    (require 'anything-config)
    ;; (require 'anything-emms)
    ;; (require 'anything-match-plugin)
    (setq anything-sources (list
                            anything-c-source-call-source
                            anything-c-source-buffers
                            anything-c-source-buffer-not-found
                            anything-c-source-bbdb
                            ;; anything-c-source-file-name-history
                            anything-c-source-recentf
                            anything-c-source-occur
                            anything-c-source-info-pages
                            anything-c-source-man-pages
                            anything-c-source-w3m-bookmarks
                            anything-c-source-locate
                            ;; anything-c-source-file-cache
                            anything-c-source-emacs-commands))
    (define-key anything-map "\M-p" 'anything-previous-source)
    (define-key anything-map "\M-n" 'anything-next-source)))

(global-set-key (kbd "M-X") 'anything)

(setq anything-save-configuration-functions
      '(set-window-configuration . current-window-configuration))

;; (install-elisp-from-emacswiki "anything-auto-install.el")
(require 'anything-auto-install)

;; (install-elisp-from-emacswiki "anything-match-plugin.el")
(require 'anything-match-plugin)


;; --------------------------------------------------------------------------------

(eval-after-load "anything"
  '(progn
    (defun my-anything-show-kill-ring ()
      (interactive)
      (let ((anything-enable-digit-shortcuts nil))
        (anything-show-kill-ring)))

    ;; (global-set-key (kbd "M-%") 'anything-query-replace-regexp)
    (global-set-key (kbd "M-Y") 'my-anything-show-kill-ring)))

;; if anything-c-source-bbdb is not exist, use bbdb directly.
(defun any-bbdb ()
  (interactive)
  (if anything-c-source-bbdb
      (anything '(anything-c-source-bbdb))
      (bbdb)))


(defun any-occur ()
  (interactive)
  (if anything-c-source-occur
      (anything '(anything-c-source-occur))
      (occur)))

(defalias 'occur 'any-occur)

(defun any-man ()
  (interactive)
  (if anything-c-source-man-pages
      (anything '(anything-c-source-man-pages) nil "man pattern: ")
      (man)))

;;; anything-complete
(require 'anything-complete)
;; Automatically collect symbols by 150 secs
;; (anything-lisp-complete-symbol-set-timer 150)
;; replace completion commands with `anything'
;; (anything-read-string-mode 1)
;; Bind C-o to complete shell history
;; (anything-complete-shell-history-setup-key "\C-o")

(autoload 'anything-el-swank-fuzzy-indent-and-complete-symbol "anything-el-swank-fuzzy"
  "Indent the current line and perform `anything-el-swank-fuzzy-complete-symbol'." t)

(define-key emacs-lisp-mode-map (kbd "TAB")
  'anything-el-swank-fuzzy-indent-and-complete-symbol)

