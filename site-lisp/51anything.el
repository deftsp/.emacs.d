;;; 51anything.el ---

;; Copyright (C) 2008  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>

;; (autoload 'anything "anything"
;;   "Select anything. In Lisp program, some optional arguments can be used.

;; Note that all the optional arguments are prefixed because of
;; dynamic scope problem, IOW argument variables may eat
;; already-bound variables. Yuck!
;; " t)


;;; Note: anything-config.el loads anything.el.
(require 'anything-config)

;;; anything-match-plugin.el extends pattern matching. Some Anything
;;; Applications requires it. It is a must-have plugin now.
;;;
(require 'anything-match-plugin)

;;; anything-show-completion.el shows current selection prettily.
(require 'anything-show-completion)

;;; descbinds-anything.el replaces describe-bindings with anything interface.
(when (require 'descbinds-anything nil t)
  ;; Comment if you do not want to replace `describe-bindings' with `anything'.
  (descbinds-anything-install))

;;; `anything-grep' replaces standard `grep' command.
(require 'anything-grep nil t)


;; (setq anything-idle-delay 0.3           ; default 0.5
;;       anything-input-idle-delay 0.2       ; default 0.1
;;       anything-persistent-action-use-special-display nil)

(global-set-key (kbd "M-X") 'anything)


(eval-after-load "anything"
  '(progn
    ;; (require 'anything-config)
    ;; (require 'anything-emms)
    ;; (require 'anything-match-plugin)
     ;; (setq anything-sources '(anything-c-source-call-source
     ;;                         anything-c-source-buffers+
     ;;                         anything-c-source-bbdb
     ;;                         ;; anything-c-source-file-name-history
     ;;                         anything-c-source-recentf
     ;;                         ;; anything-c-source-occur
     ;;                         ;; anything-c-source-info-pages
     ;;                         anything-c-source-man-pages
     ;;                         ;; anything-c-source-w3m-bookmarks
     ;;                         ;; anything-c-source-locate
     ;;                         ;; anything-c-source-file-cache
     ;;                         ;; anything-c-source-emacs-commands
     ;;                         anything-c-source-buffer-not-found))
    ;; (define-key anything-map "\M-p" 'anything-previous-source)
    ;; (define-key anything-map "\M-n" 'anything-next-source)
    ;; (define-key anything-map "A" 'anything-deftsp-show-all)
    ;; (define-key anything-map "T" 'anything-deftsp-show-traverse-only)
    ;; (define-key anything-map "L" 'anything-deftsp-show-locate-only)
    ;; (define-key anything-map "O" 'anything-deftsp-show-occur-only)
     (set-face-attribute 'anything-candidate-number nil :foreground "Yellow" :background nil)))


(eval-after-load "anything-config"
  '(progn
     ;; (set-face-foreground 'anything-dir-priv "maroon4")
     ;; (set-face-background 'anything-dir-priv "slate gray")
     ;; (global-set-key (kbd "C-x C-f") 'anything-find-files)
     ;; (global-set-key (kbd "C-x b") 'anything-buffers+)
     (define-key anything-find-files-map (kbd "<S-tab>") 'anything-ff-run-complete-fn-at-point)
     (define-key anything-find-files-map (kbd "C-k") 'anything-erase-minibuffer-or-dwim)))


(defun anything-erase-minibuffer-or-dwim ()
  "If cursor the EOL erases whole minibuffer and insert  `~/'.
If cursor at the EOL and the whole minibuffer is `~/', erase whole minibuffer.
Or else erases whole minibuffer. "
  (interactive)
  (if (eolp)
      (if (string= anything-pattern "~/")
          (anything-set-pattern "/")
        (anything-set-pattern "~/"))
    (kill-line)))



;; (setq anything-save-configuration-functions
;;       '(set-window-configuration . current-window-configuration))

;; ;; (install-elisp-from-emacswiki "anything-auto-install.el")
;; (require 'anything-auto-install)

;; ;; (install-elisp-from-emacswiki "anything-match-plugin.el")
;; (require 'anything-match-plugin)


;; ;; --------------------------------------------------------------------------------

;; (eval-after-load "anything"
;;   '(progn
;;     (defun my-anything-show-kill-ring ()
;;       (interactive)
;;       (let ((anything-enable-digit-shortcuts nil))
;;         (anything-show-kill-ring)))

;;     ;; (global-set-key (kbd "M-%") 'anything-query-replace-regexp)
;;     (global-set-key (kbd "M-Y") 'my-anything-show-kill-ring)))

;; ;; if anything-c-source-bbdb is not exist, use bbdb directly.
;; (defun any-bbdb ()
;;   (interactive)
;;   (if anything-c-source-bbdb
;;       (anything '(anything-c-source-bbdb))
;;       (bbdb)))


;; (defun anything-occur ()
;;   (interactive)
;;   (if anything-c-source-occur
;;       (anything '(anything-c-source-occur))
;;       (occur)))

;; ;; (defalias 'occur 'anything-occur)

;; (defun anything-man ()
;;   (interactive)
;;   (if anything-c-source-man-pages
;;       (anything '(anything-c-source-man-pages) nil "man pattern: ")
;;       (man)))

;; ;;; anything-complete
;; (require 'anything-complete)
;; ;; Automatically collect symbols by 150 secs
;; ;; (anything-lisp-complete-symbol-set-timer 150)
;; ;; replace completion commands with `anything'
;; ;; (anything-read-string-mode 1)
;; ;; Bind C-o to complete shell history
;; ;; (anything-complete-shell-history-setup-key "\C-o")

;; (autoload 'anything-el-swank-fuzzy-indent-and-complete-symbol "anything-el-swank-fuzzy"
;;   "Indent the current line and perform `anything-el-swank-fuzzy-complete-symbol'." t)

;; (define-key emacs-lisp-mode-map (kbd "TAB")
;;   'anything-el-swank-fuzzy-indent-and-complete-symbol)

;; (define-key lisp-interaction-mode-map (kbd "TAB")
;;   'anything-el-swank-fuzzy-indent-and-complete-symbol)

;; (defun anything-deftsp-show-all ()
;;   (interactive)
;;   (anything-set-source-filter nil))

;; (defun anything-deftsp-show-traverse-only ()
;;   (interactive)
;;   (anything-set-source-filter '("Traverse Occur")))

;; (defun anything-deftsp-show-locate-only ()
;;   (interactive)
;;   (anything-set-source-filter '("Locate")))

;; (defun anything-deftsp-show-occur-only ()
;;   (interactive)
;;   (anything-set-source-filter '("Occur")))