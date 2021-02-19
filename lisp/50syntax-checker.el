;;; 50syntax-checker.el ---

;; Copyright (C) 2017  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>

;;; install el-get
;; makeinfo should use /usr/local/Cellar/texinfo/6.0/bin/makeinfo
;; /usr/local/Cellar/texinfo/6.0/bin/makeinfo -o doc/flycheck.info doc/flycheck.texi
;; brew link  --force texinfo

(use-package flycheck
  :defer t
  :init
  (progn
    (setq flycheck-emacs-lisp-load-path 'inherit)
    (when (fboundp 'global-flycheck-mode)
      (add-hook 'after-init-hook #'global-flycheck-mode))
    ;; (set-default 'flycheck-check-syntax-automatically nil)
    (setq flycheck-standard-error-navigation nil)

    ;; https://github.com/mantoni/eslint_d.js
    (setq flycheck-javascript-eslint-executable "eslint_d")

    ;; key bindings
    (tl/set-leader-keys
      "eb" 'flycheck-buffer
      "ec" 'flycheck-clear
      "eh" 'flycheck-describe-checker
      "el" 'tl/toggle-flycheck-error-list
      "eL" 'tl/goto-flycheck-error-list
      "es" 'flycheck-select-checker
      "eS" 'flycheck-set-checker-executable
      "ev" 'flycheck-verify-setup
      "ex" 'flycheck-explain-error-at-point))
  :config
  (with-eval-after-load "evil-evilified-state"
    (evilified-state-evilify-map flycheck-error-list-mode-map
      :mode flycheck-error-list-mode
      :bindings
      "RET" 'flycheck-error-list-goto-error
      "j" 'flycheck-error-list-next-error
      "k" 'flycheck-error-list-previous-error)))

;; toggle flycheck window
(defun tl/toggle-flycheck-error-list ()
  "Toggle flycheck's error list window.
If the error list is visible, hide it.  Otherwise, show it."
  (interactive)
  (-if-let (window (flycheck-get-error-list-window))
      (quit-window nil window)
    (flycheck-list-errors)))

(defun tl/goto-flycheck-error-list ()
  "Open and go to the error list buffer."
  (interactive)
  (unless (get-buffer-window (get-buffer flycheck-error-list-buffer))
    (flycheck-list-errors)
    (switch-to-buffer-other-window flycheck-error-list-buffer)))


;;; flycheck-posframe can not work will with company-posframe
;;; lsp-ui-sideline-mode to show the message on the right
;; https://github.com/alexmurray/flycheck-posframe
(use-package flycheck-posframe
  :after flycheck
  :config
  (setq flycheck-posframe-warning-prefix "➤ ")
  (setq flycheck-posframe-error-prefix "➤ ")
  (set-face-attribute 'flycheck-posframe-warning-face nil :inherit 'warning)
  (set-face-attribute 'flycheck-posframe-error-face nil :inherit 'error)
  (add-hook 'flycheck-mode-hook #'flycheck-posframe-mode))

;; (use-package flycheck-inline
;;   :after flycheck
;;   :init
;;   (add-hook 'flycheck-mode-hook #'flycheck-inline-mode))

;; quick-peek can not work with display-line-numbers-mode
;; [[https://github.com/cpitclaudel/quick-peek/issues/14][Emacs 26 display-line-numbers-mode breaks quick-peek · Issue #14 · cpitclaudel/quick-peek]]
;; (use-package quick-peek
;;   :after flycheck-inline
;;   :config
;;   (defun tl/flycheck-inline-display-quick-peek (msg pos)
;;     (let* ((ov (quick-peek-overlay-ensure-at pos))
;;            (contents (quick-peek-overlay-contents ov)))
;;       (setf (quick-peek-overlay-contents ov)
;;             (concat contents (when contents "\n") msg))
;;       (quick-peek-update ov)))

;;   (setq flycheck-inline-display-function #'tl/flycheck-inline-display-quick-peek
;;         flycheck-inline-clear-function #'quick-peek-hide))

;; (use-package flycheck-grammarly
;;   :after flycheck)

(provide '50syntax-checker)
;;; 50syntax-checker ends here
