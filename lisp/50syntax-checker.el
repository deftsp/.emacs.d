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
  (evilified-state-evilify-map flycheck-error-list-mode-map
    :mode flycheck-error-list-mode
    :bindings
    "RET" 'flycheck-error-list-goto-error
    "j" 'flycheck-error-list-next-error
    "k" 'flycheck-error-list-previous-error))

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



(provide '50syntax-checker)
;;; 50syntax-checker ends here
