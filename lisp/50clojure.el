;;; 50clojure.el ---

;; Copyright (C) 2012  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>

;; install clojure-mode and cider with el-get

(defun tl/clojure-mode-init ()
  (when (fboundp 'rainbow-delimiters-mode)
    (rainbow-delimiters-mode +1))
  ;; indentation
  ;; http://jbm.io/2013/11/custom-indentation-in-clojure-mode/
  (put-clojure-indent 'match 1) ; equal to (put 'match 'clojure-indent-function 1)
  (put 'macrolet 'clojure-backtracking-indent '((2) 2))
  (subword-mode +1)
  (smartparens-strict-mode +1))

(with-eval-after-load "clojure-mode"
  (add-hook 'clojure-mode-hook 'tl/clojure-mode-init))


(defun tl/cider-mode-init ()
  (setq nrepl-log-messages t
        ;; hide the *nrepl-connection* and *nrepl-server* buffers from appearing
        ;; in some buffer switching commands like switch-to-buffer
        nrepl-hide-special-buffers t
        nrepl-buffer-name-separator "-"
        nrepl-buffer-name-show-port t
        ;; nrepl-popup-stacktraces-in-repl t ; enable error buffer popping also in the REPL:
        cider-repl-tab-command 'cider-repl-indent-and-complete-symbol ; 'indent-for-tab-command
        cider-prefer-local-resources t
        cider-repl-pop-to-buffer-on-connect t
        cider-show-error-buffer t
        cider-repl-display-in-current-window nil

        ;; cider-repl-use-clojure-font-lock t
        ;; cider-known-endpoints '(("host-a" "10.10.10.1" "7888") ("host-b" "7888"))
        cider-repl-result-prefix ";; => "
        cider-interactive-eval-result-prefix ";; => ")
  (cider-turn-on-eldoc-mode +1))

(with-eval-after-load "cider"
  (add-hook 'cider-mode-hook 'tl/cider-mode-init))


(provide '50clojure)
;;; 50clojure.el ends here
