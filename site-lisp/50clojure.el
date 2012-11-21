;;; 50clojure.el ---

;; Copyright (C) 2012  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>


;;; clojure mode
;; install clojure-mode with el-get
;; (require 'clojure-mode)
;; (require 'clojure-test-mode) ; Load test support for Clojure

;;; nrepl
;; (add-hook 'nrepl-interaction-mode-hook 'nrepl-turn-on-eldoc-mode)
;; (setq nrepl-popup-stacktraces nil) ; Stop the error buffer from popping up while working in the REPL buffer:
;; (add-to-list 'same-window-buffer-names "*nrepl*")  ; Make C-c C-z switch to the *nrepl* buffer in the current window:
;; (add-hook 'nrepl-interaction-mode 'enable-paredit-mode)

(eval-after-load "clojure-mode"
  '(progn
    (add-hook 'clojure-mode-hook 'enable-paredit-mode)))

(eval-after-load "nrepl"
  '(progn
     (add-hook 'nrepl-mode-hook 'enable-paredit-mode)))


(provide '50clojure)
;;; 50clojure.el ends here
