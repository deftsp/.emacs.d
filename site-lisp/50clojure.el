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

;; (setq nrepl-popup-stacktraces-in-repl t) ; enable error buffer popping also in the REPL:

(eval-after-load "clojure-mode"
  '(progn
     (if (fboundp 'rainbow-delimiters-mode)
         (add-hook 'clojure-mode-hook 'rainbow-delimiters-mode))
     (add-hook 'clojure-mode-hook 'subword-mode)
     ;; (add-hook 'clojure-mode-hook 'enable-paredit-mode)
     (add-hook 'clojure-mode-hook 'nrepl-turn-on-eldoc-mode)))

(eval-after-load "nrepl"
  '(progn
     ;; (add-hook 'nrepl-mode-hook 'enable-paredit-mode)
     (add-hook 'nrepl-mode-hook 'subword-mode)))


(provide '50clojure)
;;; 50clojure.el ends here
