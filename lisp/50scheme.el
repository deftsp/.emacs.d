;; -*- mode: Emacs-Lisp -*-

;;; gambit
(autoload 'gambit-inferior-mode "gambit" "Hook Gambit mode into cmuscheme.")
(autoload 'gambit-mode "gambit" "Hook Gambit mode into scheme.")

(setq gambit-repl-command-prefix (kbd "ESC ESC g"))
(setq scheme-program-name "gsi -:d-")   ; mzscheme

(defun pl/gambit-remote-repl ()
  (interactive)
  (let ((scheme-program-name "telnet 127.0.0.1 7000"))
    (when (not (comint-check-proc "*scheme*"))
      (kill-buffer (get-buffer "*scheme*"))
      (scheme-interactively-start-process))
    (pop-to-buffer-same-window (get-buffer "*scheme*"))))

;; (add-hook 'scheme-mode-hook (function gambit-mode))
;; (add-hook 'inferior-scheme-mode-hook (function gambit-inferior-mode))


;;; geiser
(setq geiser-racket-binary "/Applications/Racket/bin/racket"
      geiser-mode-auto-p t ; active geiser by default in all scheme buffer
      geiser-racket-collects nil ; it seems gesier can auto find racket collects
      geiser-active-implementations '(racket))

;; scsh
;; (add-to-list 'interpreter-mode-alist '("scsh" . scheme-mode))

;;; Quack
;; http://alexott.net/en/writings/emacs-devenv/EmacsScheme.html
(eval-after-load "geiser" '(require 'quack))
(with-eval-after-load "quack"
  (setq quack-fontify-style nil    ;or 'plt 'emacs
        quack-default-program "gsi -:d-"
        quack-newline-behavior 'indent-newline-indent))

;;; tell emacs about the indentation of some not-so-well-known procedures.
;; (put 'with-error-handler 'scheme-indent-function 1)     ; 'defun)
;; (put 'with-exception-handler 'scheme-indent-function 1)
;; (put 'with-exit-exception-handler 'scheme-indent-function 1)
;; (put 'with-exit-exception-handler* 'scheme-indent-function 2)
;; (put 'my-with-exception-handler 'scheme-indent-function 2)
;; (put 'for-debug 'scheme-indent-function 'defun)
;; (put 'test-expected 'scheme-indent-function 'defun)
;; (put 'call-with-input-string 'scheme-indent-function 1)
;; (put 'with-port-locking 'scheme-indent-function 1)



(provide '50scheme)
