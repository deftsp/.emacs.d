;; -*- mode: Emacs-Lisp -*-
;; Time-stamp: <2008-09-07 21:11:28 S.P.Tseng>

(setq gdb-many-windows t)
(setq gdb-use-separate-io-buffer t)
(setq gdb-show-main t)

;; (add-hook 'gud-mode-hook
;;           '(lambda ()
;;             (local-set-key [home]       ; move to beginning of line, after prompt
;;              'comint-bol)
;;             (local-set-key [up]         ; cycle backward through command history
;;              '(lambda () (interactive)
;;                (if (comint-after-pmark-p)
;;                    (comint-previous-input 1)
;;                    (previous-line 1))))
;;             (local-set-key [down]       ; cycle forward through command history
;;              '(lambda () (interactive)
;;                (if (comint-after-pmark-p)
;;                    (comint-next-input 1)
;;                    (forward-line 1))))
;;             ))



;; (defun select-gud-interaction-window ()
;;   (interactive)
;;   (if (and (boundp 'gud-comint-buffer)
;;            gud-comint-buffer
;;            (window-live-p (get-buffer-window gud-comint-buffer)))
;;       (select-window (get-buffer-window gud-comint-buffer))
;;       (message "GUD interaction window is inactive")))

;; (define-key global-map [(f9)] 'select-gud-interaction-window)
