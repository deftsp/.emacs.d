;;; 50sideline.el ---

;; Copyright (C) 2024  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>

(use-package sideline
  :defer t
  :init
  :config
  (setq sideline-backends-left-skip-current-line t   ; don't display on current line (left)
        sideline-backends-right-skip-current-line t  ; don't display on current line (right)
        sideline-order-left 'down                    ; or 'up
        sideline-order-right 'up                     ; or 'down
        sideline-format-left "%s   "                 ; format for left aligment
        sideline-format-right "   %s"                ; format for right aligment
        sideline-priority 100                        ; overlays' priority
        sideline-display-backend-name t))            ; display the backend name

;; (use-package sideline-eldoc)

(use-package sideline-flymake
  :after sideline
  ;; :hook (flymake-mode . sideline-mode)
  :init
  ;; 'point to show errors only on point
  (setq sideline-flymake-display-mode 'point)
  ;; 'line to show errors on the current line
  (setq sideline-backends-right '(sideline-flymake)))


(provide '50sideline)
;;; 50sideline ends here
