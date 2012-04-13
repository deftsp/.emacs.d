;;; 50nethack.el ---

;; Copyright (C) 2008  S.P.Tseng

;; Author: S.P.Tseng <deftsp@gmail.com>
;; Keywords:

;;Well, nethack, and nethack-el are installed, so let's modify Nethack to make it usable.
;; (eval-after-load "nethack"
;;   '(progn                               ;20 years and Nethack still doesn't support the D-pad...
;;     (define-key nh-map-mode-map (kbd "<left>") 'nethack-command-west)
;;     (define-key nh-map-mode-map (kbd "<up>") 'nethack-command-north)
;;     (define-key nh-map-mode-map (kbd "<down>") 'nethack-command-south)
;;     (define-key nh-map-mode-map (kbd "<right>") 'nethack-command-east)
;;     (define-key nh-map-mode-map (kbd "<home>") 'nethack-command-northwest)
;;     (define-key nh-map-mode-map (kbd "<prior>") 'nethack-command-northeast)
;;     (define-key nh-map-mode-map (kbd "<end>")  'nethack-command-southwest)
;;     (define-key nh-map-mode-map (kbd "<next>") 'nethack-command-southeast)

;;     (defun nethack-x-timestamp-message ()
;;       "Add a time-stamp to every message." ;allows for easier tracking of things
;;       (insert (format "(%d) " (elt nh-status-attribute-T 0))))
;;     (add-hook 'nethack-before-print-message-hook
;;      'nethack-x-timestamp-message)

;;     (defun nethack-x-warn-low-hp (attr new old)
;;       "Print a message in `nh-message-buffer' when hitpoints get low."
;;       (if (and (string-equal attr "HP")
;;                (< new old)
;;                (< (/ new (float (car nh-status-attribute-HPmax))) 0.20))
;;           (nhapi-message 'atr-blink "Hitpoints below 20%")))
;;     (add-hook 'nethack-status-attribute-change-functions 'nethack-x-warn-low-hp)

;;     (defvar nethack-x-highlights '((" blessed " . nethack-green-face)
;;                                    (" holy " . nethack-green-face)
;;                                    (" cursed " . nethack-red-face)
;;                                    (" unholy " . nethack-green-face)
;;                                    (" cursed .* (being worn)" . nethack-orange-face))
;;       "An assoc of regexps and font colors")
;;     (add-hook 'nethack-add-menu-hook 'nethack-x-highlight-option)

;;     (defun nethack-x-highlight-option ()
;;       "Highlight a nethack menu option based on a regexp."
;;       ;; Move to the beginning of the option just added
;;       (save-excursion
;;         (let (startuptime.
;;               (end (point)))
;;           (forward-line -1)
;;           (forward-line 0)
;;           ;; A mini-hack so the option accelerator doesn't get highlighted
;;           (setq start (+ (point) 4))
;;           (mapc (lambda (x)
;;                   (if (re-search-forward (car x) nil t)
;;                       (put-text-property start end 'face (cdr x))))
;;                 nethack-x-highlights))))))

(provide '50nethack)