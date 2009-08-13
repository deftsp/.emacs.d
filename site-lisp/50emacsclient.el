;;; --50emacsclient.el

;; This starts up a server automatically.
;; use emacs daemon instead
(add-hook 'after-init-hook 'server-start)
;; make the server raise the Emacs window, work fine with stumpwm
(setq server-raise-frame t)

;;; work with stumpwm
;; (if window-system
;;     (add-hook 'server-done-hook
;;               (lambda () (shell-command "stumpish 'eval (stumpwm::return-es-called-win stumpwm::*es-win*)'"))))

;;; work with wmii
;; (if window-system
;;     (add-hook 'server-done-hook
;;               (lambda nil (shell-command "read curtag < /tmp/curtag; wmiir xwrite /ctl view $curtag"))))
;; ----------------------------------------------------------------------------------------------------


;; redefine stuff so that server-kill-buffer returns t if successful, which is required for bs-show using d.

;; (defadvice server-kill-buffer (after server-kill-buffer-report-success activate)
;;   "Return t if the buffer was really killed."
;;   (when buffer
;;     (setq ad-return-value t)))

;; work with saveplace
(eval-after-load "server"
  '(add-to-list 'server-visit-hook 'save-place-find-file-hook))


;; http://www.wanglianghome.org/blog/2009/01/customization-tips-for-emacs-daemon.html
;; (add-hook 'after-make-frame-functions
;;           (lambda (frame)
;;             (with-selected-frame frame
;;               (when window-system
;;                 (scroll-bar-mode -1)
;;                 (setq x-select-enable-clipboard t)))))
