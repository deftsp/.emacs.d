;;; 50doom-modeline.el

;; TODO: When pressing "C-c a" <org-agenda> the popup window named " *Agenda Commands*"
;; will call `fit-window-to-buffer'. It will get the right window height.
;; However, when pressing "C-c a " the second time, the window height is not
;; tall enough
(use-package doom-modeline
  :init
  (setq doom-modeline-height 25)
  :hook (after-init . doom-modeline-mode))


(provide '50doom-modeline)
