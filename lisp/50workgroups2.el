;;; 50workgroups.el ---

;; Copyright (C) 2012  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>

(require 'workgroups2 nil t)

;; autoload/autosave:
;; if you start Emacs as "emacs --daemon" - turn off autoloading of workgroups:
;;(setq wg-use-default-session-file nil)

(setq wg-default-session-file "~/.emacs.d/workgroups"
      wg-prefix-key (kbd "C-c w")
      wg-mode-line-decor-left-brace "♯"
      wg-mode-line-decor-right-brace "")

(eval-after-load "workgroups2"
  '(when (fboundp 'key-chord-define-global)
     (key-chord-define-global ".w" wg-prefixed-map)))

;; put this one at the bottom of .emacs
(when (fboundp 'workgroups-mode)
  (add-hook 'after-init-hook (lambda () (workgroups-mode 1))))


(provide '50workgroups2)
;;; 50workgroups.el ends here
