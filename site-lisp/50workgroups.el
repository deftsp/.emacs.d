;;; 50workgroups.el ---

;; Copyright (C) 2012  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>

(eval-after-load "workgroups"
  '(progn
     (setq wg-prefix-key (kbd "C-c w")
           wg-mode-line-left-brace "["
           wg-mode-line-right-brace "]"
           wg-query-for-save-on-emacs-exit t
           wg-query-for-save-on-workgroups-mode-exit t)
     (workgroups-mode 1)
     (setq wg-morph-on nil)
     (wg-load (expand-file-name' "~/.emacs.d/workgroups"))))


(provide '50workgroups)
;;; 50workgroups.el ends here
