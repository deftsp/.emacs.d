;;; 50workgroups.el ---

;; Copyright (C) 2012  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>

(eval-after-load "workgroups"
  '(progn
    (setq wg-prefix-key (kbd "C-c w")
          wg-mode-line-left-brace "◀"
          wg-mode-line-right-brace "▶")
    (setq wg-morph-on nil)
    (if (fboundp 'key-chord-define-global)
        (key-chord-define-global ".w" wg-map))
    (wg-load (expand-file-name' "~/.emacs.d/workgroups"))))

(when (fboundp 'workgroups-mode)
  (workgroups-mode 1))


(provide '50workgroups)
;;; 50workgroups.el ends here
