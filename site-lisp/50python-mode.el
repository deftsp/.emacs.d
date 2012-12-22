;;; 50python-mode.el ---

;; Copyright (C) 2012  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>
;; Keywords:


(defun pl/init-inferior-python-mode ()
  ;; do not echo input
  ;; http://stackoverflow.com/questions/8060609/python-interpreter-in-emacs-repeats-lines
  (setq comint-process-echoes t))

(add-hook 'inferior-python-mode-hook 'pl/init-inferior-python-mode)


(provide '50python-mode)
;;; 50python-mode.el ends here
