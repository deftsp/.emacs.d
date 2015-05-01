;;; 50python-mode.el ---

;; Copyright (C) 2012  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>
;; Keywords:

;;; elpy

(with-eval-after-load "elpy"
  ;; use flycheck instead of flymake
  ;; https://github.com/jorgenschaefer/elpy/issues/137
  ;; https://github.com/jorgenschaefer/elpy/issues/328
  (setq elpy-modules (delete 'elpy-module-flymake elpy-modules))
  (elpy-enable)
  (elpy-use-ipython))


;;; ropemacs
(setq ropemacs-enable-shortcuts nil
      ropemacs-global-prefix "C-c C-p"
      ropemacs-local-prefix "C-c C-p"
      ropemacs-enable-autoimport t)

;; pymacs
;; (pymacs-load "ropemacs" "rope-")

(defun pl/python-mode-init ()
  (subword-mode +1)
  ;; (virtualenv-minor-mode 1)
  ;; (ropemacs-mode)
  (setq imenu-create-index-function 'py--imenu-create-index-new))

;; python-mode set imenu-create-index-function too, make sure init function
;; orverride it by append it
(add-hook 'python-mode-hook 'pl/python-mode-init t)

(add-hook 'inferior-python-mode-hook 'pl/init-inferior-python-mode)
(defun pl/init-inferior-python-mode ()
  ;; do not echo input
  ;; http://stackoverflow.com/questions/8060609/python-interpreter-in-emacs-repeats-lines
  (setq comint-process-echoes t))


(provide '50python-mode)
;;; 50python-mode.el ends here
