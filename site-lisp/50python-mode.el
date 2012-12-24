;;; 50python-mode.el ---

;; Copyright (C) 2012  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>
;; Keywords:

;;; init for ipython
(eval-after-load "python.el"
  (setq python-shell-interpreter "ipython"
        python-shell-interpreter-args ""
        python-shell-prompt-regexp "In \\[[0-9]+\\]: "
        python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
        python-shell-completion-setup-code
        "from IPython.core.completerlib import module_completion"
        python-shell-completion-module-string-code
        "';'.join(module_completion('''%s'''))\n"
        python-shell-completion-string-code
        "';'.join(get_ipython().Completer.all_completions('''%s'''))\n"))


(setq ropemacs-enable-shortcuts nil)
(setq ropemacs-global-prefix "C-c C-p")
(setq ropemacs-local-prefix "C-c C-p")

(pymacs-load "ropemacs" "rope-")
(setq ropemacs-enable-autoimport t)

(defun pl/init-python-mode ()
  ;; (virtualenv-minor-mode 1)
  ;; (ropemacs-mode)
  )

(add-hook 'python-mode-hook 'pl/init-python-mode)

(defun pl/init-inferior-python-mode ()
  ;; do not echo input
  ;; http://stackoverflow.com/questions/8060609/python-interpreter-in-emacs-repeats-lines
  (setq comint-process-echoes t))
(add-hook 'inferior-python-mode-hook 'pl/init-inferior-python-mode)


(provide '50python-mode)
;;; 50python-mode.el ends here
