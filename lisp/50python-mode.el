;;; 50python-mode.el ---

;; Copyright (C) 2012  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>
;; Keywords:

;;; Code:

;;; python-mode from https://gitlab.com/python-mode-devs/python-mode
;; (let ((p (expand-file-name "~/.emacs.d/site-lisp/python-mode")))
;;   (add-to-list 'load-path p)
;;   (add-to-list 'auto-mode-alist
;;                '("\\.py$" . python-mode))
;;   (add-to-list 'interpreter-mode-alist
;;                '("python" . python-mode))
;;   (setq py-install-directory p)
;;   (setq py-shell-name "ipython")

;;   (autoload 'python-mode "python-mode"
;;     "Support for the Python programming language, <http://www.python.org/>" t))

;;; elpy
;; use anaconda-mode instead elpy
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

(defun annotate-pdb ()
  "Highlight break point lines."
  (interactive)
  (highlight-lines-matching-regexp "import i?pdb")
  (highlight-lines-matching-regexp "i?pdb.set_trace()"))

(defun python-toggle-breakpoint ()
  "Add a break point, highlight it."
  (interactive)
  (let ((trace (if (or (executable-find "ipdb")
                       (executable-find "ipdb3"))
                   "import ipdb; ipdb.set_trace()"
                 "import pdb; pdb.set_trace()"))
        (line (thing-at-point 'line)))
    (if (and line (string-match trace line))
        (kill-whole-line)
      (progn
        (back-to-indentation)
        (insert-string trace)
        (insert-string "\n")
        (python-indent-line)))))

;; from https://www.snip2code.com/Snippet/127022/Emacs-auto-remove-unused-import-statemen
(defun python-remove-unused-imports()
  "Use Autoflake to remove unused function"
  "autoflake --remove-all-unused-imports -i unused_imports.py"
  (interactive)
  (if (executable-find "autoflake")
      (progn
        (shell-command (format "autoflake --remove-all-unused-imports -i %s"
                               (shell-quote-argument (buffer-file-name))))
        (revert-buffer t t t))
    (message "Error: Cannot find autoflake executable.")))



;; do not warning me, I like delay set it.
(setq python-shell-prompt-detect-failure-warning nil)

(with-eval-after-load "python"
  (setq python-indent-guess-indent-offset nil
        python-indent-offset 4)

  ;; (if (executable-find "ipython")
  ;;     (setq python-shell-interpreter "ipython"
  ;;           python-shell-prompt-regexp "In \\[[0-9]+\\]: "
  ;;           python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
  ;;           python-shell-completion-setup-code "from IPython.core.completerlib import module_completion"
  ;;           python-shell-completion-module-string-code "';'.join(module_completion('''%s'''))\n"
  ;;           python-shell-completion-string-code
  ;;           "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")
  ;;   (setq python-shell-interpreter "python"))
  )


(put 'project-venv-name 'safe-local-variable 'stringp)

(defun paloryemacs/python-mode-init ()
  (setq mode-name "Python"
        tab-width 4
        ;; auto-indent on colon doesn't work well with if statement
        electric-indent-chars (delq ?: electric-indent-chars))

  (hack-local-variables)
  (when (boundp 'project-venv-name)
    (message (format "Activate virtual environment %s" project-venv-name))
    (pyvenv-workon project-venv-name))

  (annotate-pdb)
  ;; make C-j work the same way as RET
  (local-set-key (kbd "C-j") 'newline-and-indent)

  (subword-mode +1)
  (eldoc-mode +1)

  (when (fboundp 'anaconda-mode)
    (anaconda-mode +1)
    (anaconda-eldoc-mode +1))

  (smartparens-mode +1)

  (semantic-mode +1)
  (paloryemacs/lazy-load-stickyfunc-enhance)
  ;; (virtualenv-minor-mode 1)
  ;; (ropemacs-mode)
  ;; (setq imenu-create-index-function 'py--imenu-create-index-new)

  (when (string-prefix-p "*Org Src " (buffer-name))
    (flycheck-mode -1)))

;; python-mode set imenu-create-index-function too, make sure init function
;; override it by append it
(add-hook 'python-mode-hook 'paloryemacs/python-mode-init t)


(add-hook 'inferior-python-mode-hook 'paloryemacs/init-inferior-python-mode)
(defun paloryemacs/init-inferior-python-mode ()
  ;; do not echo input
  ;; http://stackoverflow.com/questions/8060609/python-interpreter-in-emacs-repeats-lines
  (setq comint-process-echoes t))

(with-eval-after-load "python"
  (define-key inferior-python-mode-map (kbd "C-j") 'comint-next-input)
  (define-key inferior-python-mode-map (kbd "C-k") 'comint-previous-input)
  (define-key inferior-python-mode-map (kbd "C-l") 'comint-clear-buffer)
  (define-key inferior-python-mode-map (kbd "C-r") 'comint-history-isearch-backward))

(paloryemacs/set-leader-keys-for-major-mode 'python-mode
  "cc" 'paloryemacs/python-execute-file
  "cC" 'paloryemacs/python-execute-file-focus
  "db" 'python-toggle-breakpoint
  "ri" 'python-remove-unused-imports
  "sB" 'python-shell-send-buffer-switch
  "sb" 'python-shell-send-buffer
  "sF" 'python-shell-send-defun-switch
  "sf" 'python-shell-send-defun
  "si" 'python-start-or-switch-repl
  "sR" 'python-shell-send-region-switch
  "sr" 'python-shell-send-region
  "hh" 'anaconda-mode-show-doc
  "hH" 'pylookup-lookup
  ;; use gtags
  ;; "gd" 'anaconda-mode-find-definitions
  ;; "ga" 'anaconda-mode-find-assignments
  ;; "gr" 'anaconda-mode-find-references
  ;; "gb" 'anaconda-mode-go-back
  ;; "g*" 'anaconda-mode-go-back
  "vs" 'pyenv-mode-set
  "vu" 'pyenv-mode-unset
  "Va" 'pyvenv-activate
  "Vd" 'pyvenv-deactivate
  "Vw" 'pyvenv-workon)

(paloryemacs/set-leader-keys-for-major-mode 'cython-mode
  "hh" 'anaconda-mode-view-doc
  "gg"  'anaconda-mode-goto)

(with-eval-after-load "evil-jumper"
  (defadvice anaconda-mode-goto (before python/anaconda-mode-goto activate)
    (evil-jumper--push)))


(defun python-shell-send-buffer-switch ()
  "Send buffer content to shell and switch to it in insert mode."
  (interactive)
  (python-shell-send-buffer)
  (python-shell-switch-to-shell)
  (evil-insert-state))

(defun python-shell-send-defun-switch ()
  "Send function content to shell and switch to it in insert mode."
  (interactive)
  (python-shell-send-defun nil)
  (python-shell-switch-to-shell)
  (evil-insert-state))

(defun python-shell-send-region-switch (start end)
  "Send region content to shell and switch to it in insert mode."
  (interactive "r")
  (python-shell-send-region start end)
  (python-shell-switch-to-shell)
  (evil-insert-state))

(defun python-start-or-switch-repl ()
  "Start and/or switch to the REPL."
  (interactive)
  (python-shell-switch-to-shell)
  (evil-insert-state))

(defun paloryemacs/python-execute-file (arg)
  "Execute a python script in a shell."
  (interactive "P")
  ;; set compile command to buffer-file-name
  ;; universal argument put compile buffer in comint mode
  (setq universal-argument t)
  (if arg
      (call-interactively 'compile)

    (set (make-local-variable 'compile-command)
         (format "python %s" (file-name-nondirectory
                              buffer-file-name)))
    (compile compile-command t)
    (with-current-buffer (get-buffer "*compilation*")
      (inferior-python-mode))))

(defun paloryemacs/python-execute-file-focus (arg)
  "Execute a python script in a shell and switch to the shell buffer in
`insert state'."
  (interactive "P")
  (paloryemacs/python-execute-file arg)
  (switch-to-buffer-other-window "*compilation*")
  (end-of-buffer)
  (evil-insert-state))

(defadvice python-indent-dedent-line-backspace
    (around python/sp-backward-delete-char activate)
  (let ((pythonp (or (not smartparens-strict-mode)
                     (char-equal (char-before) ?\s))))
    (if pythonp
        ad-do-it
      (call-interactively 'sp-backward-delete-char))))

(require 'virtualenvwrapper nil t)
(with-eval-after-load "virtualenvwrapper"
  (venv-initialize-interactive-shells)
  (venv-initialize-eshell) ;; if you want eshell support
  (setq venv-location (expand-file-name "~/.virtualenvs/")))

;;; pylookup
;; https://github.com/tsgates/pylookup
;; cd ~/.emacs.d/el-get/pylookup && make download
;; (setq pylookup-search-options '("--insensitive" "0" "--desc" "0"))
;; (with-eval-after-load "pylookup"
;;   (with-eval-after-load "evil-evilified-state"
;;     (evilify pylookup-mode pylookup-mode-map)))

;;; pdb
(setq gud-pdb-command-name "ipdb3")

(paloryemacs|define-jump-handlers python-mode)
(paloryemacs|define-jump-handlers cython-mode anaconda-mode-goto)

(add-to-list 'paloryemacs-jump-handlers-python-mode
             '(anaconda-mode-find-definitions :async t))


(provide '50python-mode)
;;; 50python-mode.el ends here
