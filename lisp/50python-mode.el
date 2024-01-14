;;; 50python-mode.el ---

;; Copyright (C) 2021  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>

;;; Code:

;;; Links
;; [[https://gitlab.com/nathanfurnal/dotemacs/-/snippets/2060535][Emacs python config ($2060535) · Snippets · Nathan Furnal / dotemacs · GitLab]]
;; [[https://towardsdatascience.com/python-environment-101-1d68bda3094d][Python Environment 101. How are pyenv and pipenv different and… ]]

;;; virtual environment
;; cd ~/foo && pipenv install
;; a directory named .venv will be create in ./foo/.venv
;; M-x pipenv-activate # active it

(defun tl/python-annotate-pdb ()
  "Highlight break point lines."
  (interactive)
  (highlight-lines-matching-regexp "import \\(pdb\\|ipdb\\|pudb\\|wdb\\)")
  (highlight-lines-matching-regexp "\\(pdb\\|ipdb\\|pudb\\|wdb\\).set_trace()"))

;; from https://www.snip2code.com/Snippet/127022/Emacs-auto-remove-unused-import-statemen
(defun tl/python-remove-unused-imports()
  "Use Autoflake to remove unused function"
  "autoflake --remove-all-unused-imports -i unused_imports.py"
  (interactive)
  (if (executable-find "autoflake")
      (progn
        (shell-command (format "autoflake --remove-all-unused-imports -i %s"
                               (shell-quote-argument (buffer-file-name))))
        (revert-buffer t t t))
    (message "Error: Cannot find autoflake executable.")))

;; https://github.com/tsgates/pylookup
(use-package pylookup
  :after python
  :commands (pylookup-lookup pylookup-update pylookup-update-all)
  :init
  (progn
    ;; (setq pylookup-search-options '("--insensitive" "0" "--desc" "0"))
    (tl/set-leader-keys-for-mode 'python-mode
      "hH" 'pylookup-lookup))
  :config
  (progn
    (setq pylookup-dir (concat user-emacs-directory "pylookup/")
          pylookup-program (concat pylookup-dir "pylookup.py")
          pylookup-db-file (concat pylookup-dir "pylookup.db"))
    (setq pylookup-completing-read 'completing-read)))


(use-package python
  :defer t
  :init
  (progn
    ;; do not warning me, I like delay set it.
    ;; (setq python-shell-prompt-detect-failure-warning nil)
    (setq python-indent-guess-indent-offset nil
          ;; remove guess indent python message
          python-indent-guess-indent-offset-verbose nil
          python-indent-offset 4)
    (setq python-shell-completion-native-enable nil))
  :config
  (progn
    ;; (tl//python-setup-shell) ; slow
    (tl/declare-prefix-for-mode 'python-mode "mc" "execute")
    (tl/declare-prefix-for-mode 'python-mode "md" "debug")
    (tl/declare-prefix-for-mode 'python-mode "mh" "help")
    (tl/declare-prefix-for-mode 'python-mode "mg" "goto")
    (tl/declare-prefix-for-mode 'python-mode "ms" "send to REPL")
    (tl/declare-prefix-for-mode 'python-mode "mr" "refactor")
    (tl/declare-prefix-for-mode 'python-mode "mv" "pyenv")
    (tl/declare-prefix-for-mode 'python-mode "mV" "pyvenv")
    (tl/set-leader-keys-for-mode 'python-mode
      "'"  'tl/python-start-or-switch-repl
      "cc" 'tl/python-execute-file
      "cC" 'tl/python-execute-file-focus
      ;; "db" 'tl/python-toggle-breakpoint
      "ri" 'tl/python-remove-unused-imports
      "sB" 'tl/python-shell-send-buffer-switch
      "sb" 'python-shell-send-buffer
      "sF" 'tl/python-shell-send-defun-switch
      "sf" 'python-shell-send-defun
      "si" 'tl/python-start-or-switch-repl
      "sR" 'tl/python-shell-send-region-switch
      "sr" 'python-shell-send-region)

    (define-key inferior-python-mode-map (kbd "C-j") 'comint-next-input)
    (define-key inferior-python-mode-map (kbd "C-k") 'comint-previous-input)
    (define-key inferior-python-mode-map (kbd "C-l") 'tl/comint-clear-buffer)
    (define-key inferior-python-mode-map (kbd "C-r") 'comint-history-isearch-backward)))

;; (put 'project-venv-name 'safe-local-variable 'stringp)

(defun tl/python-imenu-create-index ()
  (if (bound-and-true-p semantic-mode)
      (semantic-create-imenu-index)
    (python-imenu-create-index)))

(defun tl//python-imenu-create-index-use-semantic-maybe ()
  "Use semantic if the layer is enabled."
  (setq imenu-create-index-function 'tl/python-imenu-create-index))

(defun tl/python-mode-init ()
  (setq mode-name "Python"
        tab-width 4)
  (setq-local comment-inline-offset 2)

  (hack-local-variables)

  (tl/python-annotate-pdb)
  ;; make C-j work the same way as RET
  (local-set-key (kbd "C-j") 'newline-and-indent)

  (subword-mode +1)
  (eldoc-mode +1)

  ;; (when (fboundp 'anaconda-mode)
  ;;   (anaconda-mode +1)
  ;;   (anaconda-eldoc-mode +1))

  (smartparens-mode +1)

  (tl//python-imenu-create-index-use-semantic-maybe)
  ;; Anaconda provides more useful information but can not do it properly when
  ;; this mode is enabled since the minibuffer is cleared all the time.
  (semantic-idle-summary-mode 0)
  (semantic-mode +1)
  (tl/lazy-load-stickyfunc-enhance)
  ;; (virtualenv-minor-mode 1)
  ;; (ropemacs-mode)
  ;; (setq imenu-create-index-function 'py--imenu-create-index-new)

  (when (string-prefix-p "*Org Src " (buffer-name))
    (flycheck-mode -1)))

;; python-mode set imenu-create-index-function too, make sure init function
;; override it by append it
(add-hook 'python-mode-hook 'tl/python-mode-init t)


(add-hook 'inferior-python-mode-hook 'tl/init-inferior-python-mode)
(defun tl/init-inferior-python-mode ()
  ;; do not echo input
  ;; http://stackoverflow.com/questions/8060609/python-interpreter-in-emacs-repeats-lines
  (setq comint-output-filter-functions
        (remove 'ansi-color-process-output comint-output-filter-functions))
  (setq comint-process-echoes t))

(use-package cython-mode
  :defer t
  :init
  (progn
    (tl/set-leader-keys-for-mode 'cython-mode
      "hh" 'anaconda-mode-view-doc
      "gu" 'anaconda-mode-usages
      "gg"  'anaconda-mode-goto)))


;; REPL
(defun tl/python-shell-send-buffer-switch ()
  "Send buffer content to shell and switch to it in insert mode."
  (interactive)
  (python-shell-send-buffer)
  (python-shell-switch-to-shell)
  (evil-insert-state))

(defun tl/python-shell-send-defun-switch ()
  "Send function content to shell and switch to it in insert mode."
  (interactive)
  (python-shell-send-defun nil)
  (python-shell-switch-to-shell)
  (evil-insert-state))

(defun tl/python-shell-send-region-switch (start end)
  "Send region content to shell and switch to it in insert mode."
  (interactive "r")
  (python-shell-send-region start end)
  (python-shell-switch-to-shell)
  (evil-insert-state))

(defun tl/python-start-or-switch-repl ()
  "Start and/or switch to the REPL."
  (interactive)
  (let ((shell-process
         (or (python-shell-get-process)
             ;; `run-python' has different return values and different
             ;; errors in different emacs versions. In 24.4, it throws an
             ;; error when the process didn't start, but in 25.1 it
             ;; doesn't throw an error, so we demote errors here and
             ;; check the process later
             (with-demoted-errors "Error: %S"
               ;; in Emacs 24.5 and 24.4, `run-python' doesn't return the
               ;; shell process
               (call-interactively #'run-python)
               (python-shell-get-process)))))
    (unless shell-process
      (error "Failed to start python shell properly"))
    (pop-to-buffer (process-buffer shell-process))
    (evil-insert-state)))

(defun tl/python-execute-file (arg)
  "Execute a python script in a shell."
  (interactive "P")
  ;; set compile command to buffer-file-name
  ;; universal argument put compile buffer in comint mode
  (let ((universal-argument t)
        (compile-command (format "%s %s"
                                 (tl/pyenv-executable-find python-shell-interpreter)
                                 (file-name-nondirectory buffer-file-name))))
    (if arg
        (call-interactively 'compile)
      (compile compile-command t)
      (with-current-buffer (get-buffer "*compilation*")
        (inferior-python-mode)))))

(defun tl/python-execute-file-focus (arg)
  "Execute a python script in a shell and switch to the shell buffer in
 `insert state'."
  (interactive "P")
  (tl/python-execute-file arg)
  (switch-to-buffer-other-window "*compilation*")
  (end-of-buffer)
  (evil-insert-state))

(defun tl/python-execute-file (arg)
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


(defadvice python-indent-dedent-line-backspace
    (around python/sp-backward-delete-char activate)
  (let ((pythonp (or (not smartparens-strict-mode)
                     (char-equal (char-before) ?\s))))
    (if pythonp
        ad-do-it
      (call-interactively 'sp-backward-delete-char))))

;;; pdb
;; (setq gud-pdb-command-name "ipdb3")

(use-package pyvenv
  :after python
  :commands (pyvenv-activate pyvenv-workon pyvenv-deactivate)
  :init
  (add-hook 'python-mode-local-vars-hook #'pyvenv-track-virtualenv)
  :config
  (pyvenv-mode +1))


(use-package pipenv
  :commands (pipenv-activate
             pipenv-deactivate
             pipenv-shell
             pipenv-lock
             pipenv-run
             pipenv-open
             pipenv-install
             pipenv-uninstall)
  :hook (python-mode . pipenv-mode)
  :init
  (setq pipenv-executable "~/.asdf/shims/pipenv")
  (tl/set-leader-keys-for-mode 'python-mode
    "ea" 'pipenv-activate
    "ed" 'pipenv-deactivate
    "ei" 'pipenv-install
    "el" 'pipenv-lock
    "eo" 'pipenv-open
    "er" 'pipenv-run
    "es" 'pipenv-shell
    "eu" 'pipenv-uninstall))

;; Language server for Python
;; Read the docs for the different variables set in the config.
(defun tl//enable-lsp-pyright ()
  (require 'lsp-pyright)
  (lsp-deferred))

;; M-x lsp-update-server to update pyright
(use-package lsp-pyright
  :defer t
  :hook (python-mode . tl//enable-lsp-pyright)
  :config
  ;; (setq lsp-clients-python-library-directories '("/usr/" "~/miniconda3/pkgs"))
  (setq lsp-pyright-disable-language-service nil
	    lsp-pyright-disable-organize-imports nil
	    lsp-pyright-auto-import-completions t
	    ;; lsp-pyright-venv-path "~/miniconda3/envs"
	    lsp-pyright-use-library-code-for-types t))

(use-package poetry
  :after python
  :init
  (add-hook 'python-mode-hook #'poetry-tracking-mode))

(provide '50python-mode)
;;; 50python-mode.el ends here

;; Local Variables:
;; coding: utf-8-unix
;; End:
