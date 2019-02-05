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

(defvar python-auto-set-local-pyenv-version 'on-visit
  "Automatically set pyenv version from \".python-version\".

Possible values are `on-visit', `on-project-switch' or `nil'.")

(defvar python-auto-set-local-pyvenv-virtualenv 'on-visit
  "Automatically set pyvenv virtualenv from \".venv\".

Possible values are `on-visit', `on-project-switch' or `nil'.")



(paloryemacs|define-jump-handlers python-mode)
(paloryemacs|define-jump-handlers cython-mode anaconda-mode-goto)


;; from http://pedrokroger.net/2010/07/configuring-emacs-as-a-python-ide-2/
(defun paloryemacs/python-annotate-pdb ()
  "Highlight break point lines."
  (interactive)
  (highlight-lines-matching-regexp "import \\(pdb\\|ipdb\\|pudb\\|wdb\\)")
  (highlight-lines-matching-regexp "\\(pdb\\|ipdb\\|pudb\\|wdb\\).set_trace()"))

(defun paloryemacs/pyenv-executable-find (command)
  "Find executable taking pyenv shims into account."
  (if (executable-find "pyenv")
      (progn
        (let ((pyenv-string (shell-command-to-string (concat "pyenv which " command))))
          (unless (string-match "not found" pyenv-string)
            (string-trim pyenv-string))))
    (executable-find command)))

(defun paloryemacs//python-setup-shell (&rest args)
  (if (paloryemacs/pyenv-executable-find "ipython")
      (progn (setq python-shell-interpreter "ipython")
             (if (version< (replace-regexp-in-string "\n$" "" (shell-command-to-string "ipython --version")) "5")
                 (setq python-shell-interpreter-args "-i")
               (setq python-shell-interpreter-args "--simple-prompt -i")))
    (progn
      (setq python-shell-interpreter-args "-i")
      (setq python-shell-interpreter "python"))))


(defun paloryemacs//python-setup-checkers (&rest args)
  (when (fboundp 'flycheck-set-checker-executable)
    (let ((pylint (paloryemacs/pyenv-executable-find "pylint"))
          (flake8 (paloryemacs/pyenv-executable-find "flake8")))
      (when pylint
        (flycheck-set-checker-executable "python-pylint" pylint))
      (when flake8
        (flycheck-set-checker-executable "python-flake8" flake8)))))

(defun paloryemacs/python-setup-everything (&rest args)
  (apply 'paloryemacs//python-setup-shell args)
  (apply 'paloryemacs//python-setup-checkers args))


(defun paloryemacs/python-toggle-breakpoint ()
  "Add a break point, highlight it."
  (interactive)
  (let ((trace (cond ((paloryemacs/pyenv-executable-find "wdb") "import wdb; wdb.set_trace()")
                     ((paloryemacs/pyenv-executable-find "ipdb") "import ipdb; ipdb.set_trace()")
                     ((paloryemacs/pyenv-executable-find "pudb") "import pudb; pudb.set_trace()")
                     ((paloryemacs/pyenv-executable-find "ipdb3") "import ipdb; ipdb.set_trace()")
                     ((paloryemacs/pyenv-executable-find "pudb3") "import pudb; pudb.set_trace()")
                     (t "import pdb; pdb.set_trace()")))
        (line (thing-at-point 'line)))
    (if (and line (string-match trace line))
        (kill-whole-line)
      (progn
        (back-to-indentation)
        (insert trace)
        (insert "\n")
        (python-indent-line)))))

;; from https://www.snip2code.com/Snippet/127022/Emacs-auto-remove-unused-import-statemen
(defun paloryemacs/python-remove-unused-imports()
  "Use Autoflake to remove unused function"
  "autoflake --remove-all-unused-imports -i unused_imports.py"
  (interactive)
  (if (executable-find "autoflake")
      (progn
        (shell-command (format "autoflake --remove-all-unused-imports -i %s"
                               (shell-quote-argument (buffer-file-name))))
        (revert-buffer t t t))
    (message "Error: Cannot find autoflake executable.")))


(defun paloryemacs//pyenv-mode-set-local-version ()
  "Set pyenv version from \".python-version\" by looking in parent directories."
  (interactive)
  (let ((root-path (locate-dominating-file default-directory
                                           ".python-version")))
    (when root-path
      (let* ((file-path (expand-file-name ".python-version" root-path))
             (version
              (with-temp-buffer
                (insert-file-contents-literally file-path)
                (nth 0 (split-string (buffer-substring-no-properties
                                      (line-beginning-position)
                                      (line-end-position)))))))
        (if (member version (pyenv-mode-versions))
            (pyenv-mode-set version)
          (message "pyenv: version `%s' is not installed (set by %s)"
                   version file-path))))))

(defun paloryemacs//pyvenv-mode-set-local-virtualenv ()
  "Set pyvenv virtualenv from \".venv\" by looking in parent directories."
  (interactive)
  (let ((root-path (locate-dominating-file default-directory
                                           ".venv")))
    (when root-path
      (let* ((file-path (expand-file-name ".venv" root-path))
             (virtualenv
              (with-temp-buffer
                (insert-file-contents-literally file-path)
                (buffer-substring-no-properties (line-beginning-position)
                                                (line-end-position)))))
        (pyvenv-workon virtualenv)))))


(use-package anaconda-mode
  :defer t
  :diminish anaconda-mode
  :init
  (progn
    (add-hook 'python-mode-hook 'anaconda-mode)
    (add-to-list 'paloryemacs-jump-handlers-python-mode
                 '(anaconda-mode-find-definitions :async t)))
  :config
  (progn
    (paloryemacs/set-leader-keys-for-major-mode 'python-mode
      ;; use gtags
      "gd" 'anaconda-mode-find-definitions
      "ga" 'anaconda-mode-find-assignments
      "gr" 'anaconda-mode-find-references
      "gu" 'anaconda-mode-find-references
      "gb" 'anaconda-mode-go-back
      "g*" 'anaconda-mode-go-back
      "hh" 'anaconda-mode-show-doc)

    (defadvice anaconda-mode-goto (before python/anaconda-mode-goto activate)
      (evil--jumps-push))))

(use-package pyenv-mode
  :if (executable-find "pyenv")
  :commands (pyenv-mode-versions)
  :init
  (progn
    (pcase python-auto-set-local-pyenv-version
      (`on-visit
       (add-hook 'python-mode-hook 'paloryemacs//pyenv-mode-set-local-version))
      (`on-project-switch
       (add-hook 'projectile-after-switch-project-hook
                 'paloryemacs//pyenv-mode-set-local-version)))
    ;; setup shell correctly on environment switch
    (dolist (func '(pyenv-mode-set pyenv-mode-unset))
      (advice-add func :after 'paloryemacs/python-setup-everything))
    (paloryemacs/set-leader-keys-for-major-mode 'python-mode
      "vu" 'pyenv-mode-unset
      "vs" 'pyenv-mode-set))
  :config
  (progn
    ;; For a venv named 'venv3.6.2' located at '~/.pyenv/versions/venv3.6.2'
    ;; which is linked to '~/.pyenv/versions/3.6.2/envs/venv',
    ;; `python-shell-calculate-process-environment' will set VIRTUAL_ENV to
    ;; '~/.pyenv/versions/venv3.6.2'. But the function init_virtualenv in
    ;; interactiveshell.py (in IPython source) don't accept the virtualenv path
    ;; as link, and emit warning. Here we advice `pyenv-mode-full-path' to
    ;; return the true path instead of link.

    ;; See also
    ;; https://github.com/pyenv/pyenv-virtualenv/issues/113
    ;; https://github.com/ipython/ipython/issues/9774
    ;; https://github.com/ipython/ipython/pull/5939
    (defun paloryemacs//chase-virtualenv-root (p)
      (file-truename p))
    ;; (advice-remove 'pyenv-mode-full-path
    ;;                #'paloryemacs//chase-virtualenv-root)
    (advice-add 'pyenv-mode-full-path
                :filter-return #'paloryemacs//chase-virtualenv-root)))

(use-package pyvenv
  :defer t
  :init
  (progn
    (pcase python-auto-set-local-pyvenv-virtualenv
      (`on-visit
       (add-hook 'python-mode-hook 'paloryemacs//pyvenv-mode-set-local-virtualenv))
      (`on-project-switch
       (add-hook 'projectile-after-switch-project-hook
                 'paloryemacs//pyvenv-mode-set-local-virtualenv)))
    (paloryemacs/set-leader-keys-for-major-mode 'python-mode
      "Va" 'pyvenv-activate
      "Vd" 'pyvenv-deactivate
      "Vw" 'pyvenv-workon)
    ;; setup shell correctly on environment switch
    (dolist (func '(pyvenv-activate pyvenv-deactivate pyvenv-workon))
      (advice-add func :after 'paloryemacs/python-setup-everything))))

;; https://github.com/tsgates/pylookup
(use-package pylookup
  :commands (pylookup-lookup pylookup-update pylookup-update-all)
  :init
  (progn
    ;; (setq pylookup-search-options '("--insensitive" "0" "--desc" "0"))
    (evilified-state-evilify pylookup-mode pylookup-mode-map)
    (paloryemacs/set-leader-keys-for-major-mode 'python-mode
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
          python-indent-offset 4)
    (setq python-shell-completion-native-enable nil))
  :config
  (progn
    (paloryemacs//python-setup-shell) ; slow
    (paloryemacs/declare-prefix-for-mode 'python-mode "mc" "execute")
    (paloryemacs/declare-prefix-for-mode 'python-mode "md" "debug")
    (paloryemacs/declare-prefix-for-mode 'python-mode "mh" "help")
    (paloryemacs/declare-prefix-for-mode 'python-mode "mg" "goto")
    (paloryemacs/declare-prefix-for-mode 'python-mode "ms" "send to REPL")
    (paloryemacs/declare-prefix-for-mode 'python-mode "mr" "refactor")
    (paloryemacs/declare-prefix-for-mode 'python-mode "mv" "pyenv")
    (paloryemacs/declare-prefix-for-mode 'python-mode "mV" "pyvenv")
    (paloryemacs/set-leader-keys-for-major-mode 'python-mode
      "'"  'paloryemacs/python-start-or-switch-repl
      "cc" 'paloryemacs/python-execute-file
      "cC" 'paloryemacs/python-execute-file-focus
      "db" 'paloryemacs/python-toggle-breakpoint
      "ri" 'paloryemacs/python-remove-unused-imports
      "sB" 'paloryemacs/python-shell-send-buffer-switch
      "sb" 'python-shell-send-buffer
      "sF" 'paloryemacs/python-shell-send-defun-switch
      "sf" 'python-shell-send-defun
      "si" 'paloryemacs/python-start-or-switch-repl
      "sR" 'paloryemacs/python-shell-send-region-switch
      "sr" 'python-shell-send-region)

    (define-key inferior-python-mode-map (kbd "C-j") 'comint-next-input)
    (define-key inferior-python-mode-map (kbd "C-k") 'comint-previous-input)
    (define-key inferior-python-mode-map (kbd "C-l") 'paloryemacs/comint-clear-buffer)
    (define-key inferior-python-mode-map (kbd "C-r") 'comint-history-isearch-backward)))

(put 'project-venv-name 'safe-local-variable 'stringp)


(defun paloryemacs/python-imenu-create-index ()
  (if (bound-and-true-p semantic-mode)
      (semantic-create-imenu-index)
    (python-imenu-create-index)))

(defun paloryemacs//python-imenu-create-index-use-semantic-maybe ()
  "Use semantic if the layer is enabled."
  (setq imenu-create-index-function 'paloryemacs/python-imenu-create-index))

(defun paloryemacs/python-mode-init ()
  (setq mode-name "Python"
        tab-width 4)
  (setq-local comment-inline-offset 2)

  (hack-local-variables)

  (paloryemacs/python-annotate-pdb)
  ;; make C-j work the same way as RET
  (local-set-key (kbd "C-j") 'newline-and-indent)

  (subword-mode +1)
  (eldoc-mode +1)

  (when (fboundp 'anaconda-mode)
    (anaconda-mode +1)
    (anaconda-eldoc-mode +1))

  (smartparens-mode +1)
  (turn-on-evil-matchit-mode)

  (paloryemacs//python-imenu-create-index-use-semantic-maybe)
  ;; Anaconda provides more useful information but can not do it properly when
  ;; this mode is enabled since the minibuffer is cleared all the time.
  (semantic-idle-summary-mode 0)
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
  (setq comint-output-filter-functions
        (remove 'ansi-color-process-output comint-output-filter-functions))
  (setq comint-process-echoes t))

(use-package cython-mode
  :defer t
  :init
  (progn
    (paloryemacs/set-leader-keys-for-major-mode 'cython-mode
      "hh" 'anaconda-mode-view-doc
      "gu" 'anaconda-mode-usages
      "gg"  'anaconda-mode-goto)))


;; REPL
(defun paloryemacs/python-shell-send-buffer-switch ()
  "Send buffer content to shell and switch to it in insert mode."
  (interactive)
  (python-shell-send-buffer)
  (python-shell-switch-to-shell)
  (evil-insert-state))

(defun paloryemacs/python-shell-send-defun-switch ()
  "Send function content to shell and switch to it in insert mode."
  (interactive)
  (python-shell-send-defun nil)
  (python-shell-switch-to-shell)
  (evil-insert-state))

(defun paloryemacs/python-shell-send-region-switch (start end)
  "Send region content to shell and switch to it in insert mode."
  (interactive "r")
  (python-shell-send-region start end)
  (python-shell-switch-to-shell)
  (evil-insert-state))

(defun paloryemacs/python-start-or-switch-repl ()
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

(defun paloryemacs/python-execute-file (arg)
  "Execute a python script in a shell."
  (interactive "P")
  ;; set compile command to buffer-file-name
  ;; universal argument put compile buffer in comint mode
  (let ((universal-argument t)
        (compile-command (format "%s %s"
                                 (paloryemacs/pyenv-executable-find python-shell-interpreter)
                                 (file-name-nondirectory buffer-file-name))))
    (if arg
        (call-interactively 'compile)
      (compile compile-command t)
      (with-current-buffer (get-buffer "*compilation*")
        (inferior-python-mode)))))

(defun paloryemacs/python-execute-file-focus (arg)
  "Execute a python script in a shell and switch to the shell buffer in
 `insert state'."
  (interactive "P")
  (paloryemacs/python-execute-file arg)
  (switch-to-buffer-other-window "*compilation*")
  (end-of-buffer)
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



(defadvice python-indent-dedent-line-backspace
    (around python/sp-backward-delete-char activate)
  (let ((pythonp (or (not smartparens-strict-mode)
                     (char-equal (char-before) ?\s))))
    (if pythonp
        ad-do-it
      (call-interactively 'sp-backward-delete-char))))

;;; pdb
;; (setq gud-pdb-command-name "ipdb3")


(provide '50python-mode)
;;; 50python-mode.el ends here
