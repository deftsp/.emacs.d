;;; Projectile
;; https://github.com/bbatsov/projectile
(use-package projectile
  :defer t
  :commands (projectile-ack
             projectile-ag
             projectile-ripgrep
             projectile-compile-project
             projectile-dired
             projectile-find-dir
             projectile-find-file
             projectile-find-tag
             projectile-test-project
             projectile-grep
             projectile-invalidate-cache
             projectile-kill-buffers
             projectile-multi-occur
             projectile-project-p
             projectile-project-root
             projectile-recentf
             projectile-regenerate-tags
             projectile-replace
             projectile-replace-regexp
             projectile-run-async-shell-command-in-root
             projectile-run-shell-command-in-root
             projectile-switch-project
             projectile-switch-to-buffer
             projectile-vc)
  :init
  (when (eq system-type 'darwin)
    (setq projectile-tags-command "/usr/local/bin/ctags -Re %s"))
  (setq projectile-indexing-method 'alien
        projectile-generic-command "fd . -0 --type f --color=never")
  (setq projectile-sort-order 'recentf
        projectile-mode-line-prefix " Proj"
        projectile-cache-file (concat tl-cache-directory
                                      "projectile.cache")
        projectile-known-projects-file (concat tl-cache-directory
                                               "projectile-bookmarks.eld"))
  (setq projectile-enable-caching t)
  (tl/set-leader-keys
    "p!"   'projectile-run-shell-command-in-root
    "p&"   'projectile-run-async-shell-command-in-root
    "p%"   'projectile-replace-regexp
    "pa"   'projectile-toggle-between-implementation-and-test
    "pb"   'projectile-switch-to-buffer
    "pc"   'projectile-compile-project
    "pd"   'projectile-find-dir
    "pD"   'projectile-dired
    "pe"   'projectile-edit-dir-locals
    ;; I prefer to put the git sub-repos in main repo and adding the directories of sub-repos to .gitignore, without
    ;; using git submodule or git subtree. That is to say, the files in sub-repos is not in main repo. However,
    ;; `projectile-find-file' internally using git to list file which can not find files in sub-repos. The
    ;; ".projectile" file will not work if using the "alien" indexing method. But I do want to find them. I'm using
    ;; find-file-in-project now.

    ;; "pf"   'projectile-find-file
    "pF"   'projectile-find-file-dwim
    "pg"   'projectile-find-tag
    "pG"   'projectile-regenerate-tags
    "pI"   'projectile-invalidate-cache
    "pk"   'projectile-kill-buffers
    "pp"   'projectile-switch-project
    "pr"   'projectile-recentf
    "psg"  'projectile-grep
    "psr"  'projectile-ripgrep
    "pss"  'projectile-ripgrep
    "pR" 'projectile-replace
    "pT" 'projectile-test-project
    "pv" 'projectile-vc)
  :config
  (add-to-list 'projectile-project-root-files ".cabal-sandbox")
  (add-to-list 'projectile-project-root-files "Setup.hs")
  ;; (add-hook 'emacs-lisp-mode-hook 'projectile-on)
  (projectile-global-mode +1)

  (put 'projectile-project-compilation-cmd 'safe-local-variable #'stringp)
  (put 'projectile-project-run-cmd 'safe-local-variable #'stringp)
  (put 'projectile-project-compilation-dir 'safe-local-variable #'stringp)
  (put 'projectile-project-test-cmd 'safe-local-variable #'stringp)

  (with-eval-after-load "ivy"
    (setq projectile-completion-system 'ivy)))

(use-package counsel-projectile
  :after (projectile)
  :config
  (counsel-projectile-mode +1))

;; (use-package dotenv
;;   :ensure nil
;;   :after projectile
;;   :config
;;   (defun dotenv-projectile-hook ()
;;     "Projectile hook."
;;     (let ((path (dotenv-path (projectile-project-root))))
;;       (when (s-present? path)
;;         (dotenv-update-env (dotenv-load path)))))

;;   (add-to-list 'projectile-after-switch-project-hook #'dotenv-projectile-hook))
