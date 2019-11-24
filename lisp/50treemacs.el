;;; 50treemacs.el ---                                -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>
;; Keywords:

;;; treemacs
(use-package treemacs
  :commands treemacs--window-number-ten
  :defer t
  :init
  (progn
    (paloryemacs/set-leader-keys
      "ft"    #'treemacs
      "fT"    #'treemacs
      "fB"    #'treemacs-bookmark
      "f C-t" #'treemacs-find-file))
  :config
  (progn
    ;; (paloryemacs/define-evil-state-face "treemacs" "MediumPurple1")
    (use-package treemacs-evil :demand t)
    (use-package pfuture)

    (setq treemacs-icon-open-png   (propertize "⊖ " 'face 'treemacs-directory-face)
          treemacs-icon-closed-png (propertize "⊕ " 'face 'treemacs-directory-face)
          treemacs-icon-tag-node-open-png   (propertize "− " 'face 'font-lock-keyword-face)
          treemacs-icon-tag-node-closed-png (propertize "+ " 'face 'font-lock-keyword-face)
          treemacs-icon-tag-leaf-png        (propertize "🞄 " 'face 'font-lock-keyword-face)
          treemacs-no-png-images t
          treemacs-indentation-string " "
          treemacs-indentation 2
          treemacs-follow-after-init t
          treemacs-width 35
          treemacs-winum-number 10
          treemacs-position 'left
          treemacs-file-event-delay 5000
          treemacs-is-never-other-window nil
          treemacs-silent-refresh nil
          treemacs-change-root-without-asking nil
          treemacs-sorting 'alphabetic-desc
          treemacs-show-hidden-files t
          treemacs-never-persist nil
          treemacs-goto-tag-strategy 'refetch-index
          treemacs-collapse-dirs (if (executable-find "python") 3 0))

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode t)

    (defun paloryemacs/treemacs-mode-init ()
      ;; (setq mode-line-format nil)

      )
    (add-hook 'treemacs-mode-hook #'paloryemacs/treemacs-mode-init)

    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred)) (`(t . _)
       (treemacs-git-mode 'simple)))

    (defun paloryemacs/treemacs-header-with-brackets (current-root)
      (format "[%s]" (file-name-nondirectory current-root)))
    (setq treemacs-header-function #'paloryemacs/treemacs-header-with-brackets)

    ;; this boundp check guards against a new feature that not all treemacs installations will have
    ;; TODO remove this guard in a few weeks
    (when (boundp 'treemacs-git-mode)
      (treemacs-git-mode 'extended))))

(use-package treemacs-projectile
  :defer t
  :init
  (progn
    (defun paloryemacs/treemacs-project-toggle ()
      "Toggle and add the current project to treemacs if not already added."
      (interactive)
      (if (eq (treemacs-current-visibility) 'visible)
          (delete-window (treemacs-get-local-window))
        (let ((path (projectile-ensure-project (projectile-project-root)))
              (name (projectile-project-name)))
          (unless (treemacs-current-workspace)
            (treemacs--find-workspace))
          (treemacs-do-add-project-to-workspace path name)
          (treemacs-select-window))))


    (paloryemacs/set-leader-keys
      "fp" #'paloryemacs/treemacs-project-toggle
      "fP" #'treemacs-projectile)))

(use-package treemacs-magit
  :after (treemacs magit))

(provide '50treemacs)
