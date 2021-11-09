;;; 50vc.el ---

(defvar temporary-file-directory "~/.backup/temp/")

;;; backup
(setq make-backup-files t)
(setq backup-directory-alist `((".*" . ,temporary-file-directory)) ; don't litter my fs tree)
      version-control t                                          ; Use version numbers for backups
      kept-new-versions 16                                       ; Number of newest versions to keep
      kept-old-versions 2                                        ; Number of oldest versions to keep
      delete-old-versions t                                      ; Ask to delete excess backup versions?
      ;; preserve the owner and group of the file you’re editing (this is especially important if you edit files as root).
      backup-by-copying-when-mismatch t
      backup-by-copying t               ; don't clobber symlinks
      ;; copy linked files, don't rename.
      backup-by-copying-when-linked t)

;;;; VC
;; While we are thinking about symlinks, I don't like being asked whether I want to follow a symlink; I do, already!
(setq vc-follow-symlinks t
      vc-initial-comment t
      ;; auto-revert-check-vc-info t
      ;; vc-cvs-stay-local nil
      vc-diff-switches diff-switches)

;;; auto-save
(add-hook 'suspend-hook 'do-auto-save) ;; Auto-Save on ^Z
(setq auto-save-interval 1200           ; set autosaving from 300 keystrokes to 1200
      ;; save after 1 second of idle time (default is 30)
      auto-save-timeout 30)

;; (setq auto-save-file-name-transforms '((".*/\\(.*\\)" "~/.tmp/\\1" t)))

;;; Git
;;; gitsum: do interactive partial commits with Emacs in the style of darcs record.
(autoload 'gitsum "gitsum" "Entry point into gitsum-diff-mode." t)

;; git.el will look into your git configuration for committer name and email address. If that fails, it uses the Emacs
;; defaults. If you rely on environment variables for your configuration, beware. (And consider git-committer-name and
;; git-committer-email)

;;; git-emacs
;; http://tsgates.cafe24.com/git/git-emacs.html
;; (require 'git-emacs nil t)
;; (with-eval-after-load "git-emacs"
;;   (when window-system
;;     ;; git-state-decoration-colored-letter
;;     (setq git-state-modeline-decoration 'git-state-decoration-small-dot)))

;; (defadvice git--install-state-mark-modeline (around insert-after-ace-window-key activate)
;;   "Add git state mark modeline after ace window key"
;;   (if (and (boundp 'powerline-git-state-mark-modeline)
;;            (and (boundp 'powerline-git-state-mark-modeline)
;;                 powerline-git-state-mark-modeline))
;;       (progn
;;         (setq powerline-git-state-mark
;;               (git--state-decoration-dispatch
;;                stat)))
;;     (if (assq 'ace-window-display-mode mode-line-format)
;;         (let* ((left)
;;                (right mode-line-format)
;;                (next (car right)))
;;           (catch 'break
;;             (while t
;;               (when (eq (if (consp next) (car next))
;;                         'ace-window-display-mode)
;;                 (setq mode-line-format
;;                       (append left
;;                               (list next)
;;                               (list `(git--state-mark-modeline
;;                                       ,(git--state-decoration-dispatch
;;                                         stat)))
;;                               (cdr right)))
;;                 (throw 'break nil))

;;               (setq left (append left (list next))
;;                     right (cdr right)
;;                     next (car right))
;;               (when (null next)
;;                 (throw 'break nil)))))
;;       ad-do-it)))


;; (defadvice git--install-state-mark-modeline (around insert-after-ace-window-key activate)
;;   "Add git state mark modeline after ace window key"
;;   (unless (and (boundp 'powerline-git-state-mark-modeline)
;;                (and (boundp 'powerline-git-state-mark-modeline)
;;                     powerline-git-state-mark-modeline))
;;     (if (assq 'ace-window-display-mode mode-line-format)
;;         (let* ((left)
;;                (right mode-line-format)
;;                (next (car right)))
;;           (catch 'break
;;             (while t
;;               (when (eq (if (consp next) (car next))
;;                         'ace-window-display-mode)
;;                 (setq mode-line-format
;;                       (append left
;;                               (list next)
;;                               (list `(git--state-mark-modeline
;;                                       ,(git--state-decoration-dispatch
;;                                         stat)))
;;                               (cdr right)))
;;                 (throw 'break nil))

;;               (setq left (append left (list next))
;;                     right (cdr right)
;;                     next (car right))
;;               (when (null next)
;;                 (throw 'break nil)))))
;;       ad-do-it)))

;; http://blog.binchen.org/posts/yin-and-yang-in-emacs.html
(use-package vc-msg
  :commands (vc-msg-show)
  :config
  (with-eval-after-load "magit"
    (setq vc-msg-git-show-commit-function 'magit-show-commit)))

;;; magit
(use-package magit
  :defer t
  :init
  (progn
    (setq magit-completing-read-function 'ivy-completing-read) ; 'magit-ido-completing-read
    ;; (setq magit-revision-show-gravatars '("^Author:     " . "^Commit:     "))
    (autoload 'magit-grep "magit" "Command for `grep'." t) ; which is not a autload function at 2013.06.25 yet
    (tl/declare-prefix "gd" "diff")
    (tl/declare-prefix "gf" "file")
    (tl/set-leader-keys
      "gb"  'git-blame
      "gc"  'magit-clone
      "gff" 'magit-find-file
      "gfh" 'magit-log-buffer-file
      "gi"  'magit-init
      "gL"  'magit-list-repositories
      "gm"  'magit-dispatch-popup
      "gs"  'magit-status
      "gS"  'magit-stage-file
      "gU"  'magit-unstage-file)
    (global-set-key (kbd "C-x G") 'magit-status))
  :config
  (define-key magit-status-mode-map (kbd "W") 'magit-toggle-whitespace)
  (add-to-list 'magit-repository-directories '("~/.emacs.d" . 0)) ; C-u C-u M-x magit-status will ignore it
  (add-to-list 'magit-repository-directories '("~/opt/emacs" . 0)))

(use-package git-commit
  :defer 5
  :config
  (global-git-commit-mode +1))

;; (use-package magit-delta
;;   :after (magit)
;;   :hook (magit-mode . magit-delta-mode)
;;   :init
;;   (setq magit-delta-default-light-theme "Solarized (dark)"))

;; M-x magit-todos-list
(use-package magit-todos
  :defer 5
  :commands (maigt-todos-list)
  :init
  ;; FIXME: https://github.com/alphapapa/magit-todos/issues/24
  (setq magit-todos-ignored-keywords '("NOTE" "DONE" "FAIL" "PROG")
		magit-todos-exclude-globs '("legacies/*"))
  :config
  ;; automatically inserts the to-do list in Magit status buffers
  ;; (magit-todos-mode +1)
  (setq magit-todos-section-map nil))

(use-package with-editor
  :defer t
  :config
  (with-eval-after-load "evil"
    ;; start the commit window in insert mode
    (add-hook 'with-editor-mode-hook 'evil-insert-state)
    ;; add Evil bindings to accept/cancel commit
    (evil-define-key 'normal with-editor-mode-map
      ;; [escape] 'with-editor-cancel
      (kbd "RET") 'with-editor-finish)

    (add-hook 'with-editor-mode-hook 'evil-normalize-keymaps)
    (let ((mm-key dottl-major-mode-leader-key))
      (dolist (state '(normal motion))
        (evil-define-key state with-editor-mode-map
          (concat mm-key mm-key) 'with-editor-finish
          (concat mm-key "a")    'with-editor-cancel
          (concat mm-key "c")    'with-editor-finish
          (concat mm-key "k")    'with-editor-cancel)))))

;; (defun magit-toggle-whitespace ()
;;   (interactive)
;;   (if (member "-w" magit-diff-options)
;;       (magit-dont-ignore-whitespace)
;;     (magit-ignore-whitespace)))

;; (defun magit-ignore-whitespace ()
;;   (interactive)
;;   (add-to-list 'magit-diff-options "-w")
;;   (magit-refresh))

;; (defun magit-dont-ignore-whitespace ()
;;   (interactive)
;;   (setq magit-diff-options (remove "-w" magit-diff-options))
;;   (magit-refresh))

;; git-messenger
;; (require 'git-messenger)
(setq git-messenger:show-detail t)
(global-set-key (kbd "C-x v p") 'git-messenger:popup-message)


;;; darcs
;; (require 'vc-darcs)

;; It will insert the list of edited files with a colon after the filename. One of those small, simple macros that I use
;; all of the time, and thought I might share.

;; (define-key log-edit-mode-map (kbd "C-c :") 'insert-edited-files)

;; (defun insert-edited-files ()
;;   (interactive)
;;   (let ((pos (point)))
;;     (insert (mapconcat
;;              (lambda (x)
;;                (concat (file-name-nondirectory x) ": "))
;;              (log-edit-files) "\n"))
;;     (align-regexp pos (point) ": " 0 2)))

;;; diff-hl
;; https://github.com/dgutov/diff-hl
(use-package diff-hl
  :defer 3
  :config
  (global-diff-hl-mode +1)
  (with-eval-after-load "magit"
    (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
    (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)))

;;; git-gutter
;; https://github.com/syohex/emacs-git-gutter-fringe
;; it makes split window very slow, disable now
;; see also https://github.com/nonsequitur/git-gutter-plus
;; (require 'git-gutter-fringe nil t)
;; (eval-after-load "git-gutter-fringe"
;;   '(progn
;;      (setq git-gutter-fr:side 'left-fringe)))

;; (eval-after-load "git-gutter"
;;   '(progn
;;      (setq git-gutter:lighter " GG")
;;      ;; (global-set-key (kbd "C-x C-g") 'git-gutter:toggle)
;;      ;; (global-set-key (kbd "C-x v =") 'git-gutter:popup-hunk)

;;      ;; Jump to next/previous hunk
;;      ;; (global-set-key (kbd "C-x p") 'git-gutter:previous-hunk)
;;      ;; (global-set-key (kbd "C-x n") 'git-gutter:next-hunk)

;;      ;; Stage current hunk
;;      ;; (global-set-key (kbd "C-x v s") 'git-gutter:stage-hunk)
;;      ;; Revert current hunk
;;      ;; (global-set-key (kbd "C-x v r") 'git-gutter:revert-hunk)
;;      (global-git-gutter-mode t)))

;; (key-chord-define-global "GG" 'hydra-git-gutter/body)

;; (defhydra hydra-git-gutter (:body-pre (git-gutter-mode 1) :hint nil)
;;   "
;; Git gutter:
;;   _j_: next hunk        _s_tage hunk     _q_uit
;;   _k_: previous hunk    _r_evert hunk    _Q_uit and deactivate git-gutter
;;   ^ ^                   _p_opup hunk
;;   _h_: first hunk
;;   _l_: last hunk        set start _R_evision
;; "
;;   ("j" git-gutter:next-hunk)
;;   ("k" git-gutter:previous-hunk)
;;   ("h" (progn (goto-char (point-min))
;;               (git-gutter:next-hunk 1)))
;;   ("l" (progn (goto-char (point-min))
;;               (git-gutter:previous-hunk 1)))
;;   ("s" git-gutter:stage-hunk)
;;   ("r" git-gutter:revert-hunk)
;;   ("p" git-gutter:popup-hunk)
;;   ("R" git-gutter:set-start-revision)
;;   ("<escape>" nil :color blue)
;;   ("q"        nil :color blue)
;;   ("Q" (progn (git-gutter-mode -1)
;;               ;; git-gutter-fringe doesn't seem to
;;               ;; clear the markup right away
;;               (sit-for 0.1)
;;               (git-gutter:clear))
;;    :color blue))

;; Grep in a git repository using ivy
(defun counsel-git-grep-function (string &optional _pred &rest _u)
  "Grep in the current git repository for STRING."
  (split-string
   (shell-command-to-string
    (format
     "git --no-pager grep --full-name -n --no-color -i -e \"%s\""
     string))
   "\n"
   t))

(autoload 'ivy-read "ivy" "Read a string in the minibuffer, with completion.")

(autoload 'counsel-git-grep "counsel"
  "Grep for a string in the current git repository." t)

(provide '50vc)
