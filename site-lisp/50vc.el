;;; backup
(setq make-backup-files t)
(setq backup-directory-alist (quote ((".*" . "~/.backup/temp/"))) ; don't litter my fs tree)
      version-control t                                          ; Use version numbers for backups
      kept-new-versions 16                                       ; Number of newest versions to keep
      kept-old-versions 2                                        ; Number of oldest versions to keep
      delete-old-versions t                                      ; Ask to delete excess backup versions?
      ;; Preserve the owner and group of the file you’re editing (this is especially important if you edit files as root).
      backup-by-copying-when-mismatch t
      backup-by-copying t               ; don't clobber symlinks
      ;; Copy linked files, don't rename.
      backup-by-copying-when-linked t)
;;;; VC
;; While we are thinking about symlinks, I don't like being asked whether I want to follow a symlink; I do, already!
(setq vc-follow-symlinks t
      vc-initial-comment t
      ;; auto-revert-check-vc-info t
      ;; vc-cvs-stay-local nil
      vc-diff-switches diff-switches)

;;; Git
;; ls /usr/share/doc/git-core/contrib/emacs git-blame.el  ==> git.el  Makefile  vc-git.el
(require 'vc-git) ;vc-git.el a VersionControl backend (This is part of Emacs since version 22.2
(when (featurep 'vc-git)
  (add-to-list 'vc-handled-backends 'git))

;; ----------------------------------------------------------------

;;; gitsum: do interactive partial commits with Emacs in the style of darcs record.
(autoload 'gitsum "gitsum" "Entry point into gitsum-diff-mode." t)

;; git.el will look into your git configuration for committer name and email address. If that fails, it uses the Emacs
;; defaults. If you rely on environment variables for your configuration, beware. (And consider git-committer-name and
;; git-committer-email)

;;; git-emacs
;; http://tsgates.cafe24.com/git/git-emacs.html
(require 'git-emacs)
(when window-system
  (setq git-state-modeline-decoration 'git-state-decoration-small-dot))   ; git-state-decoration-colored-letter

;;; magit
(global-set-key (kbd "C-x G") 'magit-status)
(eval-after-load "magit"
  '(progn
     (define-key magit-status-mode-map (kbd "W") 'magit-toggle-whitespace)
     (setq magit-completing-read-function 'magit-ido-completing-read)))
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

;;; git-gutter
(require 'git-gutter-fringe nil t)
(eval-after-load "git-gutter-fringe"
  '(progn
     (setq git-gutter-fr:side 'right-fringe)))

(eval-after-load "git-gutter"
  '(progn
     (setq git-gutter:lighter " GG")
     (global-git-gutter-mode t)))


(provide '50vc)
