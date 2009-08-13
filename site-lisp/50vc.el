;;backup
;;emacs还有一个自动保存功能，默认在~/.emacs.d/auto-save-list里。

;; Enable backup files.
(setq make-backup-files t)
(defconst use-backup-dir t)
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

;; return a backup file path of a give file path with full directory mirroring from a root dir non-existant dir will be
;; created
;; (defun my-backup-file-name (fpath)
;;   "Return a new file path of a given file path.
;; If the new path's directories does not exist, create them."
;;   (let (backup-root bpath)
;;     (setq backup-root "~/.emacs.d/emacs-backup")
;;     (setq bpath (concat backup-root fpath "~"))
;;     (make-directory (file-name-directory bpath) bpath)
;;     bpath))
;; (setq make-backup-file-name-function 'my-backup-file-name)

;; The above will mirror all directories at the given backup dir. For example, if you are editing a file
;; "/Users/jane/web/xyz/myfile.txt", and your backup root is "/Users/jane/.emacs.d/emacs-backup", then the backup will
;; be at "/Users/jane/.emacs.d/emacs-backup/Users/jane/web/xyz/myfile.txt~".

;; If you want all backup to be flat in a dir, use the following:

;; (setq backup-directory-alist '(("" . "~/.emacs.d/emacs-backup")))

;; This will create backup files flat in the given dir, and the backup file names will have "!" characters in place of
;; the directory separator. For example, if you are editing a file at "/Users/jane/web/xyz/myfile.txt", and your backup
;; dir is set at "/Users/jane/.emacs.d/emacs-backup", then the backup file will be at:
;; "/Users/jane/.emacs.d/emacs-backup/Users!jane!web!emacs!myfile.txt~". If you use long file names or many nested dirs,
;; this scheme will reach file name length limit quickly.

;;;; VC

;;While we are thinking about symlinks, I don't like being asked whether I want to follow a symlink; I do, already!
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

;;; magit
(add-to-list 'load-path "~/.emacs.d/packages/magit")
(autoload 'magit-status "magit" nil t)

;; (setq load-path (cons (expand-file-name "/usr/share/doc/git-core/contrib/emacs") load-path))
;; (require 'git)
;; (autoload 'git-blame-mode "git-blame"
;;   "Minor mode for incremental blame for Git." t)

;; (setq git--repository-bookmarks
;;       '("git://github.com/xcezx/scratch.git"
;;         "git://github.com/xcezx/dotfiles.git"))

;; git.el will look into your git configuration for committer name and email address. If that fails, it uses the Emacs
;; defaults. If you rely on environment variables for your configuration, beware. (And consider git-committer-name and
;; git-committer-email)

;;; git-emacs
;; http://tsgates.cafe24.com/git/git-emacs.html
;; (add-to-list 'load-path "~/.emacs.d/packages/git-emacs")
(require 'git-emacs)
(setq git--state-mark-modeline nil)
;;; darcs
(require 'vc-darcs)

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

