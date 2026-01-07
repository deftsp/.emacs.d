;;; 50calibre.el ---                                    -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>
;; Keywords:

;;; Code:

(use-package calibredb
  :defer t
  :config
  ;; The maximum number of entries to display in a single page:
  (setq calibredb-search-page-max-rows 44)
  ;; Set positive to limit the width.
  ;; Set 0 to hide.
  ;; Set -1 to keep original length.
  (setq calibredb-id-width 4)
  ;; (setq calibredb-size-show t)
  (setq calibredb-format-nerd-icons t)
  (setq calibredb-format-all-the-icons t)
  (setq calibredb-format-icons-in-terminal t)
  (setq calibredb-format-character-icons t)
  ;; (setq calibredb-list-refresh-after-action t)
  (setq calibredb-root-dir "~/Calibre Library")
  ;; for folder driver metadata: it should be .metadata.calibre
  (setq calibredb-db-dir (expand-file-name "metadata.db" calibredb-root-dir))
  ;; (setq calibredb-library-alist '(("~/OneDrive/Org/Doc/Calibre" (name . "Calibre")) ;; with name
  ;;                                 ("~/Documents/Books Library") ;; no name
  ;;                                 ("~/Documents/LIB1")
  ;;                                 ("/Volumes/ShareDrive/Documents/Library/"))))

  (with-eval-after-load 'calibredb
    (with-eval-after-load 'evil
      (evil-set-initial-state 'calibredb-search-mode 'emacs)
      (evil-set-initial-state 'calibredb-show-mode 'emacs)
      (evil-set-initial-state 'calibredb-search-mode 'emacs))))


(provide '50calibre)
;;; 50calibre.el ends here
