;;; 50elfeed.el ---                                  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>
;;; Code:


(defun paloryemacs/elfeed-mark-all-as-read ()
  (interactive)
  (when (y-or-n-p "Really mark all items as read?")
    (mark-whole-buffer)
    (elfeed-search-untag-all-unread)))

;; http://pragmaticemacs.com/emacs/read-your-rss-feeds-in-emacs-with-elfeed/
;; functions to support syncing .elfeed between machines
;; makes sure elfeed reads index from disk before launching
(defun paloryemacs/elfeed-load-db-and-open ()
  "Wrapper to load the elfeed db from disk before opening"
  (interactive)
  (elfeed-db-load)
  (elfeed)
  (elfeed-search-update--force))

;;write to disk when quiting
(defun paloryemacs/elfeed-save-db-and-bury ()
  "Wrapper to save the elfeed db to disk before burying buffer"
  (interactive)
  (elfeed-db-save)
  (quit-window))

(defun paloryemacs/elfeed-show-emacs ()
  (interactive)
  (bookmark-maybe-load-default-file)
  (bookmark-jump "elfeed-emacs"))

(defalias 'paloryemacs/elfeed-toggle-star
  (elfeed-expose #'elfeed-search-toggle-all 'star))

(defhydra paloryemacs/hydra-elfeed ()
  "Filter"
  ("c" (elfeed-search-set-filter "@6-months-ago +cs") "cs")
  ("e" (elfeed-search-set-filter "@6-months-ago +emacs") "emacs")
  ("d" (elfeed-search-set-filter "@6-months-ago +education") "education")
  ("*" (elfeed-search-set-filter "@6-months-ago +star") "Starred")
  ("m" paloryemacs/elfeed-toggle-star "Mark")
  ("A" (elfeed-search-set-filter "@6-months-ago") "All")
  ("T" (elfeed-search-set-filter "@1-day-ago") "Today")
  ("Q" paloryemacs/elfeed-save-db-and-bury "Quit Elfeed" :color blue)
  ("q" nil "quit" :color blue))


(defun popwin:elfeed-show-entry (buff)
  (popwin:popup-buffer buff
                       :position 'right
                       :width 0.5
                       :dedicated t
                       :noselect t
                       :stick t))

(defun popwin:elfeed-kill-buffer ()
  (interactive)
  (let ((window (get-buffer-window (get-buffer "*elfeed-entry*"))))
    (kill-buffer (get-buffer "*elfeed-entry*"))
    (delete-window window)))

(use-package elfeed
  :defer t
  :init
  (progn
    (paloryemacs/set-leader-keys "af" 'elfeed)
    (setq-default elfeed-search-filter "@1-week-ago +unread ")
    (setq elfeed-use-curl t))
  :config
  (progn
    (setq elfeed-show-entry-switch #'popwin:elfeed-show-entry
          elfeed-show-entry-delete #'popwin:elfeed-kill-buffer)

    (use-package elfeed-org
      :init
      (progn
        (setq rmh-elfeed-org-files (list "~/.emacs.d/elfeed.org"))
        (setq rmh-elfeed-org-auto-ignore-invalid-feeds t)
        (elfeed-org)))

    (add-hook 'elfeed-new-entry-hook
              (elfeed-make-tagger :feed-url "cnbeta\\.com"
                                  :add '(news)))


    (evilified-state-evilify-map elfeed-search-mode-map
      :mode elfeed-search-mode
      :eval-after-load elfeed-search
      :bindings
	  "Q"  'paloryemacs/elfeed-save-db-and-bury
      "A"  'paloryemacs/elfeed-mark-all-as-read
      "c"  'elfeed-db-compact
      "gr" 'elfeed-update
      "gR" 'elfeed-search-update--force
      "gu" 'elfeed-unjam
      "o"  'elfeed-load-opml
      "q"  'quit-window
      "m"  'paloryemacs/elfeed-toggle-star
      "f"  'paloryemacs/hydra-elfeed/body
      "w"  'elfeed-web-start
      "W"  'elfeed-web-stop)
    (evilified-state-evilify-map elfeed-show-mode-map
      :mode elfeed-show-mode
      :eval-after-load elfeed-show
      :bindings
      "q" 'quit-window
      (kbd "C-j") 'elfeed-show-next
      (kbd "C-k") 'elfeed-show-prev)))


(provide '50elfeed)
;;; 50elfeed.el ends here
