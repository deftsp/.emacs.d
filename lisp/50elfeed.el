;;; 50elfeed.el ---                                  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>
;;; Code:


(defun tl/elfeed-mark-all-as-read ()
  (interactive)
  (when (y-or-n-p "Really mark all items as read?")
    (mark-whole-buffer)
    (elfeed-search-untag-all-unread)))

;; http://pragmaticemacs.com/emacs/read-your-rss-feeds-in-emacs-with-elfeed/
;; functions to support syncing .elfeed between machines
;; makes sure elfeed reads index from disk before launching
(defun tl/elfeed-load-db-and-open ()
  "Wrapper to load the elfeed db from disk before opening"
  (interactive)
  (elfeed-db-load)
  (elfeed)
  (elfeed-search-update--force))

;;write to disk when quiting
(defun tl/elfeed-save-db-and-bury ()
  "Wrapper to save the elfeed db to disk before burying buffer"
  (interactive)
  (elfeed-db-save)
  (quit-window))

(defun tl/elfeed-show-emacs ()
  (interactive)
  (bookmark-maybe-load-default-file)
  (bookmark-jump "elfeed-emacs"))

(defhydra tl/hydra-elfeed ()
  "Filter"
  ("c" (elfeed-search-set-filter "@6-months-ago +cs") "cs")
  ("e" (elfeed-search-set-filter "@6-months-ago +emacs") "emacs")
  ("d" (elfeed-search-set-filter "@6-months-ago +education") "education")
  ("*" (elfeed-search-set-filter "@6-months-ago +star") "Starred")
  ("m" tl/elfeed-toggle-star "Mark")
  ("A" (elfeed-search-set-filter "@6-months-ago") "All")
  ("T" (elfeed-search-set-filter "@1-day-ago") "Today")
  ("Q" tl/elfeed-save-db-and-bury "Quit Elfeed" :color blue)
  ("q" nil "quit" :color blue))


(defun popwin:elfeed-show-entry (buff)
  (popwin:popup-buffer buff
                       :position 'bottom
                       :width 0.5
                       :dedicated t
                       :noselect t
                       :stick t))

(defun popwin:elfeed-kill-buffer ()
  (interactive)
  (let ((buf (get-buffer "*elfeed-entry*"))
        (win (get-buffer-window (get-buffer "*elfeed-entry*"))))
    (when buf
      (kill-buffer buf))
    (when (not (one-window-p))
      (delete-window win))))

(defun tl/elfeed-quit ()
  (interactive)
  (let ((win (get-buffer-window "*elfeed-entry*")))
    (when (not (one-window-p))
      (delete-window win)))
  (quit-window))

(use-package elfeed
  :defer t
  :init
  (progn
    (tl/set-leader-keys "af" 'elfeed)
    (setq-default elfeed-search-filter "@1-week-ago +unread ")
    (setq elfeed-use-curl t))
  :config
  (progn
    (setq elfeed-show-entry-switch #'popwin:elfeed-show-entry
          elfeed-show-entry-delete #'popwin:elfeed-kill-buffer)

    (defalias 'tl/elfeed-toggle-star
      (elfeed-expose #'elfeed-search-toggle-all 'star))

    (use-package elfeed-show
      :config
      (progn
        (defun tl/elfeed-show-mode-init ()
          (setq truncate-lines nil))
        (add-hook 'elfeed-show-mode-hook 'tl/elfeed-show-mode-init)))

    (use-package elfeed-org
      :init
      (progn
        (setq rmh-elfeed-org-files (list "~/.emacs.d/elfeed.org"))
        (setq rmh-elfeed-org-auto-ignore-invalid-feeds t)
        (elfeed-org)))

    (use-package elfeed-search
      :config
      (progn
        ;; evilify bind it
        (define-key elfeed-search-mode-map  "G" nil)))

    (general-define-key
     :states 'normal
     :keymaps 'elfeed-search-mode-map
     "Q"  'tl/elfeed-save-db-and-bury
     "A"  'tl/elfeed-mark-all-as-read
     "c"  'elfeed-db-compact
     "gr" 'elfeed-update
     "gR" 'elfeed-search-update--force
     "gu" 'elfeed-unjam
     "o"  'elfeed-load-opml
     "q"  'tl/elfeed-quit
     "m"  'tl/elfeed-toggle-star
     "f"  'tl/hydra-elfeed/body
     "w"  'elfeed-web-start
     "W"  'elfeed-web-stop)


    (general-define-key
     :states 'normal
     :keymaps 'elfeed-show-mode-map

     "q" 'quit-window
     (kbd "C-j") 'elfeed-show-next
     (kbd "C-k") 'elfeed-show-prev)

    (add-hook 'elfeed-new-entry-hook
              (elfeed-make-tagger :feed-url "cnbeta\\.com"
                                  :add '(news)))))


(provide '50elfeed)
;;; 50elfeed.el ends here
