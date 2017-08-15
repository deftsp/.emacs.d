;;; 52ido.el ---

;; Copyright (C) 2008  Shihpin Tseng
;; Author: Shihpin Tseng <deftsp@gmail.com>

;; ido seem much less annoying than icicles...
(ido-mode t)                  ;  Turn on ido buffer and file behavior.
(ido-everywhere t)
;; (ido-hacks-mode 1) ; use ido-ubiquitous-mode instead, it can work with info.

(when (fboundp 'ido-ubiquitous-mode)
  (setq ido-ubiquitous-max-items 5000)
  (ido-ubiquitous-mode 1))

;;; flx
;; https://github.com/lewang/flx
(require 'flx-ido nil t)
(eval-after-load "flx-ido"
  '(progn
     (setq flx-ido-threshold 12000 ; see also gc-cons-threshold.
           ;; disable ido faces to see flx highlights.
           ido-use-faces nil
           flx-ido-use-faces t)
     (flx-ido-mode 1)))

;;; ido-vertical-mode
;; https://github.com/gempesaw/ido-vertical-mode.el
(when (fboundp 'ido-vertical-mode)
  (setq ido-vertical-define-keys 'C-n-C-p-up-down)
  (ido-vertical-mode +1))


(setq ido-enable-prefix nil
      ido-enable-regexp t
      ido-case-fold  t                  ; be case-insensitive
      ido-create-new-buffer 'always
      ido-max-dir-file-cache 200        ; default 100
      ido-max-prospects 6               ; default 12
      ido-auto-merge-delay-time -1      ; default 0.7
      ido-auto-merge-work-directories-length 0
      ido-show-dot-for-dired nil
      ;; use `find-file-at-point' that I have binding to `C-x f' instead.
      ido-use-filename-at-point nil
      ido-use-url-at-point nil          ; ... or url at point
      ido-use-virtual-buffers t
      ido-enable-tramp-completion t
      ido-default-buffer-method 'samewindow

      ;; ido-enable-flex-matching means that if the entered string does not match any buffer name, any buffer name containing
      ;; the entered characters in the given sequence will match.
      ido-enable-flex-matching t
      ;;*Non-nil means that even a unique completion must be confirmed.
      ido-confirm-unique-completion t)

;;; ido-better-flex
;; it is a little slow when use with smex
;; (require 'ido-better-flex nil t)
;; (with-eval-after-load "ido-better-flex"
;;   (ido-better-flex/enable))


;;;
;; C-f         fallback to non-ido `find-file' or switch to `ido-find-file' in ido-switch-buffer
;; C-j         If no buffer or file exactly matching the prompt exists, maybe create a new one.
;; M-m         current directory
;; C-e         Edit absolute file name entered so far with ido; terminate by RET.

;; M-n, M-p    change working directory in list
;; M-o, C-M-o  change working file name in list.
;; M-f, M-d    find file or directory will find commnd from current direcotry

;; C-t         toggle regexp
;; C-p         toggle prefix
;; C-c         toggle case
;; M-l         Toggle literal reading of this file.
;; C-a         toggle ignoring files specified with `ido-ignore-files'.
;; // -        go to the root directory.
;; ~/ -        go to the home directory.

(mapcar (lambda (str) (add-to-list 'ido-ignore-buffers str))
        '("^\\ " "^\\*Completions*" "^\\*Article\\*" "^\\*Apropos*"
          "^\\*Ibuffer*" "^\\*Backtrace*"  "^\\*Help"  "^\\*Bookmark"
          "^\\*Messages" "^\\.newsrc-dribble"  "^\\*Woman-Log"
          "^\\*Compilation" "^\\*Compile-Log" "^\\*Calendar"
          "^\\*cscope"  "^\\*grep" "*BBDB*" "*Tree*"  "*Group*"
          "*Helm Swoop*"  "*EMMS Playlist*"  "^\\*Summary\\ n" "Map_Sym.txt"
          "^\\*w3m*" "^\\#" "^\\irc.*:" "localhost:6668" "^\\*TeX\\ Help\\*"))

(mapcar (lambda (str) (add-to-list 'ido-ignore-files str))
        '("\\`auto/" "\\.prv/" "_region_" "^.DS_Store$" "\\.hi\\'"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;uses ido on the recently opened files
(global-set-key (kbd "C-x C-r") 'paloryemacs/ido-choose-from-recentf)
(defun paloryemacs/ido-choose-from-recentf ()
  "Use ido to select a recently opened file from the `recentf-list'"
  (interactive)
  (find-file
   (ido-completing-read "Recentf open: "
                        (mapcar (lambda (path)
                                  (replace-regexp-in-string
                                   (expand-file-name "~") "~" path))
                                recentf-list)
                        nil t)))

;;; ido copy selection
(defun paloryemacs/ido-copy-selection ()
  "Copy the current ido selection to the kill ring."
  (interactive)
  (kill-new
   (abbreviate-file-name
    (concat ido-current-directory
            ido-text))))

;;; Using ido to open files from file name cache-------------------------------
(defun paloryemacs/ido-file-cache-find-file (&optional init-text)
  ;;   "Using ido, interactively open file from file cache'.
  ;; First select a file, matched using ido-switch-buffer against the contents
  ;; in `file-cache-alist'. If the file exist in more than one
  ;; directory, select directory. Lastly the file is opened."
  ;;   (interactive (list (paloryemacs/ido-file-cache-read "File: "
  (interactive)
  (let* ((file-name-list (mapcar (lambda (x)
                                   (car x))
                                 file-cache-alist))
         (file (let ((ido-make-buffer-list-hook
                      (lambda ()
                        (setq ido-temp-list file-name-list)
                        (setq ido-text-init (if init-text init-text "")))))
                 (ido-read-buffer "File: ")))
         (record (assoc file file-cache-alist)))

    (ido-set-current-directory (cadr record))

    (find-file
     (expand-file-name
      file
      (if (= (length record) 2)
          (cadr record)

        (let ((ido-make-buffer-list-hook
               (lambda ()
                 (setq ido-text-init "")
                 (setq ido-temp-list (cdr record)))))
          (ido-read-buffer (format "Find %s in dir: " file))))))))

(global-set-key (kbd "ESC ESC f") 'paloryemacs/ido-file-cache-find-file) ; equal to 'C-[ C-[ f'
(define-key minibuffer-local-map [C-tab] 'paloryemacs/ido-magic-file-cache)

(defun paloryemacs/ido-magic-file-cache (arg)
  "Drop into `paloryemacs/ido-file-cache-find-file'."
  (interactive "P")
  (when (memq ido-cur-item '(file buffer))
    (setq ido-exit 'refresh)
    (paloryemacs/ido-file-cache-find-file ido-text)
    (exit-minibuffer)))


;; ---------------------------------------------------------------------------------

;;; Find files in Tags File
(defun paloryemacs/ido-find-file-in-tag-files ()
  (interactive)
  (save-excursion
    (let ((enable-recursive-minibuffers t))
      (visit-tags-table-buffer))
    (find-file
     (expand-file-name
      (ido-completing-read
       "Project file: " (tags-table-files) nil t)))))

(global-set-key (kbd "C-x f") 'paloryemacs/ido-find-file-in-tag-files)


;; Invoking bookmarks from ido
(define-key ido-file-dir-completion-map [(meta control ?b)] 'ido-goto-bookmark)
(autoload 'bookmark-completing-read "bookmark" t)
(defun ido-goto-bookmark (bookmark)
  (interactive
   (list (bookmark-completing-read "Jump to bookmark"
                                   bookmark-current-bookmark)))
  (unless bookmark
    (error "No bookmark specified"))
  (let ((filename (bookmark-get-filename bookmark)))
    (ido-set-current-directory
     (if (file-directory-p filename)
         filename
       (file-name-directory filename)))
    (setq ido-exit        'refresh
          ido-text-init   ido-text
          ido-rotate-temp t)
    (exit-minibuffer)))

;; (setq ido-execute-command-cache nil)
;;; ido-execute-command
;; (defun ido-execute-command ()
;;   (interactive)
;;   (call-interactively
;;    (intern
;;     (ido-completing-read
;;      "M-x "
;;      (progn
;;        (unless ido-execute-command-cache
;;          (mapatoms (lambda (s)
;;                      (when (commandp s)
;;                        (setq ido-execute-command-cache
;;                              (cons (format "%S" s) ido-execute-command-cache))))))
;;        ido-execute-command-cache)))))

;; (add-hook 'ido-setup-hook
;;           (lambda ()
;;             (setq ido-enable-flex-matching t)
;;             (global-set-key "\M-x" 'ido-execute-command)))

(add-hook 'ido-setup-hook 'paloryemacs/ido-keys)

(defun paloryemacs/ido-keys ()
  "Add my keybindings for ido."
  (when ido-vertical-mode
    (define-key ido-completion-map (kbd "M-j") 'ido-next-match)
    (define-key ido-completion-map (kbd "M-k") 'ido-prev-match))
  (define-key ido-completion-map (kbd "C-.") 'ido-delete-backward-updir)
  ;; (define-key ido-completion-map (kbd "C-k") 'paloryemacs/ido-erase-minibuffer-or-dwim)
  (define-key ido-completion-map (kbd "ESC ESC k") 'ido-delete-file-at-head))

;;; // - go to the root directory.
;;; ~/ - go to the home directory.
;; (defun paloryemacs/ido-erase-minibuffer-or-dwim ()
;;   "If cursor the EOL erases whole minibuffer and insert  `~/'.
;; If cursor at the EOL and the whole minibuffer is `~/', erase whole minibuffer.
;; Or else erases whole minibuffer. "
;;   (interactive)
;;   (flet ((ido-chdir-1 (&optional dir)
;;            (setq ido-text-init (if dir dir "~/"))
;;            (setq ido-exit 'chdir)
;;            (exit-minibuffer)))
;;     (if (eolp)
;;         (if (string= ido-current-directory "~/")
;;             (ido-chdir-1 "/")
;;             (ido-chdir-1))
;;         (ido-chdir-1 ))))

;;; edit as root
;; find file with ido and open it with sudo
(defun paloryemacs/ido-sudo-edit (&optional arg)
  (interactive "p")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:" (ido-read-file-name "File: ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

;; http://emacsredux.com/blog/2013/04/21/edit-files-as-root/
;; This advises ido-find-file(you might want to advise find-file instead if
;; you’re not using ido) to reopen the selected file as root(you’ll be
;; prompted for your sudo password) if you don’t have write permissions for it.
(defadvice ido-find-file (after find-file-sudo activate)
  "Find file as root if necessary."
  (unless (and buffer-file-name
               (file-writable-p buffer-file-name))
    (find-alternate-file (concat "/sudo:" user-login-name
                                 "@localhost:" buffer-file-name))))


;;; Occasionally ido
;; http://oremacs.com/2015/02/12/ido-occasional/
(require 'ido-occasional nil t)

(with-eval-after-load "ido-occasional"
  ;; (global-set-key (kbd "C-h i") (with-ido-completion info-lookup-symbol))
  (global-set-key (kbd "C-h f") (with-ido-completion describe-function))
  (global-set-key (kbd "C-h v") (with-ido-completion describe-variable)))

(provide '52ido)
