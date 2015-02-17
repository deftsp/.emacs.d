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


;; Fix ido-ubiquitous for newer packages
;; http://whattheemacsd.com//setup-ido.el-01.html
(defmacro ido-ubiquitous-use-new-completing-read (cmd package)
  `(eval-after-load ,package
     '(defadvice ,cmd (around ido-ubiquitous-new activate)
        (let ((ido-ubiquitous-enable-compatibility nil))
          ad-do-it))))

(ido-ubiquitous-use-new-completing-read slime-js-read-remote-index 'slime-js)
;; (ido-ubiquitous-use-new-completing-read webjump 'webjump)
;; (ido-ubiquitous-use-new-completing-read yas/expand 'yasnippet)
;; (ido-ubiquitous-use-new-completing-read yas/visit-snippet-file 'yasnippet)

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
;; (eval-after-load "ido-better-flex"
;;   '(progn
;;      (ido-better-flex/enable)))


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
        '("^\\ " "^\\*Completions*" "^\\*Article\\*" "^\\*Apropos*"  "^\\*Ibuffer*"
          "^\\*Backtrace*"  "^\\*Help"  "^\\*Bookmark" "^\\*Messages"
          "^\\.newsrc-dribble"  "^\\*Woman-Log"  "^\\*Compilation"
          "^\\*Compile-Log" "^\\*Calendar" "^\\*cscope"  "^\\*grep"
          "*BBDB*" "*Tree*"  "*Group*" "*EMMS Playlist*"  "^\\*Summary\\ n"
          "Map_Sym.txt" "^\\*w3m*" "^\\#" "^\\irc.*:" "localhost:6668" "^\\*TeX\\ Help\\*"))


(mapcar (lambda (str) (add-to-list 'ido-ignore-files str))
        '("\\`auto/" "\\.prv/" "_region_" "^.DS_Store$" "\\.hi\\'"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;uses ido on the recently opened files
(global-set-key (kbd "C-x C-r") 'pl/ido-choose-from-recentf)
(defun pl/ido-choose-from-recentf ()
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
(defun pl/ido-copy-selection ()
  "Copy the current ido selection to the kill ring."
  (interactive)
  (kill-new
   (abbreviate-file-name
    (concat ido-current-directory
            ido-text))))

;;; Using ido to open files from file name cache-------------------------------
(defun pl/ido-file-cache-find-file (&optional init-text)
  ;;   "Using ido, interactively open file from file cache'.
  ;; First select a file, matched using ido-switch-buffer against the contents
  ;; in `file-cache-alist'. If the file exist in more than one
  ;; directory, select directory. Lastly the file is opened."
  ;;   (interactive (list (pl/ido-file-cache-read "File: "
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

(global-set-key (kbd "ESC ESC f") 'pl/ido-file-cache-find-file) ; equal to 'C-[ C-[ f'
(define-key minibuffer-local-map [C-tab] 'pl/ido-magic-file-cache)

(defun pl/ido-magic-file-cache (arg)
  "Drop into `pl/ido-file-cache-find-file'."
  (interactive "P")
  (when (memq ido-cur-item '(file buffer))
    (setq ido-exit 'refresh)
    (pl/ido-file-cache-find-file ido-text)
    (exit-minibuffer)))


;; ---------------------------------------------------------------------------------

;;; Find files in Tags File
(defun pl/ido-find-file-in-tag-files ()
  (interactive)
  (save-excursion
    (let ((enable-recursive-minibuffers t))
      (visit-tags-table-buffer))
    (find-file
     (expand-file-name
      (ido-completing-read
       "Project file: " (tags-table-files) nil t)))))

(global-set-key (kbd "C-x f") 'pl/ido-find-file-in-tag-files)


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

(add-hook 'ido-setup-hook 'pl/ido-keys)

(defun pl/ido-keys ()
  "Add my keybindings for ido."
  (when ido-vertical-mode
    (define-key ido-completion-map (kbd "M-j") 'ido-next-match)
    (define-key ido-completion-map (kbd "M-k") 'ido-prev-match))
  (define-key ido-completion-map (kbd "C-.") 'ido-delete-backward-updir)
  ;; (define-key ido-completion-map (kbd "C-k") 'pl/ido-erase-minibuffer-or-dwim)
  (define-key ido-completion-map (kbd "ESC ESC k") 'ido-delete-file-at-head))

;;; // - go to the root directory.
;;; ~/ - go to the home directory.
;; (defun pl/ido-erase-minibuffer-or-dwim ()
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

;;; imenu goto symbol
;;; http://emacswiki.org/emacs/ImenuMode
(global-set-key (kbd "C-c j") 'pl/ido-goto-symbol)
(defun pl/ido-goto-symbol (&optional symbol-list)
  "Refresh imenu and jump to a place in the buffer using Ido."
  (interactive)
  (unless (featurep 'imenu)
    (require 'imenu nil t))
  (cond
   ((not symbol-list)
    (let ((ido-mode ido-mode)
          (ido-enable-flex-matching
           (if (boundp 'ido-enable-flex-matching)
               ido-enable-flex-matching t))
          name-and-pos symbol-names position)
      (unless ido-mode
        (ido-mode 1)
        (setq ido-enable-flex-matching t))
      (while (progn
               (imenu--cleanup)
               (setq imenu--index-alist nil)
               (pl/ido-goto-symbol (imenu--make-index-alist))
               (setq selected-symbol
                     (ido-completing-read "Symbol: " symbol-names))
               (string= (car imenu--rescan-item) selected-symbol)))
      (unless (and (boundp 'mark-active) mark-active)
        (push-mark nil t nil))
      (setq position (cdr (assoc selected-symbol name-and-pos)))
      (cond
       ((overlayp position)
        (goto-char (overlay-start position)))
       (t
        (goto-char position)))))
   ((listp symbol-list)
    (dolist (symbol symbol-list)
      (let (name position)
        (cond
         ((and (listp symbol) (imenu--subalist-p symbol))
          (pl/ido-goto-symbol symbol))
         ((listp symbol)
          (setq name (car symbol))
          (setq position (cdr symbol)))
         ((stringp symbol)
          (setq name symbol)
          (setq position
                (get-text-property 1 'org-imenu-marker symbol))))
        (unless (or (null position) (null name)
                    (string= (car imenu--rescan-item) name))
          (add-to-list 'symbol-names name)
          (add-to-list 'name-and-pos (cons name position))))))))

;;; find file with ido and open it with sudo
(defun pl/ido-sudo-edit (&optional arg)
  (interactive "p")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:" (ido-read-file-name "File: ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

;;; Occasionally ido
;; http://oremacs.com/2015/02/12/ido-occasional/
;; The only thing that ido-occasional-completing-read does is to pre-filter
;; collection with predicate and pass it on to ido-completing-read.
(defun ido-occasional-completing-read
    (prompt collection
            &optional predicate require-match initial-input
            hist def inherit-input-method)
  "Use `ido-completing-read' if the collection isn't too large.
Fall back to `completing-read' otherwise."
  (let ((filtered-collection
         (all-completions "" collection predicate)))
    (if (<= (length filtered-collection) 30000)
        (ido-completing-read
         prompt filtered-collection nil
         require-match initial-input hist
         def nil)
      (completing-read
       prompt collection predicate
       require-match initial-input hist
       def inherit-input-method))))

;;;###autoload
(defmacro with-ido-completion (fun)
  "Wrap FUN in another interactive function with ido completion."
  `(defun ,(intern (concat (symbol-name fun) "/with-ido")) ()
     ,(format "Forward to `%S' with ido completion." fun)
     (interactive)
     (let ((completing-read-function
            'ido-occasional-completing-read))
       (call-interactively #',fun))))

(global-set-key (kbd "C-h f") (with-ido-completion describe-function))
(global-set-key (kbd "C-h v") (with-ido-completion describe-variable))
;; (global-set-key (kbd "C-h i") (with-ido-completion info-lookup-symbol))


(provide '52ido)
