;;; tsp-ido.el ---

;; Copyright (C) 2008  S.P.Tseng
;; Author: S.P.Tseng <deftsp@gmail.com>

;; ido seem much less annoying than icicles...
(ido-mode t)                  ;  Turn on ido buffer and file behavior.
;; turn off  ido speed-ups everywhere file and directory names are read, otherwise `C-Tab'
;; (file-cache-minibuffer-complete) can not replace the previous path
(ido-everywhere t)
(setq ido-enable-prefix nil
      ido-enable-regexp t
      ido-case-fold  t                  ; be case-insensitive
      ido-create-new-buffer 'always
      ido-max-dir-file-cache 200        ; default 100
      ido-max-prospects 6               ; don't spam my minibuffer
      ido-auto-merge-delay-time -1      ; default 0.7
      ido-auto-merge-work-directories-length 0
      ido-show-dot-for-dired nil
      ;; use `find-file-at-point' that I have binding to `C-x f' instead.
      ido-use-filename-at-point nil
      ido-use-url-at-point nil            ; ... or url at point
      ido-enable-tramp-completion t
      ido-default-buffer-method 'samewindow

      ;; ido-enable-flex-matching means that if the entered string does not match any buffer name, any buffer name containing
      ;; the entered characters in the given sequence will match.
      ido-enable-flex-matching t
      ;;*Non-nil means that even a unique completion must be confirmed.
      ido-confirm-unique-completion t)

;;;
;; C-f         切换成普通的 Minibuffer 输入模式
;; C-j         新建或打开文件
;; M-m         新建目录
;; C-s, C-r    循环可选文件列表
;; C-e         编辑

;; M-n, M-p    工作目录历史
;; M-o, C-M-o  工作文件历史
;; M-f, M-d    使用 find 从当前位置查找文件或目录

;; C-t         Regexp 匹配开关
;; C-p         前缀/子串匹配开关
;; C-c         区分大小写匹配开关
;; C-v         版本控制开关
;; M-l         无修饰的直接打开文件

;; 有时候你要创建一个新文件，这个文件名刚好是目录下某个文件名的一部分，如果直接回车会打开已经存在文件。解决的办法是输入文
;; 件名后用 C-j 选择当前的输入。

;; 在选择缓冲区时，默认隐藏的缓冲区（也就是缓冲区名的第一个字符是空格）是不显示的。可以用 C-a 切换显示隐藏的缓冲区。

;; ido 匹配时默认是可以从文件名或缓冲区名任何位置开始。可以用 C-p 切换成前缀匹配。

;; ido 的匹配可以是精确匹配，也可以是使用正则表达式。控制的选项是 ido-enable-regexp 变量。也可以用 C-t 临时切换匹配方式。

(mapcar (lambda (str) (add-to-list 'ido-ignore-buffers str))
        '("^\\ " "^\\*Completions*" "^\\*Article\\*" "^\\*Apropos*"  "^\\*Ibuffer*"
          "^\\*Backtrace*"  "^\\*Help"  "^\\*Bookmark" "^\\*Messages"
          "^\\.newsrc-dribble"  "^\\*Woman-Log"  "^\\*Compilation"
          "^\\*Compile-Log" "^\\*Calendar" "^\\*cscope"  "^\\*grep"
          "*BBDB*" "*Tree*"  "*Group*" "*EMMS Playlist*"  "^\\*Summary\\ n"
          "Map_Sym.txt" "^\\*w3m*" "^\\#" "^\\irc.*:" "localhost:6668" "^\\*TeX\\ Help\\*"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;uses ido on the recently opened files
(global-set-key (kbd "C-c r f") 'ido-choose-from-recentf)
(defun ido-choose-from-recentf ()
  "Use ido to select a recently opened file from the `recentf-list'"
  (interactive)
  (let ((home (expand-file-name (getenv "HOME"))))
    (find-file
     (ido-completing-read "Recentf open: "
                          (mapcar (lambda (path)
                                    (replace-regexp-in-string home "~" path))
                                  recentf-list)
                          nil t))))

(defun fc-ido-copy-selection ()
  "Copy the current ido selection to the kill ring."
  (interactive)
  (kill-new
   (abbreviate-file-name
    (concat ido-current-directory
            ido-text))))

;;; Using ido to open files from file name cache-------------------------------
(defun file-cache-ido-find-file (file)
  "Using ido, interactively open file from file cache'.
First select a file, matched using ido-switch-buffer against the contents
in `file-cache-alist'. If the file exist in more than one
directory, select directory. Lastly the file is opened."
  (interactive (list (file-cache-ido-read "File: "
                                          (mapcar
                                           (lambda (x)
                                             (car x))
                                           file-cache-alist))))
  (let* ((record (assoc file file-cache-alist)))
    (find-file
     (expand-file-name
      file
      (if (= (length record) 2)
          (car (cdr record))
          (file-cache-ido-read
           (format "Find %s in dir: " file) (cdr record)))))))

(defun file-cache-ido-read (prompt choices)
  (let ((ido-make-buffer-list-hook
         (lambda ()
           (setq ido-temp-list choices))))
    (ido-read-buffer prompt)))

(global-set-key (kbd "ESC ESC f") 'file-cache-ido-find-file)
;; ---------------------------------------------------------------------------------

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

(add-hook 'ido-setup-hook 'ido-my-keys)

(defun ido-my-keys ()
 "Add my keybindings for ido."
 (define-key ido-completion-map (kbd "C-k") 'ido-erase-minibuffer-or-dwim)
 (define-key ido-completion-map (kbd "ESC ESC k") 'ido-delete-file-at-head))


(add-hook 'ido-make-file-list-hook 'ido-sort-mtime)
(add-hook 'ido-make-dir-list-hook 'ido-sort-mtime)

(defun ido-sort-mtime ()
  "Sort ido item by modified time."
  (let (ido-temp-list)
    (setq ido-temp-list
          (sort ido-temp-list
                (lambda (a b)
                  (let ((ta (nth 5 (file-attributes (concat ido-current-directory a))))
                        (tb (nth 5 (file-attributes (concat ido-current-directory b)))))
                    (if (= (nth 0 ta) (nth 0 tb))
                        (> (nth 1 ta) (nth 1 tb))
                        (> (nth 0 ta) (nth 0 tb)))))))
    (ido-to-end ;; move . files to end (again)
     (delq nil (mapcar
                (lambda (x) (if (string-equal (substring x 0 1) ".") x))
                ido-temp-list)))))



(defun ido-erase-minibuffer-or-dwim ()
  "If cursor the EOL erases whole minibuffer and insert  `~/'.
If cursor at the EOL and the whole minibuffer is `~/', erase whole minibuffer.
Or else erases whole minibuffer. "
  (interactive)
  (flet ((ido-chdir-1 (&optional dir)
           (setq ido-text-init (if dir dir "~/"))
           (setq ido-exit 'chdir)
           (exit-minibuffer)))
    (if (eolp)
        (if (string= ido-current-directory "~/")
            (ido-chdir-1 "/")
            (ido-chdir-1))
        (ido-chdir-1 ))))


;; (defun my-ido-ignore-buffers (name)
;;  "Ignore all c mode buffers -- example function for ido."
;;  (with-current-buffer name
;;    (cond ((or (derived-mode-p 'cvs-mode) (derived-mode-p 'sql-interactive-mode))
;;           nil)
;;          (t
;;           (string-match "^ ?\\*" name)))))

;; (setq-default ido-ignore-buffers '(my-ido-ignore-buffers)
;;                             ido-auto-merge-work-directories-length -1)
