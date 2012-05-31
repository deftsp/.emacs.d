;;----------------------------------------------------------------------------
;; by S.P.Tseng
;;----------------------------------------------------------------------------

(require 'dired)
(require 'ansi-color)
(require 'dired-aux)
(require 'wdired)
(require 'dired-x nil t)
(require 'dired-view)
(require 'dired+ nil t)

;;; MailcapView
;; http://www.emacswiki.org/emacs/MailcapView
(autoload 'mailcap-view-find-file-hook "mailcap-view"
  "Hook function to view the file with `mailcap-view-file' if the file isn't a text file." t)
(autoload 'mailcap-view-file "mailcap-view"
  "Using the file extension, view the FILENAME with the appropriate
handler as determined by `mailcap-mime-info'.  If ASYNC is non-nil,
run the viewer in the background and store the output in the `*Async
Shell Command*' buffer.  Otherwise the viewer is run in the foreground
and blocks emacs.  The default for ASYNC is t." t)

;; FIXME: some of the file extension will not be treat as txt file,such as .js
;; (add-hook 'find-file-hook 'mailcap-view-find-file-hook)

;; dired-x 是 dired-mode 的一个扩展。提供了许多很有用的命令和特性。
;; 1. 隐藏配置文件和其它类型的文件。通过设置 dired-omit-extensions 和
;;    dired-omit-files
;; 2. 把文件和特定的 shell 程序关联。通过设置
;;    dired-guess-shell-alist-default， 在文件上使用 "!" 会调用相应的命令
;; 另外 dired-mode 下还有不常用但是比较有用的命令。比如
;; dired-compare-directories 可以用于比较文件夹。


;; i    插入子目录
;; C-u i 使用特定的 ls 参数，插入子目录
;; C-u l 改变子目录 ls 的参数

;;some useful variable
;;dired-omit-extensions

;; How to open several files at once?
;; `C-u F' in Dired.
;; (dired-do-find-marked-files) has an optional `noselect' option.
;; (defun tsp-dired-do-find-marked-files ()
;;     "Visit all marked files at once."
;;     (interactive)
;;     (let ((file-list (dired-get-marked-files)))
;;       (mapcar 'find-file file-list)))


(setq dired-isearch-filenames 'dwim)

;; 让dired可以递归的拷贝和删除目录。
(setq dired-recursive-copies 'always)
(setq dired-recursive-deletes 'top)
;; 复制和移动时把当前 emacs 中另一个窗口中的目录为对象。这通常是我们希望的方式。
(setq dired-dwim-target t)

;; Enable `a' in dired-mode, to open files/dirs in the same buffer.
(put 'dired-find-alternate-file 'disabled nil)

(when (eq system-type 'darwin)
  (let ((ls (executable-find "gls")))   ;; brew insall coreutils
    (cond (ls (setq dired-use-ls-dired t)
              (setq insert-directory-program ls))
          (t (require 'ls-lisp)
             (setq ls-lisp-use-insert-directory-program nil)))))



(setq dired-kept-versions 1)
;; (add-hook 'dired-load-hook
;;           (function (lambda ()
;;                       (load "dired-x"))))



(when (featurep 'dired-x)
  (add-hook 'dired-load-hook
            (function (lambda ()
              (load "dired-x")
              (setq dired-guess-shell-gnutar "tar"))))
  (add-hook 'dired-mode-hook
            (function (lambda ()
              ;; Set buffer-local variables here.  For example:
              (setq dired-omit-mode t))))
  (setq dired-omit-extensions
        '(".svn/" "CVS/" ".o" "~" ".bin" ".bak" ".obj" ".map" ".ico"
          ".pif" ".lnk" ".a" ".ln" ".blg" ".bbl" ".dll" ".drv" ".vxd"
          ".386" ".elc" ".lof" ".glo" ".idx" ".lot" ".fmt" ".tfm"
          ".class" ".lib" ".mem" ".x86f" ".sparcf" ".fasl"
          ".ufsl" ".fsl" ".dxl" ".pfsl" ".dfsl" ".lo" ".la" ".gmo"
          ".mo" ".toc" ".aux" ".cp" ".fn" ".ky" ".pg" ".tp" ".vr"
          ".cps" ".fns" ".kys" ".pgs" ".tps" ".vrs" ".pyc" ".pyo"
          ".idx" ".lof" ".lot" ".glo" ".blg" ".bbl" ".cps" ".fn"
          ".fns" ".ky" ".kys" ".pg" ".pgs" ".tp" ".tps" ".vr" ".vrs"
          ".pdb" ".ilk" ".lrc"))

  (setq dired-omit-size-limit 150000)
  (setq dired-guess-shell-alist-user
        `((,(regexp-opt '(".gif" ".png" ".bmp" ".jpg" ".tif" ".jpeg"))
            '("qiv"                     ; feh, "xloadimage -onroot"
              ))
          ("\\.htm[l]?$" "firefox")
          ("\\.dvi$"    "xdvi")
          ("\\.rar$"    "unrar x")
          ("\\.pdf$"    ,(if (eq system-type 'gnu/linux)
                             "xpdf * &"
                             "open -a Preview"))
          ("\\.pdf.gz$" "zxpdf")
          ("\\.chm$"    "xchm")
          ("\\.djvu$"   "djview")
          ("\\.jar$"    "unzip")
          ("\\.tar.bz2$" "tar jxf")
          ;; (".doc" (xwl-dired-wvHtml))
          (,(regexp-opt '(".doc" ".ppt" ".xls" ".doc")) "soffice")
          ;; default
          ,@dired-guess-shell-alist-default
          ;; match any files
          (".*" `(,(format "tar zcf %s.tar.gz"
                           (file-name-nondirectory (dired-get-filename)))
                   ,(format "zip -r %s.jar"
                            (file-name-nondirectory (dired-get-filename)))
                   "qiv")))))

;; (setq dired-omit-files
;;       (concat "^\\.?#\\|^\\.$\\|^\\.\\.$\\|^#.*#$\\|^nohup.out$\\|\\.jlc$"
;;               "\\|"
;;               (regexp-opt '("TAGS" "cscope.out"))))
(setq dired-omit-files "^\\.?#\\|^\\.$\\|^\\.\\.$\\|^\\.\\|^~")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;显示文件大小按k bytes
;;(setq dired-listing-switches "-lA")
(setq dired-listing-switches "-alh")

;; 通过编辑 buffer 编辑文件名,之后 C-c C-c 提交修改；
(require 'wdired nil t)
(when (featurep 'wdired)
  (autoload 'wdired-change-to-wdired-mode "wdired")
  (define-key dired-mode-map "r" 'wdired-change-to-wdired-mode))
;; % u 将标记的文件改为大写；
;; % l 将标记的文件改为小写。

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; List directories first in dired(seems to not work)
;;(require 'ls-lisp)

;; list directories first
(defun sof/dired-sort ()
  "Dired sort hook to list directories first."
  (save-excursion
    (let (buffer-read-only)
      (forward-line 2) ;; beyond dir. header
      (sort-regexp-fields t "^.*$" "[ ]*." (point) (point-max))))
  (and (featurep 'xemacs)
       (fboundp 'dired-insert-set-properties)
       (dired-insert-set-properties (point-min) (point-max)))
  (set-buffer-modified-p nil))
(add-hook 'dired-after-readin-hook 'sof/dired-sort)


;;;;
;; Ask for confirm when opening some binary alike(.avi, .dvi, etc) files
;; by accident.
(defadvice dired-find-file (around ask-confirm-open-binary-file)
  (let ((f (file-name-nondirectory (dired-filename-at-point))))
    (if (or (string-match
             (concat "\\.\\("
                     (regexp-opt '("dvi" "pdf" "avi" "mp3" "sub"))
                     "\\)$")
             f)
            ;; ELF bin file
            (string-match "ELF" (dired-show-file-type f)))
        (when (y-or-n-p (format "Really open `%s'? " f))
          ad-do-it)
        ad-do-it)))
(ad-activate 'dired-find-file)

;; (define-key dired-mode-map (kbd "w") (lambda ()
;;                                        (interactive)
;;                                        (dired-copy-filename-as-kill 0)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reuse dired buffer when jump to other directories
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defadvice dired-find-file (around dired-find-file-single-buffer activate)
;;   "Replace current buffer if file is a directory."
;;   (interactive)
;;   (let ((orig (current-buffer))
;;         (filename (dired-get-file-for-visit)))
;;     ad-do-it
;;     (when (and (file-directory-p filename)
;;              (not (eq (current-buffer) orig)))
;;       (kill-buffer orig))))

;;----------------------------------------------------------------------------------------------------
;; (defun dired-up-directory-after-kill ()
;;   "Call 'dired-up-directory' after calling '(kill-buffer (current-buffer))'."
;;   (interactive)
;;   (let ((buf (current-buffer)))
;;     (dired-up-directory)
;;     (kill-buffer buf)))

(defadvice dired-up-directory (around dired-up-directory-single-buffer activate)
  "Replace current buffer if file is a directory."
  (interactive)
  (let ((orig (current-buffer)))
    ad-do-it
    (kill-buffer orig)))



;; dired-advertised-find-file is an alias for dired-find-file
(defun dired-advertised-find-file-after-kill ()
  "Call 'dired-advertised-find-file' after calling '(kill-buffer (current-buffer))'."
  (interactive)
  (let ((buf (current-buffer)))
    (dired-advertised-find-file)
    (kill-buffer buf)))

;; (defun tsp-dired-mode-hook-fun ()
;; (setq dired-bind-jump nil
;;       dired-bind-man nil
;;       dired-bind-info nil)
;; (set-face-foreground 'dired-marked "GreenYellow")
;; (define-key dired-mode-map "\M-o" nil)
;; (define-key dired-mode-map "^" 'dired-up-directory-after-kill)
;; (define-key dired-mode-map "\M-\C-m" 'dired-advertised-find-file-after-kill)
;; (define-key dired-mode-map "\C-co" 'dired-omit-mode)
;; (if (tsp-locate-file ".tex" (pwd))
;;     (add-to-list 'dired-omit-extensions ".log")
;;     (setq dired-omit-extensions (remove ".log" dired-omit-extensions))))
;; (defun tsp-locate-file (suffix dir)
;;   ;; return non-nil or nil
;;   (with-temp-buffer
;;     (goto-char (point-min))
;;     (call-process "ls" nil t t (expand-file-name dir))
;;     (goto-char (point-min))
;;     (search-forward-regexp (concat "^.*" suffix "$") (point-max) t)))

;; (add-hook 'dired-mode-hook 'tsp-dired-mode-hook-fun)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;

(define-key dired-mode-map (kbd ";") 'dired-view-minor-mode-toggle)
;; (define-key dired-mode-map (kbd ":") 'dired-view-minor-mode-dired-toggle)


;; (defun tsp-dired-wvHtml ()
;;   (concat "wvHtml --charset=gb2312 * "
;;    (tsp-dired-get-filename-no-extention) ".html"))

;; (define-key dired-mode-map (kbd "v") 'tsp-dired-w3m-find-file)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sorting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; "s" Toggle between sort by date/name and refresh the dired buffer.
;; (defmacro tsp-dired-define-sort (key switch)
;;   "redefine the key `s KEY' to sort use switch to ls."
;;   `(define-key dired-mode-map ,(concat "s" key)
;;      (lambda ()
;;        (interactive)
;;        (dired-sort-other (concat dired-listing-switches
;;                                  ,switch
;;                                  (if tsp-dired-sort-reverse
;;                                      "r"
;;                                      "")
;;                                  (if tsp-dired-sort-recursive
;;                                      "R"
;;                                      ""))))))
;; (defmacro tsp-dired-define-toggle (key var)
;;   `(define-key dired-mode-map ,(concat "s" key)
;;      (lambda ()
;;        (interactive)
;;        (setq ,var (not ,var))
;;        (message "%s %s."
;;                 (get ',var 'variable-documentation)
;;                 (if ,var
;;                     "enabled"
;;                     "disabled")))))
;; (add-hook 'dired-mode-hook
;;           (lambda ()
;;             ;; dired use `s' to switch sort by name/time, we undefine it so
;;             ;; that it can be used as prefix
;;             (define-key dired-mode-map
;;                 (kbd "s")
;;               nil)
;;             (defvar tsp-dired-sort-reverse nil
;;               "sort reversely")
;;             (defvar tsp-dired-sort-recursive nil
;;               "sort recursively")
;;             (tsp-dired-define-sort "X" "X")
;;             (tsp-dired-define-sort "t" "t")
;;             (tsp-dired-define-sort "S" "S")
;;             (tsp-dired-define-sort "U" "U")
;;             (tsp-dired-define-sort "u" "ut") ; sort by access time
;;             (tsp-dired-define-sort "c" "ct") ; sort by ctime
;;             (tsp-dired-define-sort "n" "")   ; sort by name :)
;;             (tsp-dired-define-toggle "r" tsp-dired-sort-reverse)
;;             (tsp-dired-define-toggle "R" tsp-dired-sort-recursive)))


;; Sort methods that affect future sessions
(defun tsp-dired-sort-by-default ()
  (interactive)
  (setq dired-listing-switches "-lh")
  (dired-sort-other dired-listing-switches))

(defun tsp-dired-sort-by-show-all ()
  (interactive)
  (setq dired-listing-switches "-lhA")
  (dired-sort-other dired-listing-switches))

(defun tsp-dired-sort-by-for-solaris ()
  "Solaris `ls' doesn't support `-h' option, stupid!"
  (interactive)
  (setq dired-listing-switches "-lA")
  (dired-sort-other dired-listing-switches))

;; Sort methods that affect current session only
(defun tsp-dired-sort-by-date ()
  (interactive)
  (dired-sort-other
   (concat dired-listing-switches "tr")))

(defun tsp-dired-sort-by-extenstion ()
  (interactive)
  (dired-sort-other
   (concat dired-listing-switches "X")))

(defun tsp-dired-sort-by-invisible-only ()
  (interactive)
  (dired-sort-other
   (concat dired-listing-switches "d .*")))

(defun tsp-dired-sort-by-size ()
  (interactive)
  (dired-sort-other
   (concat dired-listing-switches "S")))

(define-key dired-mode-map (kbd "s") nil)
(define-key dired-mode-map (kbd "s RET") 'tsp-dired-sort-by-default)
(define-key dired-mode-map (kbd "s a") 'tsp-dired-sort-by-show-all)
(define-key dired-mode-map (kbd "s t") 'tsp-dired-sort-by-date)
(define-key dired-mode-map (kbd "s X") 'tsp-dired-sort-by-extenstion)
(define-key dired-mode-map (kbd "s s") 'tsp-dired-sort-by-for-solaris)
(define-key dired-mode-map (kbd "s .") 'tsp-dired-sort-by-invisible-only)
(define-key dired-mode-map (kbd "s z") 'tsp-dired-sort-by-size)

;; Sorting ends there-------------------------------------------------------------------------------------

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Shell command guess
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defadvice dired-run-shell-command (around tsp-dired-run-shell-command (command))
;;   "run a shell command COMMAND .
;; If the COMMAND ends with `&' then run it in background and *discard* the
;; output, otherwise simply let the original `dired-run-shell-command' run it."
;;   (if (string-match "&[[:blank:]]*$" command)
;;       (let ((proc (start-process "tsp-shell" nil shell-file-name
;;                                  shell-command-switch
;;                                  (substring command 0 (match-beginning 0)))))
;;         (set-process-sentinel proc 'shell-command-sentinel))
;;       ad-do-it))
;; (ad-activate 'dired-run-shell-command)

;; (defmacro tsp-dired-define-assoc-group (patterns actions &optional name)
;;   "define an assoc entry to help dired guess the shell command.
;; PATTERN is a list of regexps used to match the filename.
;; NAME is a list of string or expression which eval to a string
;; to denote what shell command to execute. Optional NAME is
;; the name of this group , just for documentation purpose."
;;   (let ((item (gensym)))
;;     `(dolist (,item ',patterns)
;;        (setq dired-guess-shell-alist-user
;;              (cons (list ,item ,@actions) dired-guess-shell-alist-user)))))

;; (add-hook 'dired-load-hook
;;           (lambda ()
;;             (setq dired-guess-shell-alist-user nil)
;;             (tsp-dired-define-assoc-group
;;              ("rm" "rmvb" "RM" "RMVB" "avi" "mpg" "mpeg" "mov")
;;              ("mplayer * &")
;;              video)
;;             (tsp-dired-define-assoc-group
;;              ("pdf" "PDF")
;;              ("acroread * &" "xpdf * &")
;;              pdf-document)
;;             (tsp-dired-define-assoc-group
;;              ("png" "jpg" "jpeg" "gif")
;;              ("xloadimage * &" "gqview * &")
;;              image)
;;             (tsp-dired-define-assoc-group
;;              ("chm" "CHM")
;;              ("xchm * &")
;;              chm-document)
;;             (tsp-dired-define-assoc-group
;;              ("html" "HTML" "htm" "HTML")
;;              ("firefox * &"))))



;;; Actions
;; Run shell command at background
;; (defun dired-run-shell-command (command)
;;   (let ((handler
;;   (find-file-name-handler
;;    (directory-file-name default-directory)
;;    'shell-command)))
;;     (if handler
;;  (apply handler 'shell-command (list command))
;;       ;; (shell-command command)))
;;       (tsp-shell-command-asynchronously command)))
;;   ;; Return nil for sake of nconc in dired-bunch-files.
;;   nil)

;; (defun tsp-shell-command-asynchronously (cmd)
;;   (start-process-shell-command cmd nil cmd))



;; 1. When there is only one match, just do it! Don't bother me to type
;;    an extra RET!
;; 2. Use ido style prompt when there are mutiple matches
;; (defun dired-guess-shell-command (prompt files)
;;   "Ask user with PROMPT for a shell command, guessing a default from FILES."

;;   (let ((default (dired-guess-default files))
;;         default-list old-history val (failed t))
;;     (if (null default)
;;         ;; Nothing to guess
;;         (read-from-minibuffer prompt nil nil nil 'dired-shell-command-history)
;;         ;; Save current history list
;;         (setq old-history dired-shell-command-history)
;;         (if (listp default)
;;             ;; More than one guess
;;             (setq default-list default
;;                   default (car default)
;;                   prompt (concat
;;                           prompt
;;                           (format "{%d guesses} " (length default-list))))
;;             ;; Just one guess
;;             (setq default-list (list default)))
;;         ;; Push all guesses onto history so that they can be retrieved with M-p
;;         ;; and put the first guess in the prompt but not in the initial value.
;;         (setq dired-shell-command-history
;;               (append default-list dired-shell-command-history)
;;               prompt (concat prompt (format "[%s] " default)))
;;         ;; The unwind-protect returns VAL, and we too.
;;         (unwind-protect
;;              ;; BODYFORM
;;              (progn
;;                (if (= (length default-list) 1)
;;                    (progn
;;                      (message "Running `%s' at background" default)
;;                      (setq val default))
;;                    (setq val
;;                          ;;                     (read-from-minibuffer prompt nil nil nil
;;                          ;;                     'dired-shell-command-history))
;;                          (ido-completing-read prompt default-list nil nil nil
;;                                               'dired-shell-command-history))
;;                    (setq failed nil)
;;                    ;; If we got a return, then use default.
;;                    (if (equal val "")
;;                        (setq val default)))
;;                val)
;;           ;; UNWINDFORMS
;;           ;; Undo pushing onto the history list so that an aborted
;;           ;; command doesn't get the default in the next command.
;;           (setq dired-shell-command-history old-history)
;;           (if (not failed)
;;               (or (equal val (car-safe dired-shell-command-history))
;;                  (setq dired-shell-command-history
;;                        (cons val dired-shell-command-history))))))))


;; I don't like to use wildcards in shell command, so simply replace
;; them with operated files.
;; (setq dired-star-subst-regexp "\\(^\\|[  ]\\)\\*\\([     ]\\|$\\)")
;; "\\*"
;; (setq dired-quark-subst-regexp "\\(^\\|[     ]\\)\\?\\([     ]\\|$\\)")
;; "\\?")


;;; Apply Shell's Color Scheme to Dired
(when (string=
       (car (split-string (shell-command-to-string "echo $OSTYPE")))
       "linux-gnu")

  (setq tsp-dircolors-string
        (replace-regexp-in-string
         ":$" "" (cadr
                  (split-string
                   (shell-command-to-string "dircolors")
                   "'"))))

  ;; colored by file extensions
  (setq tsp-dircolors-extensions
        (split-string
         (replace-regexp-in-string
          "=[0-9;]+\\|\\*\\." ""
          (replace-regexp-in-string "^[^*]*" "" tsp-dircolors-string))
         ":"))

  (defun tsp-dircolors-get-escape-seq (regexp)
    "Get escape-seq by matching REGEXP against `tsp-dircolors-string'.
e.g., (tsp-dircolors-get-escape-seq \"*.gz\") => \"01;31\""
    (string-match (concat regexp "=\\([^:]+\\):") tsp-dircolors-string)
    (match-string 1 tsp-dircolors-string))

  (setq dired-font-lock-keywords
        `(,(list dired-subdir-regexp '(1 dired-header-face)) ; Directory headers.
           ;;
           ;; Dired marks.
           ,(list dired-re-mark '(0 dired-mark-face))
           ;;
           ;; We make heavy use of MATCH-ANCHORED, since the regexps don't identify the
           ;; file name itself.  We search for Dired defined regexps, and then use the
           ;; Dired defined function `dired-move-to-filename' before searching for the
           ;; simple regexp ".+".  It is that regexp which matches the file name.
           ;;
           ;; Marked files.
           ,(list (concat "^[" (char-to-string dired-marker-char) "]")
                  '(".+" (dired-move-to-filename) nil (0 dired-marked-face)))
           ;;
           ;; Flagged files.
           ,(list (concat "^[" (char-to-string dired-del-marker) "]")
                  '(".+" (dired-move-to-filename) nil (0 dired-flagged-face)))
           ;; People who are paranoid about security would consider this more
           ;; important than other things such as whether it is a directory.
           ;; But we don't want to encourage paranoia, so our default
           ;; should be what's most useful for non-paranoids. -- rms.
;;;   ;;
;;;   ;; Files that are group or world writable.
;;;   (list (concat dired-re-maybe-mark dired-re-inode-size
;;;      "\\([-d]\\(....w....\\|.......w.\\)\\)")
;;;  '(1 dired-warning-face)
;;;  '(".+" (dired-move-to-filename) nil (0 dired-warning-face)))
           ;; However, we don't need to highlight the file name, only the
           ;; permissions, to win generally.  -- fx.
           ;; Fixme: we could also put text properties on the permission
           ;; fields with keymaps to frob the permissions, somewhat a la XEmacs.
           ,(list (concat dired-re-maybe-mark dired-re-inode-size
                          "[-d]....\\(w\\)....") ; group writable
                  '(1 dired-warning-face))
           ,(list (concat dired-re-maybe-mark dired-re-inode-size
                          "[-d].......\\(w\\).") ; world writable
                  '(1 dired-warning-face))
           ;;
           ;; Subdirectories.
           ,(list dired-re-dir
                  '(".+" (dired-move-to-filename) nil (0 dired-directory-face)))
           ;;
           ;; Symbolic links.
           ,(list dired-re-sym
                  '(".+" (dired-move-to-filename) nil (0 dired-symlink-face)))

           ;; executables
           ,(list dired-re-exe
                  `(".+"
                    (dired-move-to-filename)
                    nil
                    (0 (ansi-color-get-face ,(tsp-dircolors-get-escape-seq "ex")))))

           ;; colorful by extensions
           ,@(mapcar (lambda (ext)
                       `(,(format ".*\\.%s$" ext)
                          (".+"
                           (dired-move-to-filename)
                           nil
                           (0 (ansi-color-get-face ,(tsp-dircolors-get-escape-seq ext))))))
                     tsp-dircolors-extensions)

           ;;
           ;; Files suffixed with `completion-ignored-extensions'.
           (eval .
                 ;; It is quicker to first find just an extension, then go back to the
                 ;; start of that file name.  So we do this complex MATCH-ANCHORED form.
                 (list (concat "\\(" (regexp-opt completion-ignored-extensions) "\\|#\\)$")
                       '(".+" (dired-move-to-filename) nil (0 dired-ignored-face))))
           ;;
           ;; Files suffixed with `completion-ignored-extensions'
           ;; plus a character put in by -F.
           (eval .
                 (list (concat "\\(" (regexp-opt completion-ignored-extensions)
                               "\\|#\\)[*=|]$")
                       '(".+" (progn
                                (end-of-line)
                                ;; If the last character is not part of the filename,
                                ;; move back to the start of the filename
                                ;; so it can be fontified.
                                ;; Otherwise, leave point at the end of the line;
                                ;; that way, nothing is fontified.
                                (unless (get-text-property (1- (point)) 'mouse-face)
                                  (dired-move-to-filename)))
                         nil (0 dired-ignored-face)))))))


(defun tsp-view-chm (file)
  (interactive
   (list (let ((file (dired-get-filename)))
           (or file
               (read-file-name "Open chm: ")))))
  (if (and file (string-match "\\.chm$" file))
      (let ((proc (get-process "archmage")))
        (with-current-buffer (get-buffer-create " *archmage*")
          (if (and proc (eq (process-status proc) 'run))
              (kill-process proc))
          (erase-buffer)
          (setq proc (start-process "archmage" (current-buffer)
                                    "archmage" "-p" "66616" file))
          (sit-for 0.5)
          (if (eq (process-status proc) 'run)
              (w3m "http://localhost:66616")
              (display-buffer (current-buffer)))))
      (message "Not chm file!")))


(defun dired-mouse-find-file (event)
  "In dired, visit the file or directory name you click on in this window."
  (interactive "e")
  (let (file)
    (save-excursion
      (set-buffer (window-buffer (posn-window (event-end event))))
      (save-excursion
        (goto-char (posn-point (event-end event)))
        (setq file (dired-get-filename))))
    (select-window (posn-window (event-end event)))
    (find-file (file-name-sans-versions file t))))

(add-hook 'dired-load-hook (function (lambda ()
                             (define-key dired-mode-map [mouse-2] 'dired-mouse-find-file))))



;; (,(regexp-opt
;;              '(".mp3" ".ogg" ".wav" ".avi" ".mpg" ".mpeg" ".wmv" ".mkv" ".mp4" ".flac"
;;                ".dat" ".wma" ".ape" ".asf" ".rmvb" ".rm" ".mkv" ".VOB" ".vob" ".flv" ".mov"))
;;             (let ((fn-list (remove-if #'(lambda (f)
;;                                           (not (string= (file-name-extension f)
;;                                                       "mp3")))
;;                                       (dired-get-marked-files))))
;;               (if fn-list
;;                   (progn (mapcar 'emms-add-file fn-list)
;;                          (dired-unmark-all-marks)
;;                          (keyboard-quit))
;;                   (emms-add-file (dired-mode-map)))))

(define-key dired-mode-map (kbd "M-RET") 'emms-add-dired)



;;; gnus-dired-mode
;; C-c C-m C-a
;; "Send dired's marked files as an attachment (`gnus-dired-attach'). You
;; will be prompted for a message buffer."
;; (add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode)


;;; Omitting Git-ignored files in Emacs dired
;; -----------------------------------------------------------------------
;; (add-hook 'dired-load-hook #'(lambda nil (load "dired-x" t)))

;; (eval-after-load "dired-x"
;;   '(progn
;;      (defvar dired-omit-regexp-orig (symbol-function 'dired-omit-regexp))

;;      (defun dired-omit-regexp ()
;;        (let ((file (expand-file-name ".git"))
;;              parent-dir)
;;          (while (and (not (file-exists-p file))
;;                      (progn
;;                        (setq parent-dir
;;                              (file-name-directory
;;                               (directory-file-name
;;                                (file-name-directory file))))
;;                        ;; Give up if we are already at the root dir.
;;                        (not (string= (file-name-directory file)
;;                                      parent-dir))))
;;            ;; Move up to the parent dir and try again.
;;            (setq file (expand-file-name ".git" parent-dir)))
;;          ;; If we found a change log in a parent, use that.
;;          (if (file-exists-p file)
;;              (let ((regexp (funcall dired-omit-regexp-orig)))
;;                (assert (stringp regexp))
;;                (concat
;;                 regexp
;;                 (if (> (length regexp) 0)
;;                     "\\|" "")
;;                 "\\("
;;                 (mapconcat
;;                  #'(lambda (str)
;;                      (concat "^"
;;                              (regexp-quote
;;                               (substring str 13
;;                                          (if (= ?/ (aref str (1- (length str))))
;;                                              (1- (length str))
;;                                            nil)))
;;                              "$"))
;;                  (split-string (shell-command-to-string
;;                                 "git clean -d -x -n")
;;                                "\n" t)
;;                  "\\|")
;;                 "\\)"))
;;            (funcall dired-omit-regexp-orig))))))
;; -----------------------------------------------------------------------------------------

;;; tumme - Dired 的图片浏览器
;; "tumme" means thumb in Swedish. You need ImageMagick installed for tumme to work.
;; M-x tummel-dired RET path/to/photo/dir RET
;; C-S-{n,p} 逐个浏览图片的缩略图
;; C-t .     当前文件的缩略图
;; C-t d     所有标记的文件的缩略图
;; TAB       互换窗口



;; Read Info files with ‘I’ in Dired
;; (defun dired-do-info ()
;;   (interactive)
;;   "In dired, read the Info file named on this line."
;;   (info (dired-get-filename)))
;; (add-hook 'dired-mode-hook
;;           (lambda ()
;;             (local-set-key "I" 'dired-do-info)))


(eval-after-load "dired+"
  '(progn
     (set-face-attribute 'diredp-dir-heading nil :foreground "magenta" :background "#555555")
     (set-face-attribute 'diredp-display-msg nil :foreground "chartreuse")
     (set-face-attribute 'diredp-file-name nil :foreground "dodger blue")
     (set-face-attribute 'diredp-dir-priv nil :foreground "steel blue" :background nil :weight 'bold)
     (set-face-attribute 'diredp-no-priv nil :foreground nil :background nil)
     (set-face-attribute 'diredp-read-priv nil :foreground "deep sky blue" :background 'unspecified)
     (set-face-attribute 'diredp-write-priv nil :foreground "yellow" :background 'unspecified)
     (set-face-attribute 'diredp-exec-priv nil :foreground "red" :background 'unspecified)
     (set-face-attribute 'diredp-flag-mark-line nil :foreground "white"
                         :background "Blue4")))

;;;tips

;; 进入dired模式，然后在各个文件上用m标记多个需要编译的el文件，然后按一下B就可以批量重编译el文件了。方便吧


(provide '50dired)
