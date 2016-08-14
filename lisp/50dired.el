;;; 50dired.el ---

;; (require 'dired)
;; (require 'ansi-color)
;; (require 'dired-aux)
;; (require 'wdired)
;; (require 'dired-x nil t)
;; (require 'dired-view nil t)
;; (require 'dired+ nil t)

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
(when (eq system-type 'gnu/linux)
  (add-hook 'find-file-hook 'mailcap-view-find-file-hook))

;; How to open several files at once?
;; `F' `dired-do-find-marked-files'

(setq dired-isearch-filenames 'dwim
      dired-recursive-copies 'always
      dired-recursive-deletes 'top
      ;; If non-nil, Dired tries to guess a default target directory.
      ;; This means: if there is a dired buffer displayed in the next window,
      ;; use its current subdir, instead of the current subdir of this dired buffer.
      dired-dwim-target t
      dired-guess-shell-gnutar "tar"
      dired-kept-versions 1)

;; Enable `a' in dired-mode, to open files/dirs in the same buffer.
(put 'dired-find-alternate-file 'disabled nil)

;;; darwin only
(when (eq system-type 'darwin)
  (let ((ls (executable-find "gls")))   ;; brew insall coreutils
    (cond (ls (setq dired-use-ls-dired t)
              (setq insert-directory-program ls))
          (t (require 'ls-lisp)
             (setq ls-lisp-use-insert-directory-program nil)))))

;;; omit mode
;; C-x M-o
(eval-after-load "dired-x"
  '(progn
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
     (setq dired-guess-shell-alist-user    ; use ! to call guess command
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
                   "qiv"))))
     (setq dired-omit-files
           (concat "^\\.?#\\|^\\.$\\|^\\.\\.$\\|^~\\|^\\.\\|^#.*#$\\|^nohup.out$\\|\\.jlc$"
                   "\\|"
                   (regexp-opt '("TAGS" "cscope.out"))))))

;;; wdired
;; press `r' to modify the filename in the dired buffer, `C-c C-c' to commit
(autoload 'wdired-change-to-wdired-mode "wdired")
(define-key dired-mode-map "r" 'wdired-change-to-wdired-mode)

;;;
;; Ask for confirm when opening some binary alike(.avi, .dvi, etc) files by accident.
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

;;; reuse dired buffer when jump to other directories
;; (defadvice dired-find-file (around dired-find-file-single-buffer activate)
;;   "Replace current buffer if file is a directory."
;;   (interactive)
;;   (let ((orig (current-buffer))
;;         (filename (dired-get-file-for-visit)))
;;     ad-do-it
;;     (when (and (file-directory-p filename)
;;              (not (eq (current-buffer) orig)))
;;       (kill-buffer orig))))

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

(add-hook 'dired-mode-hook 'paloryemacs/dired-mode-hook-init)
(defun paloryemacs/dired-mode-hook-init ()
  (setq dired-bind-jump nil
        dired-bind-man nil
        dired-bind-info nil)
  ;; (define-key dired-mode-map "\M-o" nil)
  ;; (define-key dired-mode-map "^" 'dired-up-directory-after-kill)
  ;; (define-key dired-mode-map "\M-\C-m" 'dired-advertised-find-file-after-kill)
  ;; (define-key dired-mode-map "\C-co" 'dired-omit-mode)
  (define-key dired-mode-map (kbd "W") 'paloryemacs/dired-w3m-find-file)
  (define-key dired-mode-map (kbd ";") 'dired-view-minor-mode-toggle)
  (define-key dired-mode-map (kbd ":") 'dired-view-minor-mode-dired-toggle)
  (define-key dired-mode-map (kbd "/") 'dired-narrow)

  ;; dired use `s' to switch sort by name/time, we undefine it so that
  ;; it can be used as prefix
  (define-key dired-mode-map (kbd "s") nil)
  (paloryemacs/dired-define-sort "RET" "")   ; default
  (paloryemacs/dired-define-sort "X" "X")    ; sort alphabetically by entry extension
  (paloryemacs/dired-define-sort "t" "t")    ; sort by modification time, newest first
  (paloryemacs/dired-define-sort "S" "S")    ; sort by file size
  (paloryemacs/dired-define-sort "U" "U")    ; do not sort; list entries in directory order
  (paloryemacs/dired-define-sort "u" "ut")   ; sort by access time
  (paloryemacs/dired-define-sort "c" "ct")   ; sort by ctime
  (paloryemacs/dired-define-sort "n" "n")    ; sort by like -l, but list numeric user and group ID
  (paloryemacs/dired-define-sort "." "d .*") ; sort by invisible only
  (paloryemacs/dired-define-toggle "r" paloryemacs/dired-sort-reverse)
  (paloryemacs/dired-define-toggle "R" paloryemacs/dired-sort-recursive)

  (dired-omit-mode +1))

(defun paloryemacs/dired-w3m-find-file ()
  (interactive)
  (let ((file (dired-get-filename)))
    (when (y-or-n-p (format "Open 'w3m' %s " (file-name-nondirectory file)))
      (w3m-find-file file))))

;;; sorting
;; http://lifegoo.pluskid.org/wiki/EnhanceDired.html
(defconst paloryemacs/default-listing-switches "-alh")
(defvar paloryemacs/dired-sort-reverse nil "sort reversely")
(defvar paloryemacs/dired-sort-recursive nil "sort recursively")

(setq dired-listing-switches paloryemacs/default-listing-switches)

;; "s" Toggle between sort by date/name and refresh the dired buffer.
(defmacro paloryemacs/dired-define-sort (key switches)
  "redefine the key `s KEY' to sort use switches to ls."
  ;; `(define-key dired-mode-map ,(concat "s" key)
  `(define-key dired-mode-map (kbd ,(concat "s " key))
     (lambda ()
       (interactive)
       (let ((new-switches
              (concat paloryemacs/default-listing-switches
                      ,switches
                      (if paloryemacs/dired-sort-reverse "r" "")
                      (if paloryemacs/dired-sort-recursive "R" ""))))
         (setq dired-listing-switches new-switches)
         (dired-sort-other new-switches)))))

(defmacro paloryemacs/dired-define-toggle (key var)
  `(define-key dired-mode-map ,(concat "s" key)
     (lambda ()
       (interactive)
       (setq ,var (not ,var))
       (message "%s %s."
                (get ',var 'variable-documentation)
                (if ,var "enabled" "disabled")))))

;;; Shell command guess
;; (defadvice dired-run-shell-command (around paloryemacs/dired-run-shell-command (command))
;;   "run a shell command COMMAND .
;; If the COMMAND ends with `&' then run it in background and *discard* the
;; output, otherwise simply let the original `dired-run-shell-command' run it."
;;   (if (string-match "&[[:blank:]]*$" command)
;;       (let ((proc (start-process "paloryemacs/shell" nil shell-file-name
;;                                  shell-command-switch
;;                                  (substring command 0 (match-beginning 0)))))
;;         (set-process-sentinel proc 'shell-command-sentinel))
;;       ad-do-it))
;; (ad-activate 'dired-run-shell-command)

;; (defmacro paloryemacs/dired-define-assoc-group (patterns actions &optional name)
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
;;             (paloryemacs/dired-define-assoc-group
;;              ("rm" "rmvb" "RM" "RMVB" "avi" "mpg" "mpeg" "mov")
;;              ("mplayer * &")
;;              video)
;;             (paloryemacs/dired-define-assoc-group
;;              ("pdf" "PDF")
;;              ("acroread * &" "xpdf * &")
;;              pdf-document)
;;             (paloryemacs/dired-define-assoc-group
;;              ("png" "jpg" "jpeg" "gif")
;;              ("xloadimage * &" "gqview * &")
;;              image)
;;             (paloryemacs/dired-define-assoc-group
;;              ("chm" "CHM")
;;              ("xchm * &")
;;              chm-document)
;;             (paloryemacs/dired-define-assoc-group
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
;;       (paloryemacs/shell-command-asynchronously command)))
;;   ;; Return nil for sake of nconc in dired-bunch-files.
;;   nil)

;; (defun paloryemacs/shell-command-asynchronously (cmd)
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

  (setq paloryemacs/dircolors-string
        (replace-regexp-in-string
         ":$" "" (cadr
                  (split-string
                   (shell-command-to-string "dircolors")
                   "'"))))

  ;; colored by file extensions
  (setq paloryemacs/dircolors-extensions
        (split-string
         (replace-regexp-in-string
          "=[0-9;]+\\|\\*\\." ""
          (replace-regexp-in-string "^[^*]*" "" paloryemacs/dircolors-string))
         ":"))

  (defun paloryemacs/dircolors-get-escape-seq (regexp)
    "Get escape-seq by matching REGEXP against `paloryemacs/dircolors-string'.
e.g., (paloryemacs/dircolors-get-escape-seq \"*.gz\") => \"01;31\""
    (string-match (concat regexp "=\\([^:]+\\):") paloryemacs/dircolors-string)
    (match-string 1 paloryemacs/dircolors-string))

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
                    (0 (ansi-color-get-face ,(paloryemacs/dircolors-get-escape-seq "ex")))))

           ;; colorful by extensions
           ,@(mapcar (lambda (ext)
                       `(,(format ".*\\.%s$" ext)
                          (".+"
                           (dired-move-to-filename)
                           nil
                           (0 (ansi-color-get-face ,(paloryemacs/dircolors-get-escape-seq ext))))))
                     paloryemacs/dircolors-extensions)

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


(defun paloryemacs/view-chm (file)
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

;;; emms
(define-key dired-mode-map (kbd "M-RET") 'emms-add-dired)

;;; gnus-dired-mode
;; C-c C-m C-a
;; "Send dired's marked files as an attachment (`gnus-dired-attach'). You
;; will be prompted for a message buffer."
;; (add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode)

;;; tumme - Dired's image browser
;; "tumme" means thumb in Swedish. You need ImageMagick installed for tumme to work.
;; M-x tummel-dired RET path/to/photo/dir RET
;; C-S-{n,p} browser thumbnail one by one
;; C-t .     thumbnail fo current file
;; C-t d     thumbnails of all of the marked file
;; TAB       exchange windows

;;; work with ImageMagic
;; thanks to http://ergoemacs.org/emacs/emacs_dired_convert_images.html
(define-key dired-mode-map (kbd "ESC ESC i s") 'paloryemacs/image-scale)
(defun paloryemacs/image-scale (file-list scale-args)
  "Create a scaled version of images of marked files in dired.
The new names have \"-s\" appended before the file name extension.
Requires ImageMagick shell tool."
  (interactive
   (list (dired-get-marked-files) (read-from-minibuffer "scale args: ")))
  (require 'dired)
  (mapc
   (lambda (ξ f)
     (let ((new-file-name (concat (file-name-sans-extension ξ f) "-s" (file-name-extension ξ f t)))
           cmd-str)
       (while (file-exists-p new-file-name)
         (setq new-file-name (concat (file-name-sans-extension new-file-name) "-s" (file-name-extension new-file-name t))))

       ;; relative paths used to get around Windows/Cygwin path remapping problem
       (let ((cmd-str (concat "convert -scale "
                              scale-args
                              " "
                              (file-relative-name ξ f)
                              " "
                              (file-relative-name new-file-name))))
         (shell-command cmd-str))))

   file-list))

;;; zip file/dir
(defun paloryemacs/2zip ()
  "Zip the current file/dir in `dired'.
If multiple files are marked, only zip the first one.
Require unix zip commandline tool."
  (interactive)
  (require 'dired)
  (let ((file-name (elt (dired-get-marked-files) 0)))
    (shell-command (format "zip -r '%s.zip' '%s'" (file-relative-name file-name) (file-relative-name file-name)))))

;;; open in external application
(define-key dired-mode-map (kbd "M-O") 'paloryemacs/open-in-external-application)
(defun paloryemacs/open-in-external-application ()
  "Open the current file or dired marked files in external app.
Works in Microsoft Windows, Mac OS X, Linux."
  (interactive)
  (let* ((ξ file-list (if (eq major-mode 'dired-mode)
                         (dired-get-marked-files)
                         (list (buffer-file-name))))
         (do-or-not-p (if (<= (length ξ file-list) 5)
                          t
                          (y-or-n-p "Open more than 5 files?") )))

    (when do-or-not-p
      (cond
        ((eq system-type 'windows-nt)
         (mapc (lambda (file-path)
                 (w32-shell-execute "open" (replace-regexp-in-string "/" "\\" file-path t t)))
               ξ file-list))
        ((eq system-type 'darwin)
         (mapc (lambda (file-path)
                 (let ((process-connection-type nil))
                   (start-process "" nil "open" file-path)))
               ξ file-list))
        ((eq system-type 'gnu/linux)
         (mapc (lambda (file-path)
                 (let ((process-connection-type nil))
                   (start-process "" nil "xdg-open" file-path)))
               ξ file-list))
        (t (message "unsupported system!"))))))

(setq diredp-hide-details-initially-flag nil)
(setq diredp-hide-details-propagate-flag nil)

;;; tips
;; mark mutiple files in dired mode with m, press B to compile them to *.el.
;; dired-compare-directories
;; w     dired-copy-filename-as-kill. `C-u 0' prefix copy absolute file name
;; i     insert sub-directory
;; C-u i insert sub-directory with specify arguments of ls
;; C-u l change the arguments of ls of sub-directory

;; % u   Rename all marked (or next ARG) files to upper case.
;; % l   Rename all marked (or next ARG) files to lower case.



(provide '50dired)
