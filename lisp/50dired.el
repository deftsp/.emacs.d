;;; 50dired.el ---                               -*- lexical-binding: t; -*-

;; TODO: TRY [[https://github.com/syohex/emacs-dired-k][syohex/emacs-dired-k: Highlighting dired buffer like k]]

(use-package dired
  :defer t
  :init
  (progn
    ;; Enable `a' in dired-mode, to open files/dirs in the same buffer.
    (put 'dired-find-alternate-file 'disabled nil)
    (setq dired-isearch-filenames 'dwim
          dired-recursive-copies 'always
          dired-recursive-deletes 'top
          ;; If non-nil, Dired tries to guess a default target directory.
          ;; This means: if there is a dired buffer displayed in the next window,
          ;; use its current subdir, instead of the current subdir of this dired buffer.
          dired-dwim-target t
          dired-guess-shell-gnutar "tar"
          dired-listing-switches "-AlhXG --group-directories-first"
          dired-kept-versions 1))
  :config
  (add-hook 'dired-mode-hook 'tl/dired-mode-hook-init)
  (defun tl/dired-mode-hook-init ())

  (progn
    (tl/set-leader-keys-for-mode 'dired-mode
      "u" 'tl/dired-up-directory-reuse-dir-buffer
      "z" 'reveal-in-osx-finder)
    (define-key dired-mode-map (kbd "^") 'tl/dired-up-directory-reuse-dir-buffer)
    (define-key dired-mode-map (kbd "W") 'tl/dired-w3m-find-file)
    (define-key dired-mode-map (kbd "I") 'dired-maybe-insert-subdir)
    (define-key dired-mode-map [mouse-2] 'dired-mouse-find-file)

    ;; [[https://emacs.stackexchange.com/a/13380][diredp header line follow link]]
    (defface tl/dired-mouseover-face
      '((t (:inherit dired-header :underline t)))
      "Face for `tl/dired-mouseover-face'."
      :group 'dired)

    ;; header line abbrev HOME and breadcrumb
    (defvar tl/dired-mouse-map
      (let ((map (make-sparse-keymap)))
        (define-key map [mouse-2] 'tl/dired-follow-link)
        (define-key map [return] 'tl/dired-follow-link)
        (define-key map [follow-link] 'mouse-face)
        map)
      "Keymap for mouse when in `dired-mode'.")

    ;; Author:  Drew Adams -- http://emacs.stackexchange.com/a/13411/2287
    (defun tl/dired-follow-link (event)
      "Follow the link in the dired directory heading, causing a new
dired buffer to be opened."
      (interactive (list last-nonmenu-event))
      (run-hooks 'mouse-leave-buffer-hook)
      (with-current-buffer (window-buffer (posn-window (event-start event)))
        (let ((path  (get-text-property (posn-point (event-start event)) 'breadcrumb)))
          (dired path))))

    (defun tl//dired-breadcrumb-add-properties (beg end path &optional display)
      (add-text-properties
       beg end
       (list
        'display (and display (propertize display 'face 'dired-header))
        'breadcrumb path
        'face 'dired-header
        'help-echo (format "mouse-2, RET: Follow the link to \"%s\"" path)
        'keymap tl/dired-mouse-map
        'mouse-face 'tl/dired-mouseover-face)))

    (add-hook 'dired-after-readin-hook 'tl/dired-propertize-directory-heading)

    (defun tl/dired-propertize-directory-heading ()
      (interactive)
      (unless (buffer-narrowed-p)
        (let* ((inhibit-read-only t)
               (home (getenv "HOME"))
               path-begin)
          (progn ;; save-excursion
            (goto-char (point-min))
            (setq peol (point-at-eol))
            (set-text-properties (point) peol nil)

            (if (re-search-forward
                 (format "\\([^/\\]+\\)\\(%s\\)[/:]"
                         (regexp-quote home))
                 peol t)
                (progn
                  (setq path-begin (match-beginning 2))
                  (tl//dired-breadcrumb-add-properties
                   (match-beginning 0)  ; include the prefix two spaces
                   (match-end 2)
                   home
                   "~"))
              (re-search-forward "\\([^/\\]+\\)[/\\]" peol t)
              (setq path-begin (match-end 1))

              (tl//dired-breadcrumb-add-properties
               (match-beginning 0) (1+ (match-end 1)) "/" "/"))

            (while (re-search-forward "\\([^/\\]+\\)[/\\:]" peol t)
              (tl//dired-breadcrumb-add-properties
               (match-beginning 1)
               (match-end 1)
               (buffer-substring-no-properties path-begin (match-end 1))))))))


    (defun tl/dired-find-file-ace-window ()
      "Use ace window to select a window for opening a file from dired."
      (interactive)
      (let ((file (dired-get-file-for-visit)))
        (if (> (length (aw-window-list)) 1)
            (aw-select "" (lambda (window)
                            (aw-switch-to-window window)
                            (find-file file)))
          (find-file-other-window file))))


    (defun tl/dired-up-directory-reuse-dir-buffer ()
      (interactive) (find-alternate-file ".."))

    (general-define-key
     :states 'normal
     :keymaps 'dired-mode-map
     (kbd "ESC ESC i s") 'tl/image-scale
     (kbd "M-O")   'tl/open-in-external-application
     (kbd "S-SPC") 'tl/jump-to-org-agenda
     (kbd "%")     'nil
     (kbd "j")     'dired-hacks-next-file
     (kbd "k")     'dired-hacks-previous-file
     (kbd "^")     'tl/dired-up-directory-reuse-dir-buffer
     (kbd "gu")    'tl/dired-up-directory-reuse-dir-buffer
     (kbd "l")     'dired-find-file
     ;; (kbd "i")     'dired-omit-mode
     (kbd "I")     'dired-maybe-insert-subdir
     (kbd "/")     'dired-narrow
     (kbd "M-r")   'dired-do-redisplay
     (kbd "o")     'tl/dired-find-file-ace-window
     (kbd "r")     'wdired-change-to-wdired-mode
     (kbd "s")     'hydra-dired-quick-sort/body
     (kbd "gg")    'tl/dired-back-to-top
     (kbd "gr")    'revert-buffer
     (kbd "G")     'tl/dired-jump-to-bottom)

    (use-package dired-narrow
      :defer t
      :bind (:map dired-mode-map
             ("/" . dired-narrow)))

    ;; not work well with dired-sidebar
    ;; (use-package diredfl
    ;;   :config
    ;;   (diredfl-global-mode +1))

    (use-package dired-rainbow
      :config
      (progn
        (dired-rainbow-define-chmod directory (:inherit default :foreground "#6cb2eb" :weight bold)  "d.*")
        (dired-rainbow-define html "#eb5286" ("css" "less" "sass" "scss" "htm" "html" "jhtm" "mht" "eml" "mustache" "xhtml"))
        (dired-rainbow-define xml "#f2d024" ("xml" "xsd" "xsl" "xslt" "wsdl" "bib" "json" "msg" "pgn" "rss" "yaml" "yml" "rdata"))
        (dired-rainbow-define document "#9561e2" ("docm" "doc" "docx" "odb" "odt" "pdb" "pdf" "ps" "rtf" "djvu" "epub" "odp" "ppt" "pptx"))
        (dired-rainbow-define markdown "#ffed4a" ("org" "etx" "info" "markdown" "md" "mkd" "nfo" "pod" "rst" "tex" "textfile" "txt"))
        (dired-rainbow-define database "#6574cd" ("xlsx" "xls" "csv" "accdb" "db" "mdb" "sqlite" "nc"))
        (dired-rainbow-define media "#de751f" ("mp3" "mp4" "MP3" "MP4" "avi" "mpeg" "mpg" "flv" "ogg" "mov" "mid" "midi" "wav" "aiff" "flac"))
        (dired-rainbow-define image "#f66d9b" ("tiff" "tif" "cdr" "gif" "ico" "jpeg" "jpg" "png" "psd" "eps" "svg"))
        (dired-rainbow-define log "#c17d11" ("log"))
        (dired-rainbow-define shell "#f6993f" ("awk" "bash" "bat" "sed" "sh" "zsh" "vim"))
        (dired-rainbow-define interpreted "#38c172" ("lua" "py" "ipynb" "rb" "pl" "t" "msql" "mysql" "pgsql" "sql" "r" "clj" "cljs" "scala" "js"))
        (dired-rainbow-define compiled "#4dc0b5" ("asm" "cl" "lisp" "el" "c" "h" "c++" "h++" "hpp" "hxx" "m" "cc" "cs" "cp" "cpp" "go" "f" "for" "ftn" "f90" "f95" "f03" "f08" "s" "rs" "hi" "hs" "pyc" ".java"))
        (dired-rainbow-define executable "#8cc4ff" ("exe" "msi"))
        (dired-rainbow-define compressed "#51d88a" ("7z" "zip" "bz2" "tgz" "txz" "gz" "xz" "z" "Z" "jar" "war" "ear" "rar" "sar" "xpi" "apk" "xz" "tar"))
        (dired-rainbow-define packaged "#faad63" ("deb" "rpm" "apk" "jad" "jar" "cab" "pak" "pk3" "vdf" "vpk" "bsp"))
        (dired-rainbow-define encrypted "#ffed4a" ("gpg" "pgp" "asc" "bfe" "enc" "signature" "sig" "p12" "pem"))
        (dired-rainbow-define fonts "#6cb2eb" ("afm" "fon" "fnt" "pfb" "pfm" "ttf" "otf"))
        (dired-rainbow-define partition "#e3342f" ("dmg" "iso" "bin" "nrg" "qcow" "toast" "vcd" "vmdk" "bak"))
        (dired-rainbow-define vc "#0074d9" ("git" "gitignore" "gitattributes" "gitmodules"))
        (dired-rainbow-define-chmod executable-unix "#38c172" "-.*x.*")))


    (use-package dired-subtree
      :defer t
      :bind (:map dired-mode-map
             ("TAB" . dired-subtree-cycle))

      :init
      (setq dired-subtree-line-prefix "  ➜ "
            dired-subtree-use-backgrounds t)

      (define-key dired-mode-map (kbd "i") nil)
      (general-define-key
       :keymaps 'dired-mode-map
       "ii" #'dired-subtree-insert
       "ir" #'dired-subtree-remove
       "ij" #'dired-subtree-down
       "ik" #'dired-subtree-up
       "in" #'dired-subtree-next-sibling
       "ip" #'dired-subtree-previous-sibling
       "if" #'dired-subtree-apply-filter
       "ia" #'dired-subtree-narrow
       "i_" #'dired-subtree-beginning
       "i$" #'dired-subtree-end
       "im" #'dired-subtree-mark-subtree
       "im" #'dired-subtree-unmark-subtree
       "if" #'dired-subtree-only-this-file
       "id" #'dired-subtree-only-this-directory))

    (use-package dired-open
      :init
      (progn
        (defun tl/dired-open-by-macos-open ()
          "Try to run `open' with default app on macOS to open the file under point."
          (interactive)
          (let ((file (ignore-errors (dired-get-file-for-visit))))
            (when file
              (let* ((process (dired-open--start-process file "open")))
                (set-process-sentinel
                 process
                 (lambda (proc _event)
                   (when (and (eq 'exit (process-status proc))
                              (not (zerop (process-exit-status proc))))
                     (message (format "Unable to open \"%s\"" file)))))
                process))))

        (setq dired-open-functions '(tl/dired-open-by-macos-open dired-open-subdir))))


    ;; Note, dired-filter-by-omit removes the files that would be
    ;; removed by dired-omit-mode, so you should not need to use both---in fact
    ;; it is discouraged, as it would make the read-in slower.

    (use-package dired-filter
      :defer t
      :init
      (setq dired-filter-stack '((omit)))
      ;; `F' default to dired-do-find-marked-files
      (define-key dired-mode-map (kbd "f") dired-filter-map)
      (define-key dired-mode-map (kbd "F") dired-filter-mark-map)
      (evil-collection-define-key 'normal 'dired-mode-map
        "f" dired-filter-map  ; bind to map, do not quote it
        "F" dired-filter-mark-map)
      (add-hook 'dired-mode-hook 'dired-filter-mode))

    (when (eq system-type 'darwin)
      (let ((ls (executable-find "gls"))) ;; brew insall coreutils
        (cond (ls (setq dired-use-ls-dired t)
                  (setq insert-directory-program ls))
              (t (require 'ls-lisp)
                 (setq ls-lisp-use-insert-directory-program nil)))))

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

    (use-package dired-quick-sort
      :defer t
      :init
      (add-hook 'dired-mode-hook 'dired-quick-sort))
    (use-package dired-x
      :init
      (progn
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
        (setq dired-omit-files
              (concat "^\\.?#\\|^\\.$\\|^\\.\\.$\\|^~\\|^\\.\\|^#.*#$\\|^nohup.out$\\|\\.jlc$"
                      "\\|"
                      "node_modules"
                      "\\|"
                      (regexp-opt '("^TAGS$" "^cscope.out$")))))
      :config
      (require 'dired-aux)
      (progn
        (setq dired-guess-shell-alist-user ; use ! to call guess command
              `((,(regexp-opt '(".gif" ".png" ".bmp" ".jpg" ".tif" ".jpeg"))
                 '("qiv"                ; feh, "xloadimage -onroot"
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
                        "qiv"))))))))


(use-package dired-sidebar
  :commands (dired-sidebar-toggle-sidebar dired-sidebar-jump-to-sidebar)
  :init
  (setq dired-sidebar-tui-update-delay 0.0)
  (setq dired-sidebar-width 42)
  (setq dired-sidebar-theme 'nerd)
  (setq dired-sidebar-subtree-line-prefix "  ➜ ")
  ;; (setq dired-sidebar-use-term-integration t)
  (setq dired-sidebar-use-custom-font t)
  (setq dired-sidebar-should-follow-file nil
        dired-sidebar-follow-file-idle-delay 3)

  (defun tl/do-dired-sidebar-follow-file ()
    (interactive)
    (dired-sidebar-follow-file))

  (tl/set-leader-keys
    ;; "ft"    #'dired-sidebar-toggle-sidebar
    "ors"   #'tl/do-dired-sidebar-follow-file
    "sf"    #'tl/do-dired-sidebar-follow-file)

  ;; TODO: use ⯇ ⯈ ⯅ ⯆  to tui
  (defun tl/dired-sidebar-init ()
    ;; FIXME: it not work, but why?
    ;; (let* ((buf (dired-sidebar-buffer))
    ;;        (win (get-buffer-window buf)))
    ;;   (when win
    ;;     (set-window-fringes (selected-window) 0 0)))

    ;; `dired-sidebar-setup-tui' will unconditional set
    ;; `dired-subtree-line-prefix' to " "

    ;; "  🠺"
    (if (dired-sidebar-using-tui-p)
        (setq-local dired-subtree-line-prefix " ")))

  (add-hook 'dired-sidebar-mode-hook 'tl/dired-sidebar-init)

  :config
  (tl/set-leader-keys-for-mode 'dired-sidebar-mode
    "u" 'dired-sidebar-up-directory)

  (defun tl/dired-sidebar-preview-file ()
    (interactive)
    (let ((file (dired-get-file-for-visit)))
      (with-selected-window (selected-window)
        (if (and dired-sidebar-cycle-subtree-on-click
                 (file-directory-p file)
                 (not (string-suffix-p "." file)))
            (dired-subtree-cycle)
          (dired-sidebar-find-file file)))))

  (define-key dired-sidebar-mode-map "p" 'tl/dired-sidebar-preview-file)

  (defun tl/dired-sidebar-tui-dired-display (orig-fun &rest args)
    "Display the icons of files in a dired buffer."
    (interactive)
    (when (or t (and (not dired-sidebar-tui-dired-displayed) dired-subdir-alist))
      (setq-local dired-sidebar-tui-dired-displayed t)
      (let ((inhibit-read-only t)
            (collapsible-icon (if (eq dired-sidebar-theme 'nerd) "⯆" "-"))
            (expandable-icon (if (eq dired-sidebar-theme 'nerd) "⯈" "+")))
        (save-excursion
          (goto-char (point-min))
          (while (not (eobp))
            (when (dired-move-to-filename nil)
              (dired-move-to-filename)
              (let ((file (dired-get-filename 'verbatim t)))
                ;; TODO: fix it
                ;; For the file named "'Icon'$'\r'" in *.localized directory in
                ;; macOS. the dired-subtree in dired-sidebar buffer will insert
                ;; the illegal file name casue dired-get-filename can not get
                ;; the filename
                (when file
                  (unless (member file '("." ".."))
                    (let ((filename (dired-get-filename nil t)))
                      (if (file-directory-p filename)
                          (if (dired-subtree--is-expanded-p)
                              (insert (concat collapsible-icon " "))
                            (insert (concat expandable-icon " ")))
                        (insert "- ")))))))
            (forward-line 1))))))



  (advice-add 'dired-sidebar-tui-dired-display :around #'tl/dired-sidebar-tui-dired-display)
  ;; (advice-remove 'dired-sidebar-tui-dired-display #'tl/dired-sidebar-tui-dired-display)

  (defun tl/dired-subtree-cycle-after-hook (&rest _)
    (when (eq major-mode 'dired-sidebar-mode)
      (dired-sidebar-tui-update-with-delay)))

  (advice-add 'dired-subtree-cycle :after #'tl/dired-subtree-cycle-after-hook)

  (push 'toggle-window-split dired-sidebar-toggle-hidden-commands)
  (push 'rotate-windows dired-sidebar-toggle-hidden-commands))


(defun tl/dired-back-to-top ()
  "Move to the first file."
  (interactive)
  (beginning-of-buffer)
  (dired-next-line 2))

(defun tl/dired-jump-to-bottom ()
  "Move to last file."
  (interactive)
  (end-of-buffer)
  (dired-next-line -1))

(defun tl/dired-w3m-find-file ()
  (interactive)
  (let ((file (dired-get-filename)))
    (when (y-or-n-p (format "Open 'w3m' %s " (file-name-nondirectory file)))
      (w3m-find-file file))))


(defun tl/view-chm (file)
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

;;; emms
;; (define-key dired-mode-map (kbd "M-RET") 'emms-add-dired)

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
(defun tl/image-scale (file-list scale-args)
  "Create a scaled version of images of marked files in dired.
The new names have \"-s\" appended before the file name extension.
Requires ImageMagick shell tool."
  (interactive
   (list (dired-get-marked-files) (read-from-minibuffer "scale args: ")))
  (require 'dired)
  (mapc
   (lambda (f)
     (let ((new-file-name (concat (file-name-sans-extension f) "-s" (file-name-extension f t)))
           cmd-str)
       (while (file-exists-p new-file-name)
         (setq new-file-name (concat (file-name-sans-extension new-file-name) "-s" (file-name-extension new-file-name t))))

       ;; relative paths used to get around Windows/Cygwin path remapping problem
       (let ((cmd-str (concat "convert -scale "
                              scale-args
                              " "
                              (file-relative-name f)
                              " "
                              (file-relative-name new-file-name))))
         (shell-command cmd-str))))

   file-list))

;;; zip file/dir
(defun tl/2zip ()
  "Zip the current file/dir in `dired'.
If multiple files are marked, only zip the first one.
Require unix zip commandline tool."
  (interactive)
  (require 'dired)
  (let ((file-name (elt (dired-get-marked-files) 0)))
    (shell-command (format "zip -r '%s.zip' '%s'" (file-relative-name file-name) (file-relative-name file-name)))))

;;; open in external application
(defun tl/open-in-external-application ()
  "Open the current file or dired marked files in external app.
Works in Microsoft Windows, Mac OS X, Linux."
  (interactive)
  (let* ((file-list (if (eq major-mode 'dired-mode)
                        (dired-get-marked-files)
                      (list (buffer-file-name))))
         (do-or-not-p (if (<= (length file-list) 5)
                          t
                        (y-or-n-p "Open more than 5 files?") )))

    (when do-or-not-p
      (cond
       ((eq system-type 'windows-nt)
        (mapc (lambda (file-path)
                (w32-shell-execute "open" (replace-regexp-in-string "/" "\\" file-path t t)))
              file-list))
       ((eq system-type 'darwin)
        (mapc (lambda (file-path)
                (let ((process-connection-type nil))
                  (start-process "" nil "open" file-path)))
              file-list))
       ((eq system-type 'gnu/linux)
        (mapc (lambda (file-path)
                (let ((process-connection-type nil))
                  (start-process "" nil "xdg-open" file-path)))
              file-list))
       (t (message "unsupported system!"))))))


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



;;; tips
;; mark mutiple files in dired mode with m, press B to compile them to *.el.
;; dired-compare-directories
;; w     dired-copy-filename-as-kill. `C-u 0' prefix copy absolute file name
;; i     insert sub-directory
;; C-u i insert sub-directory with specify arguments of ls
;; C-u l change the arguments of ls of sub-directory

;; % u   Rename all marked (or next ARG) files to upper case.
;; % l   Rename all marked (or next ARG) files to lower case.
;; `F' `dired-do-find-marked-files' ; open several files at once?

(provide '50dired)

;; Local Variables:
;; coding: utf-8-unix
;; End:
