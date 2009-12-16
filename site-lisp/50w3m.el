;;; w3m

(add-to-list 'load-path "~/.emacs.d/packages/emacs-w3m")

(require 'w3m-ems)
(require 'w3m)
(require 'w3m-session)

;; Rendering Frames

;; If you want to render frames within Emacs, you have to use w3mmee rather than w3m -- w3mmee is w3m with multibyte
;; encoding extension. The problem seems to be that w3m and w3mmee behave differently with the -dump_extra flag.

;; You will also need the utility mbconv (character encoding scheme converter) for this to work out of the box -- this
;; is provided by either the libmoe1.5 or libmoe-dev package on Debian GNU/Linux systems.

;; To use w3mmee/mbconv:

;; (require 'executable)
;; (let ((w3mmee (executable-find "w3mmee"))
;;       (mbconv (executable-find "mbconv")))
;;   (when (and w3mmee mbconv)
;;     (setq w3m-command w3mmee)))
;; (require 'w3m)

;; In order to handle text/html part with emacs-w3m under SEMI MUAs such as Wanderlust, you have to put the following
;; line in your ~/.emacs file:
;; (require 'mime-w3m)


;; You might also need to configure w3mmee such that "English" is set as the "Language" in the "Character Encoding
;; Settings" section of the "Option Setting Panel".

(setq w3m-language "en"
      w3m-bookmark-file-coding-system 'utf-8
      w3m-coding-system 'utf-8
      w3m-default-coding-system 'utf-8
      w3m-file-coding-system 'utf-8
      w3m-file-name-coding-system 'utf-8
      w3m-terminal-coding-system 'utf-8
      w3m-input-coding-system 'utf-8
      w3m-output-coding-system 'utf-8)

(setq w3m-favicon-image nil
      w3m-use-favicon nil
      w3m-use-toolbar t
      w3m-symbol 'w3m-default-symbol
      w3m-key-binding 'info
      w3m-tab-width 16
      w3m-process-modeline-format " loaded: %s"
      ;; Conkeror seems not to understand the "new-tab" option to be an 'openURL' call
      browse-url-firefox-new-window-is-tab t
      browse-url-new-window-flag t
      w3m-session-autosave t)

(setq w3m-form-textarea-edit-mode 'org-mode)
(add-hook 'w3m-form-input-textarea-mode-hook
          '(lambda nil
            (setq outline-regexp "=+")))


(if window-system
    (setq browse-url-generic-program "firefox" ; The name of the browser program used by `browse-url-generic'.
          ;; used by the `browse-url-at-point', `browse-url-at-mouse', and `browse-url-of-file' commands.
          ;; browse-url-generic w3m-browse-url w3m-browse-url-new-tab or browse-url-firefox
          browse-url-browser-function '(("file:.*/usr/local/share/gtk-doc/html" . w3m-goto-url-new-session)
                                        ("file:.*/usr/share/gtk-doc/html" . w3m-goto-url-new-session)
                                        ("." . w3m-goto-url-new-session)))
    ;; browse-url-lynx-emacs
    (setq browse-url-browser-function 'w3m-goto-url-new-session))


(global-set-key (kbd "C-c C-o") 'browse-url-at-point)
(global-set-key (kbd "C-c O") 'browse-url)

(autoload 'browse-url "browse-url" "Open up in browsers hyperlinks." t)

;; When browsing in Gnus, sometimes it's a good idea to open up something graphically.
(global-set-key (kbd "C-c B") 'browse-url-firefox)

(setq w3m-command-arguments '("-F") ; default arguments passed to the w3m command
      w3m-use-cookies t
      w3m-cookie-accept-bad-cookies t
      w3m-icon-directory "~/.emacs.d/packages/w3m/icons"
      w3m-fill-column 100)


;;; w3m-toggle-proxy
(defvar deftsp-w3m-proxy nil)
(defun deftsp-w3m-toggle-proxy ()
  (interactive)
  (if deftsp-w3m-proxy
      (progn
        (setq w3m-command-arguments '("-F"))
        (setq deftsp-w3m-proxy nil)
        (message "w3m proxy: OFF"))
      (setq w3m-command-arguments '("-F" "-o" "http_proxy=http://127.0.0.1:8118/"))
      (setq deftsp-w3m-proxy t)
      (message "w3m proxy: ON")))

(setq w3m-no-proxy-domains '("google.com" "google.cn"))


(setq w3m-default-display-inline-image nil)
(setq w3m-default-toggle-inline-images nil)
(setq w3m-home-page "file:///home/deftsp/proj/personal-site/web/WelcomePage.html")


;; Run `w3m-view-this-url' without switching to the newly created buffer.
(setq w3m-view-this-url-new-session-in-background t)
(setq w3m-toggle-inline-images-permanently t)
;; (add-hook 'w3m-fontify-after-hook 'remove-w3m-output-garbages)
;; (defun remove-w3m-output-garbages ()
;;   (interactive)
;;   (let ((buffer-read-only))
;;     (setf (point) (point-min))
;;     (while (re-search-forward "[\200-\240]" nil t)
;;       (replace-match " "))
;;     (set-buffer-multibyte t))
;;   (set-buffer-modified-p nil))


(setq w3m-default-symbol
      '("─┼" " ├" "─┬" " ┌" "─┤" " │" "─┐" ""
        "─┴" " └" "──" ""   "─┘" ""   ""   ""
        "─┼" " ┠" "━┯" " ┏" "─┨" " ┃" "━┓" ""
        "━┷" " ┗" "━━" ""   "━┛" ""   ""   ""
        " ù" " □" " ☆" " ○" " ■" " ★" " ◎"
        " ●" " △" " ●" " ○" " □" " ●" "≪ ↑ ↓ "))


;; I decided I don't want to see some chars
;; (add-hook 'w3m-display-hook
;;           (lambda (url)
;;             (let ((buffer-read-only nil))
;;               (replace-string "\u00ad" ""))))



;;; with dired
(add-hook 'dired-mode-hook
          (lambda ()
            (define-key dired-mode-map (kbd "C-c f w") 'dired-w3m-find-file)))

(defun dired-w3m-find-file ()
  (interactive)
  (let ((file (dired-get-filename)))
    (if (y-or-n-p (format "Open 'w3m' %s " (file-name-nondirectory file)))
        (w3m-find-file file))))

;;; Browsing the current buffer
;; Assign this code to a key to "preview" a buffer full of HTML in w3m.
(defun w3m-browse-current-buffer ()
  (interactive)
  (let ((filename (concat (make-temp-file "w3m-") ".html")))
    (unwind-protect
         (progn
           (write-region (point-min) (point-max) filename)
           (w3m-find-file filename))
      (delete-file filename))))

(defun deftsp-w3m-goto-url ()
  (interactive)
  (let ((w3m-current-url ""))
    (call-interactively #'w3m-goto-url)))


;; Downloading Files Asynchronously

;; Since w3m-el downloads everything synchronously it can sometimes interfere with productivity. If you use w3m-el to
;; download and save large files here are two solutions to do so asynchronously. One uses wget and the other uses cURL.
;; Neither of these solutions really helps with the problem of synchronous browsing,only eliminates synchronous
;; downloading. EmacsWget is probably superior than the following function.

(defun w3m-download-with-wget (loc)
  (interactive "DSave to: ")
  (let ((url (or (w3m-anchor) (w3m-image))))
    (if url
        (let ((proc (start-process "wget" (format "*wget %s*" url)
                                   "wget" "--passive-ftp" "-nv"
                                   "-P" (expand-file-name loc) url)))
          (with-current-buffer (process-buffer proc)
            (erase-buffer))
          (set-process-sentinel proc (lambda (proc str)
                                       (message "wget download done"))))
        (message "Nothing to get"))))

;; (defun w3m-download-with-curl (loc)
;;   (define-key w3m-mode-map "c"
;;     (lambda (dir)
;;       (interactive "DSave to: ")
;;       (cd dir)
;;       (start-process "curl" "*curl*" "curl.exe" "-O" "-s" (w3m-anchor)))))

;; To take advantage of these, just call either function while the point is on a URL. You will then be prompted for the
;; directory to download the URL into. Note the examples of binding these functions below in the alternate keymap. These
;; functions are a replacement for w3m-download-this-url.


;; Using TextMode for textareas

;; The default mode for editing textareas is somewhat annoying - I'd much rather use TextMode?. Here's a small addition
;; to your .emacs to make that possible:
;; (eval-after-load "w3m-form"
;;   '(progn
;;     (define-minor-mode tsp:w3m-textarea-mode
;;      "Minor mode used when editing w3m textareas."
;;      nil " tsp:w3m-textarea" w3m-form-input-textarea-map)
;;     (defun tsp:w3m-textarea-hook ()
;;       ;; protect the form local variables from being killed by `text-mode'
;;       (mapcar (lambda (v)
;;                 (if (string-match "^w3m-form-input-textarea.*"
;;                                   (symbol-name (car v)))
;;                     (put (car v) 'permanent-local t)))
;;               (buffer-local-variables))
;;       (text-mode)
;;       (tsp:w3m-textarea-mode))
;;     (add-hook 'w3m-form-input-textarea-mode-hook 'tsp:w3m-textarea-hook)))
;; (setq w3m-form-input-textarea-mode-hook nil)

;; I just simply use this as my w3m-form-input-textarea-mode-hook:
;; (defun dka-w3m-textarea-hook()
;;   (save-excursion
;;     (while (re-search-forward "\r\n" nil t)
;;       (replace-match "\n" nil nil))
;;     (delete-other-windows)))

;; (add-hook 'w3m-form-input-textarea-mode-hook 'dka-w3m-textarea-hook)
;;----------------------------------------------------------------------------------------------------
;;Search
;;----------------------------------------------------------------------------------------------------
;; Make the previous search engine the default for the next
;; search.
(defadvice w3m-search (after change-default activate)
  (let ((engine (nth 1 minibuffer-history)))
    (when (assoc engine w3m-search-engine-alist)
      (setq w3m-search-default-engine engine))))

;; (setq w3m-search-default-engine "google-groups")
(setq w3m-search-engine-alist
      '(("yahoo" "http://search.yahoo.com/bin/search?p=%s")
        ("google" "http://www.google.com/search?q=%s")
        ("google-cn" "http://www.google.cn/search?q=%s")
        ("google-groups" "http://groups.google.com/groups?q=%s")
        ("All the Web" "http://www.alltheweb.com/search?web&_sb_lang=en&q=%s")
        ("altavista" "http://altavista.com/sites/search/web?q=\"%s\"&kl=ja&search=Search")
        ("rpmfind" "http://rpmfind.net/linux/rpm2html/search.php?query=%s" nil)
        ("debian-pkg" "http://packages.debian.org/cgi-bin/search_contents.pl?directories=yes&arch=i386&version=unstable&case=insensitive&word=%s")
        ("debian-bts" "http://bugs.debian.org/cgi-bin/pkgreport.cgi?archive=yes&pkg=%s")
        ("eiei" "http://www.dictionary.com/cgi-bin/dict.pl?term=%s&r=67")
        ("amazon" "http://www.amazon.com/exec/obidos/search-handle-form/250-7496892-7797857" nil "url=index=blended&field-keywords=%s")
        ("weather" "http://www.weather.com/search/search?where=%s&what=WeatherLocalUndeclared")
        ("ebay" "http://search.ebay.com/search/search.dll?query=%s")
        ("wikipedia-en" "http://en.wikipedia.org/wiki/Special:Search?search=%s")
        ("wikipedia-zh" "http://zh.wikipedia.org/wiki/Special:Search?search=%s")
        ("worldclock" "http://www.timeanddate.com/worldclock/results.html?query=%s")
        ("emacs-wiki" "http://www.emacswiki.org/cgi-bin/wiki.pl?search=%s")))


(eval-after-load "w3m"
  '(add-to-list 'w3m-uri-replace-alist '("\\`wi:" w3m-search-uri-replace "wikipedia")))

;; Google Suggest
;; Intrigued by the Firefox google bar completion, I hacked the following function for use with emacs-w3m:

(defun google-suggest ()
  "Search `w3m-search-default-engine' with google completion canditates."
  (interactive)
  (w3m-search w3m-search-default-engine
              (completing-read  "Google search: "
                                (dynamic-completion-table
                                 google-suggest-aux))))

(defun google-suggest-aux (input)
  (with-temp-buffer
    (insert
     (shell-command-to-string
      (format "w3m -dump_source %s"
              (shell-quote-argument
               (format
                "http://www.google.com/complete/search?hl=en&js=true&qu=%s"
                input)))))
    (read
     (replace-regexp-in-string "," ""
                               (progn
                                 (goto-char (point-min))
                                 (re-search-forward "\(" (point-max) t 2)
                                 (backward-char 1)
                                 (forward-sexp)
                                 (buffer-substring-no-properties
                                  (1- (match-end 0)) (point)))))))



;; Typing the search engine and query at once like Firefox

;; Note: At least in the developer version of emacs-w3m (it's pretty stable, don't be afraid to grab a fresh CVS co) you can simply type

;; gg:<your search string>

;; to do a google search. There are some other search engines predefined and you can easily add your other favorite
;; engines. Have a look at 'w3m-uri-replace-alist'.

;; You know how Firefox or Internet Explorer lets you type in the address bar any search query if you define a bookmark
;; with "keyword" (IE requires the registry, hm). Let's do the same thing with w3m by defining a new function.

;; Add the following to your init files (eg. .emacs). Then run M-x piyo-w3m-search. By default, if you don't type your
;; search engine name, the whole string will be the query. If you do type a search engine name "google" for example,
;; that search engine will be used. I also enabled completion on the search engine name but allow space to enter as
;; self-insert-command (thanks offby1).

;; Here's a use case.

;; 1. I want to look up the entry "recursion" on "wikipedia-en" (see Customization example above)
;; 2. Type M-x piyo-w3m-search (you can bind this to a key, of course)
;; 3. Type "wiki" then hit tab to complete "wikipedia-en"
;; 4. Type space then "recursion".
;; 5. Hit enter and bask in w3m search stuff

;; The code:

(require 'w3m-search)                   ; or auto-load?
(defvar piyo-w3m--query-history nil
  "The query history isn't saved seperately!")
(defun piyo-w3m-search (line)
  "Modeled on `w3m-search', but if the first word is a search engine as defined in `w3m-search-default-engine',
 then use that engine instead."
  (interactive (piyo-w3m--read-query-smart))
  (let* ((defeng w3m-search-default-engine)
         (srceng (mapcar 'car w3m-search-engine-alist))
         (sepr " ")
         (brok (split-string line sepr))
         (possiblesea (pop brok)))
    (apply 'w3m-search
           (if (member possiblesea srceng)
               (list possiblesea (join-string brok " "))
               (list defeng (join-string (push possiblesea brok) " "))))))

(defun join-string (lst &optional seperator)
  "The reverse of `split-string'"
  (interactive)
  (mapconcat 'identity lst seperator))

(defun piyo-w3m--read-query-dumb ()
  "For reference. Not reference by running code."
  (let ((defeng w3m-search-default-engine))
    (list (w3m-search-read-query
           (format "%s search: " defeng)
           (format "%s search (default %%s): " defeng)
           'piyo-w3m--query-history))))

(defun piyo-w3m--read-query-smart ()
  "Use `completing-read' to find the first matching search engine. But allow space input."
  (let ((defeng w3m-search-default-engine)
        (minibuffer-local-completion-map (copy-sequence minibuffer-local-completion-map)))
    (setcdr (assoc ?\s                  ; change space character
                   minibuffer-local-completion-map) 'self-insert-command)
    (let ((completion-ignore-case t))
      (list (completing-read (format "%s search: " defeng)
                             w3m-search-engine-alist nil nil nil 'piyo-w3m--query-history)))))



;;Search ends here----------------------------------------------------------------------------------------------

;;----------------------------------------------------------------------------------------------------

(defun wicked/w3m-open-current-page-in-firefox ()
  "Open the current URL in Mozilla Firefox."
  (interactive)
  (browse-url-firefox w3m-current-url))

(defun wicked/w3m-open-link-or-image-in-firefox ()
  "Open the current link or image in Firefox."
  (interactive)
  (browse-url-firefox (or (w3m-anchor)
                          (w3m-image))))


(defvar deftsp-w3m-map)
(let ((map (make-keymap)))
  (suppress-keymap map)
  (define-key map [backspace] 'w3m-scroll-down-or-previous-url)
  (define-key map " " 'w3m-scroll-up-or-next-url)
  (define-key map "u" 'w3m-scroll-up-or-next-url)

  (define-key map "b" 'w3m-scroll-down-or-previous-url)
  (define-key map "f" 'wicked/w3m-open-current-page-in-firefox)
  (define-key map "F" 'wicked/w3m-open-link-or-image-in-firefox)

  (define-key map "\t" 'w3m-next-anchor)
  (define-key map [tab] 'w3m-next-anchor)
  (define-key map [(shift tab)] 'w3m-previous-anchor)
  (define-key map [(shift iso-lefttab)] 'w3m-previous-anchor)
  (define-key map "\C-m" 'w3m-view-this-url)
  (define-key map (kbd "<C-return>") 'w3m-view-this-url-new-session)
  (define-key map (kbd "<C-kp-enter>") 'w3m-view-this-url-new-session)

  (define-key map "a" 'w3m-bookmark-add-current-url)
  (define-key map "\M-a" 'w3m-bookmark-add-this-url)
  (define-key map "+" 'w3m-antenna-add-current-url)
  (define-key map "A" 'w3m-antenna)
  (define-key map "C" (make-sparse-keymap))
  (define-key map "Ct" 'w3m-redisplay-with-content-type)
  (define-key map "Cc" 'w3m-redisplay-with-charset)
  (define-key map "CC" 'w3m-redisplay-and-reset)
  (define-key map "d" 'w3m-download)
  ;; (define-key map "D" 'w3m-download-this-url)
  ;; (define-key map "D" 'w3m-download-with-wget)
  ;; (define-key map "D" 'w3m-download-with-curl)
  (define-key map "D" 'w3m-wget)
  (define-key map "\M-D" 'w3m-dtree)
  (define-key map "e" 'w3m-edit-current-url)
  (define-key map "E" 'w3m-edit-this-url)
  (define-key map "g" 'deftsp-w3m-goto-url)
  (define-key map "G" 'w3m-goto-url-new-session)
  (define-key map "h" 'describe-mode)
  (define-key map "H" 'w3m-gohome)
  (define-key map "i" (if (w3m-display-graphic-p)
                          'w3m-toggle-inline-image
                          'w3m-view-image))
  (define-key map "I" 'w3m-toggle-inline-images)
  (define-key map "k" 'w3m-delete-buffer)
  (when (w3m-display-graphic-p)
    (define-key map "\M-[" 'w3m-zoom-out-image)
    (define-key map "\M-]" 'w3m-zoom-in-image))
  (define-key map "l" 'deftsp-w3m-go-to-linknum)
  (define-key map "\M-i" 'w3m-save-image)

  (define-key map "\M-l" 'w3m-horizontal-recenter)
  (define-key map "M" 'w3m-view-url-with-external-browser)
  (define-key map "o" 'w3m-history)
  (define-key map "O" 'w3m-db-history)

  (define-key map "\M-n" 'w3m-next-buffer)
  (define-key map "\M-p" 'w3m-previous-buffer)
  ;; (define-key map "N" 'w3m-namazu)
  (define-key map "n" 'w3m-view-next-page)
  (define-key map "p" 'w3m-view-previous-page)

  (define-key map "N" 'surfkeys:next)
  (define-key map "P" 'surfkeys:prev)

  (define-key map "q" 'w3m-close-window)
  (define-key map "Q" 'w3m-quit)
  (define-key map "r" 'w3m-redisplay-this-page)
  (define-key map "R" 'w3m-reload-this-page)
  (define-key map "\C-tR" 'w3m-reload-all-pages)
  (define-key map "s" 'w3m-search)
  (define-key map "S" (lambda ()
                        (interactive)
                        (let ((current-prefix-arg t))
                          (call-interactively 'w3m-search))))
  (define-key map "\M-s" 'w3m-session-select)
  (define-key map "\M-S" 'w3m-session-save)
  (define-key map "t" 'deftsp-w3m-toggle-proxy)
  (define-key map "^" 'w3m-view-parent-page)
  (define-key map "v" 'w3m-bookmark-view)
  (define-key map "V" 'w3m-bookmark-view-new-session)
  (define-key map "W" 'w3m-weather)
  (define-key map "y" 'w3m-print-this-url)
  (define-key map "Y" 'w3m-print-current-url)
  (define-key map "=" 'w3m-view-header)
  (define-key map "\\" 'w3m-view-source)
  (define-key map "?" 'describe-mode)
  (define-key map "!" 'w3m-redisplay-with-content-type)
  (define-key map ">" 'w3m-scroll-left)
  (define-key map "<" 'w3m-scroll-right)
  (define-key map "." 'w3m-shift-left)
  (define-key map "," 'w3m-shift-right)
  (define-key map "\C-a" 'w3m-beginning-of-line)
  (define-key map "\C-e" 'w3m-end-of-line)

  (define-key map "^" 'w3m-view-parent-page)
  (define-key map "]" 'w3m-next-form)
  (define-key map "[" 'w3m-previous-form)
  (define-key map "}" 'w3m-next-image)
  (define-key map "{" 'w3m-previous-image)
  (define-key map "\C-c\M-l" 'w3m-delete-left-tabs)
  (define-key map "\C-c\M-r" 'w3m-delete-right-tabs)
  (define-key map "\C-c\M-w" 'w3m-delete-other-buffers)
  (define-key map "\C-c\C-a" 'w3m-switch-buffer)
  (define-key map "\C-c\C-c" 'w3m-submit-form)
  (define-key map "\C-c\C-n" 'w3m-next-buffer)
  (define-key map "\C-c\C-p" 'w3m-previous-buffer)
  ;; a convenient interactive way to switch from the minibuffer with the common M-n and M-p keys.
  (define-key map "\C-c\C-a" 'w3m-switch-buffer)
  (define-key map "\C-c\C-k" 'w3m-process-stop)
  (define-key map "\C-c\C-m" 'w3m-move-unseen-buffer)
  (define-key map "\C-c\C-s" 'w3m-select-buffer)
  (define-key map "\C-c\C-t" 'w3m-copy-buffer)
  (define-key map "\C-c\C-w" 'w3m-delete-buffer)
  (define-key map [(button2)] 'w3m-mouse-view-this-url)
  (define-key map [(shift button2)] 'w3m-mouse-view-this-url-new-session)
  (setq deftsp-w3m-map map))
(add-hook 'w3m-mode-hook '(lambda () (use-local-map deftsp-w3m-map)))



;;; SwitchToBuffer
(global-set-key (kbd "C-c w") 'my-w3m-switch-to-buffer)

(defvar *w3m-last-buffer* nil
  "Hold the buffer of last displayed.")

(defadvice w3m-close-window (before set-w3m-last-buffer activate)
  "Set the buffer of last displayed."
  (setf *w3m-last-buffer* (current-buffer)))


(defun w3m-open/restore-session ()
  (if (w3m-load-list w3m-session-file)
      (w3m-session-select)
      (call-interactively 'w3m)))

(defun my-w3m-switch-to-buffer (arg)
  "Select the ARG'th w3m buffer."
  (interactive "p")
  (let* ((arg (if (= arg 0) 10 (1- arg)))
         (w3m-list-buffers (w3m-list-buffers))
         (buf (if (> arg -1) (nth arg w3m-list-buffers))))
    (cond  ((not (w3m-list-buffers))
            (if (equal winring-name (and (featurep 'ecb)
                                   (symbol-value 'ecb-winman-winring-name)))
                (progn
                  (winring-next-configuration)
                  (w3m-open/restore-session))
                (w3m-open/restore-session)))
           ((and (eq major-mode 'w3m-mode) buf)
            (switch-to-buffer buf)
            (run-hooks 'w3m-select-buffer-hook)
            (w3m-select-buffer-update))
           ((not (eq major-mode 'w3m-mode))
            (if (member *w3m-last-buffer* w3m-list-buffers)
                (progn
                  (switch-to-buffer *w3m-last-buffer*)
                  (run-hooks 'w3m-select-buffer-hook)
                  (w3m-select-buffer-update))
                (switch-to-buffer (first w3m-list-buffers)))))))

(add-hook 'w3m-mode-hook
          (lambda ()
            (dolist (bufnum '(0 1 2 3 4 5 6 7 8 9))
              (let* ((bufstr (number-to-string bufnum))
                     (funcname (concat "my-w3m-switch-to-buffer-" bufstr)))
                (eval `(defun ,(intern funcname) ()
                         (interactive)
                         (my-w3m-switch-to-buffer ,bufnum)))
                (define-key w3m-mode-map bufstr
                  (intern funcname))))))

;; webjump
(require 'webjump)
(global-set-key (kbd "C-c j j") 'webjump)
(setq webjump-sites
      '(("google" . [simple-query "www.google.com" "www.google.com/search?q=" ""])
        ("Google Groups" . [simple-query "groups.google.com" "groups.google.com/groups?q=" ""])
        ("brightgate" . [simple-query "www.brightgate.com" "www.brightgate.com/cgi-bin/search/fast/meta?q=" ""])
        ("Roget's Internet Thesaurus" . [simple-query "www.thesaurus.com" "www.thesaurus.com/cgi-bin/htsearch?config=roget&words=" ""])
        ("Debian GNU/Linux" . [simple-query "www.debian.org" "search.debian.org/?q=" ""])
        ("Python Documentation" . [simple-query "starship.python.net" "http://starship.python.net/crew/theller/pyhelp.cgi?keyword=" "&version=current"])
        ("Dictionary.com" . [simple-query "www.dictionary.com" "www.dictionary.com/cgi-bin/dict.pl?term=" "&db=*"])
        ("leo" . [simple-query "dict.leo.org" "dict.leo.org/?search=" ""])
        ("linux apps" . "www.appwatch.com/Linux/")
        ("latex online help" . "http://www.giss.nasa.gov/latex/ltx-2.html")
        ("latex online index" . "http://www.weinelt.de/latex/index.html")
        ("mensa" . "http://www.cycamp.at/mensen/menuplan.php?location=ulm&week_add=")
        ("khg-mensa" . "http://madonna.khg-heim.uni-linz.ac.at/mensa/aktueller.menueplan.html")
        ("spiegel" . "www.spiegel.de")
        ("solidot" . "http://solidot.org/")
        ("slashdot" . "http://slashdot.org/")
        ("wetter" . "www.oon.at/wetter/oberoesterreich/prognose.asp")))



;; (add-hook 'kill-buffer-hook 'make-w3m-backup-copy)

;; (defun make-w3m-backup-copy ()
;;   (when (string-match "^w3m" (buffer-name))
;;     (write-file (concat (buffer-file-name) ".last"))))



;; (defun w3m-new-tab ()
;;   (interactive)
;;   (w3m-copy-buffer nil nil nil t))

;; (defun w3m-browse-url-new-tab (url &optional new-session)
;;   (interactive)
;;   (w3m-new-tab)
;;   (w3m-browse-url url))


(defun browse-url-default-browser (url &rest args)
  (apply
   (cond
     ((executable-find browse-url-firefox-program) 'browse-url-firefox)
     ((executable-find browse-url-xterm-program) 'browse-url-lynx-xterm)
     (t
      (lambda (&ignore args) (error "No usable browser found"))))
   url args))


(require 'w3m-lnum)
(defun deftsp-w3m-go-to-linknum ()
  "Turn on link numbers and ask for one to go to."
  (interactive)
  (let ((active w3m-link-numbering-mode))
    (when (not active) (w3m-link-numbering-mode))
    (unwind-protect
         (w3m-move-numbered-anchor (read-number "Anchor number: "))
      (when (not active) (w3m-link-numbering-mode)))))


(defun campaigns/check-buffer-urls ()
  "Checks the current buffer for broken links, skipping those that match
entries in the `no-check' list."
  (interactive)
  (let ((matches '())
        (no-check '("mailto"))
        (fails 0))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward thing-at-point-url-regexp nil t)
        (add-to-list 'matches (match-string-no-properties 0))
        t))
    (pop-to-buffer "*Link checking results*")
    (erase-buffer)
    (insert "Checking your links...\n\n")
    (mapc (lambda (url)
            (let ((parsed (url-generic-parse-url url)))
              (insert (format "* %s ..." url))
              (if (member (elt parsed 0) no-check)
                  (insert "SKIPPING\n")
                  (condition-case err
                      (if (url-http-file-exists-p url)
                          (insert "OK\n")
                          (insert "ERROR!\n")
                          (setq fails (1+ fails)))
                    (error (insert "ERROR!\n")
                           (setq fails (1+ fails)))))))
          matches)
    (insert (format "\nFinish checking %d links. There were %d problems.\n"
                    (length matches) fails))))

;;; Filtering To Downcast To ASCII

;; emacs-w3m allows users to filter content, and this feature can be used to downcast entities, unicode, and other
;; “?”-spawning characters into something of your choice in ASCII.

;; (setq w3m-use-filter t)
;; ;; send all pages through one filter
;; (setq w3m-filter-rules `(("\\`.+" w3m-filter-all)))

;; (defun w3m-filter-all (url)
;;   (let ((list '(("&#187;" "&gt;&gt;")
;;                 ("&laquo;" "&lt;")
;;                 ("&raquo;" "&gt;")
;;                 ("&ouml;" "o")
;;                 ("&#8230;" "...")
;;                 ("&#8216;" "'")
;;                 ("&#8217;" "'")
;;                 ("&rsquo;" "'")
;;                 ("&lsquo;" "'")
;;                 ("\u2019" "\'")
;;                 ("\u2018" "\'")
;;                 ("\u201c" "\"")
;;                 ("\u201d" "\"")
;;                 ("&rdquo;" "\"")
;;                 ("&ldquo;" "\"")
;;                 ("&#8220;" "\"")
;;                 ("&#8221;" "\"")
;;                 ("\u2013" "-")
;;                 ("\u2014" "-")
;;                 ("&#8211;" "-")
;;                 ("&#8212;" "-")
;;                 ("&ndash;" "-")
;;                 ("&mdash;" "-")
;;                 )))
;;     (while list
;;       (let ((pat (car (car list)))
;;             (rep (car (cdr (car list)))))
;;         (goto-char (point-min))
;;         (while (search-forward pat nil t)
;;           (replace-match rep))
;;         (setq list (cdr list))))))

;;; Like surfkeys of firefox
(defun surfkeys:next ()
  (interactive)
  (save-excursion
    (goto-char (point-max))
    (if (re-search-backward "Next\\|下一页" nil t)
        (w3m-view-this-url)
        (message "No such page"))))

(defun surfkeys:prev ()
  (interactive)
  (save-excursion
    (goto-char (point-max))
    (if (re-search-backward "Prev\\|上一页" nil t)
        (w3m-view-this-url)
        (message "No such page"))))


;; google-region
(defun google-region (&optional flags)
  "Google the selected region"
  (interactive)
  (let ((query (buffer-substring (region-beginning) (region-end))))
    (browse-url (concat "http://www.google.com/search?ie=utf-8&oe=utf-8&q=" query))))
;; press control-c g r to google the selected region
(global-set-key (kbd "C-c g r") 'google-region)

;;; Quick searches
;; http://sachachua.com/wp/2008/08/18/emacs-and-w3m-quick-searches/
;; use M-x browse-url RET g emacs RET to do a Google search for all things Emacs, and use M-x w3m-goto-url (usually
;; bound to g) inside w3m to use the quick searches.

(setq wicked/quick-search-alist
      '(("^g?:? +\\(.*\\)" . ;; Google Web
         "http://www.google.com/search?q=\\1")

        ("^g!:? +\\(.*\\)" . ;; Google Lucky
         "http://www.google.com/search?btnI=I%27m+Feeling+Lucky&q=\\1")

        ("^dict:? +\\(.*\\)" . ;; Dictionary
         "http://dictionary.reference.com/search?q=\\1")

        ("^ew:? *?\\(.*\\)" . ;; Emacs Wiki Search
         "http://www.emacswiki.org/cgi-bin/wiki?search=\\1")))

;; (require 'cl-seq)
(defadvice w3m-goto-url (before wicked activate)
  "Use the quick searches defined in `wicked/quick-search-alist'."
  (let* ((my-url (replace-regexp-in-string
                  "^ *\\| *$" ""
                  (replace-regexp-in-string "[ \t\n]+" " " (ad-get-arg 0))))
         (match (assoc-if
                 (lambda (a) (string-match a my-url))
                 wicked/quick-search-alist)))
    (if match
        (ad-set-arg 0 (replace-regexp-in-string
                       (car match) (cdr match) my-url)))))

(defadvice browse-url (before wicked activate)
  "Use the quick searches defined in `wicked/quick-search-alist'."
  (let* ((my-url (replace-regexp-in-string
                  "^ *\\| *$" ""
                  (replace-regexp-in-string "[ \t\n]+" " " (ad-get-arg 0))))
         (match (assoc-if
                 (lambda (a) (string-match a my-url))
                 wicked/quick-search-alist)))
    (if match
        (ad-set-arg 0 (replace-regexp-in-string
                       (car match) (cdr match) my-url)))))

;;; fake your user agent
;; http://sachachua.com/wp/2008/08/19/emacs-and-w3m-fake-your-user-agent/

;; Sets up a number of common user agents(1) using examples from http://www.useragentstring.com
(defvar wicked/w3m-fake-user-agents ;; (1)
  `(("w3m" . ,(concat "Emacs-w3m/" emacs-w3m-version " " w3m-version))
    ("ie6" . "Mozilla/4.0 (compatible; MSIE 6.0; Windows NT 5.1)")
    ("ff3" . "Mozilla/5.0 (X11; U; Linux i686; en-US; rv:1.9.0.1) Gecko/2008070206 Firefox/3.0.1")
    ("ff2" . "Mozilla/5.0 (X11; U; Linux i686; en-US; rv:1.8.1.13) Gecko/20080208 Firefox/2.0.0.13")
    ("ie7" . "Mozilla/4.0 (compatible; MSIE 7.0; Windows NT 5.1; .NET CLR 2.0.50727)")
    ("ie5.5" . "Mozilla/4.0 (compatible; MSIE 5.5; Windows 98)")
    ("iphone" . "Mozilla/5.0 (iPhone; U; CPU iPhone OS 2_0 like Mac OS X; en-us) AppleWebKit/525.18.1 (KHTML, like Gecko) Version/3.1.1 Mobile/5A347 Safari/525.20")
    ("safari" . "Mozilla/5.0 (Macintosh; U; Intel Mac OS X 10_5_2; en-us) AppleWebKit/525.13 (KHTML, like Gecko) Version/3.1 Safari/525.13")
    ("google" . "Mozilla/5.0 (compatible; Googlebot/2.1; +http://www.google.com/bot.html)"))
  "*Associative list of user agent names and strings.")

(defvar wicked/w3m-fake-user-agent-sites ;; (2)
  '(("^https?://www\\.useragentstring\\.com" . "ff2"))
  "*Associative list of regular expressions matching URLs and the agent keyword or value.
 The first matching entry will be used.")

(defun wicked/w3m-set-user-agent (agent)
  "Set the user agent to AGENT based on `wicked/w3m-fake-user-agents'.
 If AGENT is not defined in `wicked/w3m-fake-user-agents', it is used as the user agent.
 If AGENT is empty, the default w3m user agent will be used."
  (interactive
   (list
    (completing-read "User-agent [w3m]: "
                     (mapcar 'car wicked/w3m-fake-user-agents)
                     nil nil nil nil "w3m"))) ;; (3)
  (if agent
      (progn
        (setq w3m-user-agent
              (or
               (and (string= agent "") (assoc "w3m" wicked/w3m-fake-user-agents)) ;; (4)
               (cdr (assoc agent wicked/w3m-fake-user-agents))                  ;; (5)
               agent))                                                          ;; (6)
        (setq w3m-add-user-agent t))
      (setq w3m-add-user-agent nil)))

(defun wicked/w3m-reload-this-page-with-user-agent (agent)
  "Browse this page using AGENT based on `wicked/w3m-fake-user-agents'.
 If AGENT is not defined in `wicked/w3m-fake-user-agents', it is used as the user agent.
 If AGENT is empty, the default w3m user agent will be used."
  (interactive (list (completing-read "User-agent [w3m]: "
                                      (mapcar 'car wicked/w3m-fake-user-agents)
                                      nil nil nil nil "w3m")))
  (let ((w3m-user-agent w3m-user-agent)
        (w3m-add-user-agent w3m-add-user-agent))
    (wicked/w3m-set-user-agent agent) ;; (7)
    (w3m-reload-this-page)))

(defadvice w3m-header-arguments (around wicked activate) ;; (8)
  "Check `wicked/w3m-fake-user-agent-sites' for fake user agent definitions."
  (let ((w3m-user-agent w3m-user-agent)
        (w3m-add-user-agent w3m-add-user-agent)
        (sites wicked/w3m-fake-user-agent-sites))
    (while sites
      (if (string-match (caar sites) (ad-get-arg 1))
          (progn
            (wicked/w3m-set-user-agent (cdar sites))
            (setq sites nil))
          (setq sites (cdr sites))))
    ad-do-it))

;;; toggles between the W3M web browser and other buffers you're working on
;; (defun wicked/toggle-w3m ()
;;   "Switch to a w3m buffer or return to the previous buffer."
;;   (interactive)
;;   (if (derived-mode-p 'w3m-mode)
;;       ;; Currently in a w3m buffer
;;       ;; Bury buffers until you reach a non-w3m one
;;       (while (derived-mode-p 'w3m-mode)
;;         (bury-buffer))
;;       ;; Not in w3m
;;       ;; Find the first w3m buffer
;;       (let ((list (buffer-list)))
;;         (while list
;;           (if (with-current-buffer (car list)
;;                 (derived-mode-p 'w3m-mode))
;;               (progn
;;                 (switch-to-buffer (car list))
;;                 (setq list nil))
;;               (setq list (cdr list))))
;;         (unless (derived-mode-p 'w3m-mode)
;;           (call-interactively 'w3m)))))

;; (global-set-key (kbd " ") 'wicked/toggle-w3m)

;; renames w3m buffers to include the page title (or URL, if there is no title), and works great for me.
(add-hook 'w3m-display-hook
          (lambda (url)
            (rename-buffer
             (format "*w3m: %s*" (or w3m-current-title
                                     w3m-current-url)) t)))


;; Despite IDNA support is bundled with Emacs since 2003 (C-h f idna-*), Emacs-w3m does not handle international
;; domains. It simply encodes all non-ASCII characters in the given URL, which makes in the context of IDNs non-sense.
;; But advising the function w3m-url-transfer-encode-string—responsible for urlencoding unsafe data—to pre-encode IDNs
;; may help a lot. My solution below has a known limitation, so that after activating encode-idna,
;; w3m-url-transfer-encode-string will refuse invalid URL schemes.

;; (defadvice w3m-url-transfer-encode-string
;;   (around encode-idna (url &optional coding))
;;   (let* ((host (w3m-http-url-host
;;                 (w3m-parse-http-url (w3m-canonicalize-url url))))
;;          (url (replace-regexp-in-string host (idna-to-ascii host) url)))
;;     ad-do-it))
;; (ad-activate 'w3m-url-transfer-encode-string)


;;; wget

(autoload 'wget "wget" "wget interface for Emacs." t)
(autoload 'wget-web-page "wget" "wget interface to download whole web page." t)
(load "w3m-wget")
(setq wget-basic-options '("-v"))
(setq wget-download-directory "~/dl")

;; FIXME: find way to let ad-do-it run after w3m session been selected.
(defadvice slime-hyperspec-lookup (around open/restore-w3m-session-first activate compile)
  "When open a new page with slime-hyperspec-lookup open/restore a w3m session first."
  (if (w3m-list-buffers)
      ad-do-it
      (if (w3m-load-list w3m-session-file)
          (if (equal winring-name (and (featurep 'ecb)
                                       (symbol-value 'ecb-winman-winring-name)))
              (progn
                (winring-next-configuration)
                (message "open/restore a w3m session and run `slime-hyperspec-lookup' again.")
                (w3m-session-select))
              (message "open/restore a w3m session and run `slime-hyperspec-lookup' again.")
              (w3m-session-select))
          ad-do-it)))


;; tips
;; a tab list via C-c C-s
