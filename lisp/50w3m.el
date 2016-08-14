;;; 50w3m.el ---

;;; install
;; install emacs-w3m with el-get

(setq w3m-key-binding 'info) ;; set before load "w3m.el"

(require 'w3m)

;;; browser
(setq browse-url-browser-function '(("file:.*/usr/local/share/gtk-doc/html" . w3m-goto-url-new-session)
                                    ("file:.*/usr/share/gtk-doc/html" . w3m-goto-url-new-session)
                                    ;; ("file:///usr/local/.*html" . w3m-goto-url-new-session)
                                    ;; for OS X: can't open urls with # https://github.com/areina/helm-dash/issues/36
                                    ("file:.*\.docset/Contents/Resources/Documents/" . w3m-goto-url-new-session)
                                    ("." . browse-url-default-browser))
      browse-url-firefox-new-window-is-tab t
      browse-url-new-window-flag nil)


(global-set-key (kbd "C-c C-o") 'browse-url-at-point)
(global-set-key (kbd "C-c O") 'browse-url)

(setq w3m-language "en"
      w3m-bookmark-file-coding-system 'utf-8
      w3m-coding-system 'utf-8
      w3m-default-coding-system 'utf-8
      w3m-file-coding-system 'utf-8
      w3m-file-name-coding-system 'utf-8
      w3m-terminal-coding-system 'utf-8
      w3m-input-coding-system 'utf-8
      w3m-output-coding-system 'utf-8
      w3m-home-page ""
      w3m-favicon-image nil
      w3m-use-favicon nil
      w3m-use-toolbar t
      w3m-tab-width 16
      w3m-toggle-inline-images-permanently t
      w3m-session-autosave t
      w3m-new-session-in-background t
      w3m-form-textarea-edit-mode 'org-mode
      w3m-enable-google-feeling-lucky t
      w3m-command-arguments '("-F") ; default arguments passed to the w3m command. "-F" automatically render frame
      w3m-use-cookies t
      w3m-cookie-accept-bad-cookies t
      w3m-fill-column 120)

;;; icon path
(let ((p "~/.emacs.d/el-get/emacs-w3m/icons"))
  (if (file-exists-p p)
      (setq w3m-icon-directory p)
    (message "w3m-icon-directory: %s is not exist" p)))

(defun paloryemacs/w3m-mode-setup ()
  ;; (define-key w3m-mode-map "S" 'w3m-session-save)
  ;; (define-key w3m-mode-map "t" 'paloryemacs/w3m-toggle-proxy)
  (define-key w3m-mode-map "N" 'surfkeys:next)
  (define-key w3m-mode-map "P" 'surfkeys:prev)
  ;; (define-key w3m-mode-map "d" 'paloryemacs/w3m-download-with-wget) ; default value: w3m-dtree 'paloryemacs/w3m-download-with-curl/w3m-wget
  (define-key w3m-mode-map "\M-n" 'w3m-next-buffer)
  (define-key w3m-mode-map "\M-p" 'w3m-previous-buffer)

  (define-key w3m-mode-map "g" 'paloryemacs/w3m-goto-url)
  (define-key w3m-mode-map "k" 'w3m-delete-buffer)
  (define-key w3m-mode-map "u" 'w3m-scroll-up-or-next-url))

(add-hook 'w3m-mode-hook 'paloryemacs/w3m-mode-setup)

;;; w3m-toggle-proxy
(defvar paloryemacs/w3m-proxy-p nil)
(defun paloryemacs/w3m-toggle-proxy ()
  (interactive)
  (if paloryemacs/w3m-proxy-p
      (progn
        (setq w3m-command-arguments '("-F"))
        (setq paloryemacs/w3m-proxy-p nil)
        (message "w3m proxy: OFF"))
    (progn
      (setq w3m-command-arguments '("-F" "-o" "http_proxy=http://127.0.0.1:8118/"))
      (setq paloryemacs/w3m-proxy-p t)
      (message "w3m proxy: ON"))))

(setq w3m-no-proxy-domains '("google.com" "google.cn"))

;;; Downloading Files Asynchronously
;; Since w3m-el downloads everything synchronously it can sometimes interfere with productivity. If you use w3m-el to
;; download and save large files here are two solutions to do so asynchronously. One uses wget and the other uses cURL.
;; Neither of these solutions really helps with the problem of synchronous browsing,only eliminates synchronous
;; downloading. EmacsWget is probably superior than the following function.
(defun paloryemacs/w3m-download-with-wget (loc)
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

(defun paloryemacs/w3m-download-with-curl (loc)
  (define-key w3m-mode-map "c"
    (lambda (dir)
      (interactive "DSave to: ")
      (cd dir)
      (start-process "curl" "*curl*" "curl.exe" "-O" "-s" (w3m-anchor)))))

;; To take advantage of these, just call either function while the point is on a URL. You will then be prompted for the
;; directory to download the URL into. Note the examples of binding these functions below in the alternate keymap. These
;; functions are a replacement for w3m-download-this-url.

;;; Search
(eval-after-load "w3m-search"
  '(progn
     (defadvice w3m-search (after change-default activate)
       (let ((engine (nth 1 minibuffer-history)))
         (when (assoc engine w3m-search-engine-alist)
           (setq w3m-search-default-engine engine))))
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
             ("emacs-wiki" "http://www.emacswiki.org/cgi-bin/wiki.pl?search=%s")))))

;; Make the previous search engine the default for the next search.
(eval-after-load "w3m"
  '(add-to-list 'w3m-uri-replace-alist '("\\`wi:" w3m-search-uri-replace "wikipedia")))


;;; switch to buffer
(defvar *w3m-last-buffer* nil
  "Hold the buffer of last displayed.")

(defadvice w3m-close-window (before set-w3m-last-buffer activate)
  "Set the buffer of last displayed."
  (setf *w3m-last-buffer* (current-buffer)))


(defun w3m-open/restore-session ()
  (if (w3m-load-list w3m-session-file)
      (w3m-session-select)
      (call-interactively 'w3m)))

;; bind keychord ";w" to `paloryemacs/w3m-switch-to-buffer'
(defun paloryemacs/w3m-switch-to-buffer (arg)
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
                     (funcname (concat "paloryemacs/w3m-switch-to-buffer-" bufstr)))
                (eval `(defun ,(intern funcname) ()
                         (interactive)
                         (paloryemacs/w3m-switch-to-buffer ,bufnum)))
                (define-key w3m-mode-map bufstr
                  (intern funcname))))))

;;; webjump
;; (global-set-key (kbd "C-c j j") 'webjump)
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


;;; google-region
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

;;; usefull function
;; Browsing the current buffer
;; "preview" a buffer full of HTML in w3m.
(defun paloryemacs/w3m-browse-current-buffer ()
  (interactive)
  (let ((filename (concat (make-temp-file "w3m-") ".html")))
    (unwind-protect
        (progn
          (write-region (point-min) (point-max) filename)
          (w3m-find-file filename))
      (delete-file filename))))

(defun paloryemacs/w3m-goto-url ()
  (interactive)
  (let ((w3m-current-url ""))
    (call-interactively #'w3m-goto-url)))

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



(provide '50w3m)
;; tips
;; a tab list via C-c C-s
