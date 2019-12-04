;; -*- mode: Emacs-Lisp -*-

(use-package emms-setup
  :config
  (progn
    (emms-all)
    (setq emms-player-list '(emms-player-mpd))))

(use-package emms
  :defer t
  :init
  (progn
    (setq emms-directory (expand-file-name "emms" user-emacs-directory)
          emms-lyrics-display-format "%s"
          emms-playing-time-style 'bar      ; 'time
          emms-playing-time-display-p nil
          emms-playing-time-display-format "%s"
          emms-mode-line-icon-before-format "["
          emms-mode-line-format " %s]"
          emms-volume-amixer-control "PCM" ; "Master"
          emms-history-start-playing nil
          emms-show-format "NP: %s"
          emms-repeat-playlist t)
    ;; coding
    (setq emms-i18n-default-coding-system '(utf-8 . utf-8)
          emms-cache-file-coding-system 'utf-8)

    (setq later-do-interval 0.0001
          emms-info-asynchronously nil))
  :config
  (progn
    (emms-history-load)
    (emms-cache-enable)
    (emms-score-enable)))

(use-package emms-player-simple-mpv
  :after (emms)
  :config
  (progn
    (use-package emms-player-simple-mpv-control-functions)
    (emms-player-simple-mpv-playlist-mode-setup-keybinds)
    (define-emms-simple-player-mpv tl-mpv '(file url streamlist playlist)
      (concat "\\`\\(http[s]?\\|mms\\)://\\|"
              (apply #'emms-player-simple-regexp
                     "aac" "pls" "m3u"
                     emms-player-base-format-list))
      "mpv" "--no-terminal" "--force-window=no" "--audio-display=no")

    (emms-player-simple-mpv-add-to-converters
     'emms-player-tl-mpv "." '(playlist)
     (lambda (track-name) (format "--playlist=%s" track-name)))

    (add-to-list 'emms-player-list 'emms-player-tl-mpv)

    (dolist (map (list emms-playlist-mode-map
                       emms-stream-mode-map))
      (define-key map (kbd "m") 'emms-player-simple-mpv-mute)
      (define-key map (kbd "[") 'emms-player-simple-mpv-speed-decrease)
      (define-key map (kbd "]") 'emms-player-simple-mpv-speed-increase)
      (define-key map (kbd "{") 'emms-player-simple-mpv-speed-halve)
      (define-key map (kbd "}") 'emms-player-simple-mpv-speed-double)
      (define-key map (kbd "<backspace>") 'emms-player-simple-mpv-speed-normal)
      (define-key map (kbd "T") 'emms-player-simple-mpv-ontop)
      (define-key map (kbd "F") 'emms-player-simple-mpv-fullscreen)
      (define-key map (kbd "9") 'emms-volume-lower)
      (define-key map (kbd "0") 'emms-volume-raise))

    (let ((map emms-playlist-mode-map))
      (define-key map (kbd ",") 'emms-player-simple-mpv-playlist-prev)
      (define-key map (kbd ".") 'emms-player-simple-mpv-playlist-next))))

;; define-emms-combined-source:Define a `emms-play-X' and `emms-add-X' function for SOURCES.
;; fixme: only add file  not  playlist.m3u
;; (define-emms-combined-source all nil
;;   '((emms-source-playlist-directory "/mnt/d/mp3")
;;     (emms-source-playlist-file "/mnt/d/mp3/playlist.m3u")))

;;; mpd connect
;; (emms-player-mpd-connect)
;; dump cache
;; (emms-cache-set-from-mpd-all)
;; (emms-cache-save)

(use-package emms-mode-line-icon
  :after (emms)
  :init
  (progn
    (setq emms-mode-line-icon-color "DarkBlue"))
  :config
  (progn
    ;; display title only I redefine follow function in emms-mode-line-icon.el
    (defun emms-mode-line-icon-function ()
      (concat " "
              emms-mode-line-icon-before-format
              (propertize "NP:" 'display emms-mode-line-icon-image-cache)
              (format emms-mode-line-format (emms-track-get
                                             (emms-playlist-current-selected-track)
                                             'info-title))))
    (setq emms-mode-line-mode-line-function 'emms-mode-line-icon-function)
    (setq emms-mode-line-titlebar-function nil)))

(use-package emms-source-file
  :after (emms)
  :init
  (progn
    (setq emms-source-file-default-directory (expand-file-name "~/media/mp3/")
          emms-source-playlist-default-format 'm3u)
    (when (eq system-type 'darwin)
      (setq emms-source-file-gnu-find "gfind"))
    ;; use faster finding facility if you have GNU find
    (when (executable-find emms-source-file-gnu-find)
      (setq emms-source-file-directory-tree-function
            'emms-source-file-directory-tree-find))))

;; (add-hook 'emms-player-started-hook 'emms-show)

(use-package emms-playlist-mode
  :after (emms)
  :init
  (progn
    (setq emms-playlist-mode-open-playlists t
          emms-playlist-mode-window-width  30)))


(use-package emms-player-mpd
  :after (emms)
  :init
  (progn
    (setq emms-player-mpd-server-name "localhost"
          emms-player-mpd-server-port "6600"
          emms-player-mpd-music-directory "~/media/mp3"
          ;; emms-player-mpd-server-password "mypassword"
          ;; emms-volume-change-function 'emms-volume-mpd-change
          emms-player-mpd-sync-playlist t
          emms-player-mpd-verbose nil)


    ))

(use-package emms-player-simple
  :after (emms)
  :init
  (progn
    (setq emms-player-mpg321-command-name "mpg123")))


(use-package emms-lyrics
  :after (emms)
  :init
  (progn
    ;; (setq emms-lyrics-dir "/mnt/d/Program\ Files/TTPlayer/Lyrics")
    (setq emms-lyrics-scroll-p nil
          emms-lyrics-display-on-modeline t
          emms-lyrics-coding-system nil)))

(use-package emms-playlist-limit
  :after (emms)
  :config
  (progn
    (add-hook 'emms-playlist-limit-hook 'emms-playlist-sort-by-natural-order)))

;;; Track Show Format(in emms-playlist-mode)
;; emms-last-played
(setq emms-last-played-format-alist
      '(((emms-last-played-seconds-today) . "%a %H:%M")
        (604800                           . "%a %H:%M") ; this week
        ((emms-last-played-seconds-month) . "%d")
        ((emms-last-played-seconds-year)  . "%m-%d")
        (t                                . "%Y-%m-%d")))


(defvar tl-emms-playlist-last-track nil)
(defvar tl-emms-playlist-last-indent "\\")

(defun tl/emms-track-description (track)
  "Return a description of the current track."
  (let* ((name (emms-track-name track))
         (type (emms-track-type track))
         (short-name (file-name-nondirectory name))
         (play-count (or (emms-track-get track 'play-count) 0))
         (last-played (or (emms-track-get track 'last-played) '(0 0 0)))
         (empty "..."))
    (prog1
        (case type
          ((file url)
           (let* ((artist (or (emms-track-get track 'info-artist) empty))
                  (year (emms-track-get track 'info-year))
                  (playing-time (or (emms-track-get track 'info-playing-time) 0))
                  (min (/ playing-time 60))
                  (sec (% playing-time 60))
                  (album (or (emms-track-get track 'info-album) empty))
                  (tracknumber (or (emms-track-get track 'info-tracknumber) ""))
                  (short-name (file-name-sans-extension (file-name-nondirectory name)))
                  (title (or (emms-track-get track 'info-title) short-name))
                  (ext (file-name-extension name))

                  ;; last track
                  (ltrack tl-emms-playlist-last-track)
                  (lartist (or (and ltrack (emms-track-get ltrack 'info-artist))
                               empty))
                  (lalbum (or (and ltrack (emms-track-get ltrack 'info-album))
                              empty))

                  (same-album-p (and (not (string= lalbum empty))
                                     (string= album lalbum))))

             (format "♪ %10s  %3d   %-20s%-50s%-40s%-12s%-10s%s"
                     (emms-last-played-format-date last-played)
                     play-count
                     artist

                     ;; Combine indention, tracknumber, title.
                     ;; (format "%s%s%-40s"
                     (concat
                      (if same-album-p  ; indention by album
                          (setq tl-emms-playlist-last-indent
                                (concat " " tl-emms-playlist-last-indent))
                        (setq tl-emms-playlist-last-indent "\\")
                        "")

                      (if (string= tracknumber "") "" (format "%2s." tracknumber))

                      title)

                     ;; album
                     (cond ((string= album empty) empty)
                           ;; (same-album-p "  ")

                           (t (concat "《" album "》")))

                     (or year empty)
                     (if (or (> min 0)  (> sec 0))
                         (format "%02d:%02d" min sec)
                       empty)
                     ext)))
          ((url)
           (concat (symbol-name type)
                   ":"
                   (decode-coding-string
                    (encode-coding-string name 'utf-8)
                    'gbk)))
          (t
           (format "%-3d%s"
                   play-count
                   (concat (symbol-name type) ":" name))))

      (setq tl-emms-playlist-last-track track))))

(setq emms-track-description-function 'tl/emms-track-description)


;;To get track information from MusicPD, invoke the following:
;; (add-to-list 'emms-info-functions 'emms-info-mpd)
;; `mp3info' Only supported ID3 versions 1.0 and 1.1.
;; (add-to-list 'emms-info-functions 'emms-info-mp3info)

;;; Libtag support
;; cd ~/.emacs.d/packages/emms
;; make emms-print-metadata
;; cd ~/bin && ln -s ~/.emacs.d/packages/emms/emms-print-metadata
;; (require 'emms-info-libtag)
;; (add-to-list 'emms-info-functions 'emms-info-libtag)
;; (setq emms-info-libtag-coding-system 'utf-8)

;; Switch to the radio buffer
;; (defun my-emms-streams ()
;;   (interactive)
;;   (let ((buf (get-buffer emms-stream-buffer-name)))
;;     (if buf
;;         (switch-to-buffer buf)
;;       (emms-streams))))

;; keybinding
;; Rebind sort map prefix key
;; (add-hook 'emms-playlist-mode-hook
;;           '(lambda ()
;;             (local-unset-key "s")
;;             (setq emms-playlist-sort-prefix "s")
;;             (emms-playlist-sort-map-setup)))


;; (defun tl/emms-playlist-mode-hook ()
;;   (toggle-truncate-lines 1))
;; (add-hook 'emms-playlist-mode-hook 'tl/emms-playlist-mode-hook)


;;; Misc
;; (defun tl/playlist-mode-delete-track-at ()
;;   "Delete the track at point in emms-playlist buffer"
;;   (interactive)
;;   (if (emms-playlist-ensure-playlist-buffer)
;;       (widen)
;;     (let* ((file-name (emms-track-get
;;                        (emms-playlist-track-at) 'name))
;;            (file-name-lyric (emms-replace-regexp-in-string
;;                              (concat "\\." (file-name-extension file-name) "\\'")
;;                              ".lrc"
;;                              file-name)))
;;       (if file-name
;;           (when (y-or-n-p (concat "Delete \"" file-name "\" from Disk? "))
;;             (emms-with-inhibit-read-only-t
;;              (save-excursion
;;                (let ((kill-whole-line t))
;;                  (goto-char (point-at-bol))
;;                  (kill-line)
;;                  (delete-file file-name)
;;                  (if (file-exists-p file-name-lyric)
;;                      (delete-file file-name-lyric))))
;;              (emms-playlist-mode-play-smart)))))))


;; (defun emms-google-for-lyrics ()
;;   (interactive)
;;   (browse-url
;;    (concat "http://www.google.com/search?q="
;;            (replace-regexp-in-string " +" "+"
;;                                      (concat "lyrics "
;;                                              (delete ?- (emms-track-description
;;                                                          (emms-playlist-current-selected-track))))))))


;; play what i mean
;; (defun emms-pwim (truc &optional rien)
;;   "Plays the TRUC specified, whatever it is. The function tries to
;; guess the type of TRUC, between playlist, directory containing
;; playable tracks, and files. If the directory does not contain
;; playable tracks, but some sub-directories, it will play the
;; tree."
;;   (interactive
;;    (find-file-read-args "Play what ? " t))
;;   (cond
;;    ((file-exists-p truc)    (emms-play-file truc))
;;    ((file-directory-p truc) (emms-play-directory truc))))

;; add what i mean
;; (defun emms-awim (truc &optional rien)
;;   "Adds the TRUC specified, whatever it is. The function tries to
;; guess the type of TRUC, between playlist, directory containing
;; playable tracks, and files. If the directory does not contain
;; playable tracks, but some sub-directories, it will add the
;; tree."
;;   (interactive
;;    (find-file-read-args "Add what ? " t))
;;   (cond
;;    ((file-exists-p truc)    (emms-add-file truc))
;;    ((file-directory-p truc) (emms-add-directory truc))))


;; ;;----------------------------------------------------------------------------------------------------
;; (defun dired-emms-add-file ()
;;   "Adds the current file or directory to the EMMS playing list."
;;   (interactive)
;;   (let ((fname (dired-get-filename)))
;;     (if (string= (downcase (substring fname -4 nil)) ".m3u")
;;         (emms-add-m3u-playlist fname)
;;         (emms-add-file fname))))



;; UTF-8环境下正确显示采用gbk编码的id3

;; Emms 调用程序 mp3info 在 emacs 中显示 mp3 的 id3 信息，但现在中文 mp3 歌曲的 id3 信息一般以 gbk 来编码，这在 utf-8 环
;; 境中会出现乱码。解决方法是用 iconv 将 mp3info 的输出先转为 utf-8，然后再交给 emms。
;; 创建 mp3info，内容如下:

;; #!/bin/sh
;; /usr/bin/mp3info "$@" | iconv -f gbk -t utf-8

;; 然后告诉 emms 使用新建立的 mp3info：
;; (setq emms-info-mp3info-program-name "~/bin/mp3info.sh")

;;;----------------------------------------------------------------------------------------------------
;; (defun tl/emms-lyrics-visit-lyric ()
;;   "Visit playing track's lyric file.
;; If we can't find it from local disk, then search it from internet."
;;   (interactive)
;;   (let* ((track (emms-playlist-current-selected-track))
;;          (name (emms-track-get track 'name))
;;          (lrc (funcall emms-lyrics-find-lyric-function
;;                        (emms-replace-regexp-in-string
;;                         (concat "\\." (file-name-extension name) "\\'")
;;                         ".lrc"
;;                         (file-name-nondirectory name)))))
;;     (if (and lrc (file-exists-p lrc) (not (string= lrc "")))
;;         (find-file lrc)
;;       (message "lyric file does not exist, search it from internet...")
;;       (let ((title (emms-track-get track 'title))
;;             (filename (file-name-sans-extension
;;                        (file-name-nondirectory name)))
;;             (url ""))
;;         (unless title
;;           (setq title filename))
;;         (cond ((string-match "\\cc" title) ; chinese lyrics
;;                ;; Since tag info might be encoded using various coding
;;                ;; systems, we'd better fall back on filename.
;;                (setq url (format
;;                           "http://mp3.baidu.com/m?f=ms&rn=10&tn=baidump3lyric&ct=150994944&word=%s&lm=-1"
;;                           (emms-lyrics-url-quote-plus
;;                            ;; (encode-coding-string filename 'gb2312)
;;                            (emms-i18n-iconv 'utf-8 'cp936 title)))))
;;               (t                      ; english lyrics
;;                (setq url (format "http://search.lyrics.astraweb.com/?word=%s"
;;                                  ;;"http://www.lyrics007.com/cgi-bin/s.cgi?q="
;;                                  (emms-lyrics-url-quote-plus title)))))
;;         (browse-url url)
;;         (message "lyric file does not exist, search it from internet...done")))))

(use-package emms-browser
  :after (emms)
  :init
  (progn
    ;; (put 'emms-browser-delete-files 'disabled nil)
    (setq emms-browser-info-year-format      "%i+ %n"
          emms-browser-info-genre-format     "%i+ %n"
          emms-browser-info-performer-format "%i+ %n"
          emms-browser-info-composer-format  "%i+ %n"
          emms-browser-info-artist-format    "%i* %n" ; ●
          emms-browser-info-album-format     "%i- %n" ; ◎
          emms-browser-info-title-format     "%i♪ %n"
          emms-browser-playlist-info-year-format      "%i+ %n"
          emms-browser-playlist-info-genre-format     "%i+ %n"
          emms-browser-playlist-info-performer-format "%i+ %n"
          emms-browser-playlist-info-composer-format  "%i+ %n"
          emms-browser-playlist-info-artist-format    "%i* %n"
          emms-browser-playlist-info-album-format     "%i- %n"
          emms-browser-playlist-info-title-format     "%i♪ %n"))
  :config
  (progn
    (emms-browser-make-filter "EVERYTHING" 'ignore)
    (emms-browser-make-filter "ALL-FILES"
                              (emms-browser-filter-only-type 'file))
    (emms-browser-make-filter "LAST-WEEK"
                              (emms-browser-filter-only-recent 7))
    (emms-browser-make-filter "LAST-3 MONTHS" (emms-browser-filter-only-recent 90))
    (emms-browser-make-filter "LAST-MONTH" (emms-browser-filter-only-recent 30))
    (emms-browser-make-filter "LAST-WEEK" (emms-browser-filter-only-recent 7))

    (emms-browser-make-filter "LAST-MONTH-NOT-PLAYED"
                              (lambda (track)
                                (not (funcall (emms-browser-filter-only-recent 30) track))))
    (emms-browser-make-filter "CLASSICAL" (emms-browser-filter-only-dir "~/media/mp3/classical"))
    (emms-browser-make-filter "PENDING" (emms-browser-filter-only-dir "~/media/mp3/pending"))
    (emms-browser-set-filter (assoc "EVERYTHING" emms-browser-filters))

    (add-hook 'emms-browser-filter-changed-hook 'tl/emms-browser-filter-changed)
    (defun tl/emms-browser-filter-changed ()
      (interactive)
      (if (string= emms-browser-current-filter-name "PENDING")
          (setq emms-browser-get-track-field-function 'emms-browser-get-track-field-simple)
        (setq emms-browser-get-track-field-function 'emms-browser-get-track-field-use-directory-name)))))


;; (define-key emms-browser-mode-map (kbd "W W") 'emms-browser-lookup-multi)
;; (defun emms-browser-lookup-multi ()
;;   (interactive)
;;   (emms-browser-lookup-wikipedia 'info-artist)
;;   (emms-browser-lookup-wikipedia 'info-album)
;;   (emms-browser-lookup-pitchfork 'info-artist))

;; set up a default cover
;; (setq emms-browser-default-covers
;;       (list "/home/resolve/misc/cover_small.jpg" nil nil))


;;; mp3 crawler from http://mp3.baidu.com
;; (require 'wget nil t)
;; (defun tl/mp3-crawler (title)
;;   "Download mp3 with TITLE from http://mp3.baidu.com."
;;   (interactive "sTitle: ")
;;   (let* ((urlencoded-title (emms-url-quote-plus
;;                             (emms-i18n-iconv 'utf-8 'gbk title)))
;;          (url1 (concat "http://mp3.baidu.com/m?f=ms&rn=&tn=baidump3&ct=134217728&word="
;;                        urlencoded-title
;;                        "&lm=0")))
;;     (url-retrieve url1 'tl/mp3-crawler-url1-callback (list title))))

;; (defun tl/mp3-crawler-url1-callback (status title)
;;   (let (url2)
;;     (goto-char (point-min))
;;     (search-forward "<td class=tdn>" nil t 1)
;;     (re-search-forward "href=\"\\([^\"]+\\)\"" nil t 1)
;;     (setq url2 (replace-regexp-in-string
;;                 "\\cc"                  ; This is baidu's trick, we can simply ignore non-ascii texts.
;;                 ""
;;                 (emms-i18n-iconv 'gbk 'utf-8 (match-string 1))))
;;     ;; hexify/urlencode reserved characters
;;     (mapc (lambda (i)
;;             (setq url2 (replace-regexp-in-string
;;                         i (url-hexify-string i) url2)))
;;           '(";" " "))
;;     (url-retrieve url2 'tl/mp3-crawler-url2-callback (list title))
;;     (kill-buffer (current-buffer))))

;; (defun tl/mp3-crawler-url2-callback (status title)
;;   (let (url3)
;;     (goto-char (point-min))
;;     (search-forward "<li class=\"li\" style=\"margin-right:10px;\">" nil t 1)
;;     (re-search-forward "href=\"\\([^\"]+\\)\"" nil t 1)
;;     (setq url3 (match-string 1))
;;     (let ((wget-default-options
;;            (append wget-default-options
;;                    (list "-O" (concat (replace-regexp-in-string " " "_" title)
;;                                       ".mp3")))))
;;       (wget url3))
;;     (kill-buffer (current-buffer))))

;; Tag editor
;; Rename the file corresponding to track at point
;; (setq emms-tag-editor-rename-format "%a - %t") ; default "%a - %l - %n - %t"
;; (define-key emms-playlist-mode-map (kbd "E") 'emms-tag-editor-edit)

;; (defun emms-playlist-mode-copy-filename-as-kill ()
;;   "Like `dired-copy-filename-as-kill'."
;;   (interactive)
;;   (save-window-excursion
;;     (emms-playlist-mode-jump)
;;     (dired-copy-filename-as-kill 0)))

;; (define-key emms-playlist-mode-map (kbd "w") 'emms-playlist-mode-copy-filename-as-kill)

;; (define-key emms-playlist-mode-map (kbd "M-r") 'emms-tag-editor-retag)
;; (defun emms-tag-editor-retag ()
;;   "fill the tags from the filename accoring to the `emms-tag-editor-rename-format'."
;;   (interactive)
;;   (if (emms-mark-has-markedp)
;;       (emms-tag-editor-retag-marked-tracks)
;;       (emms-tag-editor-retag-track (emms-tag-editor-track-at))))

;; (defun emms-tag-editor-retag-track (track)
;;   (when (eq (emms-track-get track 'type) 'file)
;;     (let* ((regexp "\\(.*\\) - \\([^/]+\\)\\.[^.]+$")
;;            (filename (emms-track-name track))
;;            (file-basename (file-name-nondirectory filename)))
;;       (if (string-match regexp file-basename)
;;           (let ((artist (match-string 1 file-basename))
;;                 (title (match-string 2 file-basename)))
;;             (if (and (string< "" artist) (string< "" title)
;;                      (emms-track-set track 'info-artist artist)
;;                      (emms-track-set track 'info-title title)
;;                      (emms-track-set track 'tag-modified t))
;;                 (progn (emms-tag-editor-erase-buffer emms-tag-editor-log-buffer)
;;                        (emms-tag-editor-apply (list track))
;;                        (message "Seting info-artist and info-title of file \"%s\" to \"%s\" and \"%s\"...done"
;;                                 file-basename artist title)
;;                        (run-at-time 1 nil
;;                                     'delete-window (get-buffer-window emms-tag-editor-log-buffer)))))))))

;; (defun emms-tag-editor-retag-marked-tracks ()
;;   (let ((tracks (emms-mark-mapcar-marked-track
;;                  'emms-tag-editor-track-at t)))
;;     (if (null tracks)
;;         (message "No track marked!")
;;         (dolist (track tracks)
;;           (emms-tag-editor-retag-track track)))))

;; default use `mp3info' to edit id3v1, use `id3v2' or `mid3v2' instead.
;; brew info install mp3info id3v2
(setq emms-tag-editor-tagfile-functions
      '(("mp3" "id3v2"
         ((info-artist . "a")
          (info-title . "t")
          (info-album . "A")
          (info-tracknumber . "T")
          (info-year . "y")
          (info-genre . "g")
          (info-note . "c")))
        ("ogg" . emms-tag-editor-tag-ogg)
        ("flac" . emms-tag-editor-tag-flac)))


;;; evil integrate
(defun tl/evil-emms-playlist-mode-insert-newline-above ()
  "Insert a newline above point."
  (interactive)
  (emms-with-inhibit-read-only-t
   (evil-insert-newline-above)))

(defun tl/evil-emms-playlist-mode-insert-newline-below ()
  "Insert a newline below point."
  (interactive)
  (emms-with-inhibit-read-only-t
   (evil-insert-newline-below)))

(defun tl/evil-emms-playlist-mode-paste-before ()
  "Pastes the latest yanked playlist items before the cursor position.
The return value is the yanked text."
  (interactive)
  (emms-with-inhibit-read-only-t
   (goto-char (point-at-bol))
   (yank)
   (emms-playlist-mode-correct-previous-yank)
   (evil-previous-line)
   (evil-beginning-of-line)))

(defun tl/evil-emms-playlist-mode-paste-after ()
  "Pastes the latest yanked playlist items behind point.
The return value is the yanked text."
  (interactive)
  (evil-next-line)
  (tl/evil-emms-playlist-mode-paste-before))

(general-define-key
 :states 'normal
 :keymaps 'emms-browser-mode-map
 "x" 'emms-pause
 "X" 'emms-stop
 "r" 'emms-random
 "<" 'emms-seek-backward
 ">" 'emms-seek-forward
 (kbd "<return>") 'emms-browser-add-tracks
 (kbd "C-<return>") 'emms-browser-add-tracks-and-play

 ;; volume controls
 "+" 'emms-volume-raise
 "=" 'emms-volume-raise
 "-" 'emms-volume-lower

 "u" 'emms-playlist-mode-undo

 ;; motion
 "[" 'emms-browser-prev-non-track
 "]" 'emms-browser-next-non-track
 (kbd "C-k") 'emms-browser-prev-non-track
 (kbd "C-j") 'emms-browser-next-non-track
 "gk" 'emms-browser-prev-non-track
 "gj" 'emms-browser-next-non-track

 (kbd "<tab>") 'emms-browser-toggle-subitems-recursively
 (kbd "<backtab>") 'emms-browser-toggle-subitems-recursively
 "^" 'emms-browser-move-up-level
 (kbd "SPC") 'emms-browser-toggle-subitems
 "g1" 'emms-browser-collapse-all
 "g2" 'emms-browser-expand-to-level-2
 "g3" 'emms-browser-expand-to-level-3
 "g4" 'emms-browser-expand-to-level-4
 "g0" 'emms-browser-expand-all
 "ga" 'emms-browse-by-artist
 "gA" 'emms-browse-by-album
 "gb" 'emms-browse-by-genre
 "gy" 'emms-browse-by-year
 "gc" 'emms-browse-by-composer
 "gp" 'emms-browse-by-performer
 "zm" 'emms-browser-collapse-all
 "zr" 'emms-browser-expand-all
 "zo" 'emms-browser-expand-one-level
 ;; TODO find a real replacement for zc
 "zc" 'emms-browser-collapse-all

 ;; TODO find a way to integrate this with evil-collection-evil-search
 "/" 'emms-isearch-buffer ; This shows hidden items during search.
 "n" 'isearch-repeat-forward
 "N" 'isearch-repeat-backward

 ;; filter
 "fj" 'emms-browser-previous-filter
 "fk" 'emms-browser-next-filter
 "fa" 'emms-browser-show-EVERYTHING

 "s" (lookup-key emms-browser-mode-map (kbd "s"))
 "g" (lookup-key emms-browser-mode-map (kbd "W")) ;; TODO: This overrides other "g-" prefixed keys.

 "C" 'emms-browser-clear-playlist
 "D" 'emms-browser-delete-files
 "gd" 'emms-browser-view-in-dired
 "d" 'emms-browser-view-in-dired)

(general-define-key
 :states 'normal
 :keymaps 'emms-playlist-mode-map
 ;; playback controls
 "x" 'emms-pause
 "X" 'emms-stop
 "r" 'emms-random
 ;; "r" 'emms-toggle-repeat-track
 ;; "R" 'emms-toggle-repeat-playlist
 "<" 'emms-seek-backward
 ">" 'emms-seek-forward
 (kbd "C-j") 'emms-next
 (kbd "C-k") 'emms-previous
 "gj" 'emms-next
 "gk" 'emms-previous
 (kbd "<return>") 'emms-playlist-mode-play-smart

 "m" 'emms-playlist-mode-mark
 "U" 'emms-playlist-mode-unmark

 ;; volume controls
 "+" 'emms-volume-raise
 "=" 'emms-volume-raise
 "-" 'emms-volume-lower

 "L"  'emms-player-simple-mpv-ab-loop

 ;; score
 "Su" 'emms-score-up-file-on-line
 "Sd" 'emms-score-down-file-on-line
 "So" 'emms-score-show-file-on-line
 "Sl" 'emms-score-less-tolerant
 "Sm" 'emms-score-more-tolerant
 "St" 'emms-score-set-tolerance
 "Ss" 'emms-score-show-playing

 ;; motion
 "gg" 'emms-playlist-mode-first
 "G" 'emms-playlist-mode-last
 "]" 'emms-playlist-mode-next
 "[" 'emms-playlist-mode-previous

 "D" 'emms-playlist-mode-kill-track  ; emms-browser uses "D"
 "C" 'emms-playlist-mode-clear
 "O" 'tl/evil-emms-playlist-mode-insert-newline-above
 "o" 'tl/evil-emms-playlist-mode-insert-newline-below
 "P" 'tl/evil-emms-playlist-mode-paste-before
 "p" 'tl/evil-emms-playlist-mode-paste-after

 "u" 'emms-playlist-mode-undo

 "ze" 'emms-tag-editor-edit
 "R" 'emms-tag-editor-rename

 "." 'emms-playlist-mode-center-current
 "d" 'emms-playlist-mode-goto-dired-at-point
 "gd" 'emms-playlist-mode-goto-dired-at-point ; "d" does the same, keep "gd" for consistency.

 "zs" 'emms-show
 "a" 'emms-playlist-mode-add-contents
 "zp" 'emms-playlist-set-playlist-buffer

 ;; filter
 "S" (lookup-key emms-playlist-mode-map (kbd "S"))
 "s" (lookup-key emms-playlist-mode-map (kbd "/"))
 ;; "" 'emms-playlist-limit-to-all ; TODO: Test.

 (kbd "M-y") 'emms-playlist-mode-yank-pop)


(general-define-key
 :states 'visual
 :keymaps 'emms-playlist-mode-map
 ;; "d" 'emms-playlist-mode-kill
 "D" 'emms-playlist-mode-kill)

(general-define-key
 :states 'normal
 :keymaps 'emms-browser-search-mode-map
 "q" 'emms-browser-kill-search)

(general-define-key
 :states 'normal
 :keymaps 'emms-metaplaylist-mode-map
 (kbd "<return>") 'emms-metaplaylist-mode-goto-current
 (kbd "<space>") 'emms-metaplaylist-mode-set-active
 "gr" 'emms-metaplaylist-mode-update
 "C" 'emms-metaplaylist-mode-new-buffer
 "." 'emms-metaplaylist-mode-center-current
 "D" 'emms-metaplaylist-mode-kill-buffer
 "q" 'kill-this-buffer)

(tl/set-leader-keys
  "oex"   'emms-start
  "oeh"   'emms-history-load
  "oeX"   'emms-stop
  "oen"   'emms-next
  "oep"   'emms-previous
  "oeo"   'emms-show
  "oeS"   'emms-shuffle
  "oe SPC" 'emms-pause
  "oea"   'emms-add-directory-tree

  "oee"   'emms-playlist-mode-go
  "oeb"   'emms-smart-browse
  "oeu"   'emms-score-up-playing
  "oed"   'emms-score-dwon-playing
  "oeo"   'emms-score-show-playing
  "oesp"  'emms-stream-popup
  "oess"   'emms-lastfm-radio-similar-artists
  "oek"   'emms-lastfm-radio-skip
  "oet"   'emms-play-directory-tree
  "oer"   'emms-toggle-repeat-track
  "oeR"   'emms-toggle-repeat-playlist
  "oem"   'emms-lyrics-toggle-display-on-minibuffer
  "oeM"   'emms-lyrics-toggle-display-on-modeline
  "oel"   'emms-lyrics-visit-lyric

  "oed" 'tl/playlist-mode-delete-track-at

  "oef" 'emms-play-file
  "oef" 'emms-play-playlist
  "oeF" '(lambda ()
           (interactive)
           (emms-play-playlist "/mnt/d/mp3/playlist.m3u")))


(provide '52emms)
