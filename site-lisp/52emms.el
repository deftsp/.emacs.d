;; -*- mode: Emacs-Lisp -*-

;;;; EMMS

(require 'emms-setup)
(setq emms-mode-line-icon-color "DarkBlue") 
(setq emms-directory "~/.emacs.d/.emms")
(emms-devel)
(require 'emms-player-mpg321-remote)

(require 'emms-lyrics-download)
(emms-lyrics-download-toggle)

;; (emms-lastfm-enable)

(when (fboundp 'emms-cache)
  (emms-cache 1))
;; (when (fboundp 'emms-score)
;;   (emms-score 1))

;; define-emms-combined-source:Define a `emms-play-X' and `emms-add-X' function for SOURCES.
;; fixme: only add file  not  playlist.m3u
;; (define-emms-combined-source all nil
;;   '((emms-source-playlist-directory "/mnt/d/mp3")
;;     (emms-source-playlist-file "/mnt/d/mp3/playlist.m3u")))
;; (emms-add-all)

(setq emms-lyrics-display-format "%s"
      emms-playing-time-display-p nil
      emms-playing-time-display-format "%s"
      emms-mode-line-icon-before-format "["
      emms-mode-line-format " %s]"
      emms-volume-amixer-control "PCM" ; "Master"
      emms-history-start-playing nil)

;;; without music I can not live
;; (add-hook 'after-init-hook (lambda ()
;;                              (interactive)
;;                              (emms-history-load)))

;; connect
(emms-player-mpd-connect)
;; dump cache
(emms-cache-set-from-mpd-all)
;; (emms-cache-save)

(require 'emms-mode-line-icon)
;; display title only I redefine follow function in emms-mode-line-icon.el
(defun emms-mode-line-icon-function ()
  (concat " "
          emms-mode-line-icon-before-format
          (propertize "NP:" 'display emms-mode-line-icon-image-cache)
          (format emms-mode-line-format (emms-track-get
                                         (emms-playlist-current-selected-track)
                                         'info-title))))
(setq emms-mode-line-mode-line-function 'emms-mode-line-icon-function)
(setq emms-mode-line-titlebar-function nil)

;; faces
(set-face-foreground 'emms-playlist-selected-face "magenta")
(set-face-foreground 'emms-playlist-track-face  "SteelBlue2")

(when (eq system-type 'gnu/linux)
  (setq emms-source-file-gnu-find "/usr/bin/find"))

;; use faster finding facility if you have GNU find
(when (file-exists-p emms-source-file-gnu-find)
  (setq emms-source-file-directory-tree-function
        'emms-source-file-directory-tree-find))

;; (add-hook 'emms-player-started-hook 'emms-show)

(setq emms-show-format "NP: %s"
      emms-repeat-playlist t)

(setq emms-playlist-buffer-name "*EMMS Playlist*"
      emms-playlist-mode-open-playlists t
      emms-playlist-mode-window-width  30
      emms-source-playlist-default-format 'm3u)

(setq emms-player-mpd-server-name "localhost"
      emms-player-mpd-server-port "6600"
      emms-player-mpd-music-directory "/home/deftsp/media/mp3"
      ;; emms-player-mpd-server-password "mypassword"
      ;; emms-volume-change-function 'emms-volume-mpd-change
      emms-player-mpd-sync-playlist t
      emms-player-mpd-verbose nil)

(setq emms-player-mplayer-command-name "mplayer"
      emms-player-mpg321-command-name "mpg123"
      emms-player-mplayer-parameters (append emms-player-mplayer-parameters '("-subcp" "cp936"))
      emms-player-list '(emms-player-mpd
                         emms-player-mpg321-remote
                         emms-player-mplayer
                         emms-player-mplayer-playlist
                         emms-player-mpg321
                         emms-player-ogg123))
;; fixme! Set "emms-lyrics-dir" will cause tramp ...???
;; (setq emms-lyrics-dir "/mnt/d/Program\ Files/TTPlayer/Lyrics")

(setq emms-playlist-buffer-name "*EMMS Playlist*"
      emms-source-file-default-directory "/home/deftsp/media/mp3/"
      emms-score-file "~/.emacs.d/.emms/scores"
      emms-cache-file "~/.emacs.d/.emms/emms-cache"
      emms-history-file "~/.emacs.d/.emms/emms-history"
      emms-stream-bookmarks-file "~/.emacs.d/.emms/emms-streams"
      emms-playing-time-style 'bar      ; 'time
      ;; emms-score-enabled-p t
      later-do-interval 0.0001
      emms-info-asynchronously nil)
(setq emms-lyrics-scroll-p nil
      emms-lyrics-display-on-modeline t)
;; coding
(setq emms-lyrics-coding-system nil
      ;; emms-info-mp3info-coding-system 'utf-8
      ;; used for decode and encode
      emms-i18n-default-coding-system '(utf-8 . utf-8)
      emms-cache-file-coding-system 'utf-8)

(add-hook 'emms-playlist-limit-hook 'emms-playlist-sort-by-natural-order)
;; Randomize track selection
;;(add-hook 'emms-player-finished-hook 'emms-random)
;;; Track Show Format(in emms-playlist-mode)

(setq emms-last-played-format-alist
      '(((emms-last-played-seconds-today) . "%a %H:%M")
        (604800                           . "%a %H:%M") ; this week
        ((emms-last-played-seconds-month) . "%d")
        ((emms-last-played-seconds-year)  . "%m-%d")
        (t                                . "%Y-%m-%d")))

(defun tsp-upcase-initials (string)
  "Do `upcase-initials' on STRING, but do not uppercase letters
that come after quote characters."
  (with-temp-buffer
    (insert (upcase-initials string))
    (goto-char (point-min))
    (while (re-search-forward "['`]\\([[:upper:]]\\)" nil t)
      (downcase-region (match-beginning 1) (match-end 1)))
    (buffer-string)))

(defun prettyfy-string (string &optional after)
  "Strip starting and ending whitespace and replace any chars
after AFTER with '...'"
  (let ((replace-map (list
                      (cons "^[ \t]*" "")
                      (cons "[ \t]*$" "")
                      (cons (concat "^\\(.\\{"
                                    (or (number-to-string after) "10")
                                    "\\}\\).*")
                            "\\1…"))))
    (dolist (replace replace-map)
      (when (string-match (car replace) string)
        (setq string (replace-match (cdr replace) nil nil string))))
    string))

(eval-after-load 'emms
  '(progn
    (setq tsp-emms-playlist-last-track nil)
    (setq tsp-emms-playlist-last-indent "\\")

    (defun tsp-emms-track-description-function (track)
      "Return a description of the current track."
      (let* ((name (emms-track-name track))
             (type (emms-track-type track))
             (short-name (file-name-nondirectory name))
             (play-count (or (emms-track-get track 'play-count) 0))
             (last-played (or (emms-track-get track 'last-played) '(0 0 0)))
             (empty "..."))
        (prog1
            (case (emms-track-type track)
              ((file url)
               (let* ((artist (or (emms-track-get track 'info-artist) empty))
                      (year (emms-track-get track 'info-year))
                      (playing-time (or (emms-track-get track 'info-playing-time) 0))
                      (min (/ playing-time 60))
                      (sec (% playing-time 60))
                      (album (or (emms-track-get track 'info-album) empty))
                      (tracknumber (emms-track-get track 'info-tracknumber))
                      (short-name (file-name-sans-extension
                                   (file-name-nondirectory name)))
                      (title (or (emms-track-get track 'info-title) short-name))

                      ;; last track
                      (ltrack tsp-emms-playlist-last-track)
                      (lartist (or (and ltrack (emms-track-get ltrack 'info-artist))
                                  empty))
                      (lalbum (or (and ltrack (emms-track-get ltrack 'info-album))
                                 empty))

                      (same-album-p (and (not (string= lalbum empty))
                                       (string= album lalbum))))
                 (format "%3d| %-16s%-46s%-46s%-12s%s %10s"
                         play-count
                         ;;                           (if (and (not (string= lartist empty))
                         ;;                                    (string= artist lartist))
                         ;;                               empty
                         ;;                             artist)
                         (prettyfy-string artist 16)

                         ;; Combine indention, tracknumber, title.
                         ;; (format "%s%s%-40s"
                         (prettyfy-string (concat
                                           (if same-album-p ; indention by album
                                               (setq tsp-emms-playlist-last-indent
                                                     (concat " " tsp-emms-playlist-last-indent))
                                               (setq tsp-emms-playlist-last-indent "\\")
                                               "")
                                           (if (and tracknumber ; tracknumber
                                                  (not (zerop (string-to-number tracknumber))))
                                               (format "%02d." (string-to-number tracknumber))
                                               "")
                                           title) 46)

                         ;; album
                         (prettyfy-string (cond ((string= album empty) empty)
                                                ;; (same-album-p "  ")
                                                (t (concat "[" album "]"))) 46)

                         (or year empty)
                         (if (or (> min 0)  (> sec 0))
                             (format "%02d:%02d" min sec)
                             empty)
                         (emms-last-played-format-date last-played))))
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
          (setq tsp-emms-playlist-last-track track))))
    (setq emms-track-description-function
     'tsp-emms-track-description-function)))


;;To get track information from MusicPD, invoke the following:
;; (add-to-list 'emms-info-functions 'emms-info-mpd)
;; `mp3info' Only supported ID3 versions 1.0 and 1.1.
;; (add-to-list 'emms-info-functions 'emms-info-mp3info)

;;; Libtag support
;; cd ~/.emacs.d/packages/emms
;; make emms-print-metadata
;; cd ~/bin && ln -s ~/.emacs.d/packages/emms/emms-print-metadata
(require 'emms-info-libtag)
(add-to-list 'emms-info-functions 'emms-info-libtag)
(setq emms-info-libtag-coding-system 'utf-8)

;; Switch to the radio buffer
(defun my-emms-streams ()
  (interactive)
  (let ((buf (get-buffer emms-stream-buffer-name)))
    (if buf
        (switch-to-buffer buf)
        (emms-streams))))

;; keybinding
;; Rebind sort map prefix key
(add-hook 'emms-playlist-mode-hook
          '(lambda ()
            (local-unset-key "s")
            (setq emms-playlist-sort-prefix "s")
            (emms-playlist-sort-map-setup)))

;; global key-map
;; (global-set-key (kbd "s-m p")
;;                 (lambda ()
;;                   (interactive)
;;                   (if emms-playlist-buffer
;;                       (emms-playlist-mode-go)
;;                       (message "EMMS not started"))))

;; (global-set-key (kbd "s-m l")
;;                 (lambda ()
;;                   (interactive)
;;                   (if emms-playlist-buffer
;;                       (emms-playlist-mode-go-popup)
;;                       (message "EMMS not started"))))

;; (defun tsp-emms-playlist-mode-hook ()
;;   (toggle-truncate-lines 1))
;; (add-hook 'emms-playlist-mode-hook 'tsp-emms-playlist-mode-hook)


;; (global-set-key (kbd "s-m s") 'emms-stream-popup)

(global-set-key (kbd "C-c e s")   'emms-lastfm-radio-similar-artists)
(global-set-key (kbd "C-c e k")   'emms-lastfm-radio-skip)
(global-set-key (kbd "C-c e t") 'emms-play-directory-tree)
(global-set-key (kbd "C-c e x") 'emms-start)
(global-set-key (kbd "C-c e X") 'emms-history-load)
(global-set-key (kbd "C-c e v") 'emms-stop)
(global-set-key (kbd "C-c e n") 'emms-next)
(global-set-key (kbd "C-c e p") 'emms-previous)
(global-set-key (kbd "C-c e o") 'emms-show)
(global-set-key (kbd "C-c e h") 'emms-shuffle)
(global-set-key (kbd "C-c e SPC") 'emms-pause)
(global-set-key (kbd "C-c e a") 'emms-add-directory-tree)

(global-set-key (kbd "C-c e r") 'emms-toggle-repeat-track)
(global-set-key (kbd "C-c e R") 'emms-toggle-repeat-playlist)
(global-set-key (kbd "C-c e m") 'emms-lyrics-toggle-display-on-minibuffer)
(global-set-key (kbd "C-c e M") 'emms-lyrics-toggle-display-on-modeline)
(global-set-key (kbd "C-c e l") 'emms-lyrics-visit-lyric)

(global-set-key (kbd "C-c e b") 'emms-smart-browse)

(global-set-key (kbd "C-c e d") 'tsp-playlist-mode-delete-track-at)

(global-set-key (kbd "C-c e <left>")  (lambda () (interactive) (emms-seek -10)))
(global-set-key (kbd "C-c e <right>") (lambda () (interactive) (emms-seek +10)))
(global-set-key (kbd "C-c e <down>")  (lambda () (interactive) (emms-seek -60)))
(global-set-key (kbd "C-c e <up>")    (lambda () (interactive) (emms-seek +60)))

;; (global-set-key (kbd "C-c e s u") 'emms-score-up-playing)
;; (global-set-key (kbd "C-c e s d") 'emms-score-down-playing)
;; (global-set-key (kbd "C-c e s o") 'emms-score-show-playing)

(global-set-key (kbd "C-c e e") 'emms-play-file)
(global-set-key (kbd "C-c e f") 'emms-play-playlist)
(global-set-key (kbd "C-c e F") '(lambda ()
                                  (interactive)
                                  (emms-play-playlist "/mnt/d/mp3/playlist.m3u")))

;; playlist-mode-map

(define-key emms-playlist-mode-map (kbd "x") 'emms-start)
(define-key emms-playlist-mode-map (kbd "v") 'emms-stop)
(define-key emms-playlist-mode-map (kbd "h") 'emms-shuffle)
(define-key emms-playlist-mode-map (kbd "o") 'emms-show)
(define-key emms-playlist-mode-map (kbd "F") 'emms-playlist-show-current-line)
(define-key emms-playlist-mode-map (kbd "SPC") 'emms-pause)
(define-key emms-playlist-mode-map (kbd "r") 'emms-toggle-repeat-track)
;; (define-key emms-playlist-mode-map (kbd "R") 'emms-toggle-repeat-playlist)
(define-key emms-playlist-mode-map (kbd "<left>")  (lambda () (interactive) (emms-seek -10)))
(define-key emms-playlist-mode-map (kbd "<right>") (lambda () (interactive) (emms-seek +10)))
(define-key emms-playlist-mode-map (kbd "<down>")  (lambda () (interactive) (emms-seek -60)))
(define-key emms-playlist-mode-map (kbd "<up>")    (lambda () (interactive) (emms-seek +60)))

(define-key emms-playlist-mode-map (kbd "N") 'emms-next)
(define-key emms-playlist-mode-map (kbd "P") 'emms-previous)
(define-key emms-playlist-mode-map (kbd "E") 'emms-tag-editor-edit)
(define-key emms-playlist-mode-map (kbd "d") 'emms-playlist-mode-kill-track)
(define-key emms-playlist-mode-map (kbd "D") 'tsp-playlist-mode-delete-track-at)

(define-key emms-playlist-mode-map (kbd "n") 'next-line)
(define-key emms-playlist-mode-map (kbd "p") 'previous-line)
(define-key emms-playlist-mode-map (kbd "q") 'emms-playlist-mode-bury-buffer)
;; (define-key emms-playlist-mode-map (kbd "D") 'emms-playlist-mode-delete)
;; (define-key emms-playlist-mode-map (kbd "m") 'emms-playlist-mode-mark)
;; (define-key emms-playlist-mode-map (kbd "u") 'emms-playlist-mode-unmark)

(define-key emms-playlist-mode-map (kbd "0") 'emms-volume-raise)
(define-key emms-playlist-mode-map (kbd "9") 'emms-volume-lower)
;; (global-set-key (kbd "H-0") 'emms-volume-raise)
;; (global-set-key (kbd "H-9") 'emms-volume-lower)

(define-key emms-playlist-mode-map  (kbd "c") 'emms-playlist-current-clear)
(define-key emms-playlist-mode-map  (kbd "C-l") 'emms-playlist-mode-center-current)


;; (define-key emms-playlist-mode-map (kbd "S u") 'emms-score-up-file-on-line)
;; (define-key emms-playlist-mode-map (kbd "S d") 'emms-score-down-file-on-line)
;; (define-key emms-playlist-mode-map (kbd "S o") 'emms-score-show-file-on-line)
;; (define-key emms-playlist-mode-map (kbd "S l") 'emms-score-less-tolerant)
;; (define-key emms-playlist-mode-map (kbd "S m") 'emms-score-more-tolerant)
;; (define-key emms-playlist-mode-map (kbd "S t") 'emms-score-set-tolerance)
;; (define-key emms-playlist-mode-map (kbd "S s") 'emms-score-show-playing)
;; (setq emms-playlist-sort-prefix "S")
;; (define-key emms-playlist-mode-map (kbd "/") 'tsp-search)

;; (defun tsp-search ()
;;   (interactive)
;;   (goto-char (point-min))
;;   (call-interactively 'isearch-forward))

(defun emms-playlist-mode-jump ()
  "Jump to the directory of track at point in `emms-playlist-buffer'."
  (interactive)
  (let ((name
         (emms-track-get (emms-playlist-track-at) 'name)))
    (dired (file-name-directory name))
    (goto-char (point-min))
    (dired-search-forward (file-name-nondirectory name) nil t)))

(define-key emms-playlist-mode-map (kbd "C-x C-j") 'emms-playlist-mode-jump)
;; (define-key emms-playlist-mode-map (kbd "C-x C-j") 'emms-playlist-mode-goto-dired-at-point)

;;; Misc

(defun tsp-playlist-mode-delete-track-at ()
  "Delete the track at point in emms-playlist buffer"
  (interactive)
  (if (emms-playlist-ensure-playlist-buffer)
      (widen)
      (let* ((file-name (emms-track-get
                         (emms-playlist-track-at) 'name))
             (file-name-lyric (emms-replace-regexp-in-string
                               (concat "\\." (file-name-extension file-name) "\\'")
                               ".lrc"
                               file-name)))
        (if file-name
            (when (y-or-n-p (concat "Delete \"" file-name "\" from Disk? "))
              (emms-with-inhibit-read-only-t
               (save-excursion
                 (let ((kill-whole-line t))
                   (goto-char (point-at-bol))
                   (kill-line)
                   (delete-file file-name)
                   (if (file-exists-p file-name-lyric)
                       (delete-file file-name-lyric))))
               (emms-playlist-mode-play-smart)))))))


(defun emms-google-for-lyrics ()
  (interactive)
  (browse-url
   (concat "http://www.google.com/search?q="
           (replace-regexp-in-string " +" "+"
                                     (concat "lyrics "
                                             (delete ?- (emms-track-description
                                                         (emms-playlist-current-selected-track))))))))


;; play what i mean
(defun emms-pwim (truc &optional rien)
  "Plays the TRUC specified, whatever it is. The function tries to
guess the type of TRUC, between playlist, directory containing
playable tracks, and files. If the directory does not contain
playable tracks, but some sub-directories, it will play the
tree."
  (interactive
   (find-file-read-args "Play what ? " t))
  (cond
    ((file-exists-p truc)    (emms-play-file truc))
    ((file-directory-p truc) (emms-play-directory truc))))

;; add what i mean
(defun emms-awim (truc &optional rien)
  "Adds the TRUC specified, whatever it is. The function tries to
guess the type of TRUC, between playlist, directory containing
playable tracks, and files. If the directory does not contain
playable tracks, but some sub-directories, it will add the
tree."
  (interactive
   (find-file-read-args "Add what ? " t))
  (cond
    ((file-exists-p truc)    (emms-add-file truc))
    ((file-directory-p truc) (emms-add-directory truc))))




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
(defun tsp-emms-lyrics-visit-lyric ()
  "Visit playing track's lyric file.
If we can't find it from local disk, then search it from internet."
  (interactive)
  (let* ((track (emms-playlist-current-selected-track))
         (name (emms-track-get track 'name))
         (lrc (funcall emms-lyrics-find-lyric-function
                       (emms-replace-regexp-in-string
                        (concat "\\." (file-name-extension name) "\\'")
                        ".lrc"
                        (file-name-nondirectory name)))))
    (if (and lrc (file-exists-p lrc) (not (string= lrc "")))
        (find-file lrc)
        (message "lyric file does not exist, search it from internet...")
        (let ((title (emms-track-get track 'title))
              (filename (file-name-sans-extension
                         (file-name-nondirectory name)))
              (url ""))
          (unless title
            (setq title filename))
          (cond ((string-match "\\cc" title) ; chinese lyrics
                 ;; Since tag info might be encoded using various coding
                 ;; systems, we'd better fall back on filename.
                 (setq url (format
                            "http://mp3.baidu.com/m?f=ms&rn=10&tn=baidump3lyric&ct=150994944&word=%s&lm=-1"
                            (emms-lyrics-url-quote-plus
                             ;; (encode-coding-string filename 'gb2312)
                             (emms-i18n-iconv 'utf-8 'cp936 title)))))
                (t                      ; english lyrics
                 (setq url (format "http://search.lyrics.astraweb.com/?word=%s"
                                   ;;"http://www.lyrics007.com/cgi-bin/s.cgi?q="
                                   (emms-lyrics-url-quote-plus title)))))
          (browse-url url)
          (message "lyric file does not exist, search it from internet...done")))))

;;; emms-browser

(put 'emms-browser-delete-files 'disabled nil)

(setq emms-browser-info-genre-format "%i● %n"
      emms-browser-info-artist-format "%i● %n"
      emms-browser-info-album-format "%i◎ %n"
      emms-browser-info-title-format "%i♪ %n")

;; (define-key emms-browser-mode-map (kbd "W W") 'emms-browser-lookup-multi)
;; (defun emms-browser-lookup-multi ()
;;   (interactive)
;;   (emms-browser-lookup-wikipedia 'info-artist)
;;   (emms-browser-lookup-wikipedia 'info-album)
;;   (emms-browser-lookup-pitchfork 'info-artist))

;; set up a default cover
;; (setq emms-browser-default-covers
;;       (list "/home/resolve/misc/cover_small.jpg" nil nil))

;; filters
(emms-browser-make-filter "all" 'ignore)
;; Set "all" as the default filter:
(emms-browser-set-filter (assoc "all" emms-browser-filters))
(emms-browser-make-filter "all-files" (emms-browser-filter-only-type 'file))

;; (emms-browser-make-filter "70s" (emms-browser-filter-only-dir "~/mp3s/70s"))
;; (emms-browser-make-filter "classical" (emms-browser-filter-only-dir "~/Mp3s/classical"))
(emms-browser-make-filter "mp3s" (emms-browser-filter-only-dir "/home/deftsp/media/mp3"))
;; (emms-browser-make-filter "pending" (emms-browser-filter-only-dir "~/Media/pending"))
(emms-browser-make-filter "not-played"
                          (lambda (track) (not (funcall (emms-browser-filter-only-recent 365) track))))
(emms-browser-make-filter "last-3months" (emms-browser-filter-only-recent 90))
(emms-browser-make-filter "last-month" (emms-browser-filter-only-recent 30))
(emms-browser-make-filter "last-week" (emms-browser-filter-only-recent 7))
;; (emms-browser-make-filter
;;  "all"
;;  (lambda (track)
;;    (or
;;     (funcall (emms-browser-filter-only-type 'file) track)
;;     ;; ignore the pending directory
;;     (not (funcall
;;         (emms-browser-filter-only-dir "~/Media/pending") track)))))

(add-hook 'emms-browser-filter-changed-hook 'de-filter-changed)
(defun de-filter-changed ()
  (interactive)
  (if (string= emms-browser-current-filter-name "pending")
      (setq emms-browser-get-track-field-function 'emms-browser-get-track-field-simple)
      (setq emms-browser-get-track-field-function 'emms-browser-get-track-field-use-directory-name)))

;;; mp3 crawler from http://mp3.baidu.com
(require 'wget nil t)
(defun tsp-mp3-crawler (title)
  "Download mp3 with TITLE from http://mp3.baidu.com."
  (interactive "sTitle: ")
  (let* ((urlencoded-title (emms-url-quote-plus
                            (emms-i18n-iconv 'utf-8 'gbk title)))
         (url1 (concat "http://mp3.baidu.com/m?f=ms&rn=&tn=baidump3&ct=134217728&word="
                       urlencoded-title
                       "&lm=0")))
    (url-retrieve url1 'tsp-mp3-crawler-url1-callback (list title))))

(defun tsp-mp3-crawler-url1-callback (status title)
  (let (url2)
    (goto-char (point-min))
    (search-forward "<td class=tdn>" nil t 1)
    (re-search-forward "href=\"\\([^\"]+\\)\"" nil t 1)
    (setq url2 (replace-regexp-in-string
                "\\cc"                  ; This is baidu's trick, we can simply ignore non-ascii texts.
                ""
                (emms-i18n-iconv 'gbk 'utf-8 (match-string 1))))
    ;; hexify/urlencode reserved characters
    (mapc (lambda (i)
            (setq url2 (replace-regexp-in-string
                        i (url-hexify-string i) url2)))
          '(";" " "))
    (url-retrieve url2 'tsp-mp3-crawler-url2-callback (list title))
    (kill-buffer (current-buffer))))

(defun tsp-mp3-crawler-url2-callback (status title)
  (let (url3)
    (goto-char (point-min))
    (search-forward "<li class=\"li\" style=\"margin-right:10px;\">" nil t 1)
    (re-search-forward "href=\"\\([^\"]+\\)\"" nil t 1)
    (setq url3 (match-string 1))
    (let ((wget-default-options
           (append wget-default-options
                   (list "-O" (concat (replace-regexp-in-string " " "_" title)
                                      ".mp3")))))
      (wget url3))
    (kill-buffer (current-buffer))))

;; Tag editor
;; Rename the file corresponding to track at point
(setq emms-tag-editor-rename-format "%a - %t") ; default "%a - %l - %n - %t"
(define-key emms-playlist-mode-map (kbd "E") 'emms-tag-editor-edit)

(defun emms-playlist-mode-copy-filename-as-kill ()
  "Like `dired-copy-filename-as-kill'."
  (interactive)
  (save-window-excursion
    (emms-playlist-mode-jump)
    (dired-copy-filename-as-kill 0)))

(define-key emms-playlist-mode-map (kbd "w") 'emms-playlist-mode-copy-filename-as-kill)

(define-key emms-playlist-mode-map (kbd "M-r") 'emms-tag-editor-retag)
(defun emms-tag-editor-retag ()
  "fill the tags from the filename accoring to the `emms-tag-editor-rename-format'."
  (interactive)
  (if (emms-mark-has-markedp)
      (emms-tag-editor-retag-marked-tracks)
      (emms-tag-editor-retag-track (emms-tag-editor-track-at))))

(defun emms-tag-editor-retag-track (track)
  (when (eq (emms-track-get track 'type) 'file)
    (let* ((regexp "\\(.*\\) - \\([^/]+\\)\\.[^.]+$")
           (filename (emms-track-name track))
           (file-basename (file-name-nondirectory filename)))
      (if (string-match regexp file-basename)
          (let ((artist (match-string 1 file-basename))
                (title (match-string 2 file-basename)))
            (if (and (string< "" artist) (string< "" title)
                     (emms-track-set track 'info-artist artist)
                     (emms-track-set track 'info-title title)
                     (emms-track-set track 'tag-modified t))
                (progn (emms-tag-editor-erase-buffer emms-tag-editor-log-buffer)
                       (emms-tag-editor-apply (list track))
                       (message "Seting info-artist and info-title of file \"%s\" to \"%s\" and \"%s\"...done"
                                file-basename artist title)
                       (run-at-time 1 nil
                                    'delete-window (get-buffer-window emms-tag-editor-log-buffer)))))))))

(defun emms-tag-editor-retag-marked-tracks ()
  (let ((tracks (emms-mark-mapcar-marked-track
                 'emms-tag-editor-track-at t)))
    (if (null tracks)
        (message "No track marked!")
        (dolist (track tracks)
          (emms-tag-editor-retag-track track)))))

;; default use `mp3info' to edit id3v1, use `id3v2' or `mid3v2' instead.
(setq emms-tag-editor-tagfile-functions '(("mp3" "mid3v2"
                                           ((info-artist . "a")
                                            (info-title . "t")
                                            (info-album . "A")
                                            (info-tracknumber . "T")
                                            (info-year . "y")
                                            (info-genre . "g")
                                            (info-note . "c")))
                                          ("ogg" . emms-tag-editor-tag-ogg)
                                          ("flac" . emms-tag-editor-tag-flac)))


;;; PATCH
;; emms-player-mplayer.el
;;add ape and flv support to mplayer
(define-emms-simple-player mplayer '(file url)
  (regexp-opt '(".ogg" ".mp3" ".wav" ".mpg" ".mpeg" ".wmv" ".wma"
                ".mov" ".avi" ".divx" ".ogm" ".asf" ".mkv" "http://" "mms://"
                ".rm" ".rmvb" ".mp4" ".flac" ".vob" ".m4a" ".ape" ".flv"))
  "Mplayer" "-Slave" "-Quiet" "-really-quiet")
