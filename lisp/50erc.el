;;;; 50erc.el


;;; modules
(eval-after-load "erc"
  '(progn
     (mapc #'(lambda (x) (add-to-list 'erc-modules x))
           '(services))
     (erc-update-modules)
     (erc-services-mode 1)))

(load "~/.emacs.d/.erc-pass")

;;; Variables
(setq erc-nick "deftsp"
      erc-nick-uniquifier "`"
      erc-email-userid "deftsp"
      erc-user-full-name user-full-name
      ;; Channel-specific prompts
      ;; This is what I use to provide channel-specific prompts (like #emacs> in the Emacs channel, and #php> in the PHP channel)
      ;; erc-prompt (lambda ()
      ;;              (if (and (boundp 'erc-default-recipients) (erc-default-target))
      ;;                  (erc-propertize (concat (erc-default-target) ">") 'read-only t 'rear-nonsticky t 'front-nonsticky t)
      ;;                (erc-propertize (concat "ERC›") 'read-only t 'rear-nonsticky t 'front-nonsticky t)))

      ;; erc-header-line-face-method t
      erc-timestamp-format "%Y-%m-%d %H:%M"
      ;; erc-pals '("johnsu01" "mwolson" "forcer")

      ;; erc-autoaway-message " autoaway after %i seconds idle. "
      ;; erc-notify-list '("johnsu01" "mwolson" "forcer")
      ;; erc-notify-interval 30

      ;; erc-log-insert-log-on-open nil
      ;; erc-log-channels t
      ;; erc-log-channels-directory "~/.erc/logs/"
      ;; erc-save-buffer-on-part t
      erc-track-visibility 'visible
      erc-hide-timestamps nil)


;;; identification
(setq erc-prompt-for-nickserv-password nil)
(setq erc-nickserv-passwords
      `((freenode     (("deftsp" . ,freenode-deftsp-pass)
                       ("Shihpin" . ,freenode-shihpin-pass)))
        (DALnet       (("nickname" . ,dalnet-pass)))))

;; erc-bbdb: /whois nickname, export the name in irc to bbdb. If the person have in bbdb, you can use M-x
;; bbdb-insert-new-field, irc TAB to add his nickname. Then you type /whois nickname, other information of the person
;; will auto store to bbdb.

;;; Connect
(defun erc-freenode-maybe ()
  "Connect to IRC."
  (interactive)
  (when (y-or-n-p "You really want to play Irc? ")
    (erc :server "irc.freenode.net" :port 6667
         :nick "deftsp" :full-name "Shihpin Tseng")))


(defun erc-lambdabot-maybe ()
  (interactive)
  (when (y-or-n-p "You really want to play Lambdabot? ")
    (erc :server "localhost" :port 6667
         :nick "deftsp" :full-name "Shihpin Tseng")))

;; auto join
;; (setq erc-autojoin-channels-alist
;;       '(("freenode.net" "#emacs" "#wiki" "#nethack")
;;         ("oftc.net" "#bitlbee")))

;; Or if you'd like to join the same channels on any server:
;; (setq erc-autojoin-channels-alist '((".*" "#help" "#emacs")))

;; (setq erc-server-history-list '("localhost" ; AIM, Yahoo, ICQ
;;                                 "irc.oftc.net"
;;                                 "irc.freenode.net"))

;;; Bitlee
;; The following snippets automagically identify yourself to Bitlbee when your IRC client is a recent ERC:


;;; mode-line
;; (setq erc-mode-line-format "%t %a")     ; default "%S %a"
;; Add this to your .emacs to see the number of opped/voiced/normal members of the current channel in the modeline:
;; ncm => number of channel members.
;; (define-minor-mode ncm-mode "" nil
;;                    (:eval
;;                     (let ((ops 0)
;;                           (voices 0)
;;                           (members 0))
;;                       (maphash (lambda (key value)
;;                                  (when (erc-channel-user-op-p key)
;;                                    (setq ops (1+ ops)))
;;                                  (when (erc-channel-user-voice-p key)
;;                                    (setq voices (1+ voices)))
;;                                  (setq members (1+ members)))
;;                                erc-channel-users)
;;                       (format " %S/%S/%S" ops voices members))))
;;mode-line ends there --------------------------------------------------------------------------------


;; (add-hook 'erc-insert-post-hook 'erc-save-buffer-in-logs)

;;; Coding
;; (setq erc-server-coding-system '(utf-8 . utf-8))
;; (setq erc-encoding-coding-alist '(("#linuxfire" . chinese-iso-8bit) ; this is an alist and you can append cons cell for multi-channel
;;                                   ("#foobar" . chinese-iso-8bit)
;;                                   ("#barfoo" . chinese-big5)))

;;; query & notice
;; (setq erc-auto-query 'bury)
;; As a default, only private messages trigger automatic creation of query buffers. If you'd like have the same behavior
;; when you receive notices you can use the following:

;; (add-hook 'erc-after-connect
;;           (lambda (server nick)
;;             (add-hook 'erc-server-NOTICE-hook 'erc-auto-query)))

;;; Custom PAGE function

;; Many IRC clients allow you to execute a custom command upon receipt of a CTCP PAGE command, ERC does also. Here's a
;; short demonstration function that plays bark.wav via esdplay:
;; (defun shae-erc-page (SENDER MSG)
;;   (interactive)
;;   (progn
;;     (shell-command "esdplay /home/shae/download/bark.wav")
;;     (message (concat SENDER " sent " MSG))))
;; (setq erc-page-function 'shae-erc-page)


;;; auto fill
;; (erc-fill-mode t)
;; have the fill width be a little more dynamic and change properly when you resize a window
;; (add-hook 'window-configuration-change-hook
;;           '(lambda ()
;;             (setq erc-fill-column (- (window-width) 2))))

;; (setq erc-fill-function 'erc-fill-variable
;;       erc-fill-prefix "      + ")

;; Re-fill lines

;; You can use the following function if you'd like to change the filling or to undo the filling in files saved by ErcLogging.
;; (defun paloryemacs/erc-refill (column)
;;   "Fill the ERC messages in the current buffer to COLUMN.
;;     Using a very high number will undo any previous filling.
;;     See also `erc-fill-prefix'."
;;   (interactive "nFill at which column? ")
;;   (let ((erc-fill-column column))
;;     ;; `erc-fill' fills the whole buffer, no need to set region
;;     (erc-fill)))

;; keep the prompt line at the bottom of the window.
;; (setq erc-input-line-position -2)

;; helper defun to unfill lines that have been cut from elsewhere
;; (defun paloryemacs/erc-unfill ()
;;   "Unfill the region after the prompt. Intended to be called just before you
;;     send a line"
;;   (interactive)
;;   (save-excursion
;;     (goto-char (point-max))
;;     (goto-char (previous-single-property-change (point) 'erc-prompt))
;;     (while (search-forward "\n" nil t)
;;       (delete-backward-char 1)
;;       (just-one-space))))

;; And here is how to replace the useless binding for fill-paragraph in ERC:
;; (define-key erc-mode-map (kbd "M-q") 'paloryemacs/unfill-line)

;;; keybinding
;; If you hit RETs by mistake before being done with the current line, rebind it...
;; (add-hook 'erc-mode-hook
;;           '(lambda ()
;;             (define-key erc-mode-map "\C-m" 'newline)
;;             (define-key erc-mode-map "\C-c\C-c" 'erc-send-current-line)
;;             (define-key erc-mode-map (kbd "C-<return>") 'erc-send-current-line)
;;             ;; uncomment this to make C-g do more intuitive stuff.. when in ERC..
;;             (define-key erc-mode-map "\C-c\C-g" 'keyboard-quit)))

;; Here is a keybinding for C-c C-q to start a query:
;; (define-key erc-mode-map (kbd "C-c C-q")
;;   (lambda (nick)
;;     (interactive (list (completing-read "Nick: " channel-members)))
;;     (erc-cmd-QUERY nick)))

;;; Erc Button URL
;; Sometimes people often type URL's like:
;; <nick> Try foo.org!
;; I want my emacs to be able to buttonize that url (even though it doesn't contain a www or http, and launch my galeon
;; if i middle-click. I also want emacs to be very strict about the last letter and realize that ! is not part of the
;; url. Finally, I don't care too much about false positives, but false negatives are a pain. So, here's the tweak I
;; use:
;; (setq erc-button-url-regexp
;;       "\\([-a-zA-Z0-9_=!?#$@~`%&*+\\/:;,]+\\.\\)+[-a-zA-Z0-9_=!?#$@~`%&*+\\/:;,]*[-a-zA-Z0-9\\/]")

;;; Ignoring
;; ignore notices
;; (setq erc-hide-list '("JOIN" "PART" "QUIT"))

;; Ignoring nicks temporarily
;; Use the /IGNORE nick command to ignore a nick for this session. Since nothing gets set, next time you will see
;; messages from this nick again.
;; Ignoring nicks permanently as follows:
;; (setq erc-ignore-list '("^j[a-z]*bot!" "^fussbot!" "^shorten!"))

;; Ignoring some content
;; Sometimes people start playing with bots until it gets on your nerves. Here is how to stop it. You can ignore the
;; bots themselves using erc-ignore-list, but you need something else to ignore the bot commands.
;; (defcustom erc-foolish-content '("^<.*?> \\?" "^<.*?> , *rr"
;;                                  "\\*CLICK\\*" "\\*BANG\\*")
;;   "Regular expressions to identify foolish content.
;;     Usually what happens is that you add the bots to
;;     `erc-ignore-list' and the bot commands to this list."
;;   :group 'erc
;;   :type '(repeat regexp))

;; (defun erc-foolish-content (msg)
;;   "Check whether MSG is foolish."
;;   (erc-list-match erc-foolish-content msg))

;; (add-hook 'erc-insert-pre-hook
;;           (lambda (s)
;;             (when (erc-foolish-content s)
;;               (setq erc-insert-this nil))))


;;; Truncate buffers so they don't hog core.
;; (add-hook 'erc-insert-post-hook 'erc-truncate-buffer)
;; (setq erc-truncate-buffer-on-save t)

;;; kill buffer
;; Kill buffers for channels after /part
;; (setq erc-kill-buffer-on-part t)
;; Kill buffers for private queries after quitting the server
;; (setq erc-kill-queries-on-quit t)
;; Kill buffers for server messages after quitting the server
;; (setq erc-kill-server-buffer-on-quit t)

;;; SwitchToErc

;; The following function and keybinding allow you to switch to the next ERC buffer using 'C-c e t'. If no ERC buffers
;; exist, or ERC has not even been loaded yet, then 'irc-maybe' is called to start a new connection.

;; It sorts out all server buffers, so switching is limited between channels and query windows. I use this because I
;; rarely want to see the server buffers, unless a /notice is waiting for me, in which case I can just use `C-c
;; C-<space>’ to get there


;; (defun erc-global-get-channel-buffer-list ()
;;   "Return a list of the ERC-channel-buffers"
;;   (erc-buffer-filter '(lambda() (if (string-match
;;                                 "^[^#].*:\\([0-9]*\\|ircd\\)$"
;;                                 (buffer-name (current-buffer))) nil t)) nil))

;; (defun switch-to-irc ()
;;   "Switch to an IRC channel buffer, or run `irc-maybe'.
;;         When called repeatedly, cycle through the buffers."
;;   (interactive)
;;   (let ((buffers (erc-global-get-channel-buffer-list)))
;;     (when (eq (current-buffer) (car buffers))
;;       (bury-buffer)
;;       (setq buffers (cdr buffers)))
;;     (if buffers
;;         (switch-to-buffer (car buffers))
;;         (call-interactively 'irc-maybe))))

;; (global-set-key (kbd "M-E") 'switch-to-irc)

;;; Select  erc buffers, and M-g to 'land' in one of them:
;; (require 'assoc)                        ; for aget
;; TODO: Package assoc is obsolete, use something instead.

;; (defun paloryemacs/get-char ()
;;   (catch 'char (while t
;;                  (let ((event (read-event "channel #: ")))
;;                    (if (char-valid-p event)
;;                        (throw 'char event))))))

;; (defun paloryemacs/switch-to-buffer ()
;;   "read events(chars), and switch to appropriate erc buffer"
;;   (interactive)
;;   (let ((buffers (erc-channel-list nil))
;;         buffer)
;;     ;;  lookup-key
;;     (while (let ((char (char-to-string (paloryemacs/get-char))))
;;              (switch-to-buffer (cond ((string-match "[a-z]" char)
;;                                       ;; letters -> ??
;;                                       (aget '(("d" . "#debian")
;;                                               ("e" . "#emacs")
;;                                               ("s" . "#stumpwm")
;;                                               ("g" . "#gentoo")
;;                                               ("p" . "#postgresql")
;;                                               ("x" . "#xfree86-devel")
;;                                               ("c" . "#scsh")
;;                                               ("f" . "#sawfish")
;;                                               ("z" . "#zsh"))
;;                                             char 't))
;;                                      ((string-match "[0-9]" char)
;;                                       (nth (string-to-int char) buffers))))))))
;; (define-key erc-mode-map [(meta ?g)] 'paloryemacs/switch-to-buffer)

;; SwitchToErc ens there---------------------------------------------------------------------------------------

;;; tracking
;;Channel tracking is for keeping track of activity in channels which are currently not visible on some frame/window. It
;;is defined as global minor mode in erc-track.el. It uses 'global-mode-string' as an indicator. global-mode-string is
;;used in nearly every mode-line in different emacs modes, so you will see active channels even in windows which belong
;;to buffers which are not in erc-mode.

;; A mode-line with active channels in them might look like this:
;; :** #emacs@zahn.OPN [#lisp,#lin,#d] (ERC Voice)--Bot----------------------------
;; Note the abbreviations. This line indicates that there was activity in channel #lisp, #linux and #debian.

;; (setq erc-track-position-in-mode-line nil) ; add to the end of `global-mode-string'.
;; To also exclude messages sent by the server when you join a channel, such as the nicklist and topic:
;; (setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
;;                                 "324" "329" "332" "333" "353" "477"))

;; Switch to last active channel C-c C-SPC
;;-----------------------------------------------------------------------------------------------------
;; Clears out annoying erc-track-mode stuff for when we don't care.
;; Useful for when ChanServ restarts :P
;; (defun reset-erc-track-mode ()
;;   (interactive)
;;   (setq erc-modified-channels-alist nil)
;;   (erc-modified-channels-update))

;; Highlighting keywords
;; (setq erc-keywords '("resolve" "deftsp" "\\berc[-a-z]*\\b" "\\bemms[-a-z]*\\b"))
;; (setq erc-current-nick-highlight-type 'keyword)

;; (setq erc-track-use-faces t)
;; (setq erc-track-priority-faces-only 'all)

;;; erc-joined-channels
;; Maybe you want minimal distraction for all channels but #emacs (the following is only useful after ERC is running and
;; you've joined some channels)

;; (defun paloryemacs/erc-md-all-but-emacs ()
;;   "Minimal distraction for all channels except #emacs"
;;   (interactive)
;;   (setq erc-track-priority-faces-only
;;         (remove "#emacs" (my-erc-joined-channels))))

;; (defun paloryemacs/erc-joined-channels ()
;;   (interactive)
;;   "Return all the channels you're in as a list.  This does not include queries."
;;   (save-excursion
;;     ;; need to get out of ERC mode so we can have *all* channels returned
;;     (set-buffer "*scratch*")
;;     (mapcar #'(lambda (chanbuf)
;;                 (with-current-buffer chanbuf (erc-default-target)))
;;             (erc-channel-list erc-process))))

;; For zero distraction, use 'erc-track-exclude' instead of 'erc-track-priority-faces-only' or turn off 'erc-track-mode'
;; for a while. Cycling through buffers

;; If you're in a low or zero distraction mode like explained above, you may still want to periodically cycle through
;; existing channels to see what traffic is like. YOu can use the following fragments, then.

;; (defvar erc-channels-to-visit nil
;;   "Channels that have not yet been visited by erc-next-channel-buffer")

;; (defun erc-next-channel-buffer ()
;;   "Switch to the next unvisited channel. See erc-channels-to-visit"
;;   (interactive)
;;   (when (null erc-channels-to-visit)
;;     (setq erc-channels-to-visit
;;           (remove (current-buffer) (erc-channel-list nil))))
;;   (let ((target (pop erc-channels-to-visit)))
;;     (if target
;;         (switch-to-buffer target))))


;; (defun paloryemacs/toggle-erc-busy ()
;;   "Toggle `erc-default-face' in `erc-track-faces-priority-list'
;; so as to keep an eye on work when necessarily."
;;   (interactive)
;;   (if (memq 'erc-default-face erc-track-faces-priority-list)
;;       (progn
;;         (setq erc-track-faces-priority-list
;;               (remove 'erc-default-face
;;                       erc-track-faces-priority-list))
;;         (message "Keep an eye on work"))
;;       (setq erc-track-faces-priority-list
;;             (append erc-track-faces-priority-list
;;                     '(erc-default-face)))
;;       (message "Ah, time for tea")))

;; (global-set-key (kbd "C-c n e") 'paloryemacs/toggle-erc-busy)

;;; /REVERSE

;; (defun erc-cmd-REVERSE (&rest words)
;;   (if words
;;       (erc-send-message (apply 'concat
;;                                (reverse (split-string (mapconcat 'identity words " ")
;;                                                       ""))))
;;       (let ((limit (- (point) 1000))
;;             (pos (point))
;;             text)
;;         (while (and pos (not (let ((data (get-text-property pos 'erc-parsed)))
;;                                (and data (string= (aref data 0) "PRIVMSG")))))
;;           (setq pos (previous-single-property-change pos 'erc-parsed nil limit)))
;;         (if pos
;;             (erc-display-message nil 'notice 'active
;;                                  (apply 'concat
;;                                         (reverse (split-string (aref (get-text-property pos
;;                                                                                         'erc-parsed)
;;                                                                      3) ""))))
;;             (erc-display-message nil 'notice 'active "Nothing to reverse")))))



;;; ErcSpook

;; (defun erc-cmd-SPOOK (&rest ignore)
;;   "Send a spooky list of keywords."
;;   (let* ((spook (with-temp-buffer (spook) (buffer-string)))
;;          (output (replace-regexp-in-string "\n" " " spook)))
;;     (erc-send-message output)))

;;; ErcInfo

;; (defun erc-cmd-INFO (&rest ignore)
;;   "Send current info node."
;;   (unless (get-buffer "*info*")
;;     (error "No *info* buffer"))
;;   (let (output)
;;     (with-current-buffer "*info*"
;;       (let* ((file (file-name-nondirectory Info-current-file))
;;              (node Info-current-node))
;;         (setq output (format "(info \"(%s)%s\") <-- hit C-x C-e to evaluate"
;;                              file node))))
;;     (erc-send-message output)))




;;; ErcSound
;; http://lorien.sdsu.edu/~carroll/audio/ni.wav
;; (defun erc-say-ni (str)
;;   "Play the Ni! sound file if STR contains Ni!"
;;   (when (string-match "\\bni!" str)
;;     (play-sound-file "~/.emacs.d/ni.wav")))

;; (add-hook 'erc-insert-pre-hook 'erc-say-ni)
;; (add-hook 'erc-send-pre-hook 'erc-say-ni)


(provide '50erc)
;; Local Variables:
;; outline-regexp: ";;; "
;; mode: outline-minor
;; End:
