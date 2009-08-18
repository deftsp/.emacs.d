;;;; Gnus config-file
;;;; created by S.P.Tseng

;;; load some useful library
(eval-after-load "gnus"
  '(progn
    (load-library "smtpmail")
    (load-library "nnimap")
    (load-library "starttls")))


;; there are no sources of email it must go fetch, process, and/or otherwise transfer in.
(setq mail-sources nil)

;; We just read mail, no newsgroups.
(setq gnus-nntp-server nil                ; This variable is semi-obsolete.  Use the `gnus-select-method'variable instead.
      gnus-read-active-file nil
      ;; ".newsrc" is in a format that can be readily understood by other newsreaders. If you don't plan on using other
      ;; newsreaders, set this variable to nil to save some time on exit.
      gnus-read-newsrc-file nil
      gnus-save-newsrc-file nil
      gnus-check-new-newsgroups nil
      gnus-save-killed-list nil
      gnus-check-bogus-newsgroups nil)

;;; backend
;; Tell gnus how to save received mail， gnus called this backend
;; (setq gnus-select-method '(nntp "localhost"))
;; nnnil is a Gnus backend that provides no groups or articles. It's useful as a primary select method when you want all
;; your real select methods to be secondary or foreign.
(setq gnus-select-method '(nnnil "")
      gnus-secondary-select-methods '((nnimap "gmail"
                                       (nnimap-address "imap.gmail.com")
                                       (nnimap-server-port 993)
                                       (nnimap-stream ssl))
                                      ;; (nnmaildir "localhost"
                                      ;;  (directory "~/Mail/")
                                      ;;  (directory-files nnheader-directory-files-safe)
                                      ;;  (get-new-mail nil))
                                      ;; (nntp "news.cn99.com")
                                      (nnimap "Mail"
                                       (nnimap-address "localhost")
                                       (nnimap-server-port 143)
                                       (nnimap-authenticator login))))

(setq nnmail-scan-directory-mail-source-once t
      ;; nnimap-split-inbox '("INBOX")
      ;; nnimap-split-rule 'nnmail-split-fancy
      nntp-authinfo-function 'nntp-send-authinfo)


;;; Save the messages you've writte
;; In *Group* buffer, use G m to make new group then add follows
(setq gnus-message-archive-group '((if (message-news-p)
                                       "nnfolder+archive:mail.sent.news"
                                       "nnfolder+archive:mail.sent.mail")))




;;; Gcc Header
;; The Gcc Header specifies a local mail box that receives a copy of the sent article.
(setq gnus-gcc-mark-as-read t)
;; Toggle the Gcc Header.
(defun message-toggle-gcc ()
  "Insert or remove the \"Gcc\" header."
  (interactive)
  (save-excursion
    (save-restriction
      (message-narrow-to-headers)
      (if (message-fetch-field "Gcc")
          (message-remove-header "Gcc")
          (gnus-inews-insert-archive-gcc)))))

(eval-after-load "message"
  '(progn
    (define-key message-mode-map (kbd "C-c C-f C-g") 'message-toggle-gcc)
    ;; auto fill column
    ;; you can press `W w' or `W Q' to truncate
    (add-hook 'message-mode-hook
     (lambda ()
       (setq fill-column 80)
       (turn-on-auto-fill)))))


;;; DELETE and Expireable
;; you can use 'B DEL' to delete a mail forever, or press 'E' in summary buffer labled with expirable, then after 7 days
;; it will be deleted
(setq nnmail-expiry-wait 7)
;; Don't make email expirable by default
(eval-after-load "gnus"
  '(progn
    (remove-hook 'gnus-mark-article-hook
     'gnus-summary-mark-read-and-unread-as-read)
    (add-hook 'gnus-mark-article-hook 'gnus-summary-mark-unread-as-read)))
;; Only mails in these groups will expire, meaning they'll be deleted after a week so long as I've read them.
(setq gnus-auto-expirable-newsgroups
      "junk\\|forums\\|gentoo-announce\\|bradsucks\\|bots\\|system\\|nnrss:.*")

;;; Message buffer
;; Generate the mail headers before you edit your message.
(setq message-generate-headers-first t)
;; (setq message-generate-headers-first '(References))

;; The message buffer will be killed after sending a message.
(setq message-kill-buffer-on-exit t)
;; (setq message-max-buffers 2)

(setq message-from-style 'angles)

(setq message-syntax-checks '((sender . disabled) (from . disabled)))


;;; Gnus Deman
;; automatic group re-scan without manual effort.
;; assumes: mail groups, level <= 2; nntp groups, level >= 3.
;; look up arg interpretation for gnus-demon-add-handler.

;; level-specified group scanner.
(defun gnus-demon-scan-news-by-level (level)
  (let ((win (current-window-configuration)))
    (unwind-protect
         (save-window-excursion
           (save-excursion
             (when (gnus-alive-p)
               (save-excursion
                 (set-buffer gnus-group-buffer)
                 (gnus-group-get-new-news level)))))
      (set-window-configuration win))))

(defun gnus-demon-scan-news-of-level-2 ()
  "Scan for new mail, updating the *Group* buffer."
  (gnus-demon-scan-news-by-level 2))

;; check new mail every 3 minutes
(eval-after-load "gnus"
  '(gnus-demon-add-handler 'gnus-demon-scan-news-of-level-2 3 0))

;;; gnus registry
;; (setq gnus-registry-max-entries 2500
;;       gnus-registry-use-long-group-names t)
;; (eval-after-load "gnus"
;;   (gnus-registry-initialize))
;; Then use this in your fancy-split:
;; (: gnus-registry-split-fancy-with-parent)


;;; colorful gnus-summary-line-format
;; if date of mail is today, then the date color is red else the date color is blue
;; use defface instead
;; (copy-face 'default 'my-red-face)
;; (set-face-foreground 'my-red-face "red")
;; (copy-face 'default 'my-blue-face)
;; (set-face-foreground 'my-blue-face "blue")
;; (defun gnus-user-format-function-a (header)
;;   "Test dynamic color"
;;   (let  ((date-time (format-time-string "%m/%d" (safe-date-to-time
;;                                                  (mail-header-date header)))))
;;     (if (string= date-time (format-time-string "%m/%d"))
;;         (propertize date-time 'face 'my-red-face 'mouse-face 'my-blue-face
;;                     'gnus-face t)
;;         (propertize date-time 'face 'my-blue-face 'mouse-face 'my-red-face 'gnus-face t))))

;; gnus-summary-line-format "|%*%U%R%B%s%-66=%-20,20f|%4L | %ua |\n"
;; colorful gnus-summary-line-format ends there

;;; Gmail envy --------------------------------------------------------------------------------
;; http://emacs.wordpress.com/2007/10/07/gmail-envy/
;; (defvar *deftsp-mails*
;;   "deftsp@gmail\\.com\\|kirby1985@gmail\\.com\\|mendouer@163\\.com")
;; (defun gnus-user-format-function-j (headers)
;;   (let ((to (gnus-extra-header 'To headers)))
;;     (if (string-match *deftsp-mails* to)
;;         (if (string-match "," to) "~" "»")
;;         (if (or (string-match *deftsp-mails*
;;                              (gnus-extra-header 'Cc headers))
;;                (string-match *deftsp-mails*
;;                              (gnus-extra-header 'BCc headers)))
;;             "~"
;;             " "))))
;; (setq gnus-summary-line-format
;;       (concat "%U%R %~(pad-right 2)t%* %uj %B%~(max-right 30)~(pad-right 30)n  "
;;               "%~(max-right 90)~(pad-right 90)s %-135=%&user-date;\n"))
;; Gmail envy ends there

;; (setq gnus-user-date-format-alist
;;       '(((gnus-seconds-today) . "Today, %H:%M")
;;         ((+ 86400 (gnus-seconds-today)) . "Yesterday, %H:%M")
;;         (604800 . "%A %H:%M") ;;that's one week
;;         ((gnus-seconds-month) . "%A %d")
;;         ((gnus-seconds-year) . "%B %d")
;;         (t . "%B %d '%y"))) ;;this one is used when no other does match

;;; display format
;; I wanna keep track of the last time I rode a group
;; (add-hook 'gnus-select-group-hook 'gnus-group-set-timestamp)

;; "-" 表示右对齐
(setq gnus-group-line-format   "%P%m%M%L%6N/%-5R| %*%-16G %D\n" ; %* Positioning Point or use : instead
      gnus-summary-line-format "|%*%U%R%B%s%-66=%-20,20f|%4L |%D|\n"
      gnus-topic-line-format   "%i%n %A (%G) %v\n")

;;; Header
;; Select the header that should be shown. Yes I am interested in the used mail or news client from other people ;-)
(setq gnus-visible-headers "^From:\\|^Newsgroups:\\|^List-Id:\\|^Subject:\\|^Date:\\|^Followup-To:\\|^Reply-To:\\|^Summary:\\|^Keywords:\\|^To:\\|^[BGF]?Cc:\\|^Posted-To:\\|^Mail-Copies-To:\\|^Mail-Followup-To:\\|^Apparently-To:\\|^Gnus-Warning:\\|^Resent-From:\\|^X-Sent:\\|^User-Agent:\\|^X-Mailer:\\|^X-Newsreader:\\|^Content-Type:\\|^Content-Transfer-Encoding:")

;; Specify the order of the header lines

(setq gnus-sorted-header-list '("^From:" "^Subject:" "^Summary:" "^Keywords:" "^Newsgroups:" "^List-Id:" "^Followup-To:"
                                "^To:" "^Cc:" "^Date:" "^User-Agent:" "^X-Mailer:" "^X-Newsreader:" "^Content-Type:"
                                "^Content-Transfer-Encoding:"))

;;; Signature
(setq  gnus-signature-separator '("^-- $"
                                  "^-- *$"
                                  "^-------*$"
                                  "^ *--------*$"
                                  "^________*$"
                                  "^========*$")
       gnus-signature-limit 20.0        ; floating point for lines
       message-signature-file "~/.signature")
;;; Summary
;; How to display gnus summary thread
(if window-system
    (setq gnus-sum-thread-tree-root ">>"
          gnus-sum-thread-tree-single-indent " >"
          gnus-sum-thread-tree-leaf-with-other "+-> "
          gnus-sum-thread-tree-indent " "
          gnus-sum-thread-tree-vertical "|"
          gnus-sum-thread-tree-single-leaf "`-> "
          gnus-sum-thread-tree-false-root "~>"))
;; (setq gnus-sum-thread-tree-root "►")
;; (setq gnus-sum-thread-tree-leaf-with-other "├─►")
;; (setq gnus-sum-thread-tree-single-leaf "└─►")
;; (setq gnus-sum-thread-tree-vertical "│")
;; (setq gnus-sum-thread-tree-single-indent " ")

(setq gnus-summary-thread-gathering-function 'gnus-gather-threads-by-subject
      gnus-summary-gather-subject-limit 'fuzzy
      ;; This google group adds message number prefix at the very start of the subject, thus gnus can't
      ;; figure out that they actually belong to the same thread. To fix this, we need to instruct gnus
      ;; to fuzzy match subjects when threading.  Specifically in this case, I'm going to ignore the
      ;; number prefix when threading.
      gnus-simplify-subject-fuzzy-regexp "^\\[.+\\]")


(eval-after-load "gnus"
  '(progn
    ;; convert mail send time to local time
    (add-hook 'gnus-article-prepare-hook 'gnus-article-date-local)
    (add-hook 'gnus-article-prepare-hook (lambda ()
                                           (setq fill-column 96)
                                           (gnus-article-fill-long-lines)))))


;;; Gnus multi-pane tricks
(eval-after-load "gnus"
  '(progn
    (gnus-add-configuration
     '(article
       (horizontal 1.0
        (vertical 42 (group 1.0))
        (vertical 1.0
         (summary 0.36 point)
         (article 1.0)))))

    (gnus-add-configuration
     '(summary
       (horizontal 1.0
        (vertical 42 (group 1.0))
        (vertical 1.0 (summary 1.0 point)))))))

;; I wanna be able to access my previous post.
(setq gnus-fetch-old-headers 'some)


;;; sort
;; (setq gnus-article-sort-functions '((not gnus-article-sort-by-date)))
;; (setq gnus-thread-sort-functions '((not gnus-thread-sort-by-date)))

;; (setq gnus-thread-sort-functions '(gnus-thread-sort-by-number
;;                                    gnus-thread-sort-by-date
;;                                    gnus-thread-sort-by-total-score))


;;; REPLY
;; Hey, please remove my address from To: and Ccc: while I do a wide reply.
;; seen at http://www.gnusfr.org/gen.php3/2002/04/02/36,0,1,0.html
;; (setq message-dont-reply-to-names
;;       "utilisateur@\\(gnu\\|free\\).\\(org\\|fr\\)")

;;I want my replies to begin with something like " writes:"
(defun message-insert-citation-line ()
  (when message-reply-headers
    ;; In savannah-hackers group, we talk english
    (if (string-equal gnus-newsgroup-name "savannah-hackers")
        (insert (mail-header-from message-reply-headers) " said:\n\n")
        (insert (mail-header-from message-reply-headers) " writes:\n\n"))))

;; prevent send message by mistake
(setq gnus-confirm-mail-reply-to-news t)

;; automatically mail a copy of a Gnus followup message to the original poster.
;; (setq gnus-auto-mail-to-author t)


;;; gnus cache
;; '*' article persistent
;; 'M-*' Remove the current article from the persistent articles
;; 'Y c' Insert all the articles cached for this group into the current buffer.
(setq gnus-use-cache 'passive)

;;; group topic
;; 'T n' gnus-topic-create-topic
;; C-k and C-y
;; Use the topic mode
(eval-after-load "gnus"
  '(add-hook 'gnus-group-mode-hook 'gnus-topic-mode))

;;; group settings
(setq gnus-parameters
      '(("^nnimap\\+Mail\\:.*"
         (gcc-self . t))
        ("\\`nnrss:" ;; Display `text/html' parts in `nnrss' groups.
         (mm-discouraged-alternatives nil))
        (".*news\\.gmane\\.org.*"
         (posting-style
          (address "deftsp@gmail.com")
          (name "S.P.Tseng")
          ;; (body "\n\n\n deftsp\n -- ")
          ;; (eval (setq message-sendmail-extra-arguments '("-a" "gmail")))
          (user-mail-address "deftsp@gmail.com")))))

;; 如何 Gcc 到一份至 INBOX(或其它 imap 组)？
;; 默认情况下，gmail 将你发送的存在 sent mail 中，在 INBOX 中完全看不到(其 web 界面上倒是能看到)，这样就严重破坏了
;; thread 的完整性。可以通过 gnus-parameters 来设置：
;; (setq gnus-parameters
;; '(("nnimap+imap.gmail.com.*" (gcc-self . t))))
;; 更友好的方式是在 Group buffer 中，通常 `G p' 或 `G c' 来设置。

(setq gnus-posting-styles
      '((".*"
         ("From" "S.P.Tseng <deftsp@gmail.com>")
         ;; ("Organization" "GNU Project")
         ;; (x-face-file "~/.xface")
         ;; ("X-Accept-Language" "es, en")
         ;; ("Return-Receipt-To" "Exal de Jesus Garcia Carrillo <exal@gnu.org>")
         ;; ("X-PGP" "0x5251B462")
         ;; ("X-PGP-FP" "B17A 8AD9 8B64 DC6C 7F92  BBF7 A199 B1A1 5251 B462")
         (signature-file "~/.signature"))
        ("^nnml.*gmail"
         (name "tsp")
         (address "deftsp@gmail.com")
         (signature-file "~/.signature.tsp"))))



;;; charset
;; 进行了这种设定以后，我们看到乱码邮件时就可以用 `1 g' 指定采用 utf-8, `2 g' 指定big5等等
(setq gnus-summary-show-article-charset-alist '((1 . gbk)
                                                (3 . utf-8)
                                                (2 . big5)
                                                (4 . utf-7)))

(setq gnus-group-name-charset-group-alist
      '(("\\.com\\.cn:" . gbk)
        ("news\\.newsfan\\.net" . gbk)))
;; all groups from newsfan will use gbk coding
(setq gnus-group-name-charset-method-alist
      '(((nntp "news.newsfan.net") . gbk)))

(eval-after-load "gnus"
  '(progn
    (add-to-list 'gnus-group-charset-alist '("\\(^\\|:\\)cn\\>\\|\\<chinese\\>" gbk))))

(setq gnus-newsgroup-ignored-charsets '(unknown-8bit x-unknown x-gbk))

;;; Mail queueing
;; Sometimes, it can be useful to just queue the mails and send them later all at once (dialup users, etc...), then just add:
(setq smtpmail-queue-mail t)
;; When your network connection is ready, just do
;; M-x smtpmail-send-queued-mail RET

;;; default newsgroups
;;(setq gnus-default-subscribed-newsgroups
;;  '("gnu.emacs.help"
;;    "cn.comp.os.linux"))


;; We trash duplicates mails.
(setq gnus-summary-ignore-duplicates t)
(setq gnus-suppress-duplicates t)

;; synchronous all groups and post mails, articles in drafts
(setq gnus-asynchronous t)


;;; Mail

(setq gnus-permanently-visible-groups "\\(INBOX\\)\\|\\(inbox\\)")
(setq mail-from-style 'angles
      mail-interactive t)

;; (setq mail-default-headers "Content-Type: text/plain; charset=windows-1251\n")

;; /etc/emacs/site-start.d/00debian-var.el will read /etc/mailname
;; (setq mail-host-address "gmail.com")
(setq mail-source-delete-incoming t)    ; or n days
;; Using procmail with a spool means you can use display-time-mail-file and display-time to see when you have new mail
(setq mail-source-report-new-mail t)
(setq mail-source-idle-time-delay 5)
(setq mail-source-new-mail-available t)
(setq pop3-leave-mail-on-server t)


;; When you reply a mail, emacs will use the value of user-mail-address to compose the From: header. One can also tweak
;; gnus-posting-styles to change the address on a per group basis. But what i really wanted was to use the address in
;; the mail I was replying to.
(setq message-alternative-emails
      (regexp-opt '("deftsp@gmail.com" "kirby1985@gmail.com" "mendouer@163.com")))


;; (add-hook 'message-send-hook 'ispell-message)

(setq message-send-mail-function 'message-send-mail-with-sendmail)
(setq send-mail-function 'message-send-mail-with-sendmail)
;; (setq sendmail-program "/usr/bin/msmtp"
;;       mail-specify-envelope-from t
;;       mail-envelope-from 'header)

;; need to tell msmtp which account we're using
(setq message-sendmail-extra-arguments '("-a" "gmail"))

;; I'd like Gnus NOT to render HTML-mails but show me the text part if it's available. How to do it?
(setq mm-codxing-system-priorities '(iso-8859-1 gbk utf-8))
(eval-after-load "mm-decode" '(progn
                               (add-to-list 'mm-discouraged-alternatives "text/html")
                               (add-to-list 'mm-discouraged-alternatives "text/richtext")))

;; Set the default value of `mm-discouraged-alternatives'.
(eval-after-load "gnus-sum"
  '(add-to-list 'gnus-newsgroup-variables '(mm-discouraged-alternatives
                                            . '("text/html" "image/.*"))))


;; Display html emails via emacs-w3m
(setq mm-text-html-renderer 'w3m)
(setq mm-inline-text-html-with-images nil)


;; In mail-source, procmail store files that We store mails here - this is also defined in my .emacs, it's the only way
;; I found to avoid apparition of ~/Maildir/.
;; (setq gnus-directory "~/News/")
(setq message-directory "~/Mail/")      ;default "~/Mail/"

;; If non-nil, require your confirmation when exiting Gnus.
(setq gnus-interactive-exit t)

(setq gnus-no-groups-message "No gnus is bad news")

;; (setq message-default-headers "bcc: pll")



;;; split my mail into folders
;; (setq nnmail-split-methods
;;       '(("duplicates" "^Gnus-Warning:.*duplicate")
;;         ("root" "^\\(To:\\|CC:\\).*root@.*")
;;         ("root" "^\\(To:\\|CC:\\).*orchid@localhost.*")
;;         ("inbox" "^\\(CC:\\|To:\\).*debian@magma.ca.*")
;;         ("debian-bts" "^\\(CC:\\|To:\\).*@bugs.debian.org.*")
;;         ("emacs-pretest" "^\\(CC:\\|To:\\).*emacs-pretest-bug@gnu.org.*")
;;         ("erc" "^\\(CC:\\|To:\\).*erc-discuss@gnu.org.*")
;;         ("outgoing-mail-archive" "^From:.*debian@magma.ca.*")
;;         ("outgoing-mail-archive.gmail" "^From:.*deftsp.*")
;;         ("inbox" "")))

;; I duplicated my 10000+ emails so many times when testing out new split methods, had to use sed to remove the Xref
;; headers Gnus inserts so I could use fdupes to remove the duplicates and re-import everything. Speaking of fdupes, be
;; sure you remember to use the -f option, otherwise you'll end up deleting every file that has or is itself a
;; duplicate. Effectively losing all the emails you accidentally duplicated.
;;
;; This variable would have spared me all that trouble. Incidentally commenting code after an ordeal like this is quite
;; therapeutic.
(setq nnmail-crosspost nil)


;;; Score
;; The scoring system sorts articles and authors you read often to the beginning of the available mails. Less
;; interesting stuff is located at the end.

;; (setq gnus-use-adaptive-scoring t)
;; (setq gnus-score-expiry-days 7)
;; (setq gnus-default-adaptive-score-alist
;;       '((gnus-unread-mark)
;;         (gnus-ticked-mark (from 4))
;;         (gnus-dormant-mark (from 5))
;;         (gnus-saved-mark (from 20) (subject 5))
;;         (gnus-del-mark (from -2) (subject -5))
;;         (gnus-read-mark (from 2) (subject 1))
;;         (gnus-killed-mark (from 0) (subject -3))))

;; (gnus-killed-mark (from -1) (subject -3))))
;; (gnus-kill-file-mark (from -9999)))
;; (gnus-expirable-mark (from -1) (subject -1))
;; (gnus-ancient-mark (subject -1))
;; (gnus-low-score-mark (subject -1))
;; (gnus-catchup-mark (subject -1))))

;; (setq gnus-score-decay-constant 3)
;; (setq gnus-score-decay-scale 0.05)

;; (setq gnus-decay-scores t)
;; (gnus-decay-score 1000)

;; Increase the score for followups to a sent article.
;; (add-hook 'message-sent-hook 'gnus-score-followup-article)
;; (add-hook 'message-sent-hook 'gnus-score-followup-thread)


;; (("xref"
;;   ("cn\\..* cn\\..* cn\\..*" -1000 nil r))
;;  (mark 0)
;;  (mark-and-expunge -500))

;; gnus 中文新闻组的垃圾问题

;; 主要是 cn.bbs.comp.linux 这个组里面，垃圾很多，而且基本上就是一句话，还可能起个侮辱中国人的标题，看起来非常讨厌。
;; 不过很棒的是，gnus 有打分的功能，而且这样的垃圾文件都是交叉 post 到几个cn.*的组上的，因此可以将其给一个超级的低分，就可以不显示出来了，比较不错。

;; 因此在 SCORE 文件中增加一个规则，过滤 xref 部分
;; (("xref"
;; ("cn\\..* cn\\..*" -1000 nil r))
;; 这样，凡是交叉post 到两个 cn.* 组的都被给了 -1000 分。基本上就可以保证不被显示出来了。


;; show and update the summary buffer as it's being built, update the display every N lines.
(setq gnus-summary-display-while-building nil)
;; I like it verbose
(setq gnus-verbose 2000)
(setq gnus-verbose-backends 10)


;;; RSS
(eval-after-load "gnus"
  '(require 'nnrss))
;; Gnus has built in support for RSS feeds, just hit "G R" and enter the feed URL. Unfortunately Gnus will check RSS
;; feeds everytime you check your email, which is very impolite.

;; To get around this, you set nnrss into local mode, which will fetch news from locally stored RSS feeds.
(setq nnrss-use-local t)

;; Format RSS feed titles nicely
(eval-after-load "gnus"
  '(add-hook 'gnus-summary-mode-hook
    (lambda ()
      (if (string-match "^nnrss:.*" gnus-newsgroup-name)
          (progn (make-local-variable 'gnus-show-threads)
                 (make-local-variable 'gnus-article-sort-functions)
                 (make-local-variable 'gnus-use-adaptive-scoring)
                 (make-local-variable 'gnus-use-scoring)
                 (make-local-variable 'gnus-score-find-score-files-function)
                 (make-local-variable 'gnus-summary-line-format)
                 (setq gnus-show-threads nil
                       gnus-article-sort-functions 'gnus-article-sort-by-date
                       gnus-use-adaptive-scoring nil
                       gnus-use-scoring t
                       gnus-score-find-score-files-function 'gnus-score-find-single
                       gnus-summary-line-format "%U%R%z%d %I%(%[ %s %]%)\n"))))))


;; Hit C-u C-Enter to open an RSS article with browse-url (firefox), awesome.
;; Hit C-Enter to open an RSS article with w3m, but switch back to the summary
;; buffer straight away.
(defun browse-nnrss-url(arg)
  (interactive "p")
  (let ((url (assq nnrss-url-field
                   (mail-header-extra
                    (gnus-data-header
                     (assq (gnus-summary-article-number)
                           gnus-newsgroup-data))))))
    (if url
        (if (= arg 4)
            (browse-url-firefox (cdr url))
            (progn
              (gnus-summary-scroll-up arg)
              (with-selected-window (get-buffer-window gnus-article-buffer)
                (browse-url (cdr url)))))
        (gnus-summary-scroll-up arg))))


;; Scroll Emacs-W3M buffer from the Gnus summary
;; RET for scrolling up, M-RET for scrolling down, <space> for page down and <backspace> for page up.

;; (defun DE-summary-scroll-up (arg)
;;   (interactive "p")
;;   (unless (DE-scroll-article-window 'DE-visual-scroll-up arg)
;;     (gnus-summary-scroll-up arg)))

;; (defun DE-summary-scroll-down (arg)
;;   (interactive "p")
;;   (unless (DE-scroll-article-window 'DE-visual-scroll-down arg)
;;     (gnus-summary-scroll-down arg)))

;; (defun DE-summary-next-page (&optional arg circular stop)
;;   (interactive "P")
;;   (unless (DE-scroll-article-window 'scroll-up nil)
;;     (gnus-summary-next-page arg circular stop)))

;; (defun DE-summary-prev-page (&optional arg move)
;;   (interactive "P")
;;   (unless (DE-scroll-article-window 'scroll-down nil)
;;     (gnus-summary-prev-page arg move)))

;; (defun DE-scroll-article-window (func arg)
;;   (if (and (eq (cdr gnus-article-current) (gnus-summary-article-number))
;;            (gnus-buffer-live-p gnus-article-buffer))
;;       (progn
;;         ;; special treatment for RSS with W3M
;;         (if (and (eq (car gnus-current-select-method) 'nnrss)
;;                  (not (get-buffer-window gnus-article-buffer)))
;;             (let (flag)
;;               (mapc (lambda (buf)
;;                       (when (get-buffer-window buf)
;;                         (with-selected-window (get-buffer-window buf)
;;                           (funcall func arg))
;;                         (setq flag t)))
;;                     (w3m-list-buffers))
;;               flag)
;;             (with-selected-window (get-buffer-window gnus-article-buffer)
;;               (funcall func arg))
;;             t))
;;       (when (and (gnus-buffer-live-p gnus-article-buffer)
;;                  (get-buffer-window gnus-article-buffer))
;;         ;; reset vscroll
;;         (set-window-vscroll (get-buffer-window gnus-article-buffer) 0))
;;       nil))

(eval-after-load "nnrss"
  '(add-to-list 'nnmail-extra-headers nnrss-url-field))

;; (eval-after-load "gnus"
;;   '(progn (define-key gnus-summary-mode-map
;;            (kbd "<RET>") 'DE-summary-scroll-up)
;;     (define-key gnus-summary-mode-map
;;      (kbd "<M-return>") 'DE-summary-scroll-down)
;;     (define-key gnus-summary-mode-map
;;      (kbd "<SPC>") 'DE-summary-next-page)
;;     (define-key gnus-summary-mode-map
;;      (kbd "<backspace>") 'DE-summary-prev-page)
;;     (define-key gnus-summary-mode-map
;;      (kbd "<C-return>") 'browse-nnrss-url)))

;;; Gmail
;; to enable showing of [Gmail]/* groups
(setq gnus-ignored-newsgroups "")

;; 自动订阅新创建的 imap 组？
;; 这也要通过 group 或者 topic parameters 来设置，亦即 `G p' 或 `G c'。对于 topic 来说，好像并没有一个类似
;; gnus-parameters 的参数用于设置，有点不爽。 如果是用 `G p' 方式设置 topic：
;; ((subscribe . ".*imap.gmail.com.*"))

;; debug
;; (setq smtpmail-debug-info t)
;; (setq smtpmail-debug-verb t)

;; Choose account label to feed msmtp -a option based on From header in Message buffer; This function must be added to
;; message-send-mail-hook for on-the-fly change of From address before sending message since message-send-mail-hook is
;; processed right before sending message.
(eval-after-load "gnus"
  '(progn
    (require 'message)
    (defun tsp-feed-msmtp ()
      (if (message-mail-p)
          (save-excursion
            (let* ((from (save-restriction
                           (message-narrow-to-headers)
                           (message-fetch-field "from")))
                   (account (cond
                              ;; I use email address as account label in ~/.msmtprc
                              ((string-match "deftsp@gmail.com" from) "gmail")
                              ;; Add more string-match lines for your email accounts
                              ((string-match "mendouer@163.com" from) "163"))))
              (setq message-sendmail-extra-arguments (list "-a" account))))))

    (setq message-sendmail-envelope-from 'header)
    (add-hook 'message-send-mail-hook 'tsp-feed-msmtp)))
;; (defadvice message-send-mail (around gmail-message-send-mail protect activate)
;;   "Set up SMTP settings to use Gmail's server when mail is from a gmail.com address."
;;   (interactive "P")
;;   (if (save-restriction
;;         (message-narrow-to-headers)
;;         (string-match "gmail.com" (message-fetch-field "from")))

;;       (let ((message-send-mail-function 'smtpmail-send-it)
;;             ;; gmail says use port 465 or 587, but 25 works and those don't, go figure
;;             (smtpmail-starttls-credentials '(("smtp.gmail.com" 25 nil nil)))
;;             (smtpmail-auth-credentials '(("smtp.gmail.com" 25 "username@gmail.com" nil)))
;;             (smtpmail-default-smtp-server "smtp.gmail.com")
;;             (smtpmail-smtp-server "smtp.gmail.com")
;;             (smtpmail-smtp-service 25)
;;             (smtpmail-local-domain "yourdomain.com"))
;;         ad-do-it)
;;     ad-do-it))

;;; Index
;; Search `C-s',
;; Search bodies, too try `M-s' instead.

;; In order to make index of articles with Namazu before using this module, type M-x gnus-namazu-create-index RET.
;; Otherwise, you can create index by yourself with the following commands:
;; % mkdir ~/News/namazu
;; % mknmz -a -h -O ~/News/namazu ~/Mail ~/News/cache
;;
;; The first command makes the directory for index files, and the second command generates index files of mails and
;; persistent articles.

;; In group buffer or in summary buffer, type C-c C-n query RET.
;; (require 'gnus-namazu)
;; (gnus-namazu-insinuate)
;; (setq gnus-namazu-index-update-interval nil
;;       gnus-namazu-index-directories (list (expand-file-name "~/News/namazu")))
;; (add-hook 'gnus-startup-hook 'gnus-namazu-update-all-indices)
;; call explicitely M-x gnus-namazu-update-all-indices

;; Update the namazu index every day at 6:00am
;; (defun tsp-gnus-namazu-update-all-indices ()
;;   (interactive)
;;   (gnus-namazu-update-all-indices t))
;; (defun tsp-gnus-update-namazu-index ()
;;   (run-at-time "6:00pm" nil 'tsp-gnus-namazu-update-all-indices))
;; (add-hook 'midnight-hook 'tsp-gnus-update-namazu-index)

;; Gnus comes with an older version of imap.el  M-x locate-library and specifying imap.el. If Emacs reports
;; "~/.emacs.d/add-on/imap.el", then Gnus is configured to use the updated imap.el.
(eval-after-load "gnus"
  '(progn
    (require 'imap)
    (require 'nnir)))

;; After you've installed Namazu, create a directory for Namazu's index files, such as ~/Mail/namazu. Then index your
;; mail by typing this at the command-line:
;; mknmz --mailnews -O ~/Mail/namazu ~/Mail
;; (setq nnir-search-engine 'namazu)
(setq nnir-search-engine 'imap)
(setq nnir-namazu-index-directory (expand-file-name "~/Mail/namazu/"))
(setq nnir-namazu-remove-prefix (expand-file-name "~/Mail"))
(setq nnir-mail-backend (nth 2 gnus-secondary-select-methods))

;; G G (gnus-group-make-nnir-group) to search your mail for at keyword.


;;; Misc
;; do not include original sigs in reply's
(setq message-cite-function 'message-cite-original-without-signature)

(setq gnus-nov-is-evil nil)

(setq gnus-summary-display-arrow t
      gnus-treat-display-smileys nil      ; I do not like graphics smileys.
      gnus-keep-backlog 50              ;default 20
      ;; If the number of articles in a newsgroup is greater than this value, confirmation is required for selecting the
      ;; newsgroup. If it is nil, no confirmation is required.
      gnus-large-newsgroup 200             ; default 200
      gnus-auto-center-summary nil
      gnus-auto-select-next nil)

;;; Make Gnus into an offline newsreader.
(gnus-agentize) ; The obsolete setting.
(setq gnus-agent t) ; Now the default.
(setq gnus-agent-go-online t)
(setq gnus-agent-short-article 500)
(setq gnus-agent-long-article 1000)

;; *Regexp of From headers that may be suppressed in favor of To headers.
;; (setq gnus-ignored-from-addresses "S.P.Tseng")

(setq canlock-password "a59e8bcb8bdc05c5f3d3d51b546c85e9084a55ee")


;;; GnusPGG
;; (require 'pgg)
;; Or if you prefer using autoload Emacs feature, you can use this :
(autoload 'pgg-encrypt-region "pgg" "Encrypt the current region." t)
(autoload 'pgg-decrypt-region "pgg" "Decrypt the current region." t)
(autoload 'pgg-sign-region "pgg" "Sign the current region." t)
(autoload 'pgg-verify-region "pgg" "Verify the current region." t)
(autoload 'pgg-insert-key "pgg" "Insert the ASCII armored public key." t)
(autoload 'pgg-snarf-keys-region "pgg" "Import public keys in the current region." t)

;; Then you can tell to nmml how you want it to act when facing a signed/encrypted mail that way:

;; verify/decrypt only if mml knows about the protocl used
(setq mm-verify-option 'known)
(setq mm-decrypt-option 'known)

;; Here we make button for the multipart
(setq gnus-buttonized-mime-types '("multipart/encrypted" "multipart/signed"))

;; Automcatically sign when sending mails
;; (add-hook 'message-send-hook 'mml-secure-message-sign-pgpmime)

;; Enough explicit settings
(setq pgg-passphrase-cache-expiry 300)
(setq pgg-default-user-id "deftsp")

;; Here is a way to tell when we want to pgpsign mail according to the current group (stolen on the ding ML)
;; regexp of groups from which new messages are mime signed by default
;; (setq my-sign-mime-group-regexp "^\\(INBOX.\\|\\)mail.work")
;; ;; hook to setup message
;; (defun my-mml-secure-message-sign-mime ()
;;   (when (string-match my-sign-mime-group-regexp
;;                       gnus-newsgroup-name)
;;     (mml-secure-message-sign-smime)))

;; ;; plug this into message-setup-hook
;; (add-hook 'message-setup-hook 'my-mml-secure-message-sign-mime)



;; Automatic decryption/verification of gpg/pgp parts

;; By default, you have to press "W s" or "W p" to decrypt/verify pgp parts. Mutt does this automatically, and I think
;; gnus should do so, too. Here are my customisations to do that:

;; Tells Gnus to inline the part
(eval-after-load "mm-decode"
  '(add-to-list 'mm-inlined-types "application/pgp$"))
;; Tells Gnus how to display the part when it is requested
(eval-after-load "mm-decode"
  '(add-to-list 'mm-inline-media-tests '("application/pgp$" mm-inline-text identity)))
;; Tell Gnus not to wait for a request, just display the thing straight away.
(eval-after-load "mm-decode"
  '(add-to-list 'mm-automatic-display "application/pgp$"))
;; But don't display the signatures, please.
(eval-after-load "mm-decode"
  (quote (setq mm-automatic-display (remove "application/pgp-signature" mm-automatic-display))))

;; In Gnus 5.11 (in GNU Emacs 22) the above didn’t work for me but this does (I got the settings from
;; gnus-summary-force-verify-and-decrypt in gnus-sum.el):
;;(setq gnus-article-emulate-mime t) ; already set in my gnus but you may need it.
;; (setq gnus-buttonized-mime-types (append (list "multipart/signed" "multipart/encrypted")
;;                                          gnus-buttonized-mime-types))
;; This does the same as pressing W s for every article.

;;; Automatic signing/encryption of messages

;; You can type C-c RET s p to sign or C-c RET c p to encrypt messages with PGP/MIME (there are also corresponding
;; commands for S/MIME and PGP) but if you want to have maximum security you probably want to have all your messages
;; being encrypted if possible or at least being signed.
;; Add the following if you only want to sign or encrypt all messages:

;; (add-hook 'gnus-message-setup-hook 'mml-secure-message-sign-pgpmime)
;; (add-hook 'gnus-message-setup-hook 'mml-secure-message-encrypt-pgpmime)

;; This adds an MML command to your message just at the beginning of your message text so you can still switch from
;; signing to encryption or remove the signature by typing one of those C-c RET ... commands.

;; Does anyone have some code to make this happen only for messages that are clearly not to newsgroups or mailing lists,
;; the simplest probably being to just use this when writing a reply, as opposed to a follow-up? - bkhl

;; quit gnus properly instead of leaving auto-save files around
(defadvice save-buffers-kill-emacs (before quit-gnus (&rest args) activate)
  (let (buf)
    (when (and (fboundp 'gnus-alive-p)
               (gnus-alive-p)
               (bufferp (setq buf (get-buffer "*Group*"))))
      (with-current-buffer buf
        (gnus-group-exit)))))

;; byte-compile things like gnus-summary-line-format.
(gnus-compile)

;;; tip
;; Gnus tip: how to convert a reply to a wide reply
;; Keybinding: `C-c C-f w'
;; Description: Insert `To' and `Cc' headers as if you were doing a wide reply even if the message was not made for a
;; wide reply first.



;; Local Variables:
;; mode: emacs-lisp
;; outline-regexp: ";;; *"
;; End:
