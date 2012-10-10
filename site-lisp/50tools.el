;;;; some useful packages

(autoload 'rfc "rfc-util"
  "Prompt for an rfc number and display it in a new buffer." t)

;;; (require 'pulse-settings)


;;; thing-cmds
;; ‘C-M-SPC’ – `mark-thing’ (overwrites the standard binding for ‘mark-sexp’; `sexp’ is the default thing type)
;; `M-@’ – `cycle-thing-region’ (overwrites the standard binding for ‘mark-word’)
;; ‘C-M-U’ (aka ‘C-M-S-u’) – `mark-enclosing-sexp’
;; ‘C-M-F’ (aka ‘C-M-S-f’) – `mark-enclosing-sexp-forward’
;; ‘C-M-B’ (aka ‘C-M-S-b’) – `mark-enclosing-sexp-backward’
;; ‘C-x down’ – `next-visible-thing-repeat’ (Emacs 22 and later)
;; ‘C-x up’ – `previous-visible-thing-repeat’ (Emacs 22 and later)
;; (thgcmd-bind-keys)
;; (global-set-key (kbd "C-M-?") 'mark-thing) ; vs `mark-sexp', how to press?
;; (global-set-key (kbd "M-@") 'cycle-thing-region) ; vs `mark-word'


;;; expand-region
(global-set-key (kbd "C-=") 'er/expand-region)



;;; let ^L looks beautiful
;; (require 'pp-c-l)
;; (pretty-control-l-mode 1)

;; (load "~/.emacs.d/packages/nxhtml/autostart.el")
;; (setq nxhtml-skip-welcome t)

;; (require 'misc-fns)

;;; beautify symbol mode will cause symbol indent incorrect
(require 'beautify-symbol nil t)
;; (global-beautify-symbol-mode 1)

;;; key-chord
(require 'key-chord)
(key-chord-mode 1)
(setq key-chord-one-key-delay 0.12)    ; default 0.2
(setq key-chord-two-keys-delay 0.18)    ; default 0.1


(key-chord-define-global "''" "`'\C-b")
(key-chord-define-global "RR" 'anything-resume)
(key-chord-define-global "HH" 'woman)
(key-chord-define-global "JJ" 'anything)
(key-chord-define-global "OO" 'anything-occur)
(key-chord-define-global "RF" 'pl/ido-choose-from-recentf)

;;; space-chord
(require 'space-chord)
(setq space-chord-delay 0.08)           ; default 0.08
(space-chord-define-global "f" 'find-file)
(space-chord-define-global "r" 'remember)
(space-chord-define-global "b" 'switch-to-buffer)
(space-chord-define-global "c" 'calendar)


;;; linkd
(autoload 'linkd-mode "linkd" "Create or follow hypertext links." t)

;;; uptime
(require 'uptimes nil t)

;;; recentf
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-saved-items 500)
(setq recentf-max-menu-items 60)
(setq recentf-auto-cleanup 'never) ;;To protect tramp
;;The remote connection is NOT opened
(add-to-list 'recentf-keep 'file-remote-p)
(add-to-list 'recentf-exclude "^/su:")
(add-to-list 'recentf-exclude "^/sudo:")
(add-to-list 'recentf-exclude "\\.gpg$")
;;(recentf-exclude (quote (".ftp:.*" ".sudo:.*")))
;;(recentf-keep (file-remote-p file-readable-p))

;;; unicad
;; Unicad is short for Universal Charset Auto Detector. It is an Emacs-Lisp port of Mozilla Universal Charset Detector.
;; Unicad helps Emacs to guess the correct coding system when opening a file. It's designed to work automatically and
;; quietly without user interaction.
;; (require 'unicad nil t)
;; (unicad-enable)
;; (add-hook 'kill-emacs-hook 'unicad-disable)

;;; ascii table
(autoload 'ascii-table-show "ascii-table-show" "Create a buffer and print the ascii table" t)

;;; line numbers
(eval-after-load "linum"
  '(progn
     (set-face-foreground 'linum "#5cb2b3")
     (set-face-background 'linum "#222222")

     (setq linum-format 'pl/linum-format) ; dynamic
     (defun pl/linum-format (line)
       (propertize (format (let ((w (length (number-to-string
                                             (count-lines
                                              (point-min)
                                              (point-max))))))
                             (concat "%" (number-to-string w) "d|"))
                           line)
                   'face 'linum))))


;;; highlight current line in buffer
(require 'hl-line)
(eval-after-load "hl-line"
  '(progn
     (global-hl-line-mode t)
     (set-face-background 'hl-line "#222222")))

;;; buffer action
;; http://xwl.appspot.com/ref/buffer-action.el
(autoload 'buffer-action-compile "buffer-action")
(autoload 'buffer-action-run "buffer-action")
(setq buffer-action-table '((c-mode    "gcc -Wall -ggdb -Wextra %f -lm -o %n" "%n" "./%n")
                            (c++-mode  "g++ %f -lm -o %n" "%n" "./%n")
                            (java-mode "javac %n" "%n.class" "java %n")
                            (makefile-mode "make" nil nil)
                            ("\\.pl$" "perl -cw %f" nil "perl -s %f")
                            ("\\.php$" nil nil "php %f")
                            ("\\.tex$" "latex %f" "%n.dvi" "xdvi %n.dvi &")
                            (texinfo-mode (lambda ()
                                            (save-excursion
                                              ;; (texinfo-make-menu)
                                              (texinfo-all-menus-update)
                                              (texinfo-every-node-update)
                                              (save-buffer))
                                            (makeinfo-buffer))
                             "%n.info"
                             (lambda ()
                               (Info-revert-find-node
                                (replace-regexp-in-string
                                 "\\.texinfo*$" ".info" (buffer-action-replace "%F"))
                                (makeinfo-current-node))))
                            (emacs-lisp-mode (lambda ()
                                               (byte-compile-file (buffer-action-replace "%f")))
                             "%n.elc"
                             eval-buffer)
                            ("\\.info$" nil nil (lambda () (info (buffer-file-name))))
                            ("\\.dot$" "dot -Tjpg %f -o %n.jpg" "%n.png" "qiv %f &")))


;;; uniquify
;; add parent directory name to the buffers of the same name
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "\\`\\*")
;; (toggle-uniquify-buffer-names)

;;; header2
;;(load "~/.emacs.d/elisp/header2")
;; Update file headers when write files.
;;(add-hook 'write-file-hooks 'update-file-header)
;; Create headers for file buffers in my favorite modes.
;;(add-hook 'emacs-lisp-mode-hook 'auto-make-header)
;;(add-hook 'c-mode-common-hook   'auto-make-header)

;; (defsubst pl/header-author ()
;;   "Insert current user's name (`user-full-name') as this file's author."
;;   (insert header-prefix-string "Author: " (user-full-name) " <deftsp@gmail.com>" "\n"))

;; (defsubst pl/header-svn-keyword ()
;;   "Insert $Id$."
;;   (insert header-prefix-string "$Id$" "\n"))

;; (setq make-header-hook '(
;;                          ;;header-mode-line
;;                          header-title
;;                          pl/header-svn-keyword
;;                          ;;header-blank
;;                          ;;header-file-name
;;                          header-description
;;                          ;;header-status
;;                          ;;header-author
;;                          pl/header-author
;;                          ;;header-maintainer
;;                          ;;header-copyright
;;                          header-creation-date
;;                          ;;header-rcs-id
;;                          ;;header-version
;;                          ;;header-sccs
;;                          ;;header-modification-date
;;                          ;;header-modification-author
;;                          ;;header-update-count
;;                          ;;xbheader-url
;;                          ;;header-keywords
;;                          ;;header-compatibility
;;                          ;;header-blank
;;                          ;;header-lib-requires
;;                          ;;header-end-line
;;                          ;;header-commentary
;;                          ;;header-blank
;;                          ;;header-blank
;;                          ;;header-blank
;;                          ;;header-end-line
;;                          ;;header-history
;;                          ;;header-blank
;;                          ;;header-blank
;;                          ;; header-rcs-log
;;                          header-end-line
;;                          ;;header-free-software
;;                          ;;header-code
;;                          ;;header-eof
;;                          ))


;;; Moving lines
;; Many times you'll kill a line with the intention of pasting it back a couple of lines up/below.
(global-set-key (kbd "H-k") 'pl/move-line-up)
(global-set-key (kbd "H-j") 'pl/move-line-down)

(defun pl/move-line (&optional n)
  "Move current line N (1) lines up/down leaving point in place."
  (interactive "p")
  (when (null n)
    (setq n 1))
  (let ((col (current-column)))
    (beginning-of-line)
    (next-line 1)
    (transpose-lines n)
    (previous-line 1)
    (move-to-column col)))

(defun pl/move-line-up (n)
  "Moves current line N (1) lines up leaving point in place."
  (interactive "p")
  (pl/move-line (if (null n) -1 (- n))))

(defun pl/move-line-down (n)
  "Moves current line N (1) lines down leaving point in place."
  (interactive "p")
  (pl/move-line (if (null n) 1 n)))
;;Moving lines ends there---------------------------------------------------------------------------------------


;;; Open new line
(defun pl/open-line-with-indent ()
  "open-line with indent without moving cursor."
  (interactive)
  (save-excursion
    (open-line 1)
    (next-line 1)
    (indent-according-to-mode)
    (next-line -1)))

(global-set-key (kbd "C-o") 'pl/open-line-with-indent)


;; behave like vi's o command
(defun pl/vi-open-next-line (arg)
  "Move to the next line (like vi) and then opens a line."
  (interactive "p")
  (end-of-line)
  (open-line arg)
  (next-line 1)
  (indent-according-to-mode))

(global-set-key (kbd "C-M-o") 'pl/vi-open-next-line)  ; `C-M-o' default bind to split-line'


;; behave like vi's O command
;; (global-set-key (kbd "M-O") 'pl/vi-open-above-line)
;; (defun pl/vi-open-above-line (arg)
;;   "Open a new line before the current one."
;;   (interactive "p")
;;   (beginning-of-line)
;;   (open-line arg)
;;   (indent-according-to-mode))

;;; Shell command on region
;; A somewhat insanely powerful trick, evaluate a region via a shell command and replace the region with the resulting
;; output. Normally you would access this command via C-u M-| but since we're trying to optimize things a bit:

;; (local-set-key [(meta ?!)] 'custom-shell-command-on-region)

;; (defun custom-shell-command-on-region nil
;;   "Replace region with ``shell-command-on-region''.
;; By default, this will make mark active if it is not and then
;; prompt you for a shell command to run and replaces region with
;; the results.  This is handy for doing things like getting
;; external program locations in scripts and running grep and
;; whatnot on a region."
;;   (interactive)
;;   (save-excursion
;;     (if (equal mark-active nil)
;;         (push-mark nil nil -1))
;;     (setq string
;;           (read-from-minibuffer "Shell command on region: " nil nil nil
;;                                 'shell-command-history))
;;     (shell-command-on-region (region-beginning) (region-end) string -1)
;;                                         ; Get rid of final newline cause I normally did by hand anyway.
;;     (delete-char -1)))

;;; visible mark
;; (require 'visible-mark)
;; (global-visible-mark-mode -1)

;; visible bookmarks in buffer
;; To load the repository when bm is loaded set the variable `bm-restore-repository-on-load' to t,*before* loading bm
;; (setq bm-restore-repository-on-load t)
;; (require 'bm)
;; (setq bm-marker 'bm-marker-right
;;       bm-highlight-style 'bm-highlight-only-fringe
;;       bm-repository-file "~/.emacs.d/.bm-repository")

;; ;; (setq-default bm-buffer-persistence nil)

;; (eval-after-load "bm"
;;   '(progn
;;     (set-face-attribute 'bm-fringe-face nil :foreground "#6666aa" :background "DarkOrange1")
;;     (set-face-attribute 'bm-fringe-persistent-face nil :foreground "Black":background "DarkOrange1")))

;; ;; If you are using desktop or other packages that restore buffers on start up, bookmarks will not be restored. Loading
;; ;; the repository from file when on start up.
;; (add-hook' after-init-hook 'bm-repository-load)

;; ;; Restoring bookmarks when on file find.
;; (add-hook 'find-file-hooks 'bm-buffer-restore)

;; ;; Saving bookmark data on killing a buffer
;; (add-hook 'kill-buffer-hook 'bm-buffer-save)

;; ;; Saving the repository to file when on exit.
;; ;; kill-buffer-hook is not called when Emacs is killed, so we
;; ;; must save all bookmarks first.
;; (add-hook 'kill-emacs-hook '(lambda nil
;;                              (bm-buffer-save-all)
;;                              (bm-repository-save)))

;; ;; Update bookmark repository when saving the file.
;; (add-hook 'after-save-hook 'bm-buffer-save)

;; ;; Restore bookmarks when buffer is reverted.
;; (add-hook 'after-revert-hook 'bm-buffer-restore)

;;; bookmark+
(eval-after-load "bookmark+"
  '(progn
    (setq bmkp-last-as-first-bookmark-file nil)
    ;; (bmkp-global-auto-idle-bookmark-mode 1)
    ;; it seems  highlight automatically not work
    (setq bmkp-auto-light-when-set 'all-in-buffer
     bmkp-auto-light-when-jump 'all-in-buffer)

    (setq bmkp-light-style-autonamed 'lfringe
     bmkp-light-style-non-autonamed 'lfringe)

    (when (fboundp 'define-fringe-bitmap)
      (define-fringe-bitmap 'pl/marker-left   [#x00 #x00 #xFC #xFE #x0F #xFE #xFC #x00])
      (define-fringe-bitmap 'pl/marker-right  [#x00 #x00 #x3F #x7F #xF0 #x7F #x3F #x00])
      (setq bmkp-light-left-fringe-bitmap 'pl/marker-left)
      (setq bmkp-light-right-fringe-bitmap 'pl/marker-right))

    (set-face-attribute 'bmkp-light-fringe-autonamed nil :foreground "#222222" :background "#778899")
    (set-face-attribute 'bmkp-light-fringe-non-autonamed nil :foreground "Black":background "DarkOrange1")))

;;; bookmark history stack
;; Use the following to maintain a stack of buffer locations which you can use as a simple bookmarking system.
(global-set-key (kbd "C-c r SPC") 'point-stack-push)
(global-set-key (kbd "C-c r p") 'point-stack-pop)


(defvar point-stack nil)
(defun point-stack-push ()
  "Push current location and buffer info onto stack."
  (interactive)
  (message "Location marked.")
  (setq point-stack (cons (list (current-buffer) (point)) point-stack)))

(defun point-stack-pop ()
  "Pop a location off the stack and move to buffer"
  (interactive)
  (if (null point-stack)
      (message "Stack is empty.")
      (switch-to-buffer (caar point-stack))
      (goto-char (cadar point-stack))
      (setq point-stack (cdr point-stack))))

;;; Turns tabs into spaces
;; (defun pl/untabify ()
;;   "My untabify function as discussed and described at
;;  http://www.jwz.org/doc/tabs-vs-spaces.html
;;  and improved by Claus Brunzema:
;;  - return nil to get `write-contents-hooks' to work correctly
;;    (see documentation there)
;;  - `make-local-hook' instead of `make-local-variable'
;;  - when instead of if"
;;   (save-excursion
;;     (goto-char (point-min))
;;     (when (search-forward "\t" nil t)
;;       (untabify (1- (point)) (point-max)))
;;     nil))
;; (add-hook 'some-mode-hook
;;           '(lambda ()
;;             (make-local-hook 'write-contents-hooks)
;;             (add-hook 'write-contents-hooks 'pl/untabify nil t)))


;;; undoc
;;; Enable undoc; a mode which edits MS Word .doc files.
;;<http://www.ccs.neu.edu/home/guttman/undoc.el>
(autoload 'undoc "undoc" "A minor mode which kills MS Word files dead." t)
(autoload 'undoc-current-buffer "undoc" "" t)
(autoload 'undoc-region-after-mime-decode "undoc" "" t)

;;; The normal man command doesn't allow tab-completion.
;;iman is a wrapper around man which does so allow.
;;<http://homepage1.nifty.com/bmonkey/emacs/elisp/iman.el>
(autoload 'iman "iman" "Call the viewers of man pages and GNU Info with completion." t nil)

;; A game for fast typers! A game for Emacs!
(autoload 'typing-of-emacs "typing" "The Typing Of Emacs, a game." t)

;; Sets +x on scripts stating with a shebang
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;;; shell-toggle.el
;; shell-toggle-patched.el eshell port.
(autoload 'shell-toggle "shell-toggle"
  "Toggles between the *shell* buffer and whatever buffer you are editing." t)
(autoload 'shell-toggle-cd "shell-toggle"
  "Pops up a shell-buffer and insert a \"cd <file-dir>\" command." t)
(global-set-key (kbd "C-c g S") 'shell-toggle)    ;see also shell-toggle
(global-set-key (kbd "C-c g s") 'shell-toggle-cd) ;see also shell-toggle-cd


;;; misc func
;; (defun pl/value-to-string (value)
;;   "Convert VALUE to string.
;; This function will automatically identify the type of VALUE, and invoke
;; the appropiate conversion function"
;;   (cond ((symbolp value)
;;          (symbol-name value))
;;         ((numberp value)
;;          (number-to-string value))
;;         (t
;;          (error "Cannot convert value to string."))))


;; (defun pl/read-lines-in-buffer (&optional buffer)
;;   "Return list of lines in current buffer.
;; If BUFFER if non-nil, switch to BUFFER before reading lines. The list returned
;; will be in reverse with regard to the sequence of lines in the buffer read.
;; Empty lines will not be ignored."
;;   (save-excursion
;;     (when buffer
;;       (set-buffer buffer))
;;     (let (stringlist)
;;       ;; Start from beginning of buffer, remembering to save point.
;;       (goto-char (point-min))
;;       (while (not (eobp))
;;         ;; Because we push the new line to the front of the list, and we start
;;         ;; from the beginning of the buffer, the list will be backwards.
;;         ;; Should this be fixed?
;;         (push (buffer-substring-no-properties
;;                (line-beginning-position)
;;                (line-end-position))
;;               stringlist)
;;         (forward-line))
;;       stringlist)))

;;; convert a buffer from dos ^M end of lines to unix end of lines
;; dos <--> unix
(defun pl/dos2unix ()
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\r" nil t) (replace-match "")))

;; vice versa
(defun pl/unix2dos ()
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\n" nil t) (replace-match "\r\n")))

;;; gse number
(require 'gse-number-rect)
(global-set-key "\C-xru" 'gse-number-rectangle)

;;; yank secondary
(defun pl/yank-secondary ()
  "Insert the secondary selection at point.
  Moves point to the end of the inserted text.  Does not change mark."
  (interactive) (insert (x-get-selection 'SECONDARY)))


;;; Copy current line to next line
(global-set-key (kbd "<M-S-return>") 'pl/dup-line-down)

(defun pl/ue-select-line-down ()
  "like Shift+down in UltraEdit."
  (interactive)
  (let ((s (point)))
    (setq next-line-add-newlines t)
    (next-line 1)
    (setq next-line-add-newlines nil)
    (kill-new (buffer-substring s (point)))))

(defun pl/dup-line-down ()
  "duplicate this line at next line"
  (interactive)
  (let ((c (current-column)))
    (beginning-of-line)
    (pl/ue-select-line-down)
    (beginning-of-line)
    (yank)
    (previous-line 1)
    (move-to-column c)))


(defun pl/strip-all-blank-lines ()
  "Strip all blank lines in current buffer."
  (interactive)
  (save-excursion
    (while (re-search-forward "^[ \t]*\n" nil t)
      (replace-match "" t t))))

;; resolve file names
(defun pl/resolve-file-name (file type)
  "Resolve file name in various ways.

file is the abosolute filename.

type stands for different kinds of resolve.

 F  absolute pathname            ( /usr/local/bin/netscape.bin )
 f  file name without directory  ( netscape.bin )
 n  file name without extention  ( netscape )
 e  extention of file name       ( bin )"
  (cond
    ((string= type "F") file)
    ((string= type "f") (file-name-nondirectory file))
    ((string= type "n") (file-name-sans-extension (file-name-nondirectory file)))
    (t (file-name-extension file))))

;;; insert line number before each line.
(defun pl/numerate-lines ()
  "Insert line numbers into buffer"
  (interactive)
  (save-excursion
    (let ((max (count-lines (point-min) (point-max)))
          (line 1))
      (goto-char (point-min))
      (while (<= line max)
        (insert (format "%4d " line))
        (beginning-of-line 2)
        (setq line (+ line 1))))))


;;; a simple way of aligning columns
(defun pl/align-cols (start end max-cols)
  "Align text between point and mark as columns.  Columns are separated by
whitespace characters.  Prefix arg means align that many columns. (default
is all)"
  (interactive "r\nP")
  (save-excursion
    (let ((p start)
          pos
          end-of-line
          word
          count
          (max-cols (if (numberp max-cols) (max 0 (1- max-cols)) nil))
          (pos-list nil)
          (ref-list nil))

      ;; find the positions
      (goto-char start)
      (while (< p end)
        (beginning-of-line)
        (setq count 0)
        (setq end-of-line (save-excursion (end-of-line) (point)))
        (re-search-forward "^\\s-*" end-of-line t)
        (setq pos (current-column))     ;start of first word
        (if (null (car ref-list))
            (setq pos-list (list pos))
            (setq pos-list (list (max pos (car ref-list))))
            (setq ref-list (cdr ref-list)))
        (while (and (if max-cols (< count max-cols) t)
                    (re-search-forward "\\s-+" end-of-line t))
          (setq count (1+ count))
          (setq word (- (current-column) pos))
          ;; length of next word including following whitespaces
          (setq pos (current-column))
          (if (null (car ref-list))
              (setq pos-list (cons word pos-list))
              (setq pos-list (cons (max word (car ref-list)) pos-list))
              (setq ref-list (cdr ref-list))))
        (while ref-list
          (setq pos-list (cons (car ref-list) pos-list))
          (setq ref-list (cdr ref-list)))
        (setq ref-list (nreverse pos-list))
        (forward-line)
        (setq p (point)))

      ;; align the cols starting with last row
      (setq pos-list (copy-sequence ref-list))
      (setq start
            (save-excursion (goto-char start) (beginning-of-line) (point)))
      (goto-char end)
      (beginning-of-line)
      (while (>= p start)
        (beginning-of-line)
        (setq count 0)
        (setq end-of-line (save-excursion (end-of-line) (point)))
        (re-search-forward "^\\s-*" end-of-line t)
        (goto-char (match-end 0))
        (setq pos (nth count pos-list))
        (while (< (current-column) pos)
          (insert-char ?\040 1))
        (setq end-of-line (save-excursion (end-of-line) (point)))
        (while (and (if max-cols (< count max-cols) t)
                    (re-search-forward "\\s-+" end-of-line t))
          (setq count (1+ count))
          (setq pos   (+  pos (nth count pos-list)))
          (goto-char (match-end 0))
          (while (< (current-column) pos)
            (insert-char ?\040 1))
          (setq end-of-line (save-excursion (end-of-line) (point))))
        (forward-line -1)
        (if (= p (point-min)) (setq p (1- p))
            (setq p (point)))))))

;;; count Chinese, English words
(defun pl/count-ce-word (beg end)
  "Count Chinese and English words in marked region."
  (interactive "r")
  (let ((cn-word 0)
        (en-word 0)
        (total-word 0)
        (total-byte 0))
    (setq cn-word (count-matches "\\cc" beg end)
          en-word (count-matches "\\w+\\W" beg end))
    (setq total-word (+ cn-word en-word)
          total-byte (+ cn-word (abs (- beg end))))
    (message (format "Total: %d (cn: %d, en: %d) words, %d bytes."
                     total-word cn-word en-word total-byte))))

;;; pl/word-count-analysis (how many times a word has appeared).
(defun pl/word-count-analysis (start end)
  "Count how many times each word is used in the region.
    Punctuation is ignored."
  (interactive "r")
  (let (words)
    (save-excursion
      (goto-char start)
      (while (re-search-forward "\\w+" end t)
        (let* ((word (intern (match-string 0)))
               (cell (assq word words)))
          (if cell
              (setcdr cell (1+ (cdr cell)))
              (setq words (cons (cons word 1) words))))))
    (when (interactive-p)
      (message "%S" words))
    words))


(defun pl/list-ref (list ref)
  "Return the ref-th element of list."
  (if (= ref 0)
      (car list)
      (pl/list-ref (cdr list) (1- ref))))

(defun pl/info (file)
  (interactive
   (list (read-file-name "info: ")))
  (info file))


(defun pl/delete-line (&optional arg)
  "Delete the rest of the current line; if no nonblanks there, delete thru newline.
With prefix argument, delete that many lines from point.
Negative arguments delete lines backward.
With zero argument, deletes the text before point on the current line.

Note its difference between `pl/delete-line' and `kill-line' is
that, the deleted contents won't be inserted to the `kill-ring'."
  (if arg
      (dotimes (i arg)
        (delete-region (point) (save-excursion (forward-line)
                                               (point))))
      (if (eolp)
          (delete-region (point) (save-excursion (forward-line)
                                                 (point)))
          (delete-region (point) (save-excursion (end-of-line)
                                                 (point))))))

;; (defun pl/soft-kill-ring-save (beg end)
;;   "Same as `kill-ring-save' except it will convert hard newlines to soft newlines.
;; This could be useful for copying texts from Emacs and pasting it to blog websites."
;;   (interactive "r")
;;   (let ((content (buffer-substring-no-properties beg end)))
;;     (with-temp-buffer
;;       (insert content)
;;       (insert "\n")
;;       (goto-char (point-min))
;;       (move-beginning-of-line 2)
;;       (while (not (eobp))
;;         (if (looking-at "\n")
;;             (move-beginning-of-line 3)
;;           (if (looking-at "[[:ascii:]]")
;;               (progn (backward-delete-char-untabify 1)
;;                      (insert " "))
;;             (backward-delete-char-untabify 1))
;;           (move-beginning-of-line 2)))
;;       (copy-region-as-kill (point-min) (point-max)))))




;;----------------------------------------------------------------------------------------------------
;;; Cool utility function to refresh all open buffers
;;----------------------------------------------------------------------------------------------------
(defun pl/revert-all-buffers()
  "Refreshs all open buffers from their respective files"
  (interactive)
  (let* ((list (buffer-list))
         (buffer (car list)))
    (while buffer
      (if (string-match "\\*" (buffer-name buffer))
          (progn
            (setq list (cdr list))
            (setq buffer (car list)))
        (progn
          (set-buffer buffer)
          (revert-buffer t t t)
          (setq list (cdr list))
          (setq buffer (car list))))))
  (message "Refreshing open files"))

;;; xmsg
;; (defun pl/xmsg (string &optional geom process-name)
;;   "Invoke xmessage(1) to display message STRING.
;; However, do nothing if `window-system' is not `x'.  Other args
;; are optional: GEOM is the window geometry (default \"+0+0\");
;; PROCESS-NAME is the process object's name (default \"xmessage\")."
;;   (interactive "sMessage: ")
;;   (when (eq 'x window-system)
;;     (start-process (or process-name "xmessage") nil "xmessage"
;;                    "-geometry" (or geom "+0+0")
;;                    (or string "Emacs says hi!"))))

;; It's often nice to find the true path to a file or directory.
;; (defun pl/resolve-sym-link ()
;;   "Replace the string at the point with the true path."
;;   (interactive)
;;   (beginning-of-line)
;;   (let* ((file (buffer-substring (point)
;;                                  (save-excursion (end-of-line) (point))))
;;          (file-dir (file-name-directory file))
;;          (file-true-dir (file-truename file-dir))
;;          (file-name (file-name-nondirectory file)))
;;     (delete-region (point) (save-excursion (end-of-line) (point)))
;;     (insert (concat file-true-dir file-name))))

;; (define-key minibuffer-local-completion-map (kbd "C-r") 'resolve-sym-link)

;;;_. boxquote
(require 'boxquote nil t)
(global-set-key (kbd "C-c b y")   'boxquote-yank)
(global-set-key (kbd "C-c b r")   'boxquote-region)
(global-set-key (kbd "C-c b u")   'boxquote-unbox-region)
(global-set-key (kbd "C-c b t")   'boxquote-title)
(global-set-key (kbd "C-c b i")   'boxquote-insert-file)
(global-set-key (kbd "C-c b k")   'boxquote-kill)
(global-set-key (kbd "C-c b s")   'boxquote-shell-command)

(global-set-key (kbd "C-c b b")   'boxquote-buffer)
(global-set-key (kbd "C-c b p")   'boxquote-paragraph)
(global-set-key (kbd "C-c b n")   'boxquote-narrow-to-boxquote)

(global-set-key (kbd "C-c b w")   'boxquote-where-is)
(global-set-key (kbd "C-c b d f") 'boxquote-describe-function)
(global-set-key (kbd "C-c b d k") 'boxquote-describe-key)
(global-set-key (kbd "C-c b d v") 'boxquote-describe-variable)

;;; fuzz-match.el
(require 'fuzzy-match)

(autoload 'lisp-spell-symbol "fuzzy-match"
  "Perform spell checking on Lisp symbol preceding point." t)
;; This will define the key M-# (ESC #) to call `lisp-spell-symbol'.
(define-key esc-map "#" 'lisp-spell-symbol)

;;; replace-recent-char
(global-set-key (kbd "M-R")  'pl/replace-recent-char)
(defun pl/replace-recent-char ()
  "Replace-recent-character is interactive function for quick corrections of
recenlty typed text. It first prompts for character to search backwards. If
such character is found, following options are shown:
1, repeat the character to search in previous text.
2, M-R for delete of the found character.
3, C-t for trasposition of the found and the following character.
4, TAB for promt for character to insert after the found character.
5, ESC for no operation.
6, Any other insertable character will replace found character."
  (interactive)
  (let* ((repev (read-char "Replace back character:" t))
         (repstr (string repev)))
    (labels
        ((check () (if (fboundp 'flyspell-word) (flyspell-word)))
         (rec ()
              (save-excursion
                (let ((point (search-backward repstr (point-at-bol -3) t)))
                  (if point
                      (let (repwithev
                            (ov (make-overlay point (1+ point))))
                        (overlay-put ov 'face 'isearch)
                        (overlay-put ov 'priority 1)
                        (unwind-protect
                            (setq repwithev
                                  (read-char "Replace with (repeat  previous, M-R  delete, C-t  transpose, TAB  insert):" t))
                          (delete-overlay ov))
                        (cond ((equal repwithev (event-convert-list '(meta ?R)))
                               (delete-char 1)
                               ;; (check)
                               (message (format "Character \"%s\" deleted." repstr)))
                              ((equal repwithev (event-convert-list '(control ?t)))
                               (forward-char)
                               (transpose-chars 1)
                               ;; (check)
                               (message "Transposed."))
                              ((equal repwithev ?\t)
                               (forward-char)
                               (insert-char (read-char "Character to insert after match:" t) 1 t)
                               ;; (check)
                               (message "Insert."))
                              ((equal repwithev ?\e)
                               (message "Replace aborted."))
                              ((equal repwithev repev)
                               (rec))
                              (t
                               (delete-char 1)
                               (insert-char repwithev 1 t)
                               ;; (check)
                               (message
                                (format "Replace \"%s\" -> \"%s\" done." repstr (string repwithev))))))
                    (message (format "\"%s\" is not recent." repstr)))))))
      (rec))))

;;; (dec|inc)rement number at point
;; The following functions allow you increment or decrement what they think is a number under point:
;; (defun pl/increment-number-at-point (&optional amount)
;;   "Increment the number under point by `amount'"
;;   (interactive "p")
;;   (let ((num (number-at-point)))
;;     (when (numberp num)
;;       (let ((newnum (+ num amount))
;;             (p (point)))
;;         (save-excursion
;;           (skip-chars-backward "-.0123456789")
;;           (delete-region (point) (+ (point) (length (number-to-string num))))
;;           (insert (number-to-string newnum)))
;;         (goto-char p)))))

;; (defun pl/decrement-number-at-point (&optional amount)
;;   (interactive "p")
;;   "Decrement the number under point by `amount'"
;;   (pl/increment-number-at-point (- (abs amount))))
;; I don't use the arrow keys so I have the above functions bound like this:
;; (define-key global-map (kbd "<C-up>") 'pl/increment-number-at-point)
;; (define-key global-map (kbd "<C-down>") 'pl/decrement-number-at-point)

;;; incr-dwim
(autoload 'incr-dwim "incr"
  "Use `incr-try-alist' to find most possible method to increase thing at point or region" t)
(autoload 'decr-dwim "incr" t)
(eval-after-load "incr"
  '(setq incr-enable-feature
    '(number rotate roman date han-number)))

(global-set-key (kbd "C-c x") 'incr-dwim)
(global-set-key (kbd "C-c z") 'decr-dwim)


;;; ff-find-other-file and friends
(eval-after-load "find-file"
  '(progn
     (push '("\\.scm\\'" ("#.scm")) cc-other-file-alist)
     (push '("\\#.scm\\'" (".scm")) cc-other-file-alist)
     (push ".m" (cadr (assoc "\\.h\\'" cc-other-file-alist)))
     (push ".mm" (cadr (assoc "\\.h\\'" cc-other-file-alist)))
     (push '("\\.m\\'" (".h")) cc-other-file-alist)
     (push '("\\.mm\\'" (".h")) cc-other-file-alist)))


(defadvice ff-get-file-name (around ff-get-file-name-framework
                                    (search-dirs
                                     fname-stub
                                     &optional suffix-list))
  "Search for Mac framework headers as well as POSIX headers."
  (or
   (if (string-match "\\(.*?\\)/\\(.*\\)" fname-stub)
       (let* ((framework (match-string 1 fname-stub))
              (header (match-string 2 fname-stub))
              (fname-stub (concat framework ".framework/Headers/" header)))
         ad-do-it))
   ad-do-it))
(ad-enable-advice 'ff-get-file-name 'around 'ff-get-file-name-framework)
(ad-activate 'ff-get-file-name)

(when (eq system-type 'darwin)
  (setq cc-search-directories '("." "../include" "/usr/include" "/usr/local/include/*"
                                "/System/Library/Frameworks" "/Library/Frameworks")))

(eval-after-load "scheme"
  '(define-key scheme-mode-map (kbd "C-c S") 'ff-find-other-file))

(eval-after-load "cc-mode"
  '(define-key c-mode-base-map (kbd "C-c S") 'ff-find-other-file))



;;; finding non ascii characters
(defun pl/find-first-non-ascii-char ()
  "Find the first non-ascii character from point onwards."
  (interactive)
  (let (point)
    (save-excursion
      (setq point
            (catch 'non-ascii
              (while (not (eobp))
                (or (eq (char-charset (following-char))
                        'ascii)
                    (throw 'non-ascii (point)))
                (forward-char 1)))))
    (if point
        (goto-char point)
      (message "No non-ascii characters."))))


;;; who calls
(autoload 'who-calls "who-calls" "Display all known callers of a function" t)
(define-key emacs-lisp-mode-map "\C-c\C-w" 'who-calls)


;;; Insert a path into the current buffer
(defun pl/insert-path (file)
  "insert file"
  (interactive "FPath: ")
  (insert (expand-file-name file)))


;;; kill save rectangle
;; FIXME: it will make file be changed not line M-w
(defun kill-save-rectangle (start end &optional fill)
  "Save the rectangle as if killed, but don't kill it.  See
`kill-rectangle' for more information."
  (interactive "r\nP")
  (kill-rectangle start end fill)
  (goto-char start)
  (yank-rectangle))

;; I bound it to C-x r M-k to compliment C-x r k (just like M-w compliments C-w):
(global-set-key (kbd "C-x r M-k") 'kill-save-rectangle)

;;; smex
(eval-after-load "smex"
  '(progn
     ;; (smex-initialize)                  ; el-get has do it
     (global-set-key (kbd "M-x") 'smex)
     ;; (global-set-key (kbd "M-X") 'smex-major-mode-commands) ;
     ;; This is your old M-x.
     (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)))


;;; ace jump
;; 'C-c SPC' is used by Org mode
;; gud-break is bound to C-c C-b, C-x SPC, C-x C-a C-b.
;; both of 'M-g M-g' and 'M-g g' are bound to goto-line
(eval-after-load "ace-jump-mode"
  '(progn
    (ace-jump-mode-enable-mark-sync)
    ;; after enable ace jump mode mark sync, use `C-u C-SPC' is enough
    ;; (define-key global-map (kbd "C-x SPC") 'ace-jump-mode-pop-mark)
    (define-key global-map (kbd "M-g g") 'ace-jump-mode)))


;;; jump-char
;; <char> :: move to the next match in the current direction.
;; ; :: next match forward (towards end of buffer)
;; , :: next match backward (towards beginning of buffer)
;; C-c C-c :: invoke ace-jump-mode if available (also <M-/>)
(require 'jump-char)
(global-set-key (kbd "ESC ESC j") 'jump-char-forward)
(global-set-key (kbd "ESC ESC J") 'jump-char-backward)

;;; cclookup
;; add cclookup to your loadpath, ex) ~/.emacs.d/cclookup
(setq cclookup-dir "~/.emacs.d/lisp/cclookup")
(add-to-list 'load-path cclookup-dir)

;; load cclookup when compile time
(eval-when-compile (require 'cclookup))

;; set executable file and db file
(setq cclookup-program (concat cclookup-dir "/cclookup.py"))
(setq cclookup-db-file (concat cclookup-dir "/cclookup.db"))

;; to speedup, just load it on demand
(autoload 'cclookup-lookup "cclookup"
  "Lookup SEARCH-TERM in the Python HTML indexes." t)

(autoload 'cclookup-update "cclookup"
  "Run cclookup-update and create the database at `cclookup-db-file'." t)

;;; per-window-point
(require 'per-window-point)
(pwp-mode 1)

;;; mutiple cursors
;; From active region to multiple cursors:
(global-set-key (kbd "ESC ESC c ESC ESC c") 'mc/edit-lines)
(global-set-key (kbd "ESC ESC c C-e") 'mc/edit-ends-of-lines)
(global-set-key (kbd "ESC ESC c C-a") 'mc/edit-beginnings-of-lines)

;; Rectangular region mode
;; (global-set-key (kbd "H-SPC") 'set-rectangular-region-anchor)

;; Mark more like this
;; (global-set-key (kbd "M-æ") 'mc/mark-all-like-this)
;; (global-set-key (kbd "C-å") 'mc/mark-previous-like-this)
;; (global-set-key (kbd "C-æ") 'mc/mark-next-like-this)
;; (global-set-key (kbd "C-Æ") 'mc/mark-more-like-this-extended)
;; (global-set-key (kbd "M-å") 'mc/mark-all-in-region)


;; (require 'rename-sgml-tag)              ;
;; (define-key sgml-mode-map (kbd "C-c C-r") 'rename-sgml-tag)

;; (require 'js2-rename-var)
;; (define-key js2-mode-map (kbd "C-c C-r") 'js2-rename-var)


;;; docsetutil
(require 'docsetutil)
(define-key help-map "D" 'docsetutil-search) ; C-h D
;; Choose a docset to use:
;; M-x docsetutil-choose-docset

;; API search with completion:
;; M-x docsetutil-search

;; Full text search:
;; C-u M-x docsetutil-search

;;; find-file-in-project
(global-set-key (kbd "C-x f") 'find-file-in-project)
(eval-after-load "find-file-in-project"
  '(progn
    (dolist (pattern '("*.cpp" "*.hpp" "*.scm"))
      (add-to-list 'ffip-patterns pattern))))

;;; VolatileHighlights
(eval-after-load "volatile-highlights"
  '(progn
    (set-face-background 'vhl/default-face "#332244")
    (volatile-highlights-mode t)))



;;; info+
;; (eval-after-load "info+"
;;   '(progn
;;     ))

;;; diminish
(when (require 'diminish nil 'noerror)
  (eval-after-load "abbrev"
    '(diminish 'abbrev-mode "Abv"))
  (eval-after-load "yasnippet"
    '(diminish 'yas-minor-mode " Y"))
  (eval-after-load "paredit"
    '(diminish 'paredit-mode " π"))
  (eval-after-load "eldoc"
    '(diminish 'eldoc-mode ""))
  (eval-after-load "highlight-symbol"
    '(diminish 'highlight-symbol-mode " H")))




(provide '50tools)
