;;;; some useful packages

;;; thing-cmds
;; ‘ C-M-SPC ’ – `mark-thing ’ (overwrites the standard binding for ‘ mark-sexp ’; `sexp ’ is the default thing type)
;; `M-@’ – `cycle-thing-region ’ (overwrites the standard binding for ‘ mark-word ’)
;; ‘ C-M-U ’ (aka ‘ C-M-S-u ’) – `mark-enclosing-sexp ’
;; ‘ C-M-F ’ (aka ‘ C-M-S-f ’) – `mark-enclosing-sexp-forward ’
;; ‘ C-M-B ’ (aka ‘ C-M-S-b ’) – `mark-enclosing-sexp-backward ’
;; ‘ C-x down ’ – `next-visible-thing-repeat ’ (Emacs 22 and later)
;; ‘ C-x up ’ – `previous-visible-thing-repeat ’ (Emacs 22 and later)
;; (thgcmd-bind-keys)
;; (global-set-key (kbd "C-M-?") 'mark-thing) ; vs `mark-sexp', how to press?
;; (global-set-key (kbd "M-@") 'cycle-thing-region) ; vs `mark-word'

;;; expand-region
(use-package expand-region
  :defer t
  :init
  (progn
    (tl/set-leader-keys "v" 'er/expand-region)
    (setq expand-region-contract-fast-key "V"
          expand-region-reset-fast-key "r"))
  :config
  (progn
    (defun tl/mark-sexp-forward ()
      "Mark the sexp from the point to end of the sexp."
      (interactive)
      (set-mark (point))
      (forward-sexp 1)
      (exchange-point-and-mark))

    (defun tl/mark-next-symbol ()
      "Presumes that current symbol is already marked, skips over one
space and marks next symbol."
      (interactive)
      (when (use-region-p)
        (when (< (point) (mark))
          (exchange-point-and-mark))
        (let ((symbol-regexp "\\s_\\|\\sw"))
          (when (looking-at "\\ ")
            (forward-char 1)
            (skip-syntax-forward "_w")
            (exchange-point-and-mark)))))

    (defun tl/mark-lua-method-call ()
      "Mark the current symbol (including dots) and then paren to closing paren."
      (interactive)
      (let ((symbol-regexp "\\s_\\|\\sw\\|\\:"))
        (when (or (looking-at symbol-regexp)
                  (er/looking-back-on-line symbol-regexp))
          (skip-syntax-backward "_w:")
          (set-mark (point))
          (while (looking-at symbol-regexp)
            (forward-char))
          (if (looking-at "(")
              (forward-list))
          (exchange-point-and-mark))))

    ;; general expand list
    (add-to-list 'er/try-expand-list 'tl/mark-sexp-forward)

    (with-eval-after-load 'lua-mode
      (defun tl/lua-mode-expand-list-init ()
        (make-variable-buffer-local 'er/try-expand-list)
        (setq er/try-expand-list '(er/mark-word
                                   er/mark-symbol
                                   er/mark-symbol-with-prefix
                                   tl/mark-next-symbol
                                   er/mark-next-accessor
                                   tl/mark-lua-method-call
                                   er/mark-method-call
                                   er/mark-outside-quotes
                                   er/mark-inside-quotes
                                   er/mark-inside-pairs
                                   er/mark-outside-pairs
                                   er/mark-comment
                                   er/mark-defun
                                   er/mark-url
                                   er/mark-email)))
      (add-hook 'lua-mode-hook 'tl/lua-mode-expand-list-init))))

;;; let ^L looks beautiful
;; (require 'pp-c-l)
;; (pretty-control-l-mode 1)

(use-package sgml-mode
  :defer t
  :config
  (progn
    ;; for go lang template
    (key-chord-define html-mode-map "{{"  "{{}}\C-b\C-b")))

;;; linkd
(autoload 'linkd-mode "linkd" "Create or follow hypertext links." t)

;;; sensitive-mode
(autoload 'sensitive-mode "sensitive-mode" "Disables backup creation and auto saving." t)

;;; uptime
(use-package uptimes
  :defer t
  :commands (uptimes uptimes-this))

;;; unicad
;; https://www.emacswiki.org/emacs/Unicad
;; Unicad is short for Universal Charset Auto Detector. It is an Emacs-Lisp port of Mozilla Universal Charset Detector.
;; Unicad helps Emacs to guess the correct coding system when opening a file. It's designed to work automatically and
;; quietly without user interaction.
;; https://github.com/ukari/unicad/tree/master
(use-package unicad
  :diminish unicad-mode
  :config
  (unicad-mode +1))

;;; ascii table
(autoload 'ascii-table-show "ascii-table-show" "Create a buffer and print the ascii table" t)

(use-package display-line-numbers
  :init
  (setq display-line-numbers-type 'relative
        display-line-numbers-width-start nil
        display-line-numbers-widen t
        display-line-numbers-grow-only t)
  :config
  (with-eval-after-load 'evil
    (defun tl/should-enable-display-line-number ()
      (not (s-prefix-p "pdf-" (symbol-name major-mode))))

    (defun tl/display-line-number-relative ()
      (when (tl/should-enable-display-line-number)
        (setq-local display-line-numbers 'relative)))

    (defun tl/display-line-number-absolute ()
      (when (tl/should-enable-display-line-number)
        (setq-local display-line-numbers t)))

    (add-hook 'evil-insert-state-entry-hook #'tl/display-line-number-absolute)
    (add-hook 'evil-insert-state-exit-hook #'tl/display-line-number-relative)))

;;; uniquify
;; add parent directory name to the buffers of the same name
(setq uniquify-buffer-name-style 'post-forward
      uniquify-separator ":"
      uniquify-after-kill-buffer-p t
      uniquify-ignore-buffers-re "\\`\\*")
;; (toggle-uniquify-buffer-names)

;;; moving region/line
;; Many times you'll kill a line with the intention of pasting it back a couple of lines up/below.
(global-set-key (kbd "H-p") 'tl/move-line-or-region-up)
(global-set-key (kbd "H-n") 'tl/move-line-or-region-down)

(global-set-key (kbd "H-k") 'tl/move-line-or-region-up)
(global-set-key (kbd "H-j") 'tl/move-line-or-region-down)

(defun tl/move-line-or-region-up (n)
  (interactive "p")
  (if (region-active-p)
      (tl/move-region-up (region-beginning) (region-end) n)
      (call-interactively #'tl/move-line-up)))

(defun tl/move-line-or-region-down (n)
  (interactive "p")
  (if (region-active-p)
      (tl/move-region-down (region-beginning) (region-end) n)
      (call-interactively #'tl/move-line-down)))

;;
(defun tl/move-region (start end n)
  "Move the current region up or down by N lines."
  (interactive "r\np")
  (let ((line-text (delete-and-extract-region start end)))
    (forward-line n)
    (let ((start (point)))
      (insert line-text)
      (setq deactivate-mark nil)
      (set-mark start))))

(defun tl/move-region-up (start end n)
  "Move the current line up by N lines."
  (interactive "r\np")
  (tl/move-region start end (if (null n) -1 (- n))))

(defun tl/move-region-down (start end n)
  "Move the current line down by N lines."
  (interactive "r\np")
  (tl/move-region start end (if (null n) 1 n)))


;;
(defun tl/move-line (&optional n)
  "Move current line N (1) lines up/down leaving point in place."
  (interactive "p")
  (let ((n (if (null n) 1 n))
        (col (current-column)))
    (forward-line 1)
    (if (/= (preceding-char) ?\n) (newline))
    (transpose-lines n)
    (previous-line (if (> n 0) 1 2))
    (move-to-column col)))

(defun tl/move-line-up (n)
  "Moves current line N (1) lines up leaving point in place."
  (interactive "p")
  (tl/move-line (if (null n) -1 (- n))))

(defun tl/move-line-down (n)
  "Moves current line N (1) lines down leaving point in place."
  (interactive "p")
  (tl/move-line (if (null n) 1 n)))


;;; Open new line
(defun tl/haskell-modes-p ()
  (memq major-mode '(haskell-mode haskell-c-mode literate-haskell-mode)))

(defun tl/open-line-with-indent ()
  "open-line with indent without moving cursor."
  (interactive)
  (save-excursion
    (open-line 1)
    (next-line 1)
    (unless (tl/haskell-modes-p)
     (indent-according-to-mode))
    (next-line -1)))

(global-set-key (kbd "C-o") 'tl/open-line-with-indent)


;; behave like vi's o command
(defun tl/vi-open-next-line (arg)
  "Move to the next line (like vi) and then opens a line."
  (interactive "p")
  (end-of-line)
  (open-line arg)
  (next-line 1)
  (unless (tl/haskell-modes-p)
    (indent-according-to-mode)))

(global-set-key (kbd "C-M-o") 'tl/vi-open-next-line)  ; `C-M-o' default bind to split-line'


;; behave like vi's O command
;; (global-set-key (kbd "M-O") 'tl/vi-open-above-line)
;; (defun tl/vi-open-above-line (arg)
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

;;; bookmark+
;; (with-eval-after-load "bookmark+"
;;   (setq bmkp-last-as-first-bookmark-file nil)
;;   ;; (bmkp-global-auto-idle-bookmark-mode 1)
;;   ;; it seems  highlight automatically not work
;;   (setq bmkp-auto-light-when-set 'all-in-buffer
;;         bmkp-auto-light-when-jump 'all-in-buffer)

;;   (setq bmkp-light-style-autonamed 'lfringe
;;         bmkp-light-style-non-autonamed 'lfringe)

;;   (when (fboundp 'define-fringe-bitmap)
;;     (define-fringe-bitmap 'tl/marker-left   [#x00 #x00 #xFC #xFE #x0F #xFE #xFC #x00])
;;     (define-fringe-bitmap 'tl/marker-right  [#x00 #x00 #x3F #x7F #xF0 #x7F #x3F #x00])
;;     (setq bmkp-light-left-fringe-bitmap 'tl/marker-left)
;;     (setq bmkp-light-right-fringe-bitmap 'tl/marker-right)))


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
;; (defun tl/untabify ()
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
;;             (add-hook 'write-contents-hooks 'tl/untabify nil t)))


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
;; (defun tl/value-to-string (value)
;;   "Convert VALUE to string.
;; This function will automatically identify the type of VALUE, and invoke
;; the appropiate conversion function"
;;   (cond ((symbolp value)
;;          (symbol-name value))
;;         ((numberp value)
;;          (number-to-string value))
;;         (t
;;          (error "Cannot convert value to string."))))


;; (defun tl/read-lines-in-buffer (&optional buffer)
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
(defun tl/dos2unix ()
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\r" nil t) (replace-match "")))

;; vice versa
(defun tl/unix2dos ()
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\n" nil t) (replace-match "\r\n")))

;;; gse number
(autoload 'gse-number-rectangle "gse-number-rect"
  "Insert rising numbers on each line of the region-rectangle, shifting text right." t)
(global-set-key "\C-xru" 'gse-number-rectangle)

;;; yank secondary
(defun tl/yank-secondary ()
  "Insert the secondary selection at point.
  Moves point to the end of the inserted text.  Does not change mark."
  (interactive) (insert (x-get-selection 'SECONDARY)))


;;; Copy current line to next line
(global-set-key (kbd "<M-S-return>") 'tl/dup-line-down)

(defun tl/ue-select-line-down ()
  "like Shift+down in UltraEdit."
  (interactive)
  (let ((s (point)))
    (setq next-line-add-newlines t)
    (next-line 1)
    (setq next-line-add-newlines nil)
    (kill-new (buffer-substring s (point)))))

(defun tl/dup-line-down ()
  "duplicate this line at next line"
  (interactive)
  (let ((c (current-column)))
    (beginning-of-line)
    (tl/ue-select-line-down)
    (beginning-of-line)
    (yank)
    (previous-line 1)
    (move-to-column c)))


(defun tl/strip-all-blank-lines ()
  "Strip all blank lines in current buffer."
  (interactive)
  (save-excursion
    (while (re-search-forward "^[ \t]*\n" nil t)
      (replace-match "" t t))))

;; resolve file names
(defun tl/resolve-file-name (file type)
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
(defun tl/numerate-lines ()
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
(defun tl/align-cols (start end max-cols)
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
(defun tl/count-ce-word (beg end)
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

;;; tl/word-count-analysis (how many times a word has appeared).
(defun tl/word-count-analysis (start end)
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


(defun tl/list-ref (list ref)
  "Return the ref-th element of list."
  (if (= ref 0)
      (car list)
      (tl/list-ref (cdr list) (1- ref))))

(defun tl/info (file)
  (interactive
   (list (read-file-name "info: ")))
  (info file))


(defun tl/delete-line (&optional arg)
  "Delete the rest of the current line; if no nonblanks there, delete thru newline.
With prefix argument, delete that many lines from point.
Negative arguments delete lines backward.
With zero argument, deletes the text before point on the current line.

Note its difference between `tl/delete-line' and `kill-line' is
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

;; (defun tl/soft-kill-ring-save (beg end)
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
(defun tl/revert-all-buffers()
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
;; (defun tl/xmsg (string &optional geom process-name)
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
;; (defun tl/resolve-sym-link ()
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
;; (require 'boxquote nil t)
;; installed with el-get
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
;; (require 'fuzzy-match)
;; installed with el-get
(autoload 'lisp-spell-symbol "fuzzy-match"
  "Perform spell checking on Lisp symbol preceding point." t)
;; This will define the key M-# (ESC #) to call `lisp-spell-symbol'.
(define-key esc-map "#" 'lisp-spell-symbol)

;;; replace-recent-char
(global-set-key (kbd "M-R")  'tl/replace-recent-char)
(defun tl/replace-recent-char ()
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
    (cl-labels
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
;; (defun tl/increment-number-at-point (&optional amount)
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

;; (defun tl/decrement-number-at-point (&optional amount)
;;   (interactive "p")
;;   "Decrement the number under point by `amount'"
;;   (tl/increment-number-at-point (- (abs amount))))
;; I don't use the arrow keys so I have the above functions bound like this:
;; (define-key global-map (kbd "<C-up>") 'tl/increment-number-at-point)
;; (define-key global-map (kbd "<C-down>") 'tl/decrement-number-at-point)

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

(with-eval-after-load "find-file"
  (dolist (l '("../include" "../src"))
    (add-to-list 'cc-search-directories l t))

  (when (eq system-type 'darwin)
    (dolist (l '("/System/Library/Frameworks"
                 "/Library/Frameworks"))
      (add-to-list 'cc-search-directories l t)))

  (cl-pushnew
   '("\\.mm\\'"   (".h")) cc-other-file-alist :test 'equal)
  ;; TODO: ugly setf. pushnew put it in beginning of the list, however here need
  ;; the end
  (let ((l (cadr (assoc "\\.h\\'" cc-other-file-alist))))
    (unless (member ".mm" l)
      (setf (cadr (assoc "\\.h\\'" cc-other-file-alist))
            (append  l '(".mm"))))))




(eval-after-load "scheme"
  '(define-key scheme-mode-map (kbd "C-c S") 'ff-find-other-file))

(eval-after-load "cc-mode"
  '(define-key c-mode-base-map (kbd "C-c S") 'ff-find-other-file))



;;; finding non ascii characters
(defun tl/find-first-non-ascii-char ()
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
(defun tl/insert-path (file)
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

;; avy
(use-package avy
  :defer t
  :init
  (progn
    (setq avy-background nil)
    (setq avy-all-windows t)
    (setq avy-case-fold-search nil)
    (setq avy-timeout-seconds 0.3)
    (setq avy-keys
          (nconc (cl-loop for i from ?a to ?z collect i)
                 (cl-loop for i from ?A to ?Z collect i)
                 (cl-loop for i from ?\! to ?\@ collect i)
                 (cl-loop for i from ?\[ to ?\` collect i)
                 (cl-loop for i from ?\{ to ?\~ collect i)))
    (setq avy-keys-alist
          `((avy-goto-word-1 . ,(nconc (cl-loop for i from ?a to ?z collect i)
                                       (cl-loop for i from ?A to ?Z collect i)))))
    (setq avy-style 'at-full)
    (setq avy-styles-alist '((avy-goto-char-2 . post)))))

;;; ace-isearch
;; L := the length of input query string during isearch
;; L = 1 : ace-jump-mode or avy
;; 1 < L < 6 : isearch
;; L >= 6 : helm-swoop or swiper
;; https://github.com/tam17aki/ace-isearch
(use-package ace-isearch
  :defer 3
  :diminish ace-isearch-mode
  :init
  (progn
    (setq ace-isearch-function 'avy-goto-word-1
          ace-isearch-input-length 5
          ace-isearch-jump-delay 0.25
          ace-isearch-use-jump 'printing-char)
    (setq ace-isearch-function-from-isearch 'ace-isearch-swiper-from-isearch))
  :config
  (progn
    (define-key isearch-mode-map (kbd "C-'") 'ace-isearch-jump-during-isearch)
    (global-ace-isearch-mode +1)))

;;; ace-link
;; bind ace-link-info and ace-link-help to o in their respective modes.
(use-package ace-link
  :defer 7
  :config
  (ace-link-setup-default))

;;; goto line with feedback
;; http://whattheemacsd.com//key-bindings.el-01.html
;; remap all key bindings from goto-line to goto-line-with-feedback.
;; (global-set-key (vector 'remap 'goto-line) 'tl/goto-line-with-nlinum-and-feed-back)
;; (defun tl/goto-line-with-nlinum-and-feed-back ()
;;   "Show line numbers temporarily, while prompting for the line number input"
;;   (interactive)
;;   (let* ((is-nlinum-mode-load (boundp 'nlinum-mode))
;;          (is-nlinum-relative-mode-load (boundp 'nlinum-relative-mode))
;;          (saved-nlinum-mode-state
;;           (if is-nlinum-mode-load nlinum-mode nil))
;;          (saved-nlinum-format
;;           (if is-nlinum-mode-load nlinum-format nil))
;;          (saved-nlinum-relative-mode-state
;;           (if is-nlinum-relative-mode-load nlinum-relative-mode nil)))
;;     (unwind-protect
;;         (progn
;;           (when is-nlinum-relative-mode-load
;;             (nlinum-relative-off))
;;           (setq nlinum-format "%5d")
;;           (when is-nlinum-mode-load
;;             (if saved-nlinum-mode-state
;;                 (nlinum--flush)
;;               (nlinum-mode +1)))
;;           (goto-line (read-number "Goto line: ")))
;;       (progn
;;         (when (and is-nlinum-relative-mode-load saved-nlinum-relative-mode-state)
;;           (nlinum-relative-on))
;;         (when is-nlinum-mode-load
;;           (setq nlinum-format saved-nlinum-format)
;;           (nlinum-mode saved-nlinum-mode-state)
;;           (nlinum--flush))))))


;;; jump-char
;; <char> :: move to the next match in the current direction.
;; ; :: next match forward (towards end of buffer)
;; , :: next match backward (towards beginning of buffer)
;; C-c C-c :: invoke ace-jump-mode if available (also <M-/>)
;; `M-m' are used by emacs leader key see spacemacs
;; https://github.com/syl20bnr/spacemacs
;; (global-set-key (kbd "M-m") 'jump-char-forward)
;; (global-set-key (kbd "M-M") 'jump-char-backward)

;; toggle between the beginning of the line and the beginning of the code.
;; bind `C-a' to this function, `C-a C-a' can be used to  replace default `M-m' back-to-indentation
(defun tl/beginning-of-line-or-indentation ()
  "move to beginning of line, or indentation"
  (interactive)
  (if (bolp)
      (back-to-indentation)
    (beginning-of-line)))

(global-set-key (kbd "C-a") 'tl/beginning-of-line-or-indentation)


;;; cclookup
;; add cclookup to your loadpath, ex) ~/.emacs.d/cclookup
(setq cclookup-dir "~/.emacs.d/site-lisp/cclookup")
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

;;; visual-regexp
;; (define-key global-map (kbd "C-c r") 'vr/replace)
;; (define-key global-map (kbd "C-c q") 'vr/query-replace)
;; if you use multiple-cursors, this is for you:
;; (define-key global-map (kbd "C-c m") 'vr/mc-mark)

;;; docsetutil
(define-key help-map "D" 'docsetutil-search) ; C-h D

;; Choose a docset to use:
;; M-x docsetutil-choose-docset

;; API search with completion:
;; M-x docsetutil-search

;; Full text search:
;; C-u M-x docsetutil-search

;;; xmsi-math-symbols-input.el
(use-package xmsi-mode
  :defer 7
  :init
  (progn
    (autoload 'xmsi-mode "xmsi-math-symbols-input" "Load xmsi minor mode for
inputting math (Unicode) symbols." t))
  :config
  (xmsi-mode 1))


;;; diminish
(use-package diminish
  :config
  (progn
    (with-eval-after-load "abbrev"
      (diminish 'abbrev-mode "")) ; " Abv"
    (with-eval-after-load "subword"
      (diminish 'subword-mode " sw"))
    (with-eval-after-load "paredit"
      (diminish 'paredit-mode " π"))
    (with-eval-after-load "eldoc"
      (diminish 'eldoc-mode ""))
    (with-eval-after-load "xmsi-math-symbols-input"
      (diminish 'xmsi-mode ""))
    (with-eval-after-load "color-identifiers-mode"
      (diminish 'color-identifiers-mode))
    (with-eval-after-load "elisp-slime-nav"
      (diminish 'elisp-slime-nav-mode))
    (with-eval-after-load "guide-key"
      (diminish 'guide-key-mode))
    (with-eval-after-load "auto-complete"
      (diminish 'auto-complete-mode))
    (with-eval-after-load "golden-ratio"
      (diminish 'golden-ratio-mode " φ"))
    (with-eval-after-load "evil-lispy"
      (diminish 'evil-lispy-mode))
    (with-eval-after-load "company"
      (diminish 'company-mode))
    (with-eval-after-load "evil-snipe"
      (diminish 'evil-snipe-local-mode))
    (with-eval-after-load "evil-org"
      (diminish 'evil-org-mode " EO"))
    (with-eval-after-load "outline"
      (diminish 'outline-minor-mode))
    (with-eval-after-load "simple"
      (diminish 'auto-fill-function " F"))
    (with-eval-after-load "anzu"
      (diminish 'anzu-mode))
    (with-eval-after-load "evil-goggles"
      (diminish 'evil-goggles-mode))
    (with-eval-after-load "which-key"
      (diminish 'which-key-mode))
    (with-eval-after-load "evil-vimish-fold"
      (diminish 'evil-vimish-fold-mode))
    (with-eval-after-load "autorevert"
      (diminish 'auto-revert-mode))
    (with-eval-after-load "undo-tree"
      (diminish 'undo-tree-mode))
    (with-eval-after-load "beacon"
      (diminish 'beacon-mode))
    (with-eval-after-load "eldoc-box"
      (diminish 'eldoc-box-hover-mode)
      (diminish 'eldoc-box-hover-at-point-mode))
    (with-eval-after-load "ace-pinyin"
      (diminish 'ace-pinyin-mode))
    (with-eval-after-load "highlight-symbol"
      (diminish 'highlight-symbol-mode))))


;;; goto last change
(global-set-key (kbd "C-x C-/") 'goto-last-change)

(use-package keyfreq
  :config
  (progn
    (keyfreq-mode 1)
    (keyfreq-autosave-mode 1)))

;;; hexcolour
(autoload 'hexcolor-mode "hexcolor" nil t nil)

;;; rfc
(use-package irfc
  :defer t
  :init
  (progn
    (setq irfc-directory "~/Documents/RFC")
    (setq irfc-assoc-mode t))
  :config
  (general-define-key
   :states 'normal
   :keymaps 'irfc-mode-map
   "gp" 'irfc-page-goto
   "gn" 'irfc-page-next
   "gp" 'irfc-page-prev
   "gv" 'irfc-visit))

;;; reveal-in-finder
;; https://github.com/kaz-yos/reveal-in-osx-finder
(when (eq system-type 'darwin)
  (use-package reveal-in-osx-finder
    :defer
    :commands (reveal-in-osx-finder)))

;;; pangu-spacing
;; emacs minor-mode to add space between Chinese and English characters.
;; disable for the performance problem
;; try warplish? https://github.com/manateelazycat/wraplish
;; (use-package pangu-spacing
;;   :diminish
;;   :config
;;   (setq pangu-spacing-real-insert-separtor t)
;;   ;; it will cause org-mode can not align when mix English an Chinese
;;   (global-pangu-spacing-mode -1))

(use-package company-ledger
  :after (company beancount))

(use-package beancount
  :load-path "~/opt/beancount/editors/emacs"
  :mode ("\\.bean$" . beancount-mode)
  :init
  (add-hook 'beancount-mode-hook 'tl/beancount-mode-init)
  (defun tl/beancount-mode-init ()
    (let ((backends tl/company-common-backends))
      (push 'company-ledger backends)
      (set (make-local-variable 'company-backends) backends))

    (company-mode +1)))

(use-package minimap
  :init
  (setq minimap-window-location 'left)
  :config
  (with-eval-after-load 'winum
    (cl-pushnew minimap-buffer-name winum-ignored-buffers)))


(use-package link-hint
  ;; :bind
  ;; ("C-c l o" . link-hint-open-link)
  ;; ("C-c l c" . link-hint-copy-link)
  :commands (link-hint-open-link link-hint-copy-link))

(use-package anki-editor
  :commands (anki-editor-push-notes
             anki-editor-retry-failure-notes
             anki-editor-insert-note
             anki-editor-cloze-region)
  :config
  (setq anki-editor-create-decks t))


(use-package typit
  :commands (typit-test typit-basic-test typit-advanced-test)
  :init
  (setq typit-dict-dir "~/.emacs.d/typit-dict"
        typit-dict "number-symbol.txt"))

(use-package graphviz-dot-mode
  :defer t
  :init
  (setq graphviz-dot-indent-width 2))

;; https://github.com/algernon/kaleidoscope.el
;; (kaleidoscope-send-command :help)
;; (kaleidoscope-send-command :version)
;; (kaleidoscope-send-command :led/setAll "255 0 0")
;; The results will appear in the *kaleidoscope* buffer.
;; (use-package kaleidoscope
;;   :defer t
;;   :commands (kaleidoscope-start
;;              kaleidoscope-quit
;;              kaleidoscope-evil-state-flash-setup
;;              kaleidoscope-evil-state-flash-teardown)
;; :init
;; (progn
;;   (setq kaleidoscope-device-port "/dev/tty.usbmodemCDkbio01")))

;;; nyan-mode
;; https://github.com/TeMPOraL/nyan-mode
;; (nyan-mode +1)
;; (setq nyan-animate-nyancat t)

;;; Control Firefox via the Marionette Protocol
;; $ /Applications/Firefox.app/Contents/MacOS/firefox -marionette
;; # For macOS (open(1) does not block your terminal)
;; $ open -a Firefox --args -marionette
(use-package marionette
  :defer t
  :commands (marionette-with-page marionette-request))
(provide '50tools)
