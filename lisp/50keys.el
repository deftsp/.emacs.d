;;; 50keys.el ---

(defun pl/global-set-keys (&rest keycommands)
  "Register keys to commands."
  (while keycommands
    (let ((key (car keycommands))
          (command (cadr keycommands)))
      (eval `(global-set-key (kbd ,key) (quote ,command))))
    (setq keycommands (cdr (cdr keycommands)))))


;; register my preferred keybindings
(pl/global-set-keys
 "<f1>"    'anything-man ; 'woman-word-at-point
 "<C-f1>"  '(lambda () (interactive) (manual-entry (current-word))) ;;; load man pages when on a word and F1 is pressed
 "<S-f1>"  'iman
 ;; "<M-f1>"  'apropos                     ;the ultimate tool for hackers
 "<f2>"    'save-buffer
 "<f3>"    'find-file
 ;; "<M-f3>"  'grep-find
 "<C-f3>"  'hexl-find-file
 "<S-f3>"  'muse-project-find-file
 "<f4>"    'kill-this-buffer
 "<S-f4>"  'kill-buffer
 "<C-f4>"  'delete-window

 ;; Compile and debug
 ;; "<f5>"    'buffer-action-compile
 ;; "<C-f5>"  'buffer-action-run
 ;; "<S-f5>"  'compile
 "<f6>"    'first-error
 "<S-f6>"  'last-error

 ;; "<f9>"    'door-gnus
 "<S-f9>"  'ascii-table-show
 "<C-f9>"  'shell
 "<M-f9>"  'pl/ansi-term
 "<f10>"   'pl/w3m-switch-to-buffer
 ;; "<f11>"
 "<S-f11>" 'appt-add
 "<S-f12>" 'recentf-open-files
 "<f12>"   'list-bookmarks
 "<M-f12>" 'recentf-open-files)



(global-set-key (kbd "C-c g f") 'grep-find)
(global-set-key (kbd "C-c r g") 'rgrep)

;; (global-set-key "\C-cw" 'compare-windows)


;; (define-prefix-command 'menu-map)
;; (global-set-key (kbd "<menu>") 'menu-map)
;; (global-set-key (kbd "<menu> <menu>") (lookup-key global-map (kbd "<menu>")))
;; (global-unset-key (kbd "<menu>"))
;; (global-unset-key (kbd "<menu>") (lambda () (interactive) nil))

(global-set-key (kbd "<Scroll_Lock>") (lambda () (interactive) nil))

;;-------------------------------------------------------------------------------------

(global-set-key "\C-m" 'newline-and-indent) ; 'newline-and-indent 'reindent-then-newline-and-indent
;; (global-set-key "\C-j" 'reindent-then-newline-and-indent)

;;; indent the whole buffer
(global-set-key (kbd "C-c i w") 'pl/indent-whole-buffer)
(defun pl/indent-whole-buffer ()
  "indent whole buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))
;;----------------------------------------------------------------------
;; suspend-frame `C-x C-z'
;; (when window-system
;;   (global-unset-key "\C-z")
;;   ;; define a prefix command, make it possible to define key sequence like`C-z c c' 'C-z n`
;;   (define-prefix-command 'ctl-z-map)
;;   (global-set-key (kbd "C-z") 'ctl-z-map))

(global-set-key (kbd "C-c u") 'revert-buffer) ; how about C-x C-v?

;;; view mode
;; rebind "C-x C-q" to `view-mode' instead of `read-only-mode'
(define-key ctl-x-map "\C-q" 'view-mode)
(add-hook 'view-mode-hook 'pl/view-mode-hook)
(defun pl/view-mode-hook ()
  (define-key view-mode-map "b" 'View-scroll-page-backward)
  (define-key view-mode-map "f" 'View-scroll-page-forward)
  (define-key view-mode-map "h" 'backward-char)
  (define-key view-mode-map "l" 'forward-char)
  (define-key view-mode-map "j" 'next-line)
  (define-key view-mode-map "k" 'previous-line))


;;; HOME & END
;;"Redefine the Home/End keys to (nearly) the same as visual studio behavior... special home and end by Shan-leung
;;Maverick WOO <sw77@cornell.edu>" This is complex. In short, the first invocation of Home/End moves to the beginning of
;;the *text* line. A second invocation moves the cursor to the beginning of the *absolute* line. Most of the time this
;;won't matter or even be noticeable, but when it does (in comments, for example) it will be quite convenient.
(global-set-key [home] 'pl/smart-home)
(global-set-key [end] 'pl/smart-end)

(defun pl/smart-home ()
  "Odd home to beginning of line, even home to beginning of
text/code."
  (interactive)
  (if (and (eq last-command 'pl/smart-home)
           (/= (line-beginning-position) (point)))
      (beginning-of-line)
    (beginning-of-line-text)))

(defun pl/smart-end ()
  "Odd end to end of line, even end to begin of text/code."
  (interactive)
  (if (and (eq last-command 'pl/smart-end)
           (= (line-end-position) (point)))
      (end-of-line-text)
    (end-of-line)))

(defun end-of-line-text ()
  "Move to end of current line and skip comments and trailing space.
Require `font-lock'."
  (interactive)
  (end-of-line)
  (let ((bol (line-beginning-position)))
    (unless (eq font-lock-comment-face (get-text-property bol 'face))
      (while (and (/= bol (point))
                  (eq font-lock-comment-face
                      (get-text-property (point) 'face)))
        (backward-char 1))
      (unless (= (point) bol)
        (forward-char 1) (skip-chars-backward " \t\n"))))) ; Done with home and end keys.

;;--------------------------------------------------------------------------------
;;;Unshifted special characters
;;--------------------------------------------------------------------------------
;; In almost any given programming language you will use the special characters a lot more than numbers. Why not
;; optimize for the most common case? While the code below is for the standard US layout it will automatically handle
;; multibyte characters in order to support extended layouts.

;; (defvar *unshifted-special-chars-layout*
;;   '(("1" "!")                           ; from -> to
;;     ("2" "@")
;;     ("3" "#")
;;     ("4" "$")
;;     ("5" "%")
;;     ("6" "^")
;;     ("7" "&")
;;     ("8" "*")
;;     ("9" "(")
;;     ("0" ")")
;;     ("!" "1")
;;     ("@" "2")
;;     ("#" "3")
;;     ("$" "4")
;;     ("%" "5")
;;     ("^" "6")
;;     ("&" "7")
;;     ("*" "8")
;;     ("(" "9")
;;     (")" "0")))

;; (defun mb-str-to-unibyte-char (s)
;;   "Translate first multibyte char in s to internal unibyte representation."
;;   (multibyte-char-to-unibyte (string-to-char s)))

;; (defun remap-keyboard (mapping)
;;   "Setup keyboard translate table using a list of pairwise key-mappings."
;;   (mapcar
;;    (lambda (mb-string-pair)
;;      (apply #'keyboard-translate
;;             (mapcar #'mb-str-to-unibyte-char mb-string-pair)))
;;    mapping))

;; (remap-keyboard *unshifted-special-chars-layout*)
;; Unshifted special characters ends there--------------------------------------------------------------------------------

;;; repeat to mark multi-line
;; (global-set-key (kbd "C-z") 'pl/mark-line)
(defun pl/mark-line (&optional arg allow-extend)
  "Put point at beginning of this line, mark at end.
The line marked is the one that contains point or follows point.

With argument ARG, puts mark at end of a following line, so that
the number of lines marked equals ARG.

If ARG is negative, point is put at end of this line, mark is put
at beginning of this or a previous line.

Interactively, if this command is repeated
or (in Transient Mark mode) if the mark is active,
it marks the next ARG lines after the ones already marked."
  (interactive "p\np")
  (unless arg (setq arg 1))
  (when (zerop arg)
    (error "Cannot mark zero lines"))
  (cond ((and allow-extend
              (or (and (eq last-command this-command) (mark t))
                  (and transient-mark-mode mark-active)))
         (set-mark
          (save-excursion
            (goto-char (mark))
            (forward-line arg)
            (point))))
        (t
         (forward-line arg)
         (push-mark nil t t)
         (forward-line (- arg)))))


;;-------------------------------------------------------------------------------------------------------

(defun pl/return-current-point ()
  (interactive)
  (message "Current point is: %d" (point)))
;;----------------------------------------------------------------------------------------------------

;;; zap to char
;; Emacs provides a zap-to-char command that kills from the current point to a character. It is
;; bound to `M-z'. Examples of its usage, include:

;; 'M-z e'
;;     deletes all characters to the next occurence of "e".
;; Typing 'C-u 2 M-z e'
;;     deletes all character to the second occurence of "e".
;; Typing 'C-- M-z e'
;;     deletes all characters to the previous occurence of "e".
;; Typing 'C-u -4 M-z e'
;;     deletes all character to the fourth previous occurence of "e".

;; delete sentences (`M-z .'). You can also use 'M-k' ('kill-sentence').
;; delete an XML tag (`M-z >').In nXML mode, 'C-M-k' ('kill-sexp') does the same thing.

;; killing a quote-delimited string. It's handy when you're coding, imagine editing a
;; System.out.println("blah blah blah"); With the point positioned at the first " you could `M-2 M-z
;; "' and kill upto and including the final ", allowing you to change the output. In this case you'd
;; better use 'C-M-k' at the starting "

(global-set-key (kbd "M-z") 'pl/zap-up-to-char)

(defun pl/zap-up-to-char (arg char)
  "Kill up to and including ARGth occurrence of CHAR. "
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     (read-char "Zap to char: " t)))
  ;; Avoid "obsolete" warnings for translation-table-for-input.
  (with-no-warnings
    (if (char-table-p translation-table-for-input)
        (setq char (or (aref translation-table-for-input char) char))))
  (kill-region (point) (1- (progn
                             (let ((case-fold-search nil))
                               (search-forward (char-to-string char) nil nil arg))
                             (point))))
  (backward-char))

(global-set-key (kbd "M-Z") 'pl/zap-to-char-save)
(defun pl/zap-to-char-save (arg char)
  "Zap to a character, but save instead of kill."
  (interactive "p\ncZap to char: ")
  (save-excursion
    (zap-to-char arg char)
    (yank)))

;;; extral key map
;; (setq pl/extra-key-map (make-keymap))
;; (global-set-key [(super z)] pl/extra-key-map)
;; (define-key pl/extra-key-map "b" 'bbdb)
;; (define-key pl/extra-key-map "m" 'bbdb-and-mail-with-default-mailer)


;;; M-^: delete-indentation
;; join-line is an alias for `delete-indentation'


;;; the point doesn’t move, the window does.
(global-set-key (kbd "<H-M-up>")
                (lambda ()
                  (interactive)
                  (let ((top (line-number-at-pos (window-start)))
                        (cur (line-number-at-pos (point))))
                    (when (/= top cur)
                      (scroll-up 1)))))

(global-set-key (kbd "<H-M-down>")
                (lambda ()
                  (interactive)
                  (save-excursion
                    (let ((cur (line-number-at-pos (point)))
                          (bot (- (line-number-at-pos (window-end nil t)) 1)))
                      (when (/= bot cur)
                        (scroll-down 1))))))

;;; _/-/SPACE
(global-set-key (kbd "H--") 'pl/_-SPC)
(defun pl/_-SPC ()
  (interactive)
  (let ((char (following-char)))
    (cl-case char
      ((?\_) (progn (delete-char 1)
                    (insert ?\-)
                    (backward-char 1)))
      ((?\-) (progn (delete-char 1)
                    (insert ?\ )
                    (backward-char 1)))
      ((?\ ) (progn (delete-char 1)
                    (insert ?\_)
                    (backward-char 1)))
      (t (message "Current is not '_ '- or SPC!")))))

;;; cycle-spacing, since 24.4
(global-set-key (kbd "M-SPC") 'cycle-spacing)


;;; Quickly Find Emacs Lisp Sources
(define-key 'help-command (kbd "C-l") 'find-library)
(define-key 'help-command (kbd "C-k") 'find-function-on-key)
(define-key 'help-command (kbd "C-v") 'find-variable)

;; Find the definition of the FUNCTION near point. That's very useful!
(require 'find-func)
;; Define some key bindings for the find-function family of functions.
(find-function-setup-keys)
;; C-x F   find-function
;; C-x 4 F find-function-other-window
;; C-x 5 F find-function-other-frame
;; C-x K   find-function-on-key
;; C-x V   find-variable
;; C-x 4 V find-variable-other-window
;; C-x 5 V find-variable-other-frame


;;;
(global-set-key (kbd "C-x \\") 'align)
(global-set-key (kbd "C-x |") 'align-regexp)

;;; toggle-map
;; http://endlessparentheses.com/the-toggle-map-and-wizardry.html
(define-prefix-command 'pl/toggle-map) ; bind to "gt" in evil mode
;; (define-key ctl-x-map "t" 'pl/toggle-map) ; "C-x t" is binded to `anchored-transpose'
(define-key pl/toggle-map "c" 'column-number-mode)
(define-key pl/toggle-map "d" 'toggle-debug-on-error)
(define-key pl/toggle-map "e" 'toggle-debug-on-error)
(define-key pl/toggle-map "f" 'auto-fill-mode)
(define-key pl/toggle-map "l" 'toggle-truncate-lines)
(define-key pl/toggle-map "q" 'toggle-debug-on-quit)
(define-key pl/toggle-map "n" 'pl/narrow-or-widen-dwim)
(define-key pl/toggle-map "o" 'pl/replace-charset-to-oem)
;;; Generalized version of `read-only-mode'.
(define-key pl/toggle-map "r" 'dired-toggle-read-only)
(autoload 'dired-toggle-read-only "dired" nil t)

;; Launcher Keymap
;; (define-prefix-command 'launcher-map)
;; C-x l is `count-lines-page' by default. If you
;; use that, you can try s-l or <C-return>.
;; (define-key ctl-x-map "l" 'launcher-map)
;; (global-set-key (kbd "s-l") 'launcher-map)
;; (define-key launcher-map "c" #'calc)
;; (define-key launcher-map "d" #'ediff-buffers)
;; (define-key launcher-map "f" #'find-dired)
;; (define-key launcher-map "g" #'lgrep)
;; (define-key launcher-map "G" #'rgrep)
;; (define-key launcher-map "h" #'man) ; Help
;; (define-key launcher-map "i" #'package-install-from-buffer)
;; (define-key launcher-map "s" #'shell)

(provide '50keys)
