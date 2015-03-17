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

;; (global-set-key (kbd "M-z") 'pl/zap-up-to-char)
;; (defun pl/zap-up-to-char (arg char)
;;   "Kill up to and including ARGth occurrence of CHAR. "
;;   (interactive (list (prefix-numeric-value current-prefix-arg)
;;                      (read-char "Zap to char: " t)))
;;   ;; Avoid "obsolete" warnings for translation-table-for-input.
;;   (with-no-warnings
;;     (if (char-table-p translation-table-for-input)
;;         (setq char (or (aref translation-table-for-input char) char))))
;;   (kill-region (point) (1- (progn
;;                              (let ((case-fold-search nil))
;;                                (search-forward (char-to-string char) nil nil arg))
;;                              (point))))
;;   (backward-char))

;; (global-set-key (kbd "M-Z") 'pl/zap-to-char-save)
;; (defun pl/zap-to-char-save (arg char)
;;   "Zap to a character, but save instead of kill."
;;   (interactive "p\ncZap to char: ")
;;   (save-excursion
;;     (zap-to-char arg char)
;;     (yank)))

(global-set-key (kbd "M-z") 'ace-jump-zap-up-to-char-dwim)
(global-set-key (kbd "M-Z") 'ace-jump-zap-to-char-dwim)

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
;; both `M-SPC' and `M-S-SPC' are binded to `just-one-space'
;; "M-\" is used to switch input source on OS X. The behavior of
;; double call `cycle-spacing' as same as `delete-horizontal-space'
(global-set-key (kbd "M-S-SPC") 'cycle-spacing)

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

;;; miscellaneous toggle-map
(autoload 'dired-toggle-read-only "dired" nil t)

;; bind to "gt" as prefix key in evil normal mode
(with-eval-after-load "hydra"
  (defhydra hydra-toggle (:color blue)
    "
_a_ abbrev-mode:           %`abbrev-mode
_d_ debug-on-error:        %`debug-on-error
_f_ auto-fill-mode:        %`auto-fill-function
_n_ narrow-or-widen-dwim:  %(buffer-narrowed-p)
_g_ golden-ratio-mode:     %`golden-ratio-mode
_G_ debug-on-quit:         %`debug-on-quit
_r_ read-only-mode:        %`buffer-read-only
_t_ truncate-lines:        %`truncate-lines
_w_ whitespace-mode:       %(and (boundp 'whitespace-mode) whitespace-mode)

"
    ("a" abbrev-mode "abbrev")
    ("d" toggle-debug-on-error "debug")
    ("f" auto-fill-mode "fill")
    ("n" pl/narrow-or-widen-dwim "narrow<->widen")
    ("g" golden-ratio-mode "golden-ratio")
    ("G" toggle-debug-on-quit "debug-quit")
    ("o" pl/replace-charset-to-oem "char->oem")
    ("r" dired-toggle-read-only "read only") ; generalized version of `read-only-mode'.
    ("t" toggle-truncate-lines "truncate")
    ("w" whitespace-mode "whitespace")
    ("q" nil "cancel")))

;; Launcher Keymap
;; (define-prefix-command 'launcher-map)
;; C-x l is `count-lines-page' by default. If you
;; use that, you can try s-l or <C-return>.
;; (define-key ctl-x-map "l" 'launcher-map) ; `C-x l ' default bind to count-lines-page
;; (global-set-key (kbd "s-l") 'launcher-map)
;; (define-key launcher-map "c" #'calc)

;; Launching External Applications and Websites
;; http://endlessparentheses.com/keymap-for-launching-external-applications-and-websites.html
;; (defmacro pl/def-run (exec)
;;   "Return a function that runs the executable EXEC."
;;   (let ((func-name (intern (concat "pl/run-" exec))))
;;     `(progn
;;        (defun ,func-name ()
;;          ,(format "Run the %s executable." exec)
;;          (interactive)
;;          (start-process "" nil ,exec))
;;        ',func-name)))

;; (define-key launcher-map "m" (pl/def-run "Mathematica"))

;; (defmacro pl/def-browse (url)
;;   "Return a function that calls `browse-url' on URL."
;;   (let ((func-name (intern (concat "pl/browse-" url))))
;;     `(progn
;;        (defun ,func-name ()
;;          ,(format "Browse to the url %s." url)
;;          (interactive)
;;          (browse-url ,url))
;;        ',func-name)))

;; (define-key launcher-map "r" (pl/def-browse "http://www.reddit.com/r/emacs/"))
;; (define-key launcher-map "w" (pl/def-browse "http://www.emacswiki.org/"))

;;; Launcher
;; C-x l is `count-lines-page' by default. If you use that, you can try s-l or <C-return>.
(global-set-key
 (kbd "C-x l")
 (defhydra hydra-launcher (:color blue)
   "Launch"
   ("c" calc "calc")
   ("d" ediff-buffers "ediff buffers")
   ("f" find-dired "find dired")
   ("g" lgrep "lgrep")
   ("G" rgrep "rgrep")
   ("h" man "man")
   ("r" (browse-url "http://www.reddit.com/r/emacs/") "reddit")
   ("w" (browse-url "http://www.emacswiki.org/") "emacswiki")
   ("s" shell "shell")
   ("q" nil "cancel")))

;;; high frequency key
(key-chord-define-global
 "jf"
 (defhydra hydra-high-frequency (:color teal)
   "
org-agend_a_ org-_c_apture org-clock-_g_oto
_p_rojectile _s_cratch
undo-tree-_u_ndo undo-tree-_r_edo
"
   ("SPC" mark-sexp "mark sexp" :color red)
   ("a" org-agenda nil)
   ("c" org-capture nil)
   ("g" org-clock-goto nil)
   ("G" hydra-git-gutter/body "git gutter")
   ("h" hydra-apropos/body "apropos")
   ("j" dired-jump "dired jump")
   ("p" hydra-projectile/body nil)
   ("s" pl/switch-to-scratch nil)
   ("t" hydra-toggle/body "toggle")
   ("u" undo-tree-undo nil :color red)
   ("r" undo-tree-redo nil :color red)
   ("q" nil "cancel")))

(defun pl/switch-to-scratch ()
  (interactive)
  (switch-to-buffer "*scratch*"))

;;; Rectangle-mode Hydra
;; http://oremacs.com/2015/02/25/rectangle-hydra/
;; d deletes rectangle; it's similar to C-d.
;; n copies rectangle; it's similar to M-w.
;; q exits; it's very easy to press.
;; e exchanges the point and mark; it's also quite useful to re-activate the region if you disabled it with n or r.
;; s fills the selected rectangle with a string.
;; y yanks the rectangle that you saved before with n.
;; r deactivates or activates the rectangle at point.
;; u calls undo.
;; p kills the rectangle; it's similar to C-w.

(defun pl/exchange-point-mark ()
  (interactive)
  (if rectangle-mark-mode
      (exchange-point-and-mark)
    (let ((mk (mark)))
      (rectangle-mark-mode 1)
      (goto-char mk))))

(defhydra hydra-rectangle (:body-pre (rectangle-mark-mode 1)
                           :color pink
                           :post (deactivate-mark))
  "
  ^_k_^     _d_elete    _s_tring     |\\     _,,,--,,_
_h_   _l_   _q_uite     _y_ank       /,`.-'`'   ._  \-;;,_
  ^_j_^     _n_ew-copy  _r_eset     |,4-  ) )_   .;.(  `'-'
^^^^        _e_xchange  _u_ndo     '---''(_/._)-'(_\_)
^^^^        ^ ^         _p_aste
"
  ("h" backward-char nil)
  ("l" forward-char nil)
  ("k" previous-line nil)
  ("j" next-line nil)
  ("e" pl/exchange-point-mark nil)
  ("n" copy-rectangle-as-kill nil)
  ("d" delete-rectangle nil)
  ("r" (if (region-active-p)
           (deactivate-mark)
         (rectangle-mark-mode 1)) nil)
  ("y" yank-rectangle nil)
  ("u" undo nil)
  ("s" string-rectangle nil)
  ("p" kill-rectangle nil)
  ("q" nil nil))

(global-set-key (kbd "C-x SPC") 'hydra-rectangle/body)

;;; hydra apropos family
(defhydra hydra-apropos (:color blue :hint nil)
  "
_a_propos _c_ommand
_d_ocumentation _l_ibrary
_v_ariable _u_ser-option
^ ^ valu_e_"
  ("a" apropos)
  ("d" apropos-documentation)
  ("v" apropos-variable)
  ("c" apropos-command)
  ("l" apropos-library)
  ("u" apropos-user-option)
  ("e" apropos-value))

(provide '50keys)
