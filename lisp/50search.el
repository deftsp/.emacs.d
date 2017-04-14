;;; 50search.el ---

;; enable the use of vertical scrolling during incremental, but I bind C-v M-v
;; to other command.
(setq isearch-allow-scroll t)

;; ignore case searches
(setq-default case-fold-search t)

;;; tips
;; C-s ...
;;     C-w / Backspace:  appends / delete the next character or word at point to the search string.
;;     C-y / Backspace:  appends / deletee the current kill to the search string.
;;     C-M-y / C-M-w  Pull / delete next character from buffer into search string.

;;; Start `query-replace' with string to replace from last search string.
;; C-s SOMETHING M-% SOMEOTHERS

(setq grep-find-use-xargs 'exec) ; invoke find and grep with `find -exec {} ;'

;;; M-x rgrep or grep-find
;; location resulte with `C-x `M-g n' `M-g p' `M-g'  `C-c C-c'.

;;; search at point
;; Many times you'll want to search for the word or expression at the point.
;; Here is a feature stolen from vi:
(global-set-key (kbd "<f7>")  'paloryemacs/isearch-forward-current-symbol-keep-offset)
(global-set-key (kbd "<f8>") 'paloryemacs/isearch-backward-current-symbol-keep-offset)

(defun paloryemacs/isearch-forward-current-symbol-keep-offset ()
  (interactive)
  (let* ((curword (thing-at-point 'symbol))
         (opoint (point))
         (offset (- (end-of-thing 'symbol) opoint))
         (re-curword (if (and (equal (substring curword 0 1) "*") (equal (substring curword -1) "*"))
                         (replace-regexp-in-string "\\*" "\\\\*" curword)
                       (setq offset (1+ offset))
                       (concat "[^*]" curword "[^*]")))
         (case-fold-search))
    (if (re-search-forward re-curword nil t)
        (progn (backward-char offset)
               (message "Searching `%s' done." curword))
      (progn
        (goto-char (point-min))
        (if (re-search-forward re-curword nil t)
            (progn (message "Searching `%s' from top. `%s' done" curword (what-line))
                   (backward-char offset))
          (goto-char opoint)
          (message "Searching from top: Not found"))))))

(defun paloryemacs/isearch-backward-current-symbol-keep-offset ()
  (interactive)
  (let* ((curword (thing-at-point 'symbol))
         (opoint (point))
         (offset (- opoint (beginning-of-thing 'symbol)))
         (re-curword (if (and (equal (substring curword 0 1) "*") (equal (substring curword -1) "*"))
                         (replace-regexp-in-string "\\*" "\\\\*" curword)
                       (setq offset (1+ offset))
                       (concat "[^*]" curword "[^*]")))
         (case-fold-search))
    (if (re-search-backward re-curword nil t)
        (progn (forward-char offset)
               (message "Searching `%s' done." curword))
      (progn
        (goto-char (point-max))
        (if (re-search-backward re-curword nil t)
            (progn (message "Searching from bottom done.`%s'" (what-line))
                   (forward-char offset))
          (goto-char opoint)
          (message "Searching from bottom: Not found"))))))

;;; Moving around
;; Use the largest monitor you can afford and try to maximize the number of lines of code your system can display. The
;; idea which Steve outlines - using incremental (regular expressions) search to move around is incredibly powerful. One
;; problem though is that the isearch system is not consistent as to where the point will be located when you exit the
;; search. Most of the time you'll want to have the point at the beginning of the match which is what we'll optimize
;; for:

;; Always end searches at the beginning of the matching expression.
(add-hook 'isearch-mode-end-hook 'paloryemacs/custom-goto-match-beginning)

(defun paloryemacs/custom-goto-match-beginning ()
  "Use with isearch hook to end search at first char of match."
  (if isearch-other-end
      (when isearch-forward (goto-char isearch-other-end))))

;;; edit occurances from grep output
;; (autoload 'global-replace-lines "globrep" "Put back grepped lines" t)
;; (autoload 'global-replace "globrep" "query-replace across files" t)
;; (autoload 'global-grep-and-replace "globrep" "grep and query-replace across files" t)

;;; Find files based on regexps, descend directories
;; (load "findr" t)

;;; ireplace
;; New keybindings in isearch, regexp isearch:
;; C-Ret ... Replace the currently searched word
;;           In regexp isearch, hit C-Ret again for a preview of the replacements
;; (require 'ireplace nil t)

;;; Occur - Kin Cho <kin@dynarc.com>
(define-key occur-mode-map "F"
  (lambda (str)
    (interactive "sflush: ")
    (let ((buffer-read-only))
      (save-excursion
        (beginning-of-buffer)
        (flush-lines str)))))

(define-key occur-mode-map "K"
  (lambda (str)
    (interactive "skeep: ")
    (let ((buffer-read-only))
      (save-excursion
        (beginning-of-buffer)
        (keep-lines str)))))

;;; Get from isearch to occur without loosing your search string
(defun isearch-occur ()
  "Invoke `occur' from within isearch."
  (interactive)
  (let ((case-fold-search isearch-case-fold-search))
    (occur
     (if isearch-regexp
         isearch-string
         (regexp-quote isearch-string)))))

(define-key isearch-mode-map (kbd "C-o") 'isearch-occur)

;;; query-replace-regexp
;; http://emacs-journey.blogspot.tw/2012/06/re-builder-query-replace-this.html
(defun paloryemacs/reb-query-replace-this-regxp (replace)
  "Uses the regexp built with re-builder to query the target buffer.
This function must be run from within the re-builder buffer, not the target
buffer.

Argument REPLACE String used to replace the matched strings in the buffer.
 Subexpression references can be used (\1, \2, etc)."
  (interactive "sReplace with: ")
  (if (eq major-mode 'reb-mode)
      (let ((reg (reb-read-regexp)))
        (select-window reb-target-window)
        (save-excursion
          (beginning-of-buffer)
          (query-replace-regexp reg replace)))
      (message "Not in a re-builder buffer!")))

(eval-after-load "re-builder"
  '(define-key reb-mode-map "\C-c\M-%" 'paloryemacs/reb-query-replace-this-regxp))




;; http://patricklogan.blogspot.com/2008/09/query-replace.html
;; qurey-replace, use C-r to

;; What if you want to replace a found match but then edit that particular match slightly differenty before continuing
;; on? (Otherwise you have to remember to go back, and where to go back to, to edit that specific replacement. And what
;; if there are several variations?)

;; In this case when prompted for a replacement, type control-r for a "recusrive edit". Now you are in a position to
;; edit that specific replacement before continuing with the search for the next match. If you want to do the replace
;; and then review the replaced text before continuing with the next match, do this: type comma "," to do the replace
;; but remain at that spot. You can type control-r to recursively edit the replaced text or do the usual to continue on.

;; control-meta-c escapes the recursive edit and continues with the search.

;; What if you made a mistake with the previous match? Either you should have made the replacement and didn't, or you
;; should not have made the replacement. Type "^" and query replace takes you back to the location of the previous match
;; with the text in the state as you left it.

;; help
;; Type Space or `y' to replace one match, Delete or `n' to skip to next,
;; RET or `q' to exit, Period to replace one match and exit,
;; Comma to replace but not move point immediately,
;; C-r to enter recursive edit (C-M-c to get out again),
;; C-w to delete match and recursive edit,
;; C-l to clear the screen, redisplay, and offer same replacement again,
;; ! to replace all remaining matches with no more questions,
;; ^ to move point back to previous match,
;; E to edit the replacement string

;;; anzu -- a minor mode which displays current match and total matches information in the mode-line in various search mode.
(require 'anzu nil t)
(with-eval-after-load "anzu"
  (setq anzu-search-threshold 1000)
  (setq anzu-cons-mode-line-p nil)
  (global-anzu-mode +1)
  (global-set-key (kbd "M-%") 'anzu-query-replace)
  (global-set-key (kbd "C-M-%") 'anzu-query-replace-regexp)
  (defun paloryemacs/anzu-update-mode-line (here total)
    "Custom update function which does not propertize the status."
    (when anzu--state
      (let ((status (cl-case anzu--state
                      (search (format "(%s/%d%s)"
                                      (anzu--format-here-position here total)
                                      total (if anzu--overflow-p "+" "")))
                      (replace-query (format "(%d replace)" total))
                      (replace (format "(%d/%d)" here total)))))
        status)))
  (setq anzu-mode-line-update-function 'paloryemacs/anzu-update-mode-line)

  (with-eval-after-load "evil"
    (require 'evil-anzu)))


;;; The Silver Searcher (ag)
;; http://thetrafficstat.net/
;; run `wgrep-change-to-wgrep-mode' and edit the *ag* buffer. Press C-x C-s when you're done to make the changes to
;; buffers.
(setq ag-highlight-search t)
(setq ag-reuse-window 't)

;;; wgrep https://github.com/mhayashi1120/Emacs-wgrep
(setq wgrep-auto-save-buffer t) ; save buffer automatically when `wgrep-finish-edit'
(setq wgrep-enable-key "r")
(setq wgrep-change-readonly-file t) ; To apply all changes wheather or not buffer is read-only.

;; bind the keys like wdired
(eval-after-load 'grep
  '(define-key grep-mode-map
     (kbd "C-x C-q") 'wgrep-change-to-wgrep-mode))

(eval-after-load 'wgrep
  '(define-key grep-mode-map
     (kbd "C-c C-c") 'wgrep-finish-edit))

;;; ivy
(with-eval-after-load "ivy"
  (setq ivy-display-style 'fancy)
  (setq ivy-use-virtual-buffers t)
  (define-key ivy-minibuffer-map (kbd "C-w") 'ivy-occur)
  (define-key ivy-minibuffer-map [escape] 'minibuffer-keyboard-quit)
  (define-key ivy-minibuffer-map (kbd "M-j") 'ivy-next-line)
  (define-key ivy-minibuffer-map (kbd "M-k") 'ivy-previous-line))

;;; ivy-rich
(with-eval-after-load "ivy"
  (require 'ivy-rich nil t)
  (with-eval-after-load "ivy-rich"
    (ivy-set-display-transformer 'ivy-switch-buffer 'ivy-rich-switch-buffer-transformer)
    (setq ivy-virtual-abbreviate 'full
          ivy-rich-switch-buffer-align-virtual-buffer t)))

;; http://oremacs.com/2016/01/06/ivy-flx/
;; let flx (hopefully) sort the matches in a nice way
(setq ivy-initial-inputs-alist nil)

(setq ivy-re-builders-alist
      '((t . ivy--regex-fuzzy)))

;; (when (fboundp 'ivy-mode)
;;   (ivy-mode 1))

(autoload 'counsel-describe-function "counsel" "Forward to (`describe-function' FUNCTION) with ivy completion." t)
(autoload 'counsel-describe-variable "counsel" "Forward to (`describe-variable' VARIABLE BUFFER FRAME)." t)
(autoload 'counsel-ag "counsel" "Grep for a string in the current directory using ag. INITIAL-INPUT can be given as the initial minibuffer input." t)
(global-set-key (kbd "C-h f") 'counsel-describe-function)
(global-set-key (kbd "C-h v") 'counsel-describe-variable)

;;; swiper
(global-set-key "\C-s" 'swiper)
(global-set-key "\C-r" 'swiper)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key [f6] 'ivy-resume)


(defun paloryemacs/swiper-dwim (arg)
  "Start swiper with input as the selected region or symbol at point by default.
C-u     -> `ivy-resume' (resume from where you last left off swiper)
C-u C-u -> Start swiper without any arguments (stock behavior)"
  (interactive "P")
  (cl-case (car arg)
    (4  (ivy-resume)) ; C-u
    (16 (swiper)) ; C-u C-u
    (t  (swiper (modi/get-symbol-at-point)))))


;;; smex
(with-eval-after-load "smex"
  (unless smex-initialized-p
    (smex-initialize))
  ;; (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command) ; old M-x
  ;; (global-set-key (kbd "M-x") 'smex) ; use 'counsel-M-x
  (global-set-key (kbd "M-X") 'smex-major-mode-commands))

;; counsel-M-x use smex
(global-set-key (kbd "M-x") 'counsel-M-x)

(provide '50search)


;; 50searches ends there------------------------------------------
