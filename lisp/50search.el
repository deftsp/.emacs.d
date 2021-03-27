;;; 50search.el ---

;;; tips
;; C-s ...
;;     C-w / Backspace:  appends / delete the next character or word at point to the search string.
;;     C-y / Backspace:  appends / deletee the current kill to the search string.
;;     C-M-y / C-M-w  Pull / delete next character from buffer into search string.

;;; Start `query-replace' with string to replace from last search string.
;; C-s SOMETHING M-% SOMEOTHERS

;;; M-x rgrep or grep-find
;; location resulte with `C-x `M-g n' `M-g p' `M-g'  `C-c C-c'.
(use-package isearch
  :defer t
  :bind (("<f7>" . 'tl/isearch-forward-current-symbol-keep-offset)
         ("<f8>" . 'tl/isearch-backward-current-symbol-keep-offset))
  :init
  ;; enable the use of vertical scrolling during incremental, but I bind C-v M-v
  ;; to other command.
  (setq isearch-allow-scroll t)

  ;; ignore case searches
  (setq-default case-fold-search t)

  ;; search at point
  ;; Many times you'll want to search for the word or expression at the point.
  ;; Here is a feature stolen from vi:
  (defun tl/isearch-forward-current-symbol-keep-offset ()
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

  (defun tl/isearch-backward-current-symbol-keep-offset ()
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
            (message "Searching from bottom: Not found")))))))

;;; Moving around
;; Use the largest monitor you can afford and try to maximize the number of lines of code your system can display. The
;; idea which Steve outlines - using incremental (regular expressions) search to move around is incredibly powerful. One
;; problem though is that the isearch system is not consistent as to where the point will be located when you exit the
;; search. Most of the time you'll want to have the point at the beginning of the match which is what we'll optimize
;; for:

;; Always end searches at the beginning of the matching expression.
(add-hook 'isearch-mode-end-hook 'tl/custom-goto-match-beginning)

(defun tl/custom-goto-match-beginning ()
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
(defun tl/reb-query-replace-this-regxp (replace)
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
  '(define-key reb-mode-map "\C-c\M-%" 'tl/reb-query-replace-this-regxp))




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
(use-package anzu
  :after isearch
  :bind (("M-%" . anzu-query-replace-regexp) ; anzu-query-replace
         ("C-M-%" . anzu-query-replace-regexp))
  :init
  (setq anzu-search-threshold 1000)
  (setq anzu-cons-mode-line-p nil)
  :config
  (defun tl/anzu-update-mode-line (here total)
    "Custom update function which does not propertize the status."
    (when anzu--state
      (let ((status (cl-case anzu--state
                      (search (format "(%s/%d%s)"
                                      (anzu--format-here-position here total)
                                      total (if anzu--overflow-p "+" "")))
                      (replace-query (format "(%d replace)" total))
                      (replace (format "(%d/%d)" here total)))))
        status)))
  (setq anzu-mode-line-update-function 'tl/anzu-update-mode-line))

(use-package evil-anzu
  :after (evil anzu)
  :config
  (global-anzu-mode +1))

;;; wgrep https://github.com/mhayashi1120/Emacs-wgrep
(setq wgrep-auto-save-buffer t) ; save buffer automatically when `wgrep-finish-edit'
(setq wgrep-enable-key "r")
(setq wgrep-change-readonly-file t) ; To apply all changes wheather or not buffer is read-only.

(with-eval-after-load 'wgrep
  (define-key grep-mode-map (kbd "C-c C-c") 'wgrep-finish-edit))

(use-package grep
  :defer t
  :init
  (progn
    ;; NOTE: set to exec will cause `xref-find-references' very slow
    ;; invoke find and grep with `find -exec {} ;'
    ;; (setq grep-find-use-xargs 'exec)
    )
  :config
  (progn
    (dolist (v '("auto"
                 "target"
                 "node_modules"
                 "bower_components"
                 "*dist"
                 ".sass_cache"
                 ".cache"
                 ".npm"
                 "elpa"))
      (add-to-list 'grep-find-ignored-directories v))
    (dolist (v '("*.min.js"
                 "*.map"
                 "*.bundle.js"
                 "*.min.css"
                 "tags"
                 "TAGS"
                 "GTAGS"
                 "GRTAGS"
                 "GPATH"
                 "cscope.files"
                 "*.json"
                 "*.log"))
      (add-to-list 'grep-find-ignored-files v))
    ;; bind the keys like wdired
    (define-key grep-mode-map (kbd "C-x C-q") 'wgrep-change-to-wgrep-mode)))

;; ace-pinyin
(use-package ace-pinyin
  :defer t
  :after (avy)
  :init
  (progn
    (setq ace-pinyin-simplified-chinese-only-p t)
    (setq ace-pinyin-use-avy t))
  :config
  (ace-pinyin-global-mode +1))

;;; amx
(use-package amx
  :defer t
  :init
  (setq amx-backend 'ivy)
  :config
  (amx-mode +1))

;; `counsel-M-x' integrates with either the `amx' or `smex' package when
;; available, in that order of precedence.
(global-set-key (kbd "M-x") 'counsel-M-x)

(use-package deadgrep
  :defer t
  :commands (deadgrep)
  :init
  (general-define-key
   :states 'normal
   :keymaps 'deadgrep-mode-map
   (kbd "RET") 'deadgrep-visit-result
   (kbd "<S-return>") 'deadgrep-visit-result-other-window
   "go" 'deadgrep-visit-result-other-window
   "gr" 'deadgrep-restart
   (kbd "C-j") 'deadgrep-forward
   (kbd "C-k") 'deadgrep-backward
   (kbd "TAB") 'deadgrep-toggle-file-results

   "i" 'deadgrep-edit-mode

   ;; FIXME: Maybe we should bind this to something?
   ;; (define-key map (kbd "C-c C-k") #'deadgrep-kill-process)

   ;; Quit
   "q" 'quit-window

   "ZQ" 'evil-quit)

  (general-define-key
   :states 'normal
   :keymaps 'deadgrep-edit-mode-map
   (kbd "RET") 'deadgrep-visit-result
   (kbd "<escape>") 'deadgrep-mode)
  )

;; https://github.com/dajva/rg.el
(use-package rg
  :defer t
  :commands (rg-menu)
  :bind (("C-c s" . rg-menu)
         ("M-s g" . tl/rg-vc-or-dir)
         ("M-s r" . tl/rg-ref-in-dir)
         :map rg-mode-map
         ("s" . tl/rg-save-search-as-name))
  :init
  (setq rg-hide-command t
        rg-show-columns nil)
  :config
  (general-define-key
   :states 'normal
   :keymaps 'rg-mode-map
   "g>" 'rg-forward-history
   "g<" 'rg-back-history

   "j" 'compilation-next-error
   "k" 'compilation-previous-error

   "h" 'rg-next-file
   "l" 'rg-prev-file
   "m" 'rg-menu

   "gg" 'evil-goto-first-line
   "gr" 'rg-recompile
   "G" 'evil-goto-line

   "e" 'wgrep-change-to-wgrep-mode
   ;; Quit
   "q" 'color-rg-quit
   "ZQ" 'evil-quit)

  (rg-define-search rg-emacs-config
    "Search the emacs config."
    :dir "~/.emacs.d"
    :files "*.{el,el.gz}"
    :menu ("Custom" "e" "emacs config"))

  (rg-define-search tl/rg-vc-or-dir
    "RipGrep in project root or present directory."
    :query ask
    :format regexp
    :files "everything"
    :dir (let ((vc (vc-root-dir)))
           (if vc
               vc                       ; search root project dir
             default-directory))        ; or from the current dir
    :confirm prefix
    :flags ("--hidden -g !.git"))


  ;; https://protesilaos.com/dotemacs
  (rg-define-search tl/rg-ref-in-dir
    "RipGrep for thing at point in present directory."
    :query point
    :format regexp
    :files "everything"
    :dir default-directory
    :confirm prefix
    :flags ("--hidden -g !.git"))

  (defun tl/rg-save-search-as-name ()
    "Save `rg' buffer, naming it after the current search query.

 This function is meant to be mapped to a key in `rg-mode-map'."
    (interactive)
    (let ((pattern (car rg-pattern-history)))
      (rg-save-search-as-name (concat "«" pattern "»")))))

(provide '50search)


;; 50searches ends there------------------------------------------
