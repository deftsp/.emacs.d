;;; evil-org-mode.el ---

;; Copyright (C) 2014  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>

;;; Code:
(require 'evil)
(require 'org)

(defun eo/open-line-below-or-insert-item ()
  "Clever insertion of org item."
  (interactive)
  (when (org-at-heading-p)
    (org-show-subtree))
  (evil-open-below nil))

(defun evil-org-eol-call (fun)
  "Go to end of line and call provided function.
FUN function callback"
  (end-of-line)
  (funcall fun)
  (evil-append nil))

(define-minor-mode evil-org-mode
  "Buffer local minor mode for evil-org"
  :init-value nil
  :lighter " EO"
  :keymap (make-sparse-keymap) ; defines evil-org-mode-map
  :group 'evil-org)

(defun evil-org-mode-turn-on ()
  (evil-org-mode +1))

(add-hook 'org-mode-hook 'evil-org-mode-turn-on)

;; recompute clocks in visual selection
(evil-define-operator evil-org-recompute-clocks (beg end type register yank-handler)
  :keep-visual t
  :move-point nil
  (interactive "<r>")
  (progn
    (message "start!" )
    (save-excursion
      (while (< (point) end)
        (org-evaluate-time-range)
        (next-line)
        (message "at position %S" (point))))))

;; open org-mode links in visual selection
(defun evil-org-generic-open-links (beg end type register yank-handler incog)
  (save-excursion
    (goto-char beg)
    (catch 'break
      (while t
        (org-next-link)
          ;;; break from outer loop when there are no more
          ;;; org links
        (when (or
               (not (< (point) end))
               (not (null org-link-search-failed)))
          (throw 'break 0))

        (if (not (null incog))
            (let* ((new-arg
                      ;;; if incog is true, decide which incognito settings to
                      ;;; use dependening on the browser
                    (cond ((not (null (string-match "^.*\\(iceweasel\\|firefox\\).*$" browse-url-generic-program)))  "--private-window")
                          ((not (null (string-match "^.*\\(chrome\\|chromium\\).*$"  browse-url-generic-program)))   "--incognito"     )
                          (t "")
                          ))
                   (old-b (list browse-url-generic-args " " ))
                   (browse-url-generic-args (add-to-ordered-list 'old-b new-arg 0)))
              (progn
                (org-open-at-point)))
          (let ((browse-url-generic-args '("")))
            (org-open-at-point)))))))


;;; open links in visual selection
(evil-define-operator evil-org-open-links (beg end type register yank-handler)
  :keep-visual t
  :move-point nil
  (interactive "<r>")
  (evil-org-generic-open-links beg end type register yank-handler nil))

;;; open links in visual selection in incognito mode
(evil-define-operator evil-org-open-links-incognito (beg end type register yank-handler)
  :keep-visual t
  :move-point nil
  (interactive "<r>")
  (evil-org-generic-open-links beg end type register yank-handler t))

;; normal state shortcuts
(evil-define-key 'normal evil-org-mode-map
  (kbd "TAB") 'org-cycle
  "H" 'org-shiftleft
  "J" 'org-shiftdown
  "K" 'org-shiftup
  "L" 'org-shiftright
  "$" 'org-end-of-line ; smarter behaviour on headlines etc.
  "^" 'org-beginning-of-line ; ditto
  "-" 'org-cycle-list-bullet
  "<" 'org-metaleft ; out-dent
  ">" 'org-metaright ; indent
  "gh" 'outline-up-heading
  "gj" 'org-forward-heading-same-level
  "gk" 'org-backward-heading-same-level
  "gl" 'outline-next-visible-heading
  "gn" 'outline-next-visible-heading
  "gp" 'outline-previous-heading
  "o" 'eo/open-line-below-or-insert-item
  "O" '(lambda () (interactive) (evil-org-eol-call 'org-insert-heading))
  "t" 'org-todo
  "T" '(lambda () (interactive) (evil-org-eol-call 'org-insert-todo-heading-respect-content)))


(defmacro paloryemacs|org-emphasize (fname char)
  "Make function for setting the emphasis in org mode"
  `(defun ,fname () (interactive)
          (org-emphasize ,char)))

(dolist (prefix '(("me" . "export")
                  ("mx" . "text")
                  ("mh" . "headings")
                  ("mi" . "insert & image")
                  ("mS" . "subtrees")
                  ("mt" . "tables")
                  ("mtd" . "delete")
                  ("mti" . "insert")
                  ("mtt" . "toggle")))
  (paloryemacs/declare-prefix-for-mode 'org-mode (car prefix) (cdr prefix)))

;; Insert key for org-mode and markdown a la C-h k
;; from SE endless http://emacs.stackexchange.com/questions/2206/i-want-to-have-the-kbd-tags-for-my-blog-written-in-org-mode/2208#2208
(defun paloryemacs/insert-keybinding-org (key)
  "Ask for a key then insert its description.
Will work on both org-mode and any mode that accepts plain html."
  (interactive "kType key sequence: ")
  (let* ((tag "@@html:<kbd>@@ %s @@html:</kbd>@@"))
    (if (null (equal key "\r"))
        (insert
         (format tag (help-key-description key nil)))
      (insert (format tag ""))
      (forward-char -8))))

(paloryemacs/set-leader-keys-for-major-mode 'org-mode
  "'" 'org-edit-special
  "c" 'org-capture
  "d" 'org-deadline
  "C" 'evil-org-recompute-clocks

  "D" 'org-insert-drawer
  "ee" 'org-export-dispatch
  "f" 'org-set-effort
  "P" 'org-set-property
  ":" 'org-set-tags

  "a" 'org-agenda
  "b" 'org-tree-to-indirect-buffer
  "A" 'org-archive-subtree
  "l" 'evil-org-open-links ;'org-open-at-point
  "T" 'org-show-todo-tree

  "." 'org-time-stamp
  "!" 'org-time-stamp-inactive

  ;; headings
  "hi" 'org-insert-heading-after-current
  "hI" 'org-insert-heading
  "hs" 'org-insert-subheading

  ;; More cycling options (timestamps, headlines, items, properties)
  "L" 'org-shiftright
  "H" 'org-shiftleft
  "J" 'org-shiftdown
  "K" 'org-shiftup

  ;; Change between TODO sets
  "C-S-l" 'org-shiftcontrolright
  "C-S-h" 'org-shiftcontrolleft
  "C-S-j" 'org-shiftcontroldown
  "C-S-k" 'org-shiftcontrolup

  ;; Subtree editing
  "Sl" 'org-demote-subtree
  "Sh" 'org-promote-subtree
  "Sj" 'org-move-subtree-down
  "Sk" 'org-move-subtree-up

  ;; tables
  "ta" 'org-table-align
  "tb" 'org-table-blank-field
  "tc" 'org-table-convert
  "tdc" 'org-table-delete-column
  "tdr" 'org-table-kill-row
  "te" 'org-table-eval-formula
  "tE" 'org-table-export
  "th" 'org-table-previous-field
  "tH" 'org-table-move-column-left
  "tic" 'org-table-insert-column
  "tih" 'org-table-insert-hline
  "tiH" 'org-table-hline-and-move
  "tir" 'org-table-insert-row
  "tI" 'org-table-import
  "tj" 'org-table-next-row
  "tJ" 'org-table-move-row-down
  "tK" 'org-table-move-row-up
  "tl" 'org-table-next-field
  "tL" 'org-table-move-column-right
  "tn" 'org-table-create
  "tN" 'org-table-create-with-table.el
  "tr" 'org-table-recalculate
  "ts" 'org-table-sort-lines
  "ttf" 'org-table-toggle-formula-debugger
  "tto" 'org-table-toggle-coordinate-overlays
  "tw" 'org-table-wrap-region

  ;; Multi-purpose keys
  (or dotpaloryemacs-major-mode-leader-key ",") 'org-ctrl-c-ctrl-c
  "*" 'org-ctrl-c-star
  "RET" 'org-ctrl-c-ret
  "-" 'org-ctrl-c-minus
  "^" 'org-sort
  "/" 'org-sparse-tree

  "I" 'org-clock-in
  "n" 'org-narrow-to-subtree
  "N" 'widen
  "O" 'org-clock-out
  "q" 'org-clock-cancel
  "R" 'org-refile
  "s" 'org-schedule

  ;; insertion of common elements
  "ia" 'org-attach
  "il" 'org-insert-link
  "if" 'org-footnote-new
  "ik" 'paloryemacs/insert-keybinding-org

  ;; image
  "it"  'org-toggle-inline-images
  "ii"  'paloryemacs/org-insert-image
  "ir"  'org-redisplay-inline-images

  ;; images and other link types have no commands in org mode-line
  ;; could be inserted using yasnippet?
  ;; region manipulation
  "xb" (paloryemacs|org-emphasize paloryemacs/org-bold ?*)
  "xc" (paloryemacs|org-emphasize paloryemacs/org-code ?~)
  "xi" (paloryemacs|org-emphasize paloryemacs/org-italic ?/)
  "xr" (paloryemacs|org-emphasize paloryemacs/org-clear ? )
  "xs" (paloryemacs|org-emphasize paloryemacs/org-strike-through ?+)
  "xu" (paloryemacs|org-emphasize paloryemacs/org-underline ?_)
  "xv" (paloryemacs|org-emphasize paloryemacs/org-verbose ?=))

;; normal & insert state shortcuts.
(mapc #'(lambda (state)
          (evil-define-key state evil-org-mode-map
            (kbd "M-l") 'org-metaright
            (kbd "M-h") 'org-metaleft
            (kbd "M-k") 'org-metaup
            (kbd "M-j") 'org-metadown
            (kbd "M-L") 'org-shiftmetaright
            (kbd "M-H") 'org-shiftmetaleft
            (kbd "M-K") 'org-shiftmetaup
            (kbd "M-J") 'org-shiftmetadown
            ;; (kbd "M-o") '(lambda () (interactive)
            ;;                (evil-org-eol-call
            ;;                 '(lambda()
            ;;                    (org-insert-heading)
            ;;                    (org-metaright))))
            (kbd "M-t") '(lambda () (interactive)
                           (evil-org-eol-call
                            '(lambda()
                               (org-insert-todo-heading nil)
                               (org-metaright))))))
      '(normal insert))

(provide 'evil-org-mode)
;;; evil-org-mode.el ends here
