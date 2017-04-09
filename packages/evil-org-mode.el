;;; evil-org-mode.el ---

;; Copyright (C) 2014  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>

;;; Code:
(require 'evil)
(require 'org)


(defun eo/open-line-below-or-insert-item ()
  "Clever insertion of org item."
  (interactive)
  (if (org-in-item-p)
      (org-insert-item)
    (org-show-entry)
    (evil-open-below nil)))

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

(paloryemacs/set-leader-keys-for-major-mode 'org-mode
  "t"  'org-show-todo-tree
  "a"  'org-agenda
  "c"  'org-archive-subtree
  "C" 'evil-org-recompute-clocks
  "l"  'evil-org-open-links
  "o"  'evil-org-recompute-clocks
  "O" 'evil-open-above
  "i"  'org-toggle-inline-images
  "I"  'palory/org-insert-image
  "r"  'org-redisplay-inline-images)

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
