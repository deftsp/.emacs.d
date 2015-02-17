;;; evil-org-mode.el ---

;; Copyright (C) 2014  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>

;;; Code:
(require 'evil)
(require 'org)

(defun always-insert-item ()
  (if (org-insert-item)
      (org-insert-item)
    (insert "\n- ")))

(defun evil-org-eol-call (fun)
  (end-of-line)
  (funcall fun)
  (evil-append nil))

(define-minor-mode evil-org-mode
  "minor mode to add evil keymappings to Org-mode."
  :keymap (make-sparse-keymap)
  :lighter " EO")

;; normal state shortcuts
(evil-define-key 'normal evil-org-mode-map
  "H" 'org-beginning-of-line ; smarter behaviour on headlines etc.
  "L" 'org-end-of-line
  "$" 'org-end-of-line ; smarter behaviour on headlines etc.
  "^" 'org-beginning-of-line ; ditto
  (kbd "TAB") 'org-cycle
  "-" 'org-cycle-list-bullet
  ;; "-" 'org-ctrl-c-minus ; change bullet style
  "<" 'org-metaleft ; out-dent
  ">" 'org-metaright ; indent
  "gu" 'outline-up-heading
  "gn" 'outline-next-visible-heading
  "gh" 'outline-up-heading
  ;; "gj" 'org-goto
  "gj" (if (fboundp 'org-forward-same-level) ;to be backward compatible with older org version
           'org-forward-same-level
         'org-forward-heading-same-level)
  "gk" (if (fboundp 'org-backward-same-level)
           'org-backward-same-level
         'org-backward-heading-same-level)
  "gl" 'outline-next-visible-heading
  ;; "o" '(lambda () (interactive) (evil-org-eol-call 'always-insert-item))
  ;; "O" '(lambda () (interactive) (evil-org-eol-call 'org-insert-heading))
  "t" 'org-todo
  "T" '(lambda () (interactive) (evil-org-eol-call '(org-insert-todo-heading nil)))
  ",i" 'pl/org-insert-image
  ",t" 'org-show-todo-tree)


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
