;;; 50hideshow.el ---
;;
;; Description:
;; Author: Shihpin Tseng <deftsp@gmail.com>
;; Created: Fri Oct  5 00:00:12 2007
;; Version:
;; Last-Updated: Sun Oct 28 12:40:16 2007 (28800 CST)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

;;; universal code folding
;; set-selective-display is a simple, universal function which hides code according to its indentation level. It can be
;; used as a fall-back for hs-toggle-hiding.
(defun paloryemacs/toggle-selective-display (column)
  (interactive "P")
  (set-selective-display
   (or column
       (unless selective-display
         (1+ (current-column))))))

(defun paloryemacs/toggle-hiding (column)
  (interactive "P")
  (if hs-minor-mode
      (if (condition-case nil
              (hs-toggle-hiding)
            (error t))
          (hs-show-all))
    (paloryemacs/toggle-selective-display column)))

(global-set-key (kbd "C-x $") 'paloryemacs/toggle-hiding)


;; code folding
;; (setq hs-hide-comments-when-hiding-all t)
;; *What kind of hidden blocks to open when doing `isearch'.
;; code    -- open only code blocks
;; comment -- open only comment blocks
;; t       -- open both code and comment blocks
;; nil     -- open neither code nor comment blocks
;; (setq hs-isearch-open 'code)

;;; I use outline minor mode now.....!!!
;; (add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)
;; (add-hook 'c-mode-hook 'hs-minor-mode)
;; (add-hook 'c++-mode-hook 'hs-minor-mode)

;; (add-hook 'hs-minor-mode-hook
;;           (lambda () (local-set-key (kbd "C-c b s") 'hs-show-block)))
;; (add-hook 'hs-minor-mode-hook
;;           (lambda () (local-set-key (kbd "C-c b h") 'hs-hide-block)))
;; (add-hook 'hs-minor-mode-hook
;;           (lambda () (local-set-key (kbd "C-c b S") 'hs-show-all)))
;; (add-hook 'hs-minor-mode-hook
;;           (lambda () (local-set-key (kbd "C-c b H") 'hs-hide-all)))


;; (defadvice goto-line (after expand-after-goto-line
;;                             activate compile)

;;   "hideshow-expand affected block when using goto-line in a collapsed buffer"
;;   (save-excursion
;;     (hs-show-block)))

;; Advice is similar to a hook. It may be executed before or after an Emacs function. It can affect both the parameters
;; and the return value of the function. See http://www.emacswiki.org/cgi-bin/wiki/AdvisingFunctions
;; For expansion on goto-line, adding the following code to your .emacs file will do the trick:

;; (define-prefix-command 'hs-map)
;; (global-set-key (kbd "C-o") 'hs-map)

;; (defadvice goto-line (after expand-after-goto-line
;;                             activate compile)
;;   "hideshow-expand affected block when using goto-line in a collapsed buffer"
;;   (save-excursion
;;     (hs-show-block)))

;;;hide-lines
; hide-lines, show-all-invisible
;; (autoload 'hide-lines "hide-lines" "Hide lines based on a regexp" t)

;;; screen-lines
;; (require 'screen-lines "screen-lines" t)
;(screen-lines-mode t)
;; (global-set-key [(shift ? )] 'screen-lines-mode)

(provide '50hideshow)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 50hideshow.el ends here
