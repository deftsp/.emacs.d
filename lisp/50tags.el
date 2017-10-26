;;; 50tags.el ---

;; ctags and etags are distributed with Emacs
;; ExuberantCtags is superior to etags in many ways. You can find it here:
;; http://ctags.sourceforge.net


;; M-.                find-tag, with `C-u' find next occurence of tag
;; M-*                pop-tag-mark
;; C-M-.              find-tag-regexp
;; tags-search regexp search on all files in tags table. 'tags-loop-continue ' search for next match
;; tags-query-replace run query-replace on all the files
;; M-,                tags-loop-continue. Continue last `tags-search' or `tags-query-replace'.
;; visit-tags-table   set the `tags-file-name' (global). With prefix arg, set buffer-local instead
;; C-M-.              find-tag-regexp
;; C-x 4 .            find-tag-other-window
;; C-x 5 .            find-tag-other-frame
;; tags-apropos       list all tags in a tags file that match a regexp
;; list-tags          list all tags defined in a source file
;; C-M-i M-<TAB>      completion, using the selected tags table if one is loaded

;;;
(use-package etags
  :defer t
  :init (progn
          ;; (setq tags-table-list '("." ".." "../.."))
          ;; always add a new tags table to the current list instead of start a new one
          (setq tags-add-tables t))
  :config (progn
            ;; (use-package etags-select
            ;;   :bind (("\M-?" . etags-select-find-tag-at-point)
            ;;          ("\M-." . etags-select-find-tag)))
            (use-package etags-table
              :init (progn
                      (setq etags-table-search-up-depth 10))
              :config
              (progn
                (add-to-list  'etags-table-alist `(".*\\.[mh]$" ,(expand-file-name "~/.emacs.d/share/tags/objc.TAGS")))
                (add-to-list  'etags-table-alist `(".*\\.mm$" ,(expand-file-name "~/.emacs.d/share/tags/objc.TAGS")))
                (add-to-list  'etags-table-alist `(".*\\.hs$" ,(expand-file-name "~/.emacs.d/share/tags/yesod.TAGS")
                                                   ,(expand-file-name "~/.emacs.d/share/tags/persistent.TAGS")))))))

;;; generate tags
(defun palory/create-tags ()
  "create etag file"
  (interactive)
  (let ((suffix (read-from-minibuffer "suffix: "))
        (dir (read-from-minibuffer "create tags in: " default-directory)))
    (with-temp-buffer
      (shell-command
       (concat "find " dir " -name \"" suffix "\" | xargs etags -a")
       (buffer-name)))))


(defun palory/create-haskell-tags (sd dd name)
  (interactive (list (read-directory-name "source directory: " nil "" t)
                     (read-directory-name "destination directory: " nil "" t)
                     (read-from-minibuffer "Tag name(TAGS): " "")))

  (let ((tag-name (if (string= name "") "TAGS" name)))
    (with-temp-buffer
      (shell-command
       (concat "find " sd " -name '*.hs*' | xargs hasktags -e -x -o " dd tag-name)))))



;;; counsel-gtags
;; https://github.com/syohex/emacs-counsel-gtags
(use-package counsel-gtags
  :defer t
  :diminish counsel-gtags-mode
  :init (progn
          (defun paloryemacs//turn-counsel-gtags-mode-on ()
            (counsel-gtags-mode +1))
          (add-hook 'c-mode-common-hook 'paloryemacs//turn-counsel-gtags-mode-on)
          (add-hook 'python-mode-hook 'paloryemacs//turn-counsel-gtags-mode-on))
  :config (progn
            ;; (define-key counsel-gtags-mode-map (kbd "M-t") 'counsel-gtags-find-definition)
            ;; (define-key counsel-gtags-mode-map (kbd "M-r") 'counsel-gtags-find-reference)
            ;; (define-key counsel-gtags-mode-map (kbd "M-s") 'counsel-gtags-find-symbol)
            ;; (define-key counsel-gtags-mode-map (kbd "M-,") 'counsel-gtags-pop-stack)
            (dolist (mode '(python-mode c-mode))
              (paloryemacs/set-leader-keys-for-major-mode mode
                "gc" 'counsel-gtags-create-tags
                "gd" 'counsel-gtags-dwim
                "gD" 'counsel-gtags-find-definition
                "gu" 'counsel-gtags-update-tags
                "gr" 'counsel-gtags-find-reference
                "gs" 'counsel-gtags-find-symbol
                "gp" 'counsel-gtags-pop
                "gf" 'counsel-gtags-find-file))))

;;; ggtags
(setq ggtags-enable-navigation-keys nil)

;;; counsel-etags
;; https://github.com/redguardtoo/counsel-etags
(use-package counsel-etags
  :defer t
  :commands (counsel-etags-find-tag-at-point)
  :init (progn
          (setq counsel-etags-update-interval 300) ; 300 seconds, OPTIONAL
          ;; (setq counsel-etags-update-tags-backend
          ;;       (lambda () (shell-command "find . -type f -iname \"*.[ch]\" | etags -")))
          )
  :config
  (progn
    ;; (add-hook 'after-save-hook 'counsel-etags-virtual-update-tags)
    ))

(provide '50tags)
