;;; 50etags.el ---

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
(setq tags-add-tables t) ; always add a new tags table to the current list instead of start a new one
;; (setq tags-table-list '("." ".." "../.."))

(eval-after-load "etags-table"
  '(progn
     (add-to-list  'etags-table-alist `(".*\\.[mh]$" ,(expand-file-name "~/.emacs.d/share/tags/objc.TAGS")))
     (add-to-list  'etags-table-alist `(".*\\.mm$" ,(expand-file-name "~/.emacs.d/share/tags/objc.TAGS")))
     (add-to-list  'etags-table-alist `(".*\\.hs$" ,(expand-file-name "~/.emacs.d/share/tags/yesod.TAGS")
                                        ,(expand-file-name "~/.emacs.d/share/tags/persistent.TAGS")))
     (setq etags-table-search-up-depth 10)))

;; (global-set-key "\M-?" 'etags-select-find-tag-at-point)
(global-set-key (kbd "M-.") 'etags-select-find-tag)


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

(provide '50etags)
