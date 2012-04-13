;;; 50etags.el ---

;; M. find a tag(a definition)
;; C-u M-. find next occurence of tag
;; M-x vist-tags-table specify a new tag table, update tag table
;; M-x tags=search regexp search on all files in tags table
;; M-x tags-query-replace run query-replace on all the files
;; M=, continue last tags search or query-reaplce
;; C-M-. find-tag-regexp
;; C-x 4 . find-tag-other-window
;; C-x 5 . find-tag-other-frame


;;----------------------------------------------------------------------------------------------------
;; etags
;;----------------------------------------------------------------------------------------------------

(require 'etags-table)
;; (require 'etags-select)

(setq etags-table-search-up-depth 10)
;; (setq tags-table-list '("." ".." "../.."))


(add-to-list  'etags-table-alist '("\\.[mh]$" "~/.emacs.d/share/tags/objc.TAGS"))
(add-to-list  'etags-table-alist '("\\.mm$" "~/.emacs.d/share/tags/objc.TAGS"))



;; 一个目录下所有的 *.cpp 和 *.h 文件使用这样的正则表达式 *.[ch]*
(defun tsp-create-tags (dir-name)
  "Create tags file."
  (interactive "DDirectory: ")
  (shell-command
   (concat "find " dir-name " -type f -name  *.[ch]*  | xargs etags -a ")))

(defun tsp-generate-tag-table ()
  "Generate tag tables under current directory(Linux)."
  (interactive)
  (let
      ((exp "")
       (dir ""))
    (setq dir
          (read-from-minibuffer "generate tags in: " default-directory)
          exp
          (read-from-minibuffer "suffix: "))
    (with-temp-buffer
      (shell-command
       (concat "find " dir " -name \"" exp "\" | xargs etags ")
       (buffer-name)))))

(defun tsp-create-update-tags ()
  "Create *.[ch]* and update tags"
  (interactive)
  (shell-command
   (concat "find " default-directory " -name  \"" "*.[ch]*" "\" -type f -exec  etags -a {} \\;"))
  ;;(visit-tags-table)
  )


;; tags-files: M-x tags-files-grep is nice
;; (autoload 'tags-files-grep "tags-files" "Grep the files in the current tags table for REGEXP." t)
;; (autoload 'tags-files-list "tags-files" "List all files present in the current tags table." t)

;; (autoload 'goto-last-change "goto-last-change" "Set point to the position of the last change." t)

;;----------------------------------------------------------------------------------------------------

;;; Automatic Tags

;;;  Järneström Jonas  ki.ericsson.se> A smarter
;;;  find-tag that automagically reruns etags when it cant find a
;;;  requested item and then makes a new try to locate it.
;;;  Fri Mar 15 09:52:14 2002

;; (defadvice find-tag (around refresh-etags activate)
;;   "Rerun etags and reload tags if tag not found and redo find-tag.
;; If buffer is modified, ask about save before running etags."
;;   (let ((extension (file-name-extension (buffer-file-name))))
;;     (condition-case err
;; 	ad-do-it
;;       (error (and (buffer-modified-p)
;; 		  (not (ding))
;; 		  (y-or-n-p "Buffer is modified, save it? ")
;; 		  (save-buffer))
;; 	     (er-refresh-etags extension)
;; 	     ad-do-it))))

;; (defun er-refresh-etags (&optional extension)
;;   "Run etags on all peer files in current dir and reload them silently."
;;   (interactive)
;;   (shell-command (format "etags *.%s" (or extension "el")))
;;   (let ((tags-revert-without-query t))	; don't query, revert silently
;;     (visit-tags-table default-directory nil)))
