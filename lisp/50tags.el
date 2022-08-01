;;; 50tags.el ---

;; Etags is a command to generate 'TAGS' file which is the tag file for Emacs.
;; You can use the file with etags.el which is part of emacs package.

;; Ctags is a command to generate 'tags' file which is the tag file for vi. Now
;; Exuberant Ctags can generate 'TAGS' file by the -e option, and support 41
;; programming languages.

;; cscope is an all-in-one source code browsing tool for C language. It has own
;; fine CUI (character user interface) and tag databases (cscope.in.out,
;; cscope.out, cscope.po.out). You can use cscope from Emacs using xcscope.el
;; which is part of cscope package.

;; GNU global is a source code tagging system. Though it is similar to above
;; tools, it differs from them at the point of that it is independent from any
;; editor, and it has no user interface except for command line. Gtags is a
;; command to generate tag files for GLOBAL (GTAGS, GRTAGS, GPATH). You can use
;; GLOBAL from emacs using gtags.el which is part of GLOBAL package. In addition
;; to this, there are many elisp libraries for it (xgtags.el, ggtags.el,
;; anything-gtags.el, helm-gtags.el, etc).

;; Ebrowse is a C program shipped with Emacs. It indexes C/C++ code and
;; generates a BROWSE file. ebrowse.el provides the usual find definition and
;; completion. You can also open the BROWSE file directly in Emacs to get an
;; overview of the classes/function defined a codebase.

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



;;; generate tags
(defun tl/create-tags ()
  "create etag file"
  (interactive)
  (let ((suffix (read-from-minibuffer "suffix: "))
        (dir (read-from-minibuffer "create tags in: " default-directory)))
    (with-temp-buffer
      (shell-command
       (concat "find " dir " -name \"" suffix "\" | xargs etags -a")
       (buffer-name)))))


(defun tl/create-haskell-tags (sd dd name)
  (interactive (list (read-directory-name "source directory: " nil "" t)
                     (read-directory-name "destination directory: " nil "" t)
                     (read-from-minibuffer "Tag name(TAGS): " "")))

  (let ((tag-name (if (string= name "") "TAGS" name)))
    (with-temp-buffer
      (shell-command
       (concat "find " sd " -name '*.hs*' | xargs hasktags -e -x -o " dd tag-name)))))


;;; counsel-gtags: GNU Global with ivy completion
;; https://github.com/syohex/emacs-counsel-gtags
(defun tl/counsel-gtags-find-reference-quiet ()
  "Find reference of thing at current point or in the database."
  "Search for TAGNAME reference in tag database.
Prompt for TAGNAME if not given."
  (interactive)
  (let ((cursor-symbol (thing-at-point 'symbol)))
    (if cursor-symbol
        (counsel-gtags-find-reference (substring-no-properties cursor-symbol))
      (call-interactively 'counsel-gtags-find-reference))))

(use-package counsel-gtags
  :defer t
  :diminish counsel-gtags-mode
  :init (progn
          (setq counsel-gtags-ignore-case t
                counse1-gtags-auto-update t)
          (defun tl//turn-counsel-gtags-mode-on ()
            (counsel-gtags-mode +1))
          (add-hook 'c++-mode-hook 'counsel-gtags-mode)
          (add-hook 'c-mode-common-hook 'tl//turn-counsel-gtags-mode-on))
  :config (progn
            ;; (define-key counsel-gtags-mode-map (kbd "M-t") 'counsel-gtags-find-definition)
            ;; (define-key counsel-gtags-mode-map (kbd "M-r") 'counsel-gtags-find-reference)
            ;; (define-key counsel-gtags-mode-map (kbd "M-s") 'counsel-gtags-find-symbol)
            ;; (define-key counsel-gtags-mode-map (kbd "M-,") 'counsel-gtags-go-backward)
            (dolist (mode '(c-mode c++-mode makefile-bsdmake-mode dired-mode))
              (tl/set-leader-keys-for-mode mode
                "gC" 'counsel-gtags-create-tags
                "gd" 'counsel-gtags-dwim
                "gD" 'counsel-gtags-find-definition
                "gf" 'counsel-gtags-find-file
                "gu" 'counsel-gtags-update-tags
                "gr" 'tl/counsel-gtags-find-reference-quiet
                "gn" 'counsel-gtags-go-forward
                "gp" 'counsel-gtags-go-backward
                "gy" 'counsel-gtags-find-symbol))))

;;; ggtags
;; emacs frontend to GNU Global source code tagging system
;; (use-package ggtags
;;   :defer t
;;   :diminish ggtags-mode
;;   :init
;;   (progn
;;     (setq ggtags-mode-line-project-name nil)
;;     (setq ggtags-enable-navigation-keys nil)

;;     (defun tl/ggtags-mode-enable ()
;;       "Enable ggtags and eldoc mode.

;; For eldoc, ggtags advises the eldoc function at the lowest priority
;; so that if the major mode has better support it will use it first."
;;       (ggtags-mode +1)
;;       (eldoc-mode +1))

;;     (with-eval-after-load 'lua-mode
;;       (add-hook 'lua-mode-local-vars-hook #'tl/ggtags-mode-enable))
;;     (with-eval-after-load 'sh-script
;;       (add-hook 'sh-mode-local-vars-hook #'tl/ggtags-mode-enable))
;;     (with-eval-after-load 'js2-mode
;;       (add-hook 'js2-mode-local-vars-hook #'tl/ggtags-mode-enable))
;;     (with-eval-after-load 'scheme
;;       (add-hook 'scheme-mode-local-vars-hook #'tl/ggtags-mode-enable))
;;     (with-eval-after-load 'go-mode
;;       (add-hook 'go-mode-local-vars-hook #'tl/ggtags-mode-enable))
;;     (with-eval-after-load 'racket-mode
;;       (add-hook 'racket-mode-local-vars-hook #'tl/ggtags-mode-enable))
;;     (with-eval-after-load 'clojure-mode
;;       (add-hook 'clojure-mode-local-vars-hook #'tl/ggtags-mode-enable))
;;     (with-eval-after-load 'cc-mode
;;       (add-hook 'c++-mode-local-vars-hook #'tl/ggtags-mode-enable))
;;     (with-eval-after-load 'cc-mode
;;       (add-hook 'c-mode-local-vars-hook #'tl/ggtags-mode-enable))
;;     (with-eval-after-load 'elisp-mode
;;       (add-hook 'emacs-lisp-mode-local-vars-hook #'tl/ggtags-mode-enable))
;;     (with-eval-after-load 'python
;;       (add-hook 'python-mode-local-vars-hook #'tl/ggtags-mode-enable))
;;     (with-eval-after-load 'shell
;;       (add-hook 'shell-mode-local-vars-hook #'tl/ggtags-mode-enable))
;;     (with-eval-after-load 'haskell-mode
;;       (add-hook 'haskell-mode-local-vars-hook #'tl/ggtags-mode-enable)))
;;   :config
;;   (define-key ggtags-mode-map [menu-bar ggtags] nil))

(use-package etags
  :defer t
  :init (progn
          ;; (setq tags-table-list '("." ".." "../.."))
          ;; always add a new tags table to the current list instead of start a new one
          (setq tags-revert-without-query t)
          (setq tags-add-tables t))
  :config (progn
            (use-package etags-table
              :init (progn
                      (setq etags-table-search-up-depth 10))
              :config
              (progn
                ;; If it matches, all the rest of the list elements are put on `tags-table-list'
                (add-to-list  'etags-table-alist `(".*\\.[mh]$" ,(expand-file-name "~/.emacs.d/share/tags/objc.TAGS")))
                (add-to-list  'etags-table-alist `(".*\\.mm$" ,(expand-file-name "~/.emacs.d/share/tags/objc.TAGS")))
                (add-to-list  'etags-table-alist `(".*\\.hs$" ,(expand-file-name "~/.emacs.d/share/tags/yesod.TAGS")
                                                   ,(expand-file-name "~/.emacs.d/share/tags/persistent.TAGS")))))))

;;; counsel-etags
;; https://github.com/redguardtoo/counsel-etags
;; find project root folder and scan code automatically
;; find correct tag automatically
;; if no tag is find, it runs ripgrep or grep automatically

;; We try to setup Emacs global variable tags-file-name if it’s nil.
;; If it’s not nil, we respect existing value of tags-file-name. Please note we
;; don’t support tags-table-list.
(use-package counsel-etags
  :defer t
  :commands (counsel-etags-find-tag-at-point)
  :init
  (progn
    ;; (defun tl/counsel-etags-add-update-tags-hook ()
    ;;   ;; after-save-hook is a local variable
    ;;   (add-hook 'after-save-hook 'counsel-etags-virtual-update-tags 'append 'local))
    ;; (add-hook 'prog-mode-hook 'tl/counsel-etags-add-update-tags-hook)
    ;; (setq counsel-etags-update-tags-backend
    ;;       (lambda () (shell-command "find . -type f -iname \"*.[ch]\" | etags -")))
    ;; 300 seconds, OPTIONAL
    (setq counsel-etags-update-interval 300)))



(provide '50tags)
