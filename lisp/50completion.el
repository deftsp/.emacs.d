;;; Completion

;;; dabbrev-expand
;; (global-set-key (kbd "M-/") 'dabbrev-expand)
;; (define-key minibuffer-local-map (kbd "C-<tab>") 'dabbrev-expand)
;; (setq dabbrev-case-fold-search 'case-fold-search)

;; Continuing Expansion
;; Once you have successfully used dabbrev-expand to expand a word, hitting <space> then immediately
;; using dabbrev-expand again will continue to expand from the point that the expansion was found.
;; For example. If previously in the buffer, you had made the function call:

;; thisLongVariableName.someFunction(someArgument)

;; Then at some point later in the buffer you type: "this" then use dabbrev-expand to expand
;; "this" to "thisLongVariableName", if you then type <space> and then use dabbrev-expand again,
;; it will expand "thisLongVariableName" to "thisLongVariableName.someFunction". Continuing
;; this, if you type <space> again then dabbrev-expand again, it will add "(someArgument" and the
;; following word to the expansion.


;;; pabbrev --- I use PredictiveMode instead of.
;; (require 'pabbrev)
;; (setq pabbrev-idle-timer-verbose nil)
;; (global-pabbrev-mode -1)

;;; hippie-expand (like dabbrev-expand, but more powerful) -----------------------------------------------------
(global-set-key (kbd "M-/") 'hippie-expand)

;;; try-expand-tag
(defun he-tag-beg ()
  (let ((p (save-excursion
             (backward-word 1)
             (point))))
    p))

(autoload 'tags-completion-table "etags" "Build `tags-completion-table' on demand." t)

(defun tags-complete-tag (string predicate what)
  (save-excursion
    ;; If we need to ask for the tag table, allow that.
    (if (eq what t)
        (all-completions string (tags-completion-table) predicate)
        (try-completion string (tags-completion-table) predicate))))


(defun try-expand-tag (old)
  (unless old
    (he-init-string (he-tag-beg) (point))
    (setq he-expand-list (sort
                          (all-completions he-search-string 'tags-complete-tag) 'string-lessp)))
  (while (and he-expand-list
              (he-string-member (car he-expand-list) he-tried-table))
    (setq he-expand-list (cdr he-expand-list)))
  (if (null he-expand-list)
      (progn
        (when old (he-reset-string))
        ())
    (he-substitute-string (car he-expand-list))
    (setq he-expand-list (cdr he-expand-list))
    t))


;; The list of expansion functions tried in order by `hippie-expand'.
(setq hippie-expand-try-functions-list
      '(yas-hippie-try-expand              ; Try to expand yasnippet snippets based on prefix
        try-expand-dabbrev                 ; Try to expand word "dynamically", searching the current buffer.
        ;; try-expand-dabbrev-visible         ; Try to expand word "dynamically", searching visible window parts.
        try-expand-dabbrev-all-buffers     ; Tries to expand word "dynamically", searching all other buffers.
        try-expand-all-abbrevs             ; Try to expand word before point according to all abbrev tables.
        try-expand-dabbrev-from-kill       ; Try to expand word "dynamically", searching the kill ring.
        try-complete-file-name-partially   ; Try to complete text as a file name, as many characters as unique.
        try-complete-file-name             ; Try to complete text as a file name.
        try-expand-line                    ; Try to complete the current line to an entire line in the buffer.
        try-expand-line-all-buffers        ; Try to complete the current line, searching all other buffers.
        try-expand-list                    ; Try to complete the current beginning of a list.
        try-expand-list-all-buffers        ; Try to complete the current list, searching all other buffers.
        try-complete-lisp-symbol-partially ; Try to complete as an Emacs Lisp symbol, as many characters as unique.
        try-complete-lisp-symbol           ; Try to complete word as an Emacs Lisp symbol.
        ;; senator-try-expand-semantic        ; prefer senatro
        ;; try-expand-tag
        ;; ispell-complete-word               ; Try to complete the word before or under point
        ;; Try to complete text with something from the kill ring.
        try-expand-whole-kill))

(autoload 'senator-try-expand-semantic "senator")

;;; input inc, prompt inut file name.
(use-package abbrev
  :defer 3
  :config
  (progn
    (define-abbrev-table 'c-mode-abbrev-table
      '(("ife" "" tl/skel-c-ife 1)))
    (define-abbrev-table 'c++-mode-abbrev-table
      '(("ife" "" tl/skel-c-ife 1)))

    (mapc
     (lambda (mode)
       (define-abbrev-table mode '(("inc" "" tl/skel-include 1))))
     '(c-mode-abbrev-table c++-mode-abbrev-table))))

;; input inc and space, auto prompt input filename which can be auto-complete.
(use-package skeleton
  :defer 3
  :init
  (progn
    (setq skeleton-pair t)
    ;; do not insert newline after skeleton insertation
    (setq skeleton-end-hook nil)

    ;; (setq skeleton-pair-alist  '((?` ?` _ "''") ; "?" a space
    ;;                              (?\( _ ")")
    ;;                              (?\[ _ "]")
    ;;                              (?{ \n > _ \n ?} >)))
    (setq skeleton-pair-alist
          '((?\( _ ?\)) (?\))
            (?\[ _ ?\]) (?\])
            (?{ _ ?}) (?\})
            (?< _ ?>) (?\>)
            (?« _ ?») (?\»)
            (?` _ ?'))))
  :config
  (progn
    (define-skeleton tl/skel-include
      "generate include<>" ""
      > "#include <"
      (let ((prompt "Include File: ")
            (files (apply 'append
                          (mapcar #'(lambda (dir)
                                      (if (or (string= dir "/usr/include") (string= dir "/usr/local/include"))
                                          (directory-files dir nil "^[^.]")
                                        (flet ((last-dir (path)
                                                         (string-match ".*/\\(.*\\)" path)
                                                         (match-string 1 path)))
                                          (mapcar #'(lambda (file-name)
                                                      (concat (last-dir dir)
                                                              "/" file-name))
                                                  (directory-files dir)))))
                                  (list "/usr/include"
                                        "/usr/local/include"
                                        "/usr/include/sys"
                                        "/usr/include/netinet"
                                        "/usr/include/arpa"
                                        "/usr/include/bits")))))
        (if (fboundp 'ido-completing-read)
            (ido-completing-read prompt files nil t)
          (completing-read prompt files nil t)))
      ">\n")

    ;; trigger abbrev use space, instead of RET, otherwise the cursor will stay the wrong place
    (define-skeleton tl/skeleton-c-mode-main-func
      "generate int main(int argc, char * argv[]) automatic" nil
      "int\nmain(int argc, char *argv[]) \n{\n"
      > _  "\n" > "return 0;"
      "\n}")
    (define-abbrev-table 'c-mode-abbrev-table
      '(("main" "" tl/skeleton-c-mode-main-func 1)))
    (define-abbrev-table 'c++-mode-abbrev-table
      '(("main" "" tl/skeleton-c-mode-main-func 1)))
    ;;-------------------------------------------------------------------------------------
    (define-skeleton tl/skel-c-for-func
      "generate for () { } automatic" nil
      "for (" _ ") { " > \n
      \n
      "}"> \n)
    (define-abbrev-table 'c-mode-abbrev-table
      '(("fors" "" tl/skel-c-for-func 1)))
    (define-abbrev-table 'c++-mode-abbrev-table
      '(("fors" "" tl/skel-c-for-func 1)))
    ;;-------------------------------------------------------------------------------------

    (define-skeleton tl/skel-c-ife
      "Insert a C if ... else .. block" nil
      > "if (" _ ") {" \n
      \n
      "}" > \n
      "else {" > \n
      \n
      "}" > \n)
    (define-skeleton tl/skel-elisp-separator
      "Inserts a separator for elisp file."
      nil
      ";; ------------------------------------------------------------------------------------\n"
      ";; "_"\n"
      ";; ------------------------------------------------------------------------------------\n")

    ;; (define-skeleton skeleton-c-mode-comment-box
    ;;   "create a comment box" nil
    ;;   "/**********************************************\n"
    ;;   > " * " _ "\n"
    ;;   > " **********************************************/"
    ;;   )

    (define-skeleton tl/haskell-module-skeleton
      "Haskell hs file header"
      "Brief description: "
      "{- \|\n"
      '(setq module-name (haskell-guess-module-name))
      "   Module      : " module-name "\n"
      "   Description : " str | (concat "The \"" module-name "\" module") "\n"
      "   Copyright   : (c) Shihpin Tseng\n"
      "   License     : 3-Clause BSD-style\n"
      "   Maintainer  : deftsp@gmail.com\n"
      "\n"
      "   " _ "\n"
      "\n"
      " -}\n"
      "module " module-name " where\n\n")))


;; (add-to-list 'auto-insert-alist '("\\.hs\\'" . tl/haskell-module-skeleton))

;;; Pair Insertion

(defun tl/auto-pair ()
  (interactive)
  (make-local-variable 'skeleton-pair-alist)
  (local-set-key (kbd "\'") 'skeleton-pair-insert-maybe)
  (local-set-key (kbd "\"") 'skeleton-pair-insert-maybe)
  (local-set-key (kbd "(") 'skeleton-pair-insert-maybe)
  (local-set-key (kbd "[") 'skeleton-pair-insert-maybe)
  (local-set-key (kbd "{") 'skeleton-pair-insert-maybe))

;; (add-hook 'c-mode-hook 'tl/auto-pair)
;; (add-hook 'c++-mode-hook 'tl/auto-pair)

;; (global-set-key (kbd "<") 'skeleton-pair-insert-maybe)
;; (global-set-key (kbd "`") 'skeleton-pair-insert-maybe)


;; (global-set-key (kbd "\"") 'skeleton-pair-insert-maybe)
;; (global-set-key (kbd "(") 'skeleton-pair-insert-maybe)
;; (global-set-key (kbd "[") 'skeleton-pair-insert-maybe)
;; (global-set-key (kbd "{") 'skeleton-pair-insert-maybe)

;; (global-set-key (kbd "“") (lambda () (interactive) (insert "“”") (backward-char 1)))
;; (global-set-key (kbd "‘") (lambda () (interactive) (insert "‘’") (backward-char 1)))
;; (global-set-key (kbd "«") (lambda () (interactive) (insert "«»") (backward-char 1)))
;;-----------------------------------------------------------------------------------



;; turn on Partial Completion mode, for example 'M-x q r r' will equal to query-replace-regexp
;; when input 're-b', press Tab. cursor will go to the wrong place.
;; (setq PC-meta-flag nil)
;; (partial-completion-mode t)

;; auto complete function an variables in minibuffer
;; * Incremental minibuffer completion *
;; (icomplete-mode t)
;; (eval-after-load "icomplete" '(progn (require 'icomplete+)))
;; (setq icomplete-prospects-height 1)

(autoload 'yas-hippie-try-expand "yasnippet")

(use-package yasnippet
  :defer 5
  :diminish (yas-minor-mode . "") " Y"
  :commands (yas-global-mode yas-minor-mode)
  :init
  (progn
    (setq yas-triggers-in-field t
          yas-wrap-around-region t
          helm-yas-display-key-on-candidate t)
    (setq yas-prompt-functions '(yas-completing-prompt))
    ;; disable yas minor mode map
    ;; use hippie-expand instead
    (setq yas-minor-mode-map (make-sparse-keymap))
    ;; this makes it easy to get out of a nested expansion
    (define-key yas-minor-mode-map (kbd "M-s-/") 'yas-next-field)
    (setq yas-snippet-dirs
          (list (expand-file-name "~/.emacs.d/snippets"))))
  :config
  (progn
    ;; (defun yas-advise-indent-function (function-symbol)
    ;;   (eval `(defadvice ,function-symbol (around yas-try-expand-first activate)
    ;;            ,(format
    ;;              "Try to expand a snippet before point, then call `%s' as usual"
    ;;              function-symbol)
    ;;            (let ((yas/fallback-behavior nil))
    ;;              (unless (and (interactive-p)
    ;;                           (yas/expand))
    ;;                ad-do-it)))))

    ;; (yas-advise-indent-function 'org-cycle)
    (easy-menu-remove-item yas-minor-mode-map '("menu-bar") "YASnippet")
    (yas-global-mode +1)))

(provide '50completion)
