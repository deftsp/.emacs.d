;;; 11cc-mode.el ---
;; Author: Shihpin Tsing <deftsp@gmail.com>

;; FIXME: if not use build in cc-mode, `TAB' will be bound to `c-indent-command', not `c-indent-line-or-region'
;; (let ((cc-mode-dir (expand-file-name "~/.emacs.d/site-lisp/cc-mode")))
;;   (when (file-directory-p cc-mode-dir)
;;     (add-to-list 'load-path cc-mode-dir)))

;; (eval-after-load "info"
;;   '(pushnew (expand-file-name "~/.emacs.d/site-lisp/cc-mode") Info-default-directory-list :test #'equal))

(defconst tl/c-c++-modes '(c-mode c++-mode)
  "Primary major modes of the `c-c++' layer.")


(use-package cc-mode
  :defer t
  :init
  ;; make a #define be left-aligned
  (setq c-electric-pound-behavior (quote (alignleft)))
  ;; variable: comment-padding Padding string that `comment-region' puts between comment chars and text.

  :config
  (progn
    (define-key c-mode-base-map (kbd "%") 'tl/goto-match-paren)
    (define-key c-mode-base-map (kbd "C-m") 'c-context-line-break) ; Do a line break suitable to the context.
    ;; (define-key c-mode-base-map (kbd "H-M-j") 'tl/move-function-down)
    ;; (define-key c-mode-base-map (kbd "H-M-k") 'tl/move-function-up)
    ;; ifdef - Parse the #if...#elif...#else...#endif block in a C file.
    (local-set-key (kbd "<M-S-iso-lefttab>") 'mark-ifdef)

    (require 'ifdef)
    ;; (dolist (m '(c-mode objc-mode c++-mode))        ; Colorisation : C/C++/Object-C : Commentaires
    ;;   (tl/font-lock-add-commentaires-keywords m))
    ;; (dolist (type (list "UCHAR" "USHORT" "ULONG" "BOOL" "BOOLEAN" "LPCTSTR" "C[A-Z]\\sw+" "\\sw+_t"))
    ;;   (add-to-list 'c-font-lock-extra-types type))
    (defun tl/next-c-function ()
      "Go to start of next C function."
      (interactive)
      (c-beginning-of-defun -1))

    (defun tl/prev-c-function ()
      "Go to start of next C function."
      (interactive)
      (c-beginning-of-defun 2))

    (use-package ctypes)                     ; beautify typedefs
    (ctypes-auto-parse-mode 1)
    (add-hook 'ctypes-load-hook 'tl/ctypes-load-hook)
    (defun tl/ctypes-load-hook ()
      (ctypes-read-file "~/.ctypes_std_c" nil t t))

    ;; style I want to use in c++ mode
    ;; +   `c-basic-offset' times 1
    ;; -   `c-basic-offset' times -1
    ;; ++  `c-basic-offset' times 2
    ;; --  `c-basic-offset' times -2
    ;; *   `c-basic-offset' times 0.5
    ;; /   `c-basic-offset' times -0.5
    (c-add-style "palory"
                 '((indent-tabs-mode . nil)        ; use spaces rather than tabs
                   (c-basic-offset . 4)            ; indent by four spaces
                   (c-comment-only-line-offset . 0)
                   (c-offsets-alist
                    (statement-block-intro . +)
                    (substatement-open . 0)  ; brackets should be at same indentation level as the statements they open
                    (substatement-label . 0)
                    (member-init-intro . 0)  ; first line in a member initialization list
                    (case-label . +)         ; indent case labels by c-indent-level, too
                    (label . 0)
                    (statement-cont . +)
                    (inline-open . 0))))

    (defun tl/c-mode-common-hook ()
      (subword-mode 1) ; C-c C-w toggle it
      ;; the delete key gobbles all preceding whitespace in one fell swoop
      (c-toggle-hungry-state 1))

    (defun tl/c-mode-hook ()
      ;; (local-set-key [(control tab)] 'tempo-forward-mark)    ; move to next tempo mark
      (c-set-style "palory"))

    (defun tl/objc-mode-hook ()
      (c-set-style "palory")
      (define-key objc-mode-map (kbd "C-c C-r") 'xcode:build-and-run))


    (defun tl/cpp-mode-hook ()
      (c-set-style "palory"))

    (add-hook 'c-mode-common-hook 'tl/c-mode-common-hook)
    (add-hook 'c-mode-hook 'tl/c-mode-hook)
    (add-hook 'c++-mode-hook 'tl/cpp-mode-hook)
    (add-hook 'objc-mode-hook 'tl/objc-mode-hook)

    ;; move current function up
    ;; Probably you you can use 'delete-and-extract-region' instead of 'kill-region' plus 'yank'.
    (defun tl/move-function-up ()
      (interactive)
      (save-excursion
        (c-mark-function)
        (kill-region (region-beginning) (region-end))
        (c-beginning-of-defun 1)
        (yank)))

    ;; move current function down
    (defun tl/move-function-down ()
      (interactive)
      (save-excursion
        (c-mark-function)
        (kill-region (region-beginning) (region-end))
        (c-beginning-of-defun -1)
        (yank)))))

;; (use-package doxymacs
;;   :defer t
;;   :init
;;   (add-hook 'c-mode-common-hook 'doxymacs-mode)
;;   :config
;;   (progn
;;     (defun tl/doxymacs-font-lock-hook ()
;;       (if (or (eq major-mode 'c-mode) (eq major-mode 'c++-mode))
;;           (doxymacs-font-lock)))

;;     (add-hook 'font-lock-mode-hook 'tl/doxymacs-font-lock-hook)))

;;; indent the entire buffer
(defun tl/indent-entire-c-buffer ()
  "Indent entire buffer of C source code."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (< (point) (point-max))
      (c-indent-command)
      (end-of-line)
      (forward-char 1))))

;;----------------------------------------------------------------------------------------------------
;; jump out from a pair(like quote, parenthesis, etc.)
(defun tl/c-escape-pair ()
  (interactive)
  (let ((pair-regexp "[^])}\"'>]*[])}\"'>]"))
    (if (looking-at pair-regexp)
        (progn
          ;; be sure we can use C-u C-@ to jump back, if we goto the wrong place
          (push-mark)
          (goto-char (match-end 0)))
      (c-indent-command))))


(defun tl/do-cdecl ()
  "Compose C and C++ type declarations"
  (interactive)
  (shell-command
   (concat "cdecl explain \"" (buffer-substring (region-beginning)
                                                (region-end)) "\"")))

;;; lsp/ccls
;; (use-package ccls
;;   :hook ((c-mode c++-mode objc-mode) .
;;          (lambda () (require 'ccls) (lsp))))


(provide '11cc-mode)
