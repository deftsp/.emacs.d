;;; 05cc-mode.el ---
;; Author: Shihpin Tsing <deftsp@gmail.com>

;; FIXME: if not use build in cc-mode, `TAB' will be bound to `c-indent-command', not `c-indent-line-or-region'
;; (let ((cc-mode-dir (expand-file-name "~/.emacs.d/site-lisp/cc-mode")))
;;   (when (file-directory-p cc-mode-dir)
;;     (add-to-list 'load-path cc-mode-dir)))

;; (eval-after-load "info"
;;   '(pushnew (expand-file-name "~/.emacs.d/site-lisp/cc-mode") Info-default-directory-list :test #'equal))

(defconst paloryemacs/c-c++-modes '(c-mode c++-mode)
  "Primary major modes of the `c-c++' layer.")


(use-package cc-mode
  :defer t
  :init
  ;; make a #define be left-aligned
  (setq c-electric-pound-behavior (quote (alignleft)))
  ;; variable: comment-padding Padding string that `comment-region' puts between comment chars and text.

  :config
  (progn
    (define-key c-mode-base-map (kbd "%") 'paloryemacs/goto-match-paren)
    (define-key c-mode-base-map (kbd "C-m") 'c-context-line-break) ; Do a line break suitable to the context.
    ;; (define-key c-mode-base-map (kbd "H-M-j") 'paloryemacs/move-function-down)
    ;; (define-key c-mode-base-map (kbd "H-M-k") 'paloryemacs/move-function-up)
    ;; ifdef - Parse the #if...#elif...#else...#endif block in a C file.
    (local-set-key (kbd "<M-S-iso-lefttab>") 'mark-ifdef)

    (require 'ifdef)
    ;; (dolist (m '(c-mode objc-mode c++-mode))        ; Colorisation : C/C++/Object-C : Commentaires
    ;;   (paloryemacs/font-lock-add-commentaires-keywords m))
    ;; (dolist (type (list "UCHAR" "USHORT" "ULONG" "BOOL" "BOOLEAN" "LPCTSTR" "C[A-Z]\\sw+" "\\sw+_t"))
    ;;   (add-to-list 'c-font-lock-extra-types type))
    (defun paloryemacs/next-c-function ()
      "Go to start of next C function."
      (interactive)
      (c-beginning-of-defun -1))

    (defun paloryemacs/prev-c-function ()
      "Go to start of next C function."
      (interactive)
      (c-beginning-of-defun 2))

    (use-package ctypes)                     ; beautify typedefs
    (ctypes-auto-parse-mode 1)
    (add-hook 'ctypes-load-hook 'paloryemacs/ctypes-load-hook)
    (defun paloryemacs/ctypes-load-hook ()
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

    (defun paloryemacs/c-mode-common-hook ()
      (subword-mode 1) ; C-c C-w toggle it
      ;; the delete key gobbles all preceding whitespace in one fell swoop
      (c-toggle-hungry-state 1))

    (defun paloryemacs/c-mode-hook ()
      ;; (local-set-key [(control tab)] 'tempo-forward-mark)    ; move to next tempo mark
      (c-set-style "palory"))

    (defun paloryemacs/objc-mode-hook ()
      (c-set-style "palory")
      (define-key objc-mode-map (kbd "C-c C-r") 'xcode:build-and-run))


    (defun paloryemacs/cpp-mode-hook ()
      (c-set-style "palory"))

    (add-hook 'c-mode-common-hook 'paloryemacs/c-mode-common-hook)
    (add-hook 'c-mode-hook 'paloryemacs/c-mode-hook)
    (add-hook 'c++-mode-hook 'paloryemacs/cpp-mode-hook)
    (add-hook 'objc-mode-hook 'paloryemacs/objc-mode-hook)

    ;; move current function up
    ;; Probably you you can use 'delete-and-extract-region' instead of 'kill-region' plus 'yank'.
    (defun paloryemacs/move-function-up ()
      (interactive)
      (save-excursion
        (c-mark-function)
        (kill-region (region-beginning) (region-end))
        (c-beginning-of-defun 1)
        (yank)))

    ;; move current function down
    (defun paloryemacs/move-function-down ()
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
;;     (defun paloryemacs/doxymacs-font-lock-hook ()
;;       (if (or (eq major-mode 'c-mode) (eq major-mode 'c++-mode))
;;           (doxymacs-font-lock)))

;;     (add-hook 'font-lock-mode-hook 'paloryemacs/doxymacs-font-lock-hook)))

;;; indent the entire buffer
(defun paloryemacs/indent-entire-c-buffer ()
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
(defun paloryemacs/c-escape-pair ()
  (interactive)
  (let ((pair-regexp "[^])}\"'>]*[])}\"'>]"))
    (if (looking-at pair-regexp)
        (progn
          ;; be sure we can use C-u C-@ to jump back, if we goto the wrong place
          (push-mark)
          (goto-char (match-end 0)))
      (c-indent-command))))


(defun paloryemacs/do-cdecl ()
  "Compose C and C++ type declarations"
  (interactive)
  (shell-command
   (concat "cdecl explain \"" (buffer-substring (region-beginning)
                                                (region-end)) "\"")))
;; (use-package rtags
;;   :init
;;   (progn
;;     (setq rtags-autostart-diagnostics t)
;;     (add-hook 'rtags-jump-hook 'evil-set-jump)
;;     (rtags-diagnostics)
;;     ;; key bindings
;;     (evil-define-key 'normal rtags-mode-map
;;       (kbd "RET")   'rtags-select-other-window
;;       (kbd "M-RET") 'rtags-select
;;       (kbd "q")     'rtags-bury-or-delete)
;;     ;; TODO check for consistency with gtags key bindings
;;     ;; see https://github.com/syl20bnr/spacemacs/blob/develop/layers/+tags/gtags/funcs.el#L70
;;     (dolist (mode paloryemacs/c-c++-modes)
;;       (paloryemacs/set-leader-keys-for-major-mode mode
;;         "g." 'spacemacs/c-c++-tags-find-symbol-at-point
;;         "g," 'spacemacs/c-c++-tags-find-references-at-point
;;         "g;" 'spacemacs/c-c++-tags-find-file
;;         "g/" 'rtags-find-all-references-at-point
;;         "g[" 'rtags-location-stack-back
;;         "g]" 'rtags-location-stack-forward
;;         "g>" 'spacemacs/c-c++-tags-find-symbol
;;         "g<" 'spacemacs/c-c++-tags-find-references
;;         "gB" 'rtags-show-rtags-buffer
;;         "gd" 'rtags-print-dependencies
;;         "gD" 'rtags-diagnostics
;;         "ge" 'rtags-reparse-file
;;         "gE" 'rtags-preprocess-file
;;         "gF" 'rtags-fixit
;;         "gG" 'rtags-guess-function-at-point
;;         "gh" 'rtags-print-class-hierarchy
;;         "gI" 'spacemacs/c-c++-tags-imenu
;;         "gL" 'rtags-copy-and-print-current-location
;;         "gM" 'rtags-symbol-info
;;         "gO" 'rtags-goto-offset
;;         "gp" 'rtags-set-current-project
;;         "gR" 'rtags-rename-symbol
;;         "gs" 'rtags-print-source-arguments
;;         "gS" 'rtags-display-summary
;;         "gT" 'rtags-taglist
;;         "gv" 'rtags-find-virtuals-at-point
;;         "gV" 'rtags-print-enum-value-at-point
;;         "gX" 'rtags-fix-fixit-at-point
;;         "gY" 'rtags-cycle-through-diagnostics))))


(provide '05cc-mode)
