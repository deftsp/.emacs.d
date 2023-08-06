;;; 50smartparens.el ---                             -*- lexical-binding: t; -*-

;; Copyright (C) 2013  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>

;;; Code:

;; https://github.com/Fuco1/smartparens
;; el-get install smartparens

;;;;;;;;;
;;; keybinding management
;; paredit  -  smartparens
;; M-s      -  M-D
;; M-r      -  C-S-<backspace>
;; M-S      -  sp-split-sexp

;;; manually set `sp-keymap' instead of use predefined sp-smartparens-bindings
(defun tl/smartparens-set-keys ()
  (define-key sp-keymap (kbd "C-M-f") 'sp-forward-sexp)
  (define-key sp-keymap (kbd "C-M-b") 'sp-backward-sexp)

  (define-key sp-keymap (kbd "C-M-d") 'sp-down-sexp)
  (define-key sp-keymap (kbd "C-M-a") 'sp-backward-down-sexp)
  (define-key sp-keymap (kbd "C-S-a") 'sp-beginning-of-sexp)
  (define-key sp-keymap (kbd "C-S-d") 'sp-end-of-sexp)

  (define-key sp-keymap (kbd "C-M-e") 'sp-up-sexp)
  (define-key emacs-lisp-mode-map (kbd ")") 'sp-up-sexp)
  (define-key sp-keymap (kbd "C-M-u") 'sp-backward-up-sexp)
  (define-key sp-keymap (kbd "C-M-t") 'sp-transpose-sexp)

  (define-key sp-keymap (kbd "C-M-n") 'sp-next-sexp)
  (define-key sp-keymap (kbd "C-M-p") 'sp-previous-sexp)

  (define-key sp-keymap (kbd "C-M-k") 'sp-kill-sexp)
  (define-key sp-keymap (kbd "C-M-w") 'sp-copy-sexp)

  (define-key sp-keymap (kbd "M-<delete>") 'sp-unwrap-sexp)
  (define-key sp-keymap (kbd "M-<backspace>") 'sp-backward-unwrap-sexp)

  (define-key sp-keymap (kbd "C-<right>") 'sp-forward-slurp-sexp)
  (define-key sp-keymap (kbd "C-<left>") 'sp-forward-barf-sexp)
  (define-key sp-keymap (kbd "C-M-<left>") 'sp-backward-slurp-sexp)
  (define-key sp-keymap (kbd "C-M-<right>") 'sp-backward-barf-sexp)

  (define-key sp-keymap (kbd "M-D") 'sp-splice-sexp)
  (define-key sp-keymap (kbd "C-M-<delete>") 'sp-splice-sexp-killing-forward)
  (define-key sp-keymap (kbd "C-M-<backspace>") 'sp-splice-sexp-killing-backward)
  (define-key sp-keymap (kbd "C-S-<backspace>") 'sp-splice-sexp-killing-around)

  (define-key sp-keymap (kbd "C-]") 'sp-select-next-thing-exchange)
  (define-key sp-keymap (kbd "C-<left_bracket>") 'sp-select-previous-thing)
  (define-key sp-keymap (kbd "C-M-]") 'sp-select-next-thing)

  ;; (define-key sp-keymap (kbd "M-F") 'sp-forward-symbol)
  ;; (define-key sp-keymap (kbd "M-B") 'sp-backward-symbol)

  (define-key sp-keymap (kbd "H-t") 'sp-prefix-tag-object)
  ;; (define-key sp-keymap (kbd "H-p") 'sp-prefix-pair-object)
  (define-key sp-keymap (kbd "H-s c") 'sp-convolute-sexp)
  (define-key sp-keymap (kbd "H-s a") 'sp-absorb-sexp)
  (define-key sp-keymap (kbd "H-s e") 'sp-emit-sexp)
  (define-key sp-keymap (kbd "H-s p") 'sp-add-to-previous-sexp)
  (define-key sp-keymap (kbd "H-s n") 'sp-add-to-next-sexp)
  (define-key sp-keymap (kbd "H-s j") 'sp-join-sexp)
  (define-key sp-keymap (kbd "H-s s") 'sp-split-sexp))

(defun tl//adaptive-smartparent-pair-overlay-face ()
  (set-face-attribute 'sp-pair-overlay-face nil
                      :inherit 'lazy-highlight
                      :background 'unspecified
                      :foreground 'unspecified))

(defun tl/smartparens-pair-newline (id action context)
  (save-excursion
    (newline)
    (indent-according-to-mode)))

(defun tl/smartparens-pair-newline-and-indent (id action context)
  (tl/smartparens-pair-newline id action context)
  (indent-according-to-mode))

(defun tl/smart-closing-parenthesis ()
  (interactive)
  (let* ((sp-navigate-close-if-unbalanced t)
         (current-pos (point))
         (current-line (line-number-at-pos current-pos))
         (next-pos (save-excursion
                     (sp-up-sexp)
                     (point)))
         (next-line (line-number-at-pos next-pos)))
    (cond
     ((and (= current-line next-line)
           (not (= current-pos next-pos)))
      (sp-up-sexp))
     (t
      (insert-char ?\))))))

(defvar tl--smartparens-enabled-initially t
  "Stored whether smartparens is originally enabled or not.")

(use-package smartparens
  :defer t
  :diminish (smartparens-mode . "")
  :commands (sp-split-sexp sp-newline sp-up-sexp smartparens-global-mode)
  :init
  ;; settings
  (setq sp-show-pair-delay  0.2
        ;; fix paren highlighting in normal mode
        sp-show-pair-from-inside t
        sp-cancel-autoskip-on-backward-movement nil
        sp-highlight-pair-overlay nil
        sp-highlight-wrap-overlay nil
        sp-highlight-wrap-tag-overlay nil)

  (tl/set-leader-keys
    "js" 'sp-split-sexp
    "jn" 'sp-newline)
  :config
  (require 'smartparens-config)
  (require 'smartparens-rust)
  (defun tl//smartparens-disable-before-expand-snippet ()
    "Handler for `yas-before-expand-snippet-hook'.
Disable smartparens and remember its initial state."
    ;; Remember the initial smartparens state only once, when expanding a top-level snippet.
    (setq tl--smartparens-enabled-initially smartparens-mode)
    (when smartparens-mode
      (smartparens-mode -1)))


  (defun tl//smartparens-restore-after-exit-snippet ()
    "Handler for `yas-after-exit-snippet-hook'.
 Restore the initial state of smartparens."
    (when tl--smartparens-enabled-initially
      (smartparens-mode +1)))

  (with-eval-after-load 'yasnippet
    (add-hook 'yas-before-expand-snippet-hook
              #'tl//smartparens-disable-before-expand-snippet)
    (add-hook 'yas-after-exit-snippet-hook
              #'tl//smartparens-restore-after-exit-snippet))

  (add-to-list 'sp-ignore-modes-list 'haskell-mode)
  ;; (define-key evil-insert-state-map ")" 'tl/smart-closing-parenthesis)
  (tl//adaptive-smartparent-pair-overlay-face)
  ;; (smartparens-global-strict-mode t) ; strict mode can not worked with subword
  (show-smartparens-global-mode +1)
  ;; don't create a pair with single quote in minibuffer
  (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)
  (sp-pair "{" nil :post-handlers
           '(:add (tl/smartparens-pair-newline-and-indent "RET")))
  (sp-pair "[" nil :post-handlers
           '(:add (tl/smartparens-pair-newline-and-indent "RET")))
  ;; markdown-mode
  (sp-with-modes '(markdown-mode gfm-mode rst-mode)
    (sp-local-pair "*" "*" :bind "C-*")
    (sp-local-tag "2" "**" "**")
    (sp-local-tag "s" "```scheme" "```")
    (sp-local-tag "<"  "<_>" "</_>" :transform 'sp-match-sgml-tags))

  ;; tex-mode latex-mode
  (sp-with-modes '(tex-mode plain-tex-mode latex-mode)
    (sp-local-tag "i" "\"<" "\">"))

  ;; html-mode
  (sp-with-modes '(html-mode sgml-mode)
    (sp-local-pair "<" ">"))

  ;; haskell-mode
  (sp-with-modes '(haskell-mode inferior-haskell-mode)
    (sp-local-pair "\\{-" "-\\}"))

  ;; lisp modes
  (sp-with-modes sp--lisp-modes
    (sp-local-pair "(" nil :bind "C-(")))

(smartparens-global-mode +1)


;; (defun tl/sp-lisp-binding (map)
;;   (define-key map (kbd "M-s") 'sp-splice-sexp)
;;   (define-key map (kbd "M-S") 'sp-split-sexp)
;;   (define-key map (kbd "M-r") 'sp-splice-sexp-killing-around))

;; (tl/sp-lisp-binding lisp-mode-map)
;; (tl/sp-lisp-binding emacs-lisp-mode-map)


(use-package sexp-transient-state
  :after (smartparens evil)
  :init
  (tl/set-leader-keys "k" 'sexp-transient-state/body)

  (with-eval-after-load 'evil
    (define-key evil-normal-state-map "gs" 'sexp-transient-state/body)))

;; [[https://www.reddit.com/r/emacs/comments/b9ubvn/evil_smartparens_what_keybindings_do_you_use/][[Evil + Smartparens] What keybindings do you use? : emacs]]
;; (nvmap :prefix ", ,"
;;   :keymaps 'smartparens-mode-map
;;   "<" '(sp-backward-barf-sexp :wk "Barf backward")
;;   ">" '(sp-forward-barf-sexp :wk "Barf forward")
;;   "(" '(sp-backward-slurp-sexp :wk "Slurp backward")
;;   ")" '(sp-forward-slurp-sexp :wk "Slurp forward")
;;   "}" '(sp-slurp-hybrid-sexp :wk "Slurp (hybrid)")
;;   "+" '(sp-join-sexp :wk "Join")
;;   "-" '(sp-split-sexp :wk "Split")
;;   "a" '(sp-absorb-sexp :wk "Absorb")
;;   "c" '(sp-clone-sexp :wk "Clone")
;;   "C" '(sp-convolute-sexp :wk "Convolute")
;;   "m" '(sp-mark-sexp :wk "Mark")
;;   "r" '(sp-raise-sexp :wk "Raise")
;;   "s" '(sp-splice-sexp-killing-around :wk "Splice")
;;   "t" '(sp-transpose-sexp :wk "Transpose")
;;   "T" '(sp-transpose-hybrid-sexp :wk "Transpose (hybrid)")
;;   ;; Narrow and Widen, use default emacs for widening
;;   "n" '(sp-narrow-to-sexp :wk "Narrow"))


(provide '50smartparens)
;;; 50smartparens.el ends here
