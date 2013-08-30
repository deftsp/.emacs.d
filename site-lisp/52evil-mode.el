;;; 52evil-mode.el ---                               -*- lexical-binding: t; -*-

;; Copyright (C) 2013  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>
;; Keywords:


(require 'evil nil t)



(defface pl/evil-normal-tag
  `((t (:weight bold :foreground "orchid")))
  "Evil normal mode indicator face")

(defface pl/evil-insert-tag
  `((t (:weight bold :foreground "DeepPink")))
  "Evil insert mode indicator face")

(defface pl/evil-emacs-tag
  `((t (:weight bold :foreground "#7cfa42")))
  "Evil emacs mode indicator face")

(defface pl/evil-visual-tag
  `((t (:weight bold :foreground "Purple")))
  "Evil visual mode indicator face")

(defface pl/evil-motion-tag
  `((t (:weight bold :foreground "Cyan")))
  "Evil motion mode indicator face")

(defface pl/evil-operator-tag
  `((t (:weight bold :foreground "LawnGreen")))
  "Evil operator mode indicator face")


;; Visual indicators
(setq evil-mode-line-format 'before

      evil-emacs-state-tag    (propertize "« E »" 'face 'pl/evil-emacs-tag)
      evil-normal-state-tag   (propertize "« ☢ »" 'face 'pl/evil-normal-tag)
      evil-insert-state-tag   (propertize "« I »" 'face 'pl/evil-insert-tag)
      evil-motion-state-tag   (propertize "« M »" 'face 'pl/evil-motion-tag)
      evil-visual-state-tag   (propertize "« ∞ »" 'face 'pl/evil-visual-tag)
      evil-operator-state-tag (propertize "« O »" 'face 'pl/evil-operator-tag)

      evil-emacs-state-cursor    nil

      evil-normal-state-cursor   `(hollow ,(face-attribute 'pl/evil-normal-tag :foreground))
      evil-insert-state-cursor   `(box ,(face-attribute 'pl/evil-insert-tag :foreground))
      evil-motion-state-cursor   `(box ,(face-attribute 'pl/evil-motion-tag :foreground))
      evil-visual-state-cursor   `(box ,(face-attribute 'pl/evil-visual-tag :foreground))
      evil-operator-state-cursor `(box ,(face-attribute 'pl/evil-operator-tag :foreground)))

;; 'imap jf <ESC>' equivalent
;; (define-key evil-insert-state-map (kbd "j f") 'evil-normal-state)

;; Getting :n[ew] to work
;; As of this writing, Evil does not allow you to shorten ':new' to ':n', but you can define a command that does.
;; (evil-ex-define-cmd "n[ew]" 'evil-window-new)

;; Modes that should be insert state by default
;; (dolist (mode '(sql-interactive-mode
;;                 magit-log-edit-mode erlang-shell-mode
;;                 dired-mode inferior-moz-mode inferior-octave-mode
;;                 inferior-ess-mode
;;                 grep-mode pylookup-mode))
;;   (add-to-list 'evil-insert-state-modes mode))


(setq evil-default-state 'emacs)

;;; esc quits
;; (define-key evil-normal-state-map [escape] 'keyboard-quit)
;; (define-key evil-visual-state-map [escape] 'keyboard-quit)
;; (define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
;; (define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
;; (define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
;; (define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
;; (define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

(if (fboundp 'evil-mode)
    (evil-mode 1))

(provide '52evil-mode)
;;; 50evil-mode.el ends here
