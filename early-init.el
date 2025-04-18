;;; early-init.el -*- lexical-binding: t; -*-

;; Emacs HEAD (27+) introduces early-init.el, which is run before init.el,
;; before package and UI initialization happens.

;;; ENV
;; https://emacs-lsp.github.io/lsp-mode/page/performance/#use-plists-for-deserialization
(setenv "LSP_USE_PLISTS" "true")

;; (setenv "SBCL_HOME" "/usr/lib/sbcl")
;; this gives matlab access to the X11 windowing system, so I can see figures, etc.
;; (setenv "DISPLAY" ":0.0")

;; doom-emacs: A big contributor to startup times is garbage collection. We up
;; the gc threshold to temporarily prevent it from running, then reset it later
;; by enabling `gcmh-mode'. Not resetting it will cause stuttering/freezes.
;; https://github.com/hlissner/doom-emacs/issues/3108
(setq gc-cons-threshold most-positive-fixnum)


;; In Emacs 27+, package initialization occurs before `user-init-file' is
;; loaded, but after `early-init-file'. Doom handles package initialization, so
;; we must prevent Emacs from doing it early!
(setq package-enable-at-startup nil)
(advice-add #'package--ensure-init-file :override #'ignore)

;; Prevent the glimpse of un-styled Emacs by disabling these UI elements early.
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
;; (push '(vertical-scroll-bars . right) default-frame-alist)

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
(setq frame-inhibit-implied-resize t)

;; Ignore X resources; its settings would be redundant with the other settings
;; in this file and can conflict with later config (particularly where the
;; cursor color is concerned).
(advice-add #'x-apply-session-resources :override #'ignore)

;; https://old.reddit.com/r/emacs/comments/1bfk7mj/how_to_disable_the_startup_blinding_white/
(setq default-frame-alist
      '((ns-appearance . dark)
        (ns-transparent-titlebar . t)
        (foreground-color . "#839496")
        ;; Setting the face in here prevents flashes of
        ;; color as the theme gets activated
        (background-color . "#002b36")))
