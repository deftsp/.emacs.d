;;; 50dart-mode.el ---                               -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>
;; Keywords:



(use-package dart-mode
  :defer t
  :mode "\\.dart\\'"
  :config
  (progn
    (defun paloryemacs/dart-mode-init ()
      (dart-server +1))

    (add-hook 'dart-mode-hook #'paloryemacs/dart-mode-init)))


(use-package dart-server
  :after dart-mode
  :init
  (progn
    (setq dart-server-format-on-save t)

    (defun dart/show-buffer ()
      "Shows information at point in a new buffer"
      (interactive)
      (dart-server-show-hover t))

    (paloryemacs/declare-prefix-for-mode 'dart-mode "mf" "find")
    (paloryemacs/declare-prefix-for-mode 'dart-mode "mh" "help")
    (paloryemacs/set-leader-keys-for-major-mode 'dart-mode
      "=" 'dart-server-format
      "?" 'dart-server-show-hover
      "g" 'dart-server-goto
      "hh" 'dart-server-show-hover
      "hb" 'dart/show-buffer
      ;; There's an upstream issue with this command:
      ;; dart-server-find-refs on int opens a dart buffer that keeps growing in size #11
      ;; https://github.com/bradyt/dart-server/issues/11
      ;; When/if it's fixed, add back the key binding:
      ;; ~SPC m f f~ Find reference at point.
      ;; to the readme.org key binding table.
      ;; "ff" 'dart-server-find-refs
      "fe" 'dart-server-find-member-decls
      "fr" 'dart-server-find-member-refs
      "fd" 'dart-server-find-top-level-decls)

    (evil-define-key 'insert dart-server-map
      (kbd "<tab>") 'dart-server-expand
      (kbd "C-<tab>") 'dart-server-expand-parameters)

    (evil-set-initial-state 'dart-server-popup-mode 'motion)
    (evil-define-key 'motion dart-server-popup-mode-map
      (kbd "gr") 'dart-server-do-it-again)))

(use-package flutter
  :defer t
  :after dart-mode
  :config
  (with-eval-after-load "evil-evilified-state"
    (evilified-state-evilify-map flutter-mode-map
      :mode flutter-mode
      :bindings

      "r"  hot-reload
      "R"  hot-restart
      "h"  help
      "w"  widget-hierarchy
      "t"  rendering-tree
      "L"  layers
      "S"  accessibility-traversal-order
      "U"  accessibility-inverse-hit-test-order
      "i"  inspector
      "p"  construction-lines
      "o"  operating-systems
      "z"  elevation-checker
      "P"  performance-overlay
      "a"  timeline-events
      "s"  screenshot
      "d"  detatch
      "q"  quit
      ))
  :init
  (defun paloryemacs/flutter-run-web ()
    (interactive)
    (flutter-run "-d web"))

  (defun paloryemacs/flutter-run-chrome ()
    (interactive)
    ;; (flutter-run "-d chrome")
    (require 'flutter)
    (flutter--with-run-proc
     "-d chrome"
     (unless (get-buffer-window flutter-buffer-name)
       (pop-to-buffer-same-window buffer))))

  (defun paloryemacs/flutter-show-buffer ()
    (interactive)
    (pop-to-buffer flutter-buffer-name))

  (paloryemacs/declare-prefix-for-mode 'dart-mode "mx" "flutter")
  (paloryemacs/set-leader-keys-for-major-mode 'dart-mode
    "xc" 'paloryemacs/flutter-run-chrome
    "xb" 'paloryemacs/flutter-show-buffer
    "xw" 'paloryemacs/flutter-run-web
    "xx" 'flutter-run-or-hot-reload))

;; Optional
(use-package flutter-l10n-flycheck
  :after flutter
  :config
  (flutter-l10n-flycheck-setup))


(provide '50dart-mode)
