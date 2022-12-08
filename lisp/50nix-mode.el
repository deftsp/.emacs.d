;;; 50nix-mode.el ---                               -*- lexical-binding: t; -*-

(use-package nix-mode
  :defer t
  :mode "\\.nix\\'"
  :config
  (general-define-key
   :states 'normal
   :keymaps 'nix-mode-map
   :prefix ","
   "p"  'nix-mode-format))




(provide '50nix-mode)
