;;; 50solidity-mode.el ---                               -*- lexical-binding: t; -*-

;; https://github.com/ethereum/emacs-solidity

(use-package solidity-mode
  :defer t
  :config
  (setq solidity-comment-style 'slash)
  (define-key solidity-mode-map (kbd "C-c C-g") 'solidity-estimate-gas-at-point))

;; https://solium.readthedocs.io/en/latest/user-guide.html#installation
;; npm install -g solium
(use-package solidity-flycheck
  :after solidity-mode
  :config
  (setq flycheck-solidity-solium-soliumrcfile (expand-file-name "~/.soliumrc.json"))
  (setq flycheck-solidity-solc-addstd-contracts t)
  (setq solidity-flycheck-solium-checker-active t))

(use-package company-solidity
  :after solidity-mode)

(provide '50solidity-mode)
