;;; 50solidity-mode.el ---                               -*- lexical-binding: t; -*-

;; https://github.com/ethereum/emacs-solidity
;; https://github.com/duaraghav8/Ethlint
;; npm install -g solhint

;; npm install -g solium
;; npm install -g ethlint
;; ethlint formerly named Solium

(use-package solidity-mode
  :defer t
  :init
  (add-hook 'solidity-mode-hook 'tl/init-solidity-mode)
  :config
  (setq solidity-comment-style 'slash)
  (define-key solidity-mode-map (kbd "C-c C-g") 'solidity-estimate-gas-at-point))

(defun tl/init-solidity-mode ()
  "Hooks for solidity mode."
  ;; use solhint only
  (add-to-list 'flycheck-disabled-checkers 'solium-checker))

;; https://solium.readthedocs.io/en/latest/user-guide.html#installation
(use-package solidity-flycheck
  :after solidity-mode
  :config
  (setq flycheck-solidity-solium-soliumrcfile (expand-file-name "~/.soliumrc.json"))
  (setq flycheck-solidity-solc-addstd-contracts t)
  (setq solidity-flycheck-solium-checker-active t))

(use-package company-solidity
  :after solidity-mode)

(provide '50solidity-mode)
