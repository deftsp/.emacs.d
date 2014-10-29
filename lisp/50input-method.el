;;; 50input-method.el ---

;;; eim
;; https://github.com/wenbinye/emacs-eim
(add-to-list 'load-path "~/.emacs.d/site-lisp/eim")
(autoload 'eim-use-package "eim" "Another emacs input method")
(setq eim-use-tooltip nil) ; 暂时还不好用

(register-input-method "eim-wb" "euc-cn" 'eim-use-package "五笔" "汉字五笔输入法" "wb.txt")
(register-input-method "eim-py" "euc-cn" 'eim-use-package "拼音" "汉字拼音输入法" "py.txt")
(register-input-method "eim-eb" "euc-cn" 'eim-use-package "二笔" "汉字拼音输入法" "cjeb.txt")

(autoload 'eim-insert-ascii "eim-extra")
(global-set-key ";" 'eim-insert-ascii) ; 暂时输入英文

(set-default 'default-input-method "eim-py")

;;; emacs-inline-patch
;; (setq default-input-method "MacOSX")
;; (add-hook 'minibuffer-setup-hook 'mac-change-language-to-us)
;; (add-hook 'after-init-hook 'mac-change-language-to-us)

;; (with-eval-after-load 'evil-state
;;   (add-hook 'evil-normal-state-entry-hook 'mac-change-language-to-us))

;; (mac-set-input-method-parameter  ""com.apple.inputmethod.TCIM.Pinyin"" `cursor-color "red")
;; (mac-set-input-method-parameter "com.apple.keylayout.US" `cursor-color "blue")


(provide '50input-method)
