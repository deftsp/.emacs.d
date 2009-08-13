;;; 50eim.el ---

;; (add-to-list 'load-path "~/.emacs.d/site-lisp/eim")
(autoload 'eim-use-package "eim" "Another emacs input method")
;; Tooltip 暂时还不好用
(setq eim-use-tooltip nil)

(register-input-method "eim-wb" "euc-cn" 'eim-use-package "五笔" "汉字五笔输入法" "wb.txt")
(register-input-method "eim-py" "euc-cn" 'eim-use-package "拼音" "汉字拼音输入法" "py.txt")
(register-input-method "eim-eb" "euc-cn" 'eim-use-package "二笔" "汉字拼音输入法" "eb.txt")

;; 用 ; 暂时输入英文
(require 'eim-extra)
(global-set-key ";" 'eim-insert-ascii)

(set-default 'default-input-method "eim-eb")
