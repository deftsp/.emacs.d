;; 50input-method.el ---

;; Copyright (C) 2007  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>
;; Keywords:

;; https://github.com/laishulu/emacs-smart-input-source
(use-package sis
  :straight t
  :config
  ;; "com.apple.keylayout.US" 是 macOS 标准英文布局 ID
  (sis-ism-lazyman-config
   "com.apple.keylayout.US"
   ;; "im.rime.inputmethod.Squirrel.Rime"
   "com.apple.keylayout.US")

  ;; 2. 开启上下文感知，确保你在 Emacs 任何操作都不会意外触发系统输入法切换
  (sis-global-respect-mode t)
  (sis-global-context-mode t))


;; (global-set-key (kbd "s-/") 'toggle-input-method)

;; called by karabiner
(defun tl//on-m-comma-pressed ()
  (interactive)
  (with-current-buffer (window-buffer (selected-window))
    (activate-input-method "rime")
    (let ((state (bound-and-true-p evil-state)))
      (when (and state
                 (eq state 'normal)
                 (not (minibufferp))
                 (not isearch-mode))
        (call-interactively 'evil-insert-state)))))

;; (defun tl/on-select-previous-input-source (data)
;;   "Swith to insert when switch to Rime input method"
;;   (let ((source-id (plist-get data :source-id)))
;;     (when (string-equal source-id "im.rime.inputmethod.Squirrel.Rime")
;;       (with-selected-window (selected-window)
;;         (let ((state (bound-and-true-p evil-state)))
;;           (when (and state
;;                      (eq state 'normal)
;;                      (not (minibufferp))
;;                      (not isearch-mode))
;;             (call-interactively 'evil-insert-state)))))))

;; called by karabiner
;; (defun tl//on-esc--pressed ()
;;   (interactive)
;;   (with-current-buffer (window-buffer (selected-window))
;;     ;; 清除 Evil 记录的“上一次输入法”状态
;;     ;; 这样下次进入 insert 模式时，它就不会自动恢复了
;;     (when (boundp 'evil-input-method)
;;       (setq evil-input-method nil))
;;     (deactivate-input-method)))

;; emacs-rime 不跟 squirrel 共享安装目录，否则会锁数据库，导致 emacs-rime 或 Squirrel 其中一个无法写入自造词
;; 执行 ~/.emacs.d/scripts/setup_emacs_rime.sh, 链接 ~/Library/Rime 下的词库及输入法定义到 ~/.emacs.d/rime
;; M-x rime-sync, Squirrel 点击 Sync user data. 同步数据到 ~/Library/Rime/sync 下的不同文件夹，并且跟 sync 文件夹下的其他同步目录合并
(use-package rime
  :after (which-key)
  :demand
  :init
  (setq default-input-method "rime")
  :custom
  (rime-show-preedit t)
  (rime-user-data-dir "~/.emacs.d/rime/")
  (rime-librime-root
   (case system-type
     ;; (darwin "~/.emacs.d/librime/dist")
     ;; brew install librime
     ;; 如果是 Apple Silicon 芯片，设置 librime 目录
     (darwin "/opt/homebrew")
     (gnu/linux
      (s-trim-right (shell-command-to-string "nix-build '<nixpkgs>' --no-build-output -A librime")))))
  (rime-emacs-module-header-root
   (case system-type
     (darwin "/Applications/Emacs.app/Contents/Resources/include")
     (gnu/linux
      (concat
       (s-trim-right
        (shell-command-to-string "nix-build '<nixpkgs>' --no-build-output -A emacs"))
       "/include"))))

  :bind (:map rime-mode-map
	     ("C-." . 'rime-send-keybinding) ; 中英文标点切换
	     ("C-," . 'rime-send-keybinding) ; 全半角切换
	     ("C-s" . 'rime-send-keybinding) ; 输入法菜单
	     ;; ("C-S-l" . 'rime-inline-ascii)
	     ("C-l" . 'rime-force-enable) ; 强制切换到中文模式
	     :map rime-active-mode-map
	     ;; ("C-S-l" . 'rime-inline-ascii)
         ("C-l" . 'rime-force-enable)
	     ("S-SPC" . 'rime-send-keybinding))
  :config
  ;; FIX: librime 1.16.1，在退出 Emacs 时会崩溃，加入以下代码可以解决，老版本的 1.7.1 没这个问题
  (add-hook 'kill-emacs-hook #'rime-lib-finalize)

  (define-key rime-mode-map (kbd "<f13>") 'rime-inline-ascii)
  (define-key rime-active-mode-map (kbd "<f13>") 'rime-inline-ascii)

  ;; Emacs will automatically set default-input-method to rfc1345 if locale is
  ;; UTF-8. https://github.com/purcell/emacs.d/issues/320

  ;; (setq rime-title "ㄓ")

  ;; - 启动输入法后 mode-line-mule-info 会显示 current-input-method 的 title
  ;; - 对于 rime 输入法，显示的是其在调用 register-input-method 注册输入法的参数 rime-title, 这是静态图标

  ;; - 若用(setq mode-line-mule-info '((:eval (rime-lighter)))) 直接设置，将覆盖其他有用信息,
  ;; - 从 mode-line-mule-info 中移除 current-input-method, 输入法图标改由 powerline 通过 rime-lighter 独立渲染
  (setq-default mode-line-mule-info
                `(""
                  (:propertize "%z"
                   help-echo mode-line-mule-info-help-echo
                   mouse-face mode-line-highlight
                   local-map ,mode-line-coding-system-map)
                  (:eval (mode-line-eol-desc))))

  ;; 只有当使用系统 RIME 输入法时才有效。该配置只是让 emacs-rime 跟 Rime 的配置同步，并不是配置在 Emacs 里按 shift-l 切
  ;; 换临时英语模式
  (setq rime-inline-ascii-trigger 'shift-l) ;; support shift-l, shift-r, control-l, control-r

  ;; If one of these functions return t, the input-method will fallback to ascii mode.
  (setq rime-disable-predicates '(rime-predicate-ace-window-p
                                  rime-predicate-evil-mode-p
                                  rime-predicate-hydra-p
                                  rime-predicate-which-key-activate-p
                                  rime-predicate-current-uppercase-letter-p
                                  ;; rime-predicate-space-after-cc-p
                                  ;; rime-predicate-space-after-ascii-p
                                  ;; rime-predicate-punctuation-after-space-cc-p
                                  ;; 紧跟英文字符后，继续英文
                                  rime-predicate-after-alphabet-char-p
                                  ;; rime-predicate-after-ascii-char-p
                                  rime-predicate-prog-in-code-p))

  (setq rime-inline-predicates nil)

  (defun rime-predicate-which-key-activate-p () which-key--automatic-display)
  (setq rime-posframe-properties
        (list
         ;; :font "Sarasa Gothic SC-28"
         :font "PragmataPro-16"
         :internal-border-width 10))

  (setq rime-show-candidate 'posframe
        rime-posframe-style 'vertical))

(provide '50input-method)
