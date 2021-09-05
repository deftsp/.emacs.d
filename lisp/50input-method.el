;; 50input-method.el ---

;; Copyright (C) 2007  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>
;; Keywords:

(global-set-key (kbd "s-/") 'toggle-input-method)

(defun tl/on-select-previous-input-source (data)
  "Swith to insert when switch to Rime input method"
  (let ((source-id (plist-get data :source-id)))
    (when (string-equal source-id "im.rime.inputmethod.Squirrel.Rime")
      (with-selected-window (selected-window)
        (let ((state (bound-and-true-p evil-state)))
          (when (and state
                     (eq state 'normal)
                     (not (minibufferp))
                     (not isearch-mode))
            (call-interactively 'evil-insert-state)))))))


(use-package rime
  :after (which-key)
  :demand
  :custom
  (rime-user-data-dir "~/Library/Rime/")
  (rime-librime-root "~/.emacs.d/librime/dist")
  (rime-emacs-module-header-root "/Applications/Emacs.app/Contents/Resources/include")
  (rime-show-preedit t)
  :bind (:map rime-mode-map
	     ("C-." . 'rime-send-keybinding) ; 中英文标点切换
	     ("S-SPC" . 'rime-send-keybinding) ; 全半角切换
	     ("C-s" . 'rime-send-keybinding) ; 输入法菜单
	     ("C-l" . 'rime-inline-ascii)
	     ("M-j" . 'rime-force-enable) ; 强制切换到中文模式
	     :map rime-active-mode-map
	     ("C-l" . 'rime-inline-ascii)
	     ("S-SPC" . 'rime-send-keybinding))
  :config
  ;; Emacs will automatically set default-input-method to rfc1345 if locale is
  ;; UTF-8. https://github.com/purcell/emacs.d/issues/320
  (add-hook 'emacs-startup-hook (lambda () (setq default-input-method "rime")))
  ;; mod-line 输入法图标高亮, 用来区分中英文输入状态
  ;; (setq mode-line-mule-info '((:eval (rime-lighter))))
  ;; support shift-l, shift-r, control-l, control-r
  ;; 只有当使用系统 RIME 输入法时才有效。
  (setq rime-inline-ascii-trigger 'control-l)

  ;; If one of these functions return t, the input-method will fallback to ascii mode.
  (setq rime-disable-predicates '(rime-predicate-ace-window-p
                                  rime-predicate-evil-mode-p
                                  rime-predicate-hydra-p
                                  rime-predicate-which-key-activate-p
                                  rime-predicate-current-uppercase-letter-p
                                  rime-predicate-after-alphabet-char-p
                                  rime-predicate-space-after-cc-p
                                  rime-predicate-punctuation-after-space-cc-p
                                  rime-predicate-prog-in-code-p
                                  rime-predicate-after-ascii-char-p))
  (defun rime-predicate-which-key-activate-p () which-key--automatic-display)
  ;;FIXME: font size not work
  (setq rime-posframe-properties
        (list
         ;; :font "Sarasa Gothic SC-13"
         :font "PragmataPro-16"
         :internal-border-width 10))

  (setq rime-show-candidate 'posframe
        rime-posframe-style 'vertical))

(provide '50input-method)
