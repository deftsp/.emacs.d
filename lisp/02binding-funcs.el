;;; 02binding-funcs.el

;; https://github.com/noctuid/general.el
(use-package general
  :config
  (general-override-mode +1))

(defmacro tl/set-leader-keys (&rest bindings)
  `(general-define-key
    :keymaps '(normal insert emacs)
    :prefix dottl-leader-key
    :non-normal-prefix dottl-emacs-leader-key
    ,@bindings))

(defmacro tl/set-leader-keys-for-mode (mode &rest bindings)
  `(general-define-key
    :states '(normal)
    :keymaps (intern (format "%s-map" ,mode))
    :prefix ","
    ,@bindings))

(put 'tl/set-leader-keys-for-mode 'lisp-indent-function 'defun)

(defun tl/declare-prefix (prefix name &optional long-name)
  "Declare a prefix PREFIX. PREFIX is a string describing a key
sequence. NAME is a string used as the prefix command.
LONG-NAME if given is stored in `tl/prefix-titles'."
  (let* ((command name)
         (full-prefix (concat dottl-leader-key " " prefix))
         (full-prefix-emacs (concat dottl-emacs-leader-key " " prefix))
         (full-prefix-lst (listify-key-sequence (kbd full-prefix)))
         (full-prefix-emacs-lst (listify-key-sequence
                                 (kbd full-prefix-emacs))))
    ;; define the prefix command only if it does not already exist
    (unless long-name (setq long-name name))
    (which-key-add-key-based-replacements
      full-prefix-emacs (cons name long-name)
      full-prefix (cons name long-name))))
(put 'tl/declare-prefix 'lisp-indent-function 'defun)

(defun tl/declare-prefix-for-mode (mode prefix name &optional long-name)
  "Declare a prefix PREFIX. MODE is the mode in which this prefix command should
be added. PREFIX is a string describing a key sequence. NAME is a symbol name
used as the prefix command."
  (let  ((command (intern (concat (symbol-name mode) name)))
         (full-prefix (concat dottl-leader-key " " prefix))
         (full-prefix-emacs (concat dottl-emacs-leader-key " " prefix))
         (is-major-mode-prefix (string-prefix-p "m" prefix))
         (major-mode-prefix (concat dottl-major-mode-leader-key
                                    " " (substring prefix 1)))
         (major-mode-prefix-emacs
          (concat dottl-major-mode-emacs-leader-key
                  " " (substring prefix 1))))
    (unless long-name (setq long-name name))
    (let ((prefix-name (cons name long-name)))
      (which-key-add-major-mode-key-based-replacements mode
        full-prefix-emacs prefix-name
        full-prefix prefix-name)
      (when (and is-major-mode-prefix dottl-major-mode-leader-key)
        (which-key-add-major-mode-key-based-replacements mode major-mode-prefix prefix-name))
      (when (and is-major-mode-prefix dottl-major-mode-emacs-leader-key)
        (which-key-add-major-mode-key-based-replacements
          mode major-mode-prefix-emacs prefix-name)))))
(put 'tl/declare-prefix-for-mode 'lisp-indent-function 'defun)


(provide '02binding-funcs)
