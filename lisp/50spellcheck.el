;;; 50spellcheck.el ---                          -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>
;; Keywords:

(use-package flyspell
  :defer t
  :diminish (flyspell-mode . " S")
  :commands (spell-checking/change-dictionary)
  :init
  (progn
    ;; don't use Ispell, but the more modern Aspell
    (setq ispell-program-name "aspell")
    (setq flyspell-issue-message-flag nil)
    (setq flyspell-duplicate-distance 0)

    ;; save new words in pdict without questioning
    (setq ispell-silently-savep t)

    ;; Using https://github.com/redguardtoo/wucuo instead
    ;; programming language modes use flyspell-prog-mode and not normal spell-check.
    ;; (add-hook 'prog-mode-hook 'flyspell-prog-mode)

    ;; (defun tl/flyspell-mode-init ()
    ;;   ;; (auto-dictionary-mode +1)
    ;;   )

    ;; (add-hook 'flyspell-mode-hook 'tl/flyspell-mode-init)

    (defun tl/toggle-flyspell-mode ()
      (interactive)
      (if (bound-and-true-p flyspell-mode)
          (progn
            (flyspell-mode-off)
            (when (fboundp 'auto-dictionary-mode)
              (auto-dictionary-mode -1)))
        (if (derived-mode-p 'prog-mode)
            (flyspell-prog-mode)
          (flyspell-mode +1))))

    (advice-add 'ispell-init-process :around #'tl/suppress-messages)
    (advice-add 'ispell-kill-ispell :around #'tl/suppress-messages)

    (tl/declare-prefix "S" "spelling")
    (tl/declare-prefix "Sa" "add word to dict")
    (tl/set-leader-keys
      "Sab" 'tl/add-word-to-dict-buffer
      "Sag" 'tl/add-word-to-dict-global
      "Sas" 'tl/add-word-to-dict-session
      "Sb" 'flyspell-buffer
      "Sc"  'flyspell-correct-wrapper
      "Sd" 'spell-checking/change-dictionary
      "Sn" 'flyspell-goto-next-error
      "Ss" 'flyspell-correct-at-point
      "St" 'tl/toggle-flyspell-mode)))


;; wucuo base on flyspell, turn off flyspell-prog-mode and flyspell-mode before using this program. the configuration
;; for flyspell still works.
(use-package wucuo
  :commands (wucuo-start)
  :init
  (add-hook 'prog-mode-hook #'wucuo-start)
  (add-hook 'text-mode-hook #'wucuo-start))

(use-package flyspell-correct
  :after flyspell
  :commands (flyspell-correct-at-point
             flyspell-correct-wrapper))

;; (use-package flyspell-correct-avy-menu
;;   :after flyspell
;;   :commands (flyspell-correct-avy-menu)
;;   :init
;;   (setq flyspell-correct-interface #'flyspell-correct-avy-menu))

(use-package flyspell-correct-ivy
  :after flyspell
  :commands (flyspell-correct-ivy)
  :init
  (setq flyspell-correct-interface #'flyspell-correct-ivy))

;; (use-package flyspell-correct-popup
;;   :after flyspell
;;   :init
;;   (setq flyspell-correct-interface #'flyspell-correct-popup))


;; (use-package auto-dictionary
;;   :defer t
;;   :after flyspell
;;   :init
;;   (progn
;;     ;; Select the buffer local dictionary if it was set, otherwise
;;     ;; auto-dictionary will replace it with a guessed one at each activation.
;;     ;; https://github.com/nschum/auto-dictionary-mode/issues/5
;;     (defun tl//adict-set-local-dictionary ()
;;       "Set the local dictionary if not nil."
;;       (when (and (fboundp 'adict-change-dictionary)
;;                  ispell-local-dictionary)
;;         (adict-change-dictionary ispell-local-dictionary)))
;;     (add-hook 'auto-dictionary-mode-hook
;;               'tl//adict-set-local-dictionary 'append)))


(defun spell-checking/change-dictionary ()
  "Change the dictionary. Use the ispell version if
auto-dictionary is not used, use the adict version otherwise."
  (interactive)
  (if (fboundp 'adict-change-dictionary)
      (adict-change-dictionary)
    (call-interactively 'ispell-change-dictionary)))

;; After we add a word to ispell or correct something, flyspell's highlighting may become outdated. Let's re-run
;; highlighting after a correction.
;; (defadvice ispell (after advice)
;;   (flyspell-buffer))
;; (ad-activate 'ispell t)
;; (defadvice ispell-word (after advice)
;;   (flyspell-buffer))
;; (ad-activate 'ispell-word t)

(defun tl/add-word-to-dict-buffer ()
  "Save word at point as correct in current buffer."
  (interactive)
  (tl//add-word-to-dict 'buffer))


;; ispell-personal-dictionary
;; "File name of your personal spelling dictionary, or nil. If nil, the default
;; personal dictionary, (\"~/.ispell_DICTNAME\" for ispell or
;; \"~/.aspell.LANG.pws\" for aspell) is used, where DICTNAME is the name of
;; your default dictionary and LANG the two letter language code."

(defun tl/add-word-to-dict-global ()
  "Save word at point as a correct word globally."
  (interactive)
  (tl//add-word-to-dict 'save))

(defun tl/add-word-to-dict-session ()
  "Save word at point as correct in current session."
  (interactive)
  (tl//add-word-to-dict 'session))

(defun tl//add-word-to-dict (scope)
  "Save word at point as a correct word.
SCOPE can be:
`save' to save globally,
`session' to save in current session or
`buffer' for buffer local."
  (let ((current-location (point))
        (word (flyspell-get-word)))
    (when (consp word)
      (if (tl//word-in-dict-p (car word))
          (error "%s is already in dictionary" (car word))
        (progn
          (flyspell-do-correct scope nil (car word) current-location
                               (cadr word) (caddr word) current-location)
          (ispell-pdict-save t))))))

(defun tl//word-in-dict-p (word)
  "Check if WORD is defined in any of the active dictionaries."
  ;; use the correct dictionary
  (flyspell-accept-buffer-local-defs)
  (let (poss ispell-filter)
    ;; now check spelling of word.
    (ispell-send-string "%\n")	;put in verbose mode
    (ispell-send-string (concat "^" word "\n"))
    ;; wait until ispell has processed word
    (while (progn
             (accept-process-output ispell-process)
             (not (string= "" (car ispell-filter)))))
    ;; Remove leading empty element
    (setq ispell-filter (cdr ispell-filter))
    ;; ispell process should return something after word is sent.
    ;; Tag word as valid (i.e., skip) otherwise
    (or ispell-filter
        (setq ispell-filter '(*)))
    (if (consp ispell-filter)
        (setq poss (ispell-parse-output (car ispell-filter))))
    (or (eq poss t) (stringp poss))))


(provide '50spellcheck)
