;;; 50flashcard.el ---

;; (setq fc-base "~/.emacs.d/packages/flashcard")
;; (setq fc-default-lang 'en)
;; (load "~/.emacs.d/packages/flashcard/fc")

;; (add-hook 'flashcard-wash-question-hook
;;           'boxquote-question)

;; (defun boxquote-question ()
;;   "Boxquote the current question in flashcard."
;;   (boxquote-region (point-min) (point-max))
;;   (set-marker flashcard-marker (point-max)))

;; (add-to-list 'auto-mode-alist '("\\.deck\\'" . flashcard-mode))

;; (add-hook 'flashcard-mode-hook
;;           'flashcard-add-scroll-to-bottom)
;; (add-hook 'flashcard-positive-feedback-functions
;;           'flashcard-feedback-highlight-answer)
;; (add-hook 'flashcard-positive-feedback-functions
;;           'flashcard-feedback-congratulate)
;; (add-hook 'flashcard-positive-feedback-functions
;;           'flashcard-method-leitner-positive-feedback)

;; (set-face-attribute 'flashcard-question-face nil :height 1.5)
;; (set-face-attribute 'flashcard-answer-face nil :height 1.5)


(provide '50flashcard)