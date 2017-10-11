;; sawfish mode settings

;; this tells emacs to automatically activate the sawfish-mode whenever open
;; file with "sawfishrc" or "jl" (John Lisp) suffix
(add-to-list 'auto-mode-alist '(".*sawfishrc\\'" . sawfish-mode ))
(add-to-list 'auto-mode-alist '(".*\\.jl\\'" . sawfish-mode ))
;; if you're using ECB, tells to use the compilation buffer to show long
;; sawfish messages
;; (add-to-list 'ecb-compilation-buffer-names '("*sawfish*"))

(with-eval-after-load 'sawfish
  ;; (define-key sawfish-mode-map (kbd "C-h i") 'sawfish-info)
  (define-key sawfish-mode-map (kbd "C-h v") 'sawfish-describe-variable)
  (define-key sawfish-mode-map (kbd "C-h C-v") 'sawfish-info-variable)
  (define-key sawfish-mode-map (kbd "C-h f") 'sawfish-describe-function)
  (define-key sawfish-mode-map (kbd "C-h C-f") 'sawfish-info-function)
  (define-key sawfish-mode-map (kbd "C-h a") 'sawfish-apropos))

(provide '50sawfish)
