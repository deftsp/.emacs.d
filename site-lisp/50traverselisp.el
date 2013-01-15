;;; 50traverselisp.el ---

(require 'traverselisp)
;; (require 'anything-traverse)

;; (setq traverse-use-avfs t)

;; (global-set-key (kbd "<f5> f") 'traverse-deep-rfind)
;; (global-set-key (kbd "<f5> u") 'traverse-build-tags-in-project)
;; (global-set-key (kbd "C-c o") 'traverse-occur-current-buffer)
;; (global-set-key (kbd "C-c o") 'traverse-incremental-occur)
;; (global-set-key (kbd "C-c F") 'anything-traverse)
;; (global-set-key (kbd "C-M-|") 'traverse-toggle-split-window-h-v)


(define-key dired-mode-map (kbd "C-c t") 'traverse-dired-search-regexp-in-anything-at-point)
(define-key dired-mode-map (kbd "A") 'traverse-dired-search-regexp-in-anything-at-point)
(define-key dired-mode-map (kbd "C-c C-z") 'traverse-dired-browse-archive)

(mapc #'(lambda (x)
          (add-to-list 'traverse-ignore-files x))
      '(".ledger-cache"  "ANYTHING-TAG-FILE"))
;; (add-to-list 'traverse-ignore-dirs "emacs_backup")



(provide '50traverselisp)
