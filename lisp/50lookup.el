;;; 50lookup.el ---


;; https://www.powerthesaurus.org/
;; https://irreal.org/blog/?p=9102
;; https://github.com/SavchenkoValeriy/emacs-powerthesaurus
;; https://github.com/agzam/mw-thesaurus.el

(use-package powerthesaurus
  :defer t
  :straight t
  :commands
  (powerthesaurus-lookup-word
   powerthesaurus-lookup-word-at-point
   powerthesaurus-lookup-word-dwim))


(provide '50lookup)
