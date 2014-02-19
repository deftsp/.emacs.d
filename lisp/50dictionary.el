;;; 50dictionary.el ---
;;
;; Description:
;; Author: Shihpin Tseng <detfsp@gmail.com>
;; Created: Thu Sep 13 14:17:37 2007
;; Version:

;;; keys
(global-set-key (kbd "C-c d d") 'sdcv-search)
(global-set-key (kbd "C-c d f") 'dictionary-lookup-definition)
(global-set-key (kbd "C-c d s") 'dictionary-search)
(global-set-key (kbd "C-c d m") 'dictionary-match-words)
;; (global-set-key [mouse-3] 'dictionary-mouse-popup-matching-words)
(global-set-key (kbd "C-c d p") 'sdcv-with-tooltip)
(global-set-key (kbd "C-c d w") 'wordnet-search)


(eval-after-load 'dictionary
  '(progn
     (setq dictionary-coding-systems-for-dictionaries
           (append '(("moecomp" . utf-8)
                     ("netterm" . utf-8)
                     ("pydict"  . utf-8)
                     ("cedict"  . utf-8)
                     ("stardic" . utf-8)
                     ("cpatch"  . utf-8)
                     ("xdict"   . utf-8)
                     ("cdict"   . gbk))
                   dictionary-coding-systems-for-dictionaries))
     ;; search online
     ;; (setq dictionary-proxy-port 31280
     ;;       dictionary-proxy-server "sc.net9.org"
     ;;       dictionary-use-http-proxy t)

     ;; search at home
     (setq dictionary-server "localhost"
           dictionary-tooltip-dictionary "wn"
           dictionary-default-dictionary "*")

     ;; for dictionary tooltip mode
     ;; choose the dictionary: "wn" for WordNet
     ;; "web1913" for Webster's Revised Unabridged Dictionary(1913)
     ;; so on

     (setq global-dictionary-tooltip-mode nil
           dictionary-tooltip-mode nil)))

(defun pl/dictionary-next-dictionary ()
  (interactive)
  (end-of-line)
  (search-forward-regexp "^From" nil t)
  (beginning-of-line))

(defun pl/dictionary-prev-dictionary ()
  (interactive)
  (beginning-of-line)
  (search-backward-regexp "^From" nil t)
  (beginning-of-line))

(defun pl/dictionary-mode-hook ()
  (define-key dictionary-mode-map (kbd "<backtab>") 'dictionary-prev-link)
  (define-key dictionary-mode-map (kbd "n") 'pl/dictionary-next-dictionary)
  (define-key dictionary-mode-map (kbd "p") 'pl/dictionary-prev-dictionary))


(eval-after-load "dictionary"
  '(add-hook 'dictionary-mode-hook 'pl/dictionary-mode-hook))

;;; wordnet
(condition-case nil
    (require 'wordnet)
  (error nil))

;;; Emacs & Dictionary.app
;; http://larkery.tumblr.com/post/465585528/emacs-dictionary-app
(defun pl/mac-open-dictionary (the-word)
  "Open Dictionary.app for the-word"
  (interactive "sDictionary Lookup: ")
  (shell-command (concat "open \"dict:///" (replace-regexp-in-string "\"" "\\\\\"" the-word) "\"")))

(defun pl/lookup-current-word ()
  (interactive)
  (let ((w (current-word)))
    (if w
        (pl/mac-open-dictionary w)
      (call-interactively #'pl/mac-open-dictionary))))

(eval-after-load "key-chord"
  '(key-chord-define-global "/d" 'pl/lookup-current-word))


(provide '50dictionary)
