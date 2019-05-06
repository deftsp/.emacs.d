;;; 50web.el ---                                -*- lexical-binding: t; -*-

;; Copyright (C) 2014  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>
;; Keywords:


;;; Commentary:

;;; Code:
(use-package simple-httpd
  :defer t
  :init
  (progn
    (setq httpd-root (expand-file-name "~/www"))))

;;; web-mode.el is an autonomous emacs major-mode for editing web templates:
;;; HTML documents embedding parts (CSS / JavaScript) and blocks (client / server side)
(defhydra paloryemacs/web-mode (:color teal)
  ("j" web-mode-element-next "next" :color red)
  ("J" web-mode-element-sibling-next "next sibling" :color red)
  ("gj" web-mode-element-sibling-next :color red)
  ("k" web-mode-element-previous "previous" :color red)
  ("K" web-mode-element-sibling-previous "previous sibling" :color red)
  ("gk" web-mode-element-sibling-previous :color red)
  ("h" web-mode-element-parent "parent" :color red)
  ("l" web-mode-element-child "child" :color red)
  ("c" web-mode-element-clone "clone")
  ("d" web-mode-element-vanish "delete")
  ("D" web-mode-element-kill "kill")
  ("r" web-mode-element-rename "rename" :exit t)
  ("w" web-mode-element-wrap "wrap")
  ("p" web-mode-dom-xpath "xpath")
  ("q" nil "quit" :exit t)
  ("<escape>" nil nil :exit t))

(use-package web-mode
  :defer t
  :init
  (progn
    (defun paloryemacs/init-web-mode ()
      "Hooks for Web mode."
      (flycheck-mode -1)

      (setq-default web-mode-comment-formats
                    '(("java" . "/*")
                      ("javascript" . "/*")
                      ("jsx" . "/*" )
                      ("php" . "/*")
                      ("css" . "/*")))

      (setq web-mode-markup-indent-offset 4
            web-mode-css-indent-offset 4
            ;; script offset indentation (for JavaScript, Java, PHP, etc.)
            web-mode-code-indent-offset 4))

    ;; treat <> as jsx
    (setq web-mode-content-types-alist
          '(("jsx" . ".*\\.js\\'")))
    (setq web-mode-script-padding 4
          web-mode-style-padding 1
          web-mode-block-padding 0))
  (add-hook 'web-mode-hook 'paloryemacs/init-web-mode)
  :config
  (progn
    (paloryemacs/declare-prefix-for-mode 'web-mode "me" "errors")
    (paloryemacs/declare-prefix-for-mode 'web-mode "mg" "goto")
    (paloryemacs/declare-prefix-for-mode 'web-mode "mh" "dom")
    (paloryemacs/declare-prefix-for-mode 'web-mode "mr" "refactor")
    (paloryemacs/set-leader-keys-for-major-mode 'web-mode
      "."  'paloryemacs/web-mode/body
      "eh" 'web-mode-dom-errors-show
      "gb" 'web-mode-element-beginning
      "gc" 'web-mode-element-child
      "gp" 'web-mode-element-parent
      "gs" 'web-mode-element-sibling-next
      "hp" 'web-mode-dom-xpath
      "rc" 'web-mode-element-clone
      "rd" 'web-mode-element-vanish
      "rk" 'web-mode-element-kill
      "rr" 'web-mode-element-rename
      "rw" 'web-mode-element-wrap
      "z" 'web-mode-fold-or-unfold)))

(use-package css-mode
  :defer t
  :init
  (progn
    (setq-default css-indent-offset 2)))

;;; skewer
(defun paloryemacs/skewer-start-repl ()
  "Attach a browser to Emacs and start a skewer REPL."
  (interactive)
  (run-skewer)
  (skewer-repl))

(defun paloryemacs/skewer-load-buffer-and-focus ()
  "Execute whole buffer in browser and switch to REPL in insert state."
  (interactive)
  (skewer-load-buffer)
  (skewer-repl)
  (evil-insert-state))

(defun paloryemacs/skewer-eval-defun-and-focus ()
  "Execute function at point in browser and switch to REPL in insert state."
  (interactive)
  (skewer-eval-defun)
  (skewer-repl)
  (evil-insert-state))

(defun paloryemacs/skewer-eval-region (beg end)
  "Execute the region as JavaScript code in the attached browser."
  (interactive "r")
  (skewer-eval (buffer-substring beg end) #'skewer-post-minibuffer))

(defun paloryemacs/skewer-eval-region-and-focus (beg end)
  "Execute the region in browser and swith to REPL in insert state."
  (interactive "r")
  (paloryemacs/skewer-eval-region beg end)
  (skewer-repl)
  (evil-insert-state))

;; 1. M-x httpd-start
;; 2. Include "http://localhost:8080/skewer" as a script
;; 3. Visit the document from your browser (in httpd-root)
(use-package skewer-mode
  :defer t
  :diminish skewer-mode
  :init
  (progn
    (defun paloryemacs/skewer-clients-mode-init ()
      (display-line-numbers-mode -1))
    (add-hook 'skewer-clients-mode-hook 'paloryemacs/skewer-clients-mode-init)

    (add-hook 'js2-mode-hook 'skewer-mode))
  :config
  (progn

    (with-eval-after-load "evil-evilified-state"
      (evilified-state-evilify-map skewer-clients-mode-map
        :mode skewer-clients-mode
        :bindings
        "gr" (lookup-key skewer-clients-mode-map "g")))

    (paloryemacs/declare-prefix-for-mode 'js2-mode "ms" "skewer")
    (paloryemacs/declare-prefix-for-mode 'js2-mode "me" "eval")
    (paloryemacs/set-leader-keys-for-major-mode 'js2-mode
      "'" 'paloryemacs/skewer-start-repl
      "ee" 'skewer-eval-last-expression
      "eE" 'skewer-eval-print-last-expression
      "sb" 'skewer-load-buffer
      "sB" 'paloryemacs/skewer-load-buffer-and-focus
      "si" 'paloryemacs/skewer-start-repl
      "sf" 'skewer-eval-defun
      "sF" 'paloryemacs/skewer-eval-defun-and-focus
      "sr" 'paloryemacs/skewer-eval-region
      "sR" 'paloryemacs/skewer-eval-region-and-focus
      "ss" 'skewer-repl)))

(use-package skewer-html
  :defer t
  :diminish skewer-html-mode
  :init
  (progn
    (add-hook 'web-mode-hook 'skewer-html-mode))
  :config
  (progn
    (paloryemacs/set-leader-keys-for-minor-mode 'skewer-html-mode
      "et" 'skewer-html-eval-tag)))

(use-package skewer-css
  :defer t
  :diminish skewer-css-mode
  :init
  (progn
    (add-hook 'css-mode-hook 'skewer-css-mode))
  :config
  (progn
    (paloryemacs/set-leader-keys-for-minor-mode 'skewer-css-mode
      "ee" 'skewer-css-eval-current-declaration
      "ex" 'skewer-css-eval-current-rule
      "eb" 'skewer-css-eval-buffer
      "ec" 'skewer-css-clear-all)))


(use-package json-mode
  :defer t)

(use-package json-snatcher
  :defer t
  :init
  (paloryemacs/set-leader-keys-for-major-mode 'json-mode
    "hp" 'jsons-print-path))

;; https://github.com/yasuyk/web-beautify
(use-package web-beautify
  :defer t
  :init
  (progn
    (paloryemacs/set-leader-keys-for-major-mode 'js2-mode
      "=" 'web-beautify-js)
    (paloryemacs/set-leader-keys-for-major-mode 'json-mode
      "=" 'web-beautify-js)
    (paloryemacs/set-leader-keys-for-major-mode 'web-mode
      "=" 'web-beautify-html)
    (paloryemacs/set-leader-keys-for-major-mode 'css-mode
      "=" 'web-beautify-css)))

;; Live evaluation of JS buffer change.
(use-package livid-mode
  :commands (livid-mode)
  :defer t
  :init
  (progn
    (with-eval-after-load 'js2-mode
      (paloryemacs/set-leader-keys-for-major-mode 'js2-mode
        "sa" 'livid-mode))))

(provide '50web)
;;; 50web.el ends here
