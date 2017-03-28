
(defalias 'open 'find-file)
(defalias 'openo 'find-file-other-window)
(defalias 'lf 'load-file)

;; (defalias 'yes-or-no-p 'y-or-n-p) ; y or n is enough
;; (defalias 'list-buffers 'ibuffer) ; always use ibuffer
;; (defalias 'man 'woman)

;; shortening of often used commands
(defalias 'qrr 'query-replace-regexp)
(defalias 'lml 'list-matching-lines)
(defalias 'dml 'delete-matching-lines)
(defalias 'dnml 'delete-non-matching-lines)
(defalias 'dtw 'delete-trailing-whitespace)
(defalias 'sl 'sort-lines)
(defalias 'rc 'run-scheme)
(defalias 'rr 'reverse-region)
(defalias 'rb 'revert-buffer)
(defalias 'rs 'replace-string)

(defalias 'g 'grep)
(defalias 'gf 'grep-find)
(defalias 'fd 'find-dired)
(defalias 'ddqrr 'dired-do-query-replace-regexp)

(defalias 'ntr 'narrow-to-region)
(defalias 'lml 'list-matching-lines)
(defalias 'rof 'recentf-open-files)


(defalias 'sh 'shell)
(defalias 'fb 'flyspell-buffer)
(defalias 'sbc 'set-background-color)
(defalias 'lcd 'list-colors-display)
(defalias 'lfd 'list-faces-display)
(defalias 'cc 'calc)
(defalias 'cl 'cclookup-lookup)

;; elisp
(defalias 'eb 'eval-buffer)
(defalias 'er 'eval-region)
(defalias 'ed 'eval-defun)
(defalias 'eis 'elisp-index-search)
(defalias 'lf 'load-file)

;; major modes
;; (defalias 'perl-mode 'cperl-mode) ; always use cperl-mode
(defalias 'hm 'html-mode)
(defalias 'tm 'text-mode)
(defalias 'elm 'emacs-lisp-mode)
(defalias 'om 'org-mode)
(defalias 'ssm 'shell-script-mode)
(defalias 'cm  'c++-mode)

(defalias 'od  'paloryemacs/evil-org-drill)
(defalias 'odd 'paloryemacs/evil-org-drill-directory)
(defalias 'odr 'paloryemacs/evil-org-drill-resume)
(defalias 'oda 'paloryemacs/evil-org-drill-again)
(defalias 'odc 'paloryemacs/evil-org-drill-cram)

;; minor modes
(defalias 'wsm 'whitespace-mode)
(defalias 'gwsm 'global-whitespace-mode)
(defalias 'dsm 'desktop-save-mode)
(defalias 'acm 'auto-complete-mode)
(defalias 'vlm 'visual-line-mode)
(defalias 'glm 'global-linum-mode)

;; (defalias '\[ 'remove-square-brackets)
;; (defalias '\" 'replace-straight-quotes)

;; dev
(defalias 'grr 'paloryemacs/gambit-remote-repl)
(defalias 'hpc 'haskell-process-cabal)

;; bm
(defalias 'bt  'bm-toggle)
(defalias 'btp 'bm-toggle-buffer-persistence)
(defalias 'bn  'bm-next)
(defalias 'bp  'bm-previous)
(defalias 'bs  'bm-show)
(defalias 'bsa 'bm-show-all)
(defalias 'abl 'anything-bm-list)

;; traverselisp
(defalias 'tdr 'traverse-deep-rfind)

;; el-get
(defalias 'egi 'el-get-install)
(defalias 'egu 'el-get-update)
(defalias 'egr 'el-get-remove)
(defalias 'egfr 'el-get-find-recipe-file)

;; swiper
(defalias 'ag 'counsel-ag)
(defalias 'rg 'counsel-rg)

(provide '50alias)
