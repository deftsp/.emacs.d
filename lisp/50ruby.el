;;; 50ruby.el ---

;; Copyright (C) 2013  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>

;;; install
;; install yaml-mode, rinari with el-get
;; https://github.com/remvee/emacs-rails

;;; rinari
(require 'rinari nil t)
(eval-after-load "rinari"
  '(progn
     (add-hook 'ruby-mode-hook 'rinari-minor-mode)))

;; el-get install ruby-electric
(require 'ruby-electric nil t)

(require 'rspec-mode nil t)

;; cucumber.el
;; https://github.com/michaelklishin/cucumber.el

;;; pry
;; $ gem install pry
;; (add-to-list 'inf-ruby-implementations '("pry" . "pry -f"))
;; (setq inf-ruby-default-implementation "pry")
;; (setq inf-ruby-first-prompt-pattern "^\\[[0-9]+\\] pry\\((.*)\\)> *")
;; (setq inf-ruby-prompt-pattern "^\\[[0-9]+\\] pry\\((.*)\\)[>*\"'] *")

;;; init
(defun pl/ruby-mode-init ()
  ;; (push '(".+\\.rb$" flymake-ruby-init) flymake-allowed-file-name-masks)
  ;; (push '("Rakefile$" flymake-ruby-init) flymake-allowed-file-name-masks)
  ;; (push '("^\\(.*\\):\\([0-9]+\\): \\(.*\\)$" 1 2 nil 3) flymake-err-line-patterns)
  ;; (add-hook 'ruby-mode-hook 'pl/flymake-ruby-enable)
  (define-key ruby-mode-map "\C-c\C-z" 'pl/ruby-switch-to-inf-dwim)
  (add-to-list 'ac-sources 'ac-source-rsense-method)
  (add-to-list 'ac-sources 'ac-source-rsense-constant))

(add-hook 'ruby-mode-hook 'pl/ruby-mode-init)


;;; rsense
;; brew install rsense
;; el-get install rsense
;; http://cx4a.org/software/rsense/manual.html#Installation
;; (setq rsense-home "$RSENSE_HOME")
(require 'rsense)


;;;
(defun pl/flymake-ruby-enable ()
  (when (and buffer-file-name
             (file-writable-p
              (file-name-directory buffer-file-name))
             (file-writable-p buffer-file-name)
             (if (fboundp 'tramp-list-remote-buffers)
                 (not (subsetp
                       (list (current-buffer))
                       (tramp-list-remote-buffers)))
               t))
    ;; (local-set-key (kbd "C-c d") 'flymake-display-err-menu-for-current-line)
    (flymake-mode t)))

(defun pl/ruby-switch-to-inf-dwim ()
  (interactive)
  (unless inf-ruby-buffer
    (inf-ruby))
  (call-interactively 'ruby-switch-to-inf))


(provide '50ruby)
;;; 50ruby.el ends here
