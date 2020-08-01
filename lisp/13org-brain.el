;;; 13org-brain.el ---                               -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>
;; Keywords:


(use-package org-brain
  :after org
  :init
  (setq org-brain-path (expand-file-name "brain" org-directory))
  (setq org-id-track-globally t)
  (setq org-id-locations-file "~/.emacs.d/.org-id-locations")
  (push '("b" "Brain" plain (function org-brain-goto-end)
          "* %i%?" :empty-lines 1)
        org-capture-templates)
  (setq org-brain-refile-max-level 9)
  (setq org-brain-title-max-length 20
        org-brain-visualize-default-choices 'all
        org-brain-include-file-entries t
        org-brain-file-entries-use-title nil)

  (tl/set-leader-keys "aob" 'org-brain-visualize)
  (with-eval-after-load 'evil
    (evil-set-initial-state 'org-brain-visualize-mode 'evilified))

  :config
  (tl/set-leader-keys-for-major-mode 'org-brain-visualize-mode
    "mp" 'org-brain-change-local-parent
    "mr" 'org-brain-refile
    "sr" 'org-brain-refile)

  (with-eval-after-load "evil-evilified-state"
    (evilified-state-evilify-map org-brain-visualize-mode-map
      :mode org-brain-visualize-mode
      :bindings
      ;; (kbd "C-h") nil
      "gr" 'revert-buffer
      "j" 'forward-button
      "k" 'backward-button
      "l" 'org-brain-add-resource
      "h" 'org-brain-add-child-headline
      "n" 'org-brain-pin
      "v" 'org-brain-visualize
      "V" 'org-brain-visualize-follow))

  ;; ascii-art-to-unicode
  ;; (use-package ascii-art-to-unicode)

  ;; (defface aa2u-face '((t . nil))
  ;;   "Face for aa2u box drawing characters")

  ;; (advice-add #'aa2u-1c :filter-return
  ;;             (lambda (str) (propertize str 'face 'aa2u-face)))

  ;; (defun aa2u-org-brain-buffer ()
  ;;   (let ((inhibit-read-only t))
  ;;     (make-local-variable 'face-remapping-alist)
  ;;     (add-to-list 'face-remapping-alist
  ;;                  '(aa2u-face . org-brain-wires))
  ;;     (ignore-errors (aa2u (point-min) (point-max)))))

  ;; (with-eval-after-load 'org-brain
  ;;   (add-hook 'org-brain-after-visualize-hook #'aa2u-org-brain-buffer))

  (defun tl//org-brain-pretty-wire (old-func &rest arguments)
    (let ((args arguments))
      (if (equal args '("V"))
          (setq args '("▽")))
      (apply old-func args)))

  ;; (advice-remove 'org-brain--insert-wire 'tl//org-brain-pretty-wire)
  (advice-add 'org-brain--insert-wire :around 'tl//org-brain-pretty-wire))


(defun tl/org-brain-clear-pins ()
  (interactive)
  (setq org-brain-pins nil)
  (org-brain-save-data)
  (let ((buf (get-buffer "*org-brain*")))
    (when buf
      (with-current-buffer buf
        (revert-buffer)))))

(defun tl/org-brain-clear-history ()
  (interactive)
  (setq org-brain--vis-history nil)
  (org-brain-save-data)
  (let ((buf (get-buffer "*org-brain*")))
    (when buf
      (with-current-buffer buf
        (revert-buffer)))))

(provide '13org-brain)
