;;; 62winring.el ---

;; Copyright (C) 2008  Shhiping Tseng

;; Author: Shihping Tseng <deftsp@gmail.com>
(defvar winring-map nil
  "Keymap used for winring, window configuration rings.")
(setq winring-map (make-sparse-keymap))
(define-key winring-map "b" 'winring-submit-bug-report)
(define-key winring-map "c" 'winring-new-configuration)
(define-key winring-map "2" 'winring-duplicate-configuration)
(define-key winring-map "j" 'winring-jump-to-configuration)
(define-key winring-map "0" 'winring-delete-configuration)
(define-key winring-map "k" 'winring-delete-configuration)
(define-key winring-map "n" 'winring-next-configuration)
(define-key winring-map "p" 'winring-prev-configuration)
(define-key winring-map "r" 'winring-rename-configuration)
(define-key winring-map "v" 'winring-version)
;; Load the winring-library and enable the ECB-support for it.
;; This *MUST* be done *BEFORE* the first call to any winring-command, so also
;; before calling `winring-initialize'!
(require 'winring)


(when (eq system-type 'gnu/linux)
  '(progn
     (require 'ecb)
     (add-hook 'ecb-before-activate-hook 'ecb-winman-winring-enable-support)
     (add-hook 'ecb-deactivate-hook 'ecb-winman-winring-enable-support)
     (ecb-winman-winring-enable-support)))

(winring-initialize)
;;;;;;;;;;;;;;;

(defun winring+-names ()
  "return a list of winring names"
  (let* ((ring (winring-get-ring))
         (n (1- (ring-length ring)))
         (current (winring-name-of-current))
         (table (list (cons current -1)))
         name)
    (while (<= 0 n)
      (setq table (cons (cons (winring-name-of (ring-ref ring n)) n) table)
            n (1- n)))
    (mapcar #'car table)))

(defun winring+-name-to-index (winring-name)
  "return the index of the winring name"
  (let* ((ring (winring-get-ring))
         (n (1- (ring-length ring)))
         (current (winring-name-of-current))
         (table (list (cons current -1))))
    ;; populate the completion table
    (while (<= 0 n)
      (setq table (cons (cons (winring-name-of (ring-ref ring n)) n) table)
            n (1- n)))
    (if (string-equal winring-name "")
        (setq winring-name current))
    (cdr (assoc winring-name table))))


;; it cost me a 3 hours to fix the problem that I mistake use winring-name which is a frame-local variable as parameter.
(defun winring+-jump-to-configuration (name)
  "Go to the named window configuration, if it now exist create one."
  (if (member name (winring+-names))
      (let* ((ring (winring-get-ring))
             (index (winring+-name-to-index name))
             item)
        ;; if the current configuration was chosen, winring-complete-name
        ;; returns -1
        (when (<= 0 index)
          ;; item should be saved before `winring-save-current-configuration'
          (setq item (ring-remove ring index))
          (winring-save-current-configuration)
          (winring-restore-configuration item)))
      (winring+-new-configuration name)))

(defun winring+-delete-configuration (name)
  "Delete the current configuration and switch to the next one."
  (let ((ring (winring-get-ring))
        (index (winring+-name-to-index name)))
    (winring-restore-configuration (ring-remove ring index))))

(defun winring+-new-configuration (new-name)
  "Save the current window configuration and create an empty new
 one with the given name. If the winring configuration have been
 created, just jump to it"
  (if (string-equal new-name "")
      (error "winring name should not be a empty string." ))
  (let ((current-buffer (current-buffer)))
    (winring-save-current-configuration)
    (delete-other-windows)
    ;; (switch-to-buffer winring-new-config-buffer-name)
    (switch-to-buffer current-buffer)
    (winring-set-name new-name)))

;;;;;;;;;;;;;;;;;;;;;;;;;



(provide '62winring)