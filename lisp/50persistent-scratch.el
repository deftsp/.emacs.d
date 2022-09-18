;;; 50persistent-scratch.el ---
;;
;; Description:
;; Author: Shihpin Tseng <deftsp@gmail.com>
;; Created: Tue Oct 16 20:12:51 2007
;; Version:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

;; https://emacs-china.org/t/emacs-sublime-vscode-session-restore-package/5751
;;; persistent scratch
(defun tl/persistent-scratch-scratch-buffer-p ()
  "Return non-nil if the current buffer's name expect to persists."
  (string-match-p
   (concat (regexp-quote "*scratch*")
           "\\|" (concat "^reading-[0-9][0-9]/[0-9][0-9] [0-9][0-9]:[0-9][0-9]:[0-9][0-9]$"))
   (buffer-name)))

;; Copied from https://github.com/Fanael/persistent-scratch/issues/23
;;
;; Before killing scratch buffer, save the current scratch buffer,
;; and create a backup filename for next time back up
(defun tl/persistent-scratch-kill-buffer-query-function ()
  "Back up things before killing scratch buffer."
  (when (tl/persistent-scratch-scratch-buffer-p)
    (persistent-scratch-save)
    (persistent-scratch-new-backup))
  t)

(use-package persistent-scratch
  :init
  (add-hook 'kill-buffer-query-functions
            #'tl/persistent-scratch-kill-buffer-query-function)
  :config
  ;; enable autosave and restore the last saved state
  (persistent-scratch-setup-default)
  (setq persistent-scratch-scratch-buffer-p-function 'tl/persistent-scratch-scratch-buffer-p)
  (setq persistent-scratch-backup-directory
        (expand-file-name ".persistent-scratchs" user-emacs-directory))

  (setq persistent-scratch-save-file
        (expand-file-name ".persistent-scratch" user-emacs-directory))
  (setq persistent-scratch-autosave-interval 60))


(provide '50persistent-scratch)
