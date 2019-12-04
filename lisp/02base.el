;;; 02base.el ---                                    -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>
;; Keywords:

;;; Code:
(defun tl//run-local-vars-mode-hook ()
  "Run a hook for the major-mode after the local variables have been processed."
  (run-hooks (intern (format "%S-local-vars-hook" major-mode))))

;; hook into `hack-local-variables' in order to allow switching tl
;; configurations based on local variables
(add-hook 'hack-local-variables-hook #'tl//run-local-vars-mode-hook)

(provide '02base)
;;; 02base.el ends here
