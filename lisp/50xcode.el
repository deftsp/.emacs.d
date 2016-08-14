;;; 50xcode.el ---

;; Copyright (C) 2011  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>
;; Keywords:

;; (defun paloryemacs/xcode-compile (directory build-action)
;;   "Enhanced bh-compile function by Brett Hutley"
;;   (interactive
;;    (list (read-directory-name "Directory name: " nil default-directory nil)
;;          (read-from-minibuffer "Build action (default build): " nil nil t nil "build" nil)))
;;   (cd directory)
;;   (let ((df (directory-files "."))
;;         (has-proj-file nil))
;;     (while (and df (not has-proj-file))
;;       (let ((fn (car df)))
;;         (if (> (length fn) 10)
;;             (if (string-equal (substring fn -10) ".xcodeproj")
;;                 (setq has-proj-file t))))
;;       (setq df (cdr df)))
;;     (if has-proj-file
;;         ;; TODO configuration
;;         (compile (format "xcodebuild -configuration Debug %s" build-action))
;;       (compile "make"))))


(global-set-key (kbd "C-c o x") 'paloryemacs/open-with-xcode)
(defun paloryemacs/open-with-xcode ()
  "Open current file with Xcode."
  (interactive)
  (shell-command
   (concat "open -a /Applications/Xcode.app " "\"" (buffer-file-name) "\"")))


(defun paloryemacs/xcode:build-and-run ()
  (interactive)
  (if (directory-files "." nil ".*\.xcodeproj$" nil)
      (compile "xcodebuild -configuration Debug")
    (progn
      (cd "../")
      (xcode:build-and-run))))




;;; https://github.com/imakado/emacs-xcode-document-viewer.git
;; orig: https://github.com/sakito/emacs-xcode-document-viewer.git
(setq xcdoc:document-path
      "/Applications/Xcode.app/Contents/Developer/Platforms/\
iPhoneOS.platform/Developer/Documentation/DocSets/\
com.apple.adc.documentation.AppleiOS5_1.iOSLibrary.docset")

(setq xcdoc:open-w3m-other-buffer t)
;; (add-hook 'objc-mode-hook
;;           (lambda ()
;;             (define-key objc-mode-map (kbd "C-c w") 'xcdoc:ask-search)))


(provide '50xcode)
