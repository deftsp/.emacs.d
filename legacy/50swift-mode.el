;;; 50swift-mode.el ---                              -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>
;; Keywords:


;;; Commentary:

;;

;;; Code:

;; xcrun --show-sdk-path --sdk macosx
(setq flycheck-swift-sdk-path
      "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX10.10.sdk")

;; (with-eval-after-load 'swift-mode
;;   )


(provide '50swift-mode)
;;; 50swift-mode.el ends here
