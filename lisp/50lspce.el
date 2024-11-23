;;; 50lspce.el ---                                -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>

(use-package lspce
  :if (eq dottl-lsp-client 'lspce)
  ;; :load-path "/path/to/lspce"
  :config
  (setq lspce-send-changes-idle-time 0.1)
  (setq lspce-show-log-level-in-modeline t) ;; show log level in mode line

  ;; You should call this first if you want lspce to write logs
  (lspce-set-log-file "/tmp/lspce.log")

  ;; By default, lspce will not write log out to anywhere.
  ;; To enable logging, you can add the following line
  ;; (lspce-enable-logging)
  ;; You can enable/disable logging on the fly by calling `lspce-enable-logging' or `lspce-disable-logging'.

  ;; enable lspce in particular buffers
  (add-hook 'rust-mode-hook 'lspce-mode)
  ;; (remove-hook 'rustic-mode-hook 'lspce-mode)

  ;; modify `lspce-server-programs' to add or change a lsp server, see document
  ;; of `lspce-lsp-type-function' to understand how to get buffer's lsp type.
  ;; Bellow is what I use
  (setq lspce-server-programs
        `(("rust"  "rust-analyzer" "" lspce-ra-initializationOptions)
          ("rustic"  "rust-analyzer" "" lspce-ra-initializationOptions)
          ("python" "pylsp" "" )
          ("C" "clangd" "--all-scopes-completion --clang-tidy --enable-config --header-insertion-decorators=0")
          ("java" "java" lspce-jdtls-cmd-args lspce-jdtls-initializationOptions))))

(provide '50lspce)
