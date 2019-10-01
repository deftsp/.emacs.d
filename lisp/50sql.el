;;; 50sql.el ---                                     -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>
;; Keywords:

;;; Code:

;; https://truongtx.me/2014/08/23/setup-emacs-as-an-sql-database-client

(use-package sql-indent
  :defer t
  :commands (sqlind-minor-mode)
  :init
  (progn
    (setq-default sqlind-basic-offset 4)))

(use-package sql
  :defer t
  :config
  (progn
    (setq sql-electric-stuff (quote semicolon)
          sql-input-ring-file-name "~/.sql_history"
          sql-pop-to-buffer-after-send-region t)
    (setq sql-postgres-login-params
          '((user :default "postgres")
            (database :default "postgres")
            (server :default "localhost")
            (port :default 5433)))

    (defun paloryemacs/sql-interactive-mode-init ()
      (toggle-truncate-lines t)
      (setq-local show-trailing-whitespace nil))

    (add-hook 'sql-interactive-mode-hook
              'paloryemacs/sql-interactive-mode-init)

    (defun paloryemacs/sql-mode-init ()
      ;; (sqlind-minor-mode +1)
      )

    (add-hook 'sql-mode-hook 'paloryemacs/sql-mode-init)


    (setq sql-connection-alist
          '((localhost (sql-product 'postgres)
                       (sql-port 5433)
                       (sql-server "localhost")
                       (sql-user "postgres")
                       (sql-database "scratch"))
            (server2 (sql-product 'postgres)
                     (sql-port 5432)
                     (sql-server "localhost")
                     (sql-user "user")
                     (sql-database "db2"))))))

(provide '50sql)
;;; 50sql.el ends here
