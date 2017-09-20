;;; 50sql.el ---                                     -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>
;; Keywords:

;;; Code:

;; https://truongtx.me/2014/08/23/setup-emacs-as-an-sql-database-client

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
                 (sql-database "db2"))))



(provide '50sql)
;;; 50sql.el ends here
