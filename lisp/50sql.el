;;; 50sql.el ---                                     -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>
;; Keywords:

;;; Code:

;; https://truongtx.me/2014/08/23/setup-emacs-as-an-sql-database-client

(use-package sql-indent
  :after sql
  :commands (sqlind-minor-mode)
  :init
  (setq-default sqlind-basic-offset 4))

(use-package sql
  :defer t
  :config
  (setq sql-electric-stuff (quote semicolon)
        sql-input-ring-file-name "~/.sql_history"
        sql-product 'postgres
        sql-pop-to-buffer-after-send-region t)
  (setq sql-postgres-login-params
        '((user :default "postgres")
          (database :default "postgres")
          (server :default "localhost")
          (port :default 5433)))

  ;; (sql-set-product-feature 'postgres :prompt-regexp "^[-[:alnum:]_]*=[#>] ")
  ;; (sql-set-product-feature 'postgres :prompt-cont-regexp
  ;;                          "^[-[:alnum:]_]*[-(][#>] ")

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

  (defun tl/sql-interactive-mode-init ()
    (toggle-truncate-lines t)
    (setq-local show-trailing-whitespace nil))

  (add-hook 'sql-interactive-mode-hook
            'tl/sql-interactive-mode-init)

  (defun tl/sql-mode-init ()
    ;; (sqlind-minor-mode +1)
    )

  (add-hook 'sql-mode-hook 'tl/sql-mode-init))

(use-package dotenv
  :after (sql))

(defun tl/get-value-from-dotenv (name)
  (let ((path (dotenv-path (projectile-project-root))))
    (when (s-present? path)
      (cadr (-find (lambda (l) (string= (car l) name))
                   (dotenv-load path))))))

(defun tl/sql-update-dotenv-server (product)
  (interactive (list (intern
                      (ivy-read "sql product: "
                                '("postgres" "sqlite")))))
  (if-let ((connection-name (cdr (project-current)))
           (sql-user (tl/get-value-from-dotenv "SUPER_USER"))
           (sql-password (tl/get-value-from-dotenv "SUPER_USER_PASSWORD"))
           (sql-port (tl/get-value-from-dotenv "DB_PORT"))
           (sql-database (tl/get-value-from-dotenv "DB_NAME"))
           (sql-server "localhost"))
      (progn
        (setq sql-connection-alist (assq-delete-all
                                    connection-name
                                    sql-connection-alist))
        (add-to-list 'sql-connection-alist
                     `(,connection-name (sql-user ,sql-user)
                                        ;; (sql-password ,sql-password)
                                        (sql-port ,(string-to-number sql-port))
                                        (sql-database ,sql-database)
                                        (sql-server ,sql-server)
                                        (sql-product ',product)))
        ;; https://stackoverflow.com/questions/26677909/emacs-sql-mode-postgresql-and-inputing-password
        (setenv "PGPASSWORD" sql-password)
        (message "set sql server connection parameters for current project"))
    (user-error "Unable to set connection from dotenv")))

(use-package sqlformat
  :after sql
  :init
  ;; https://github.com/darold/pgFormatter
  (setq sqlformat-args '("-s4" "-u1" "-b"))
  (setq sqlformat-command 'pgformatter))

(general-evil-define-key '(normal visual) sql-mode-map
  :prefix ","
  "eb" 'sql-send-buffer
  "er" 'sql-send-region
  "ei" 'sql-product-interactive
  "u" 'tl/sql-update-dotenv-server
  "z" 'sql-show-sqli-buffer
  "c" 'sql-connect
  "p" 'sqlformat)

(provide '50sql)
;;; 50sql.el ends here
