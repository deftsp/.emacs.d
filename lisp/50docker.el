;;; 50docker.el ---                                  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>

;;; Code:
(use-package docker
  :defer t
  :init
  (progn
    (tl/declare-prefix "aD" "Docker")
    (tl/set-leader-keys
      "ad"  'docker
      "aDc" 'docker-containers
      "aDd" 'docker-rmi
      "aDe" 'docker-unpause
      "aDF" 'docker-pull
      "aDk" 'docker-rm
      "aDi" 'docker-images
      "aDo" 'docker-stop
      "aDP" 'docker-push
      "aDp" 'docker-pause
      "aDr" 'docker-restart
      "aDs" 'docker-start)))

;; ‘docker-tramp’ has been obsoleted, please use integrated package ‘tramp-container’

(use-package dockerfile-mode
  :defer t
  :config (evil-leader/set-key-for-mode 'dockerfile-mode "mcb" 'dockerfile-build-buffer))

(provide '50docker)
;;; 50docker.el ends here
