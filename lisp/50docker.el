;;; 50docker.el ---                                  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>

;;; Code:
(use-package docker
  :defer t
  :init
  (progn
    (paloryemacs/declare-prefix "aD" "Docker")
    (paloryemacs/set-leader-keys
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

(with-eval-after-load 'docker-containers
  (evilified-state-evilify-map docker-containers-mode-map
    :mode docker-containers-mode))

(with-eval-after-load 'docker-images
  (evilified-state-evilify-map docker-images-mode-map
    :mode docker-images-mode))

(use-package docker-tramp
  :defer t)

(use-package dockerfile-mode
  :defer t
  :config (evil-leader/set-key-for-mode 'dockerfile-mode
            "mcb" 'dockerfile-build-buffer))



(provide '50docker)
;;; 50docker.el ends here
