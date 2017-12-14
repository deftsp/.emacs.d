;;; 50docker.el ---                                  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>

;;; Code:
(use-package docker
  :defer t
  :init
  (progn
    (paloryemacs/declare-prefix "D" "Docker")
    (paloryemacs/set-leader-keys
      "Dc" 'docker-containers
      "Dd" 'docker-rmi
      "De" 'docker-unpause
      "DF" 'docker-pull
      "Dk" 'docker-rm
      "Di" 'docker-images
      "Do" 'docker-stop
      "DP" 'docker-push
      "Dp" 'docker-pause
      "Dr" 'docker-restart
      "Ds" 'docker-start)))

(use-package docker-tramp
  :defer t)

(use-package dockerfile-mode
  :defer t
  :config (evil-leader/set-key-for-mode 'dockerfile-mode
            "mcb" 'dockerfile-build-buffer))



(provide '50docker)
;;; 50docker.el ends here
