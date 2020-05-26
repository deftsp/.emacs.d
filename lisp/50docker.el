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
      "aDs" 'docker-start))
  :config
  (evilified-state-evilify-map docker-container-mode-map
    :mode docker-container-mode
    :bindings
    ";"  'docker-container-ls
    "?"  'docker-container-help
    "C"  'docker-container-cp
    "D"  'docker-container-rm
    "I"  'docker-container-inspect
    "K"  'docker-container-kill
    "L"  'docker-container-logs
    "O"  'docker-container-stop
    "P"  'docker-container-pause
    "R"  'docker-container-restart
    "S"  'docker-container-start
    "a"  'docker-container-attach
    "b"  'docker-container-shells
    "d"  'docker-container-diff
    "f"  'docker-container-open
    "r"  'docker-container-rename-selection)

  (evilified-state-evilify-map docker-image-mode-map
    :mode docker-image-mode
    :bindings
    "?"  'docker-image-help
    ";"  'docker-image-ls
    "D"  'docker-image-rm
    "F"  'docker-image-pull
    "I"  'docker-image-inspect
    "P"  'docker-image-push
    "R"  'docker-image-run
    "T"  'docker-image-tag-selection)

  (evilified-state-evilify-map docker-network-mode-map
    :mode docker-network-mode
    :bindings
    ";"  'docker-network-ls
    "?"  'docker-network-help
    "D"  'docker-network-rm)

  (evilified-state-evilify-map docker-volume-mode-map
    :mode docker-volume-mode
    :bindings
    ";"  'docker-volume-ls
    "?"  'docker-volume-help
    "D"  'docker-volume-rm
    "d"  'docker-volume-dired-selection)

  (evilified-state-evilify-map docker-machine-mode-map
    :mode docker-machine-mode
    :bindings
    ";"  'docker-machine-ls
    "?"  'docker-machine-help
    "C"  'docker-machine-create
    "D"  'docker-machine-rm
    "E"  'docker-machine-env-selection
    "O"  'docker-machine-stop
    "R"  'docker-machine-restart
    "S"  'docker-machine-start))

(use-package docker-tramp
  :defer t)

(use-package dockerfile-mode
  :defer t
  :config (evil-leader/set-key-for-mode 'dockerfile-mode
            "mcb" 'dockerfile-build-buffer))



(provide '50docker)
;;; 50docker.el ends here
