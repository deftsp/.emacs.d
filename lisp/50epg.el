;;; 50epg.el ---

;; Copyright (C) 2009  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>

;; EasyPG
;; every time you open a file with the extension .gpg, EasyPG will do the work for you.
;; (epa-file-enable)
;; I would like to use minibuffer to input passphrase not pop up a graphical window to ask you the keyphrase.
;; http://www.enigmacurry.com/2009/01/14/extending-emacs-with-advice/
;; (setenv "GPG_AGENT_INFO" nil)

(defadvice epg--start (around advice-epg-disable-agent disable)
  "Make epg--start not able to find a gpg-agent"
  (let ((agent (getenv "GPG_AGENT_INFO")))
    (setenv "GPG_AGENT_INFO" nil)
    ad-do-it
    (setenv "GPG_AGENT_INFO" agent)))

(defun epg-disable-agent ()
  "Make EasyPG bypass any gpg-agent"
  (interactive)
  (ad-enable-advice 'epg--start 'around 'advice-epg-disable-agent)
  (ad-activate 'epg--start)
  (message "EasyPG gpg-agent bypassed"))

(defun epg-enable-agent ()
  "Make EasyPG use a gpg-agent after having been disabled with epg-disable-agent"
  (interactive)
  (ad-disable-advice 'epg--start 'around 'advice-epg-disable-agent)
  (ad-activate 'epg--start)
  (message "EasyPG gpg-agent re-enabled"))

(provide '50epg)