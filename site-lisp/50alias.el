
;; I used this in my .emacs so that I can simply open file instead of find-file. This has the added advantage that I can
;; M-x open file when I can’t remember what C-x C-f does ;P
(defalias 'open 'find-file)
(defalias 'openo 'find-file-other-window)
