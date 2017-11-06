;; Changes all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)
(fset 'gnus-yes-or-no-p 'y-or-n-p)

;; shell scripts
(setq-default sh-basic-offset 2)
(setq-default sh-indentation 2)

;; No need for ~ files when editing
(setq create-lockfiles nil)

;; Go straight to scratch buffer on startup
(setq inhibit-startup-message t)

;; magit emacsclient
(set-variable 'magit-emacsclient-executable "/usr/local/bin/emacsclient")

;; Misc attempts to make Emacs not slow down
(setq font-lock-maximum-decoration 6)
(setq-default bidi-display-reordering nil)
