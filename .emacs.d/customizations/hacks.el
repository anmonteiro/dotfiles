(defalias 'find-if 'cl-find-if)

;; Misc attempts to make Emacs not slow down
(setq font-lock-maximum-decoration 6)
(setq-default bidi-display-reordering nil)

;; set GC threshold to 10MB
;; http://www.wilfred.me.uk/.emacs.d/init.html#orgd9a37d6
(setq gc-cons-threshold (* 10 1024 1024))

(provide 'hacks)
