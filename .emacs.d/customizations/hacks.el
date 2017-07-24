(defalias 'find-if 'cl-find-if)

;; set GC threshold to 10MB
;; http://www.wilfred.me.uk/.emacs.d/init.html#orgd9a37d6
(setq gc-cons-threshold (* 10 1024 1024))

(provide 'hacks)
