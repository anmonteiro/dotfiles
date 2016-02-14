(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(add-hook 'markdown-mode-hook
          (lambda ()
            (setq fci-handle-truncate-lines nil)
            (setq truncate-partial-width-windows nil)
            (setq truncate-lines nil)))
