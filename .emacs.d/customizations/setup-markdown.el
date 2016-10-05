(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))

(add-hook 'markdown-mode-hook
          (lambda ()
            (setq fci-handle-truncate-lines nil)
            (setq truncate-partial-width-windows nil)
            (setq truncate-lines nil)))

(setq markdown-command "pandoc -c file:///Users/anmonteiro/Documents/github/dotfiles/.emacs.d/github-pandoc.css --from markdown_github -t html5 --mathjax --highlight-style pygments --standalone")

(add-hook 'markdown-mode-hook 'flyspell-mode)
(add-hook 'markdown-mode-hook 'smartparens-mode)
