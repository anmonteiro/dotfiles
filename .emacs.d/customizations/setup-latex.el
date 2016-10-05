;;;;
;; LaTeX
;;;;


;; Enable paredit for LaTeX
(add-hook 'latex-mode-hook 'smartparens-mode)
(add-hook 'LaTeX-mode-hook 'smartparens-mode)
(add-hook 'tex-mode-hook 'smartparens-mode)
(add-hook 'TeX-mode-hook 'smartparens-mode)
(add-hook 'bibtex-mode-hook 'smartparens-mode)

(defun truncate-lines ()
  (setq fci-handle-truncate-lines nil)
  (setq truncate-partial-width-windows nil)
  (setq truncate-lines nil))

;; truncate lines when editing LaTeX
(add-hook 'latex-mode-hook 'truncate-lines)
(add-hook 'LaTeX-mode-hook 'truncate-lines)
(add-hook 'tex-mode-hook 'truncate-lines)
(add-hook 'TeX-mode-hook 'truncate-lines)
(add-hook 'bibtex-mode-hook 'truncate-lines)

(add-hook 'latex-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'tex-mode-hook 'flyspell-mode)
(add-hook 'TeX-mode-hook 'flyspell-mode)
(add-hook 'bibtex-mode-hook 'flyspell-mode)
