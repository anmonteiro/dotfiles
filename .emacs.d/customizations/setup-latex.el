;;;;
;; LaTeX
;;;;


;; Enable paredit for LaTeX
(add-hook 'latex-mode-hook 'enable-paredit-mode)
(add-hook 'LaTeX-mode-hook 'enable-paredit-mode)
(add-hook 'tex-mode-hook 'enable-paredit-mode)
(add-hook 'TeX-mode-hook 'enable-paredit-mode)
(add-hook 'bibtex-mode-hook 'enable-paredit-mode)

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
