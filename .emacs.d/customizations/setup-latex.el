;;;;
;; LaTeX
;;;;


;; Enable paredit for LaTeX
(add-hook 'latex-mode-hook 'enable-paredit-mode)
(add-hook 'LaTeX-mode-hook 'enable-paredit-mode)
(add-hook 'tex-mode-hook 'enable-paredit-mode)
(add-hook 'TeX-mode-hook 'enable-paredit-mode)
(add-hook 'bibtex-mode-hook 'enable-paredit-mode)

(add-hook 'latex-mode-hook
          (lambda ()
            (define-key latex-mode-map "{" #'paredit-open-curly)
            (define-key latex-mode-map "}" #'paredit-close-curly)))
(add-hook 'LaTeX-mode-hook
          (lambda ()
            (define-key LaTeX-mode-map "{" #'paredit-open-curly)
            (define-key LaTeX-mode-map "}" #'paredit-close-curly)))
(add-hook 'tex-mode-hook
          (lambda ()
            (define-key tex-mode-map "{" #'paredit-open-curly)
            (define-key tex-mode-map "}" #'paredit-close-curly)))
(add-hook 'TeX-mode-hook
          (lambda ()
            (define-key TeX-mode-map "{" #'paredit-open-curly)
            (define-key TeX-mode-map "}" #'paredit-close-curly)))
(add-hook 'bibtex-mode-hook
          (lambda ()
            (define-key bibtex-mode-map "{" #'paredit-open-curly)
            (define-key bibtex-mode-map "}" #'paredit-close-curly)))


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
