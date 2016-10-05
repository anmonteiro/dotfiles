;; enable smartparens default configuration
(require 'smartparens-config)

(setq sp-base-key-bindings 'paredit)

(show-smartparens-global-mode t)
(sp-use-paredit-bindings)

;; Markdown mode

(sp-with-modes '(markdown-mode gfm-mode rst-mode)
  (sp-local-pair "*" "*"
                 :wrap "C-*"
                 :unless '(sp--gfm-point-after-word-p sp-point-at-bol-p)
                 :post-handlers '(("[d1]" "SPC"))
                 :skip-match 'sp--gfm-skip-asterisk)
  (sp-local-pair "**" "**")
  (sp-local-pair "_" "_" :wrap "C-_" :unless '(sp-point-after-word-p)))

(defun sp--gfm-point-after-word-p (id action context)
  "Return t if point is after a word, nil otherwise.
This predicate is only tested on \"insert\" action."
  (when (eq action 'insert)
    (sp--looking-back-p (concat "\\(\\sw\\)" (regexp-quote id)))))

(defun sp--gfm-skip-asterisk (ms mb me)
  (save-excursion
    (goto-char mb)
    (save-match-data (looking-at "^\\* "))))

;;; tex-mode latex-mode
(sp-with-modes '(tex-mode plain-tex-mode latex-mode)
  (sp-local-tag "i" "\"<" "\">"))

(sp-with-modes '(js-mode js2-mode web-mode)
  (sp-local-pair "/*" "*/" :post-handlers '((" | " "SPC")
                                            ("* ||\n[i]" "RET"))))
