;; javascript / html
(add-to-list 'auto-mode-alist '("\\.js$" . js-mode))
(add-to-list 'auto-mode-alist '("\\.eslintrc.*$" . json-mode))
(add-to-list 'auto-mode-alist '("\\.babelrc$" . json-mode))

;; http://codewinds.com/blog/2015-04-02-emacs-flycheck-eslint-jsx.html
;; use web-mode for .jsx files
(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))

;; disable jshint since we prefer eslint checking
(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
    '(javascript-jshint json-python-json javascript-jshint
      javascript-gjslint javascript-jscs)))

;; use eslint with web-mode for jsx files
(flycheck-add-mode 'javascript-eslint 'web-mode)

;; customize flycheck temp file prefix
(setq-default flycheck-temp-prefix ".flycheck")

;; adjust indents for web-mode to 2 spaces
(defun custom-web-mode-hook ()
  "Hooks for Web mode. Adjust indents"
  ;;; http://web-mode.org/
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))
(add-hook 'web-mode-hook 'custom-web-mode-hook)

;; for better jsx syntax-highlighting in web-mode
(defadvice web-mode-highlight-part (around tweak-jsx activate)
  (if (equal web-mode-content-type "jsx")
    (let ((web-mode-enable-part-face nil))
      ad-do-it)
    ad-do-it))

(defun my/use-eslint-from-node-modules ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/eslint/bin/eslint.js"
                                        root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))

(defun my/use-flow-from-node-modules ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (flow (and root
                    (expand-file-name "node_modules/flow-bin/vendor/flow"
                                      root))))
    (when (and flow (file-executable-p flow))
      (setq-local flycheck-javascript-flow-executable flow))))

(add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)
(add-hook 'flycheck-mode-hook #'my/use-flow-from-node-modules)

;; Flycheck + Flowtype
(require 'flycheck-flow)
(flycheck-add-next-checker 'javascript-flow 'javascript-eslint)

(add-hook 'js-mode-hook 'subword-mode)
(add-hook 'js-mode-hook 'enable-paredit-mode)
(add-hook 'js2-mode-hook 'subword-mode)
(add-hook 'js2-mode-hook 'enable-paredit-mode)
(add-hook 'web-mode-hook 'subword-mode)
(add-hook 'web-mode-hook 'enable-paredit-mode)
(add-hook 'json-mode-hook 'enable-paredit-mode)
(add-hook 'html-mode-hook 'subword-mode)

(add-hook 'js-mode-hook
          (lambda ()
            (define-key js-mode-map "{" #'paredit-open-curly)
            (define-key js-mode-map "}" #'paredit-close-curly)))
(add-hook 'js2-mode-hook
          (lambda ()
            (define-key js2-mode-map "{" #'paredit-open-curly)
            (define-key js2-mode-map "}" #'paredit-close-curly)))
(add-hook 'json-mode-hook
          (lambda ()
            (define-key json-mode-map "{" #'paredit-open-curly)
            (define-key json-mode-map "}" #'paredit-close-curly)))
(add-hook 'web-mode-hook
          (lambda ()
            (define-key web-mode-map "{" #'paredit-open-curly)
            (define-key web-mode-map "}" #'paredit-close-curly)))

;; http://lpaste.net/edit/47052
(defun paredit-singlequote (&optional n)
  "Insert a pair of single-quotes.
With a prefix argument N, wrap the following N S-expressions in
  single-quotes, escaping intermediate characters if necessary.
If the region is active, `transient-mark-mode' is enabled, and the
  region's start and end fall in the same parenthesis depth, insert a
  pair of single-quotes around the region, again escaping intermediate
  characters if necessary.
Inside a comment, insert a literal single-quote.
At the end of a string, move past the closing single-quote.
In the middle of a string, insert a backslash-escaped single-quote.
If in a character literal, do nothing.  This prevents accidentally
  changing a what was in the character literal to become a meaningful
  delimiter unintentionally."
  (interactive "P")
  (cond ((paredit-in-string-p)
         (if (eq (cdr (paredit-string-start+end-points))
                 (point))
             (forward-char)             ; We're on the closing quote.
             (insert ?\\ ?\' )))
        ((paredit-in-comment-p)
         (insert ?\' ))
        ((not (paredit-in-char-p))
         (paredit-insert-pair n ?\' ?\' 'paredit-forward-for-quote))))

(add-hook 'js-mode-hook
          '(lambda ()
             (define-key js-mode-map "'" #'paredit-singlequote)))
(add-hook 'js2-mode-hook
          '(lambda ()
             (define-key js2-mode-map "'" #'paredit-singlequote)))
(add-hook 'web-mode-hook
          '(lambda ()
             (define-key web-mode-map "'" #'paredit-singlequote)))
(add-hook 'json-mode-hook
          '(lambda ()
             (define-key json-mode-map "'" #'paredit-singlequote)))

(setq js-indent-level 2)
(eval-after-load "sgml-mode"
  '(progn
     (require 'tagedit)
     (tagedit-add-paredit-like-keybindings)
     (add-hook 'html-mode-hook (lambda () (tagedit-mode 1)))))


;; coffeescript
(add-to-list 'auto-mode-alist '("\\.coffee.erb$" . coffee-mode))
(add-hook 'coffee-mode-hook 'subword-mode)
(add-hook 'coffee-mode-hook 'highlight-indentation-current-column-mode)
(add-hook 'coffee-mode-hook
          (defun coffee-mode-newline-and-indent ()
            (define-key coffee-mode-map "\C-j" 'coffee-newline-and-indent)
            (setq coffee-cleanup-whitespace nil)))
(custom-set-variables
 '(coffee-tab-width 2))
