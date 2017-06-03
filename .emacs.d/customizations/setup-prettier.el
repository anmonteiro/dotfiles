(require 'prettier-js)

(setq prettier-js-width-mode 'fill)
(setq prettier-js-args '("--single-quote" "--trailing-comma=all" "--parser=flow"))

(defun my/use-prettier-from-node-modules ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (prettier (and root
                        (expand-file-name "node_modules/.bin/prettier"
                                          root))))
    (when (and prettier (file-executable-p prettier))
      (setq-local prettier-js-command prettier))))

(add-hook 'js-mode-hook 'prettier-js-mode)
(add-hook 'js-mode-hook #'my/use-prettier-from-node-modules)
(add-hook 'js2-mode-hook 'prettier-js-mode)
(add-hook 'js2-mode-hook #'my/use-prettier-from-node-modules)
(add-hook 'web-mode-hook 'prettier-js-mode)
(add-hook 'web-mode-hook #'my/use-prettier-from-node-modules)
;; CSS
(add-hook 'css-mode-hook 'prettier-js-mode)
(add-hook 'css-mode-hook #'my/use-prettier-from-node-modules)
(add-hook 'scss-mode-hook 'prettier-js-mode)
(add-hook 'scss-mode-hook #'my/use-prettier-from-node-modules)

(provide 'setup-prettier)
;;; setup-prettier.el ends here
