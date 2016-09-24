;; Neotree
;; https://github.com/jaypei/emacs-neotree

(require 'neotree)

;; every time when the neotree window is  opened, it will try to find current
;; file and jump to node.
(setq-default neo-smart-open t)

;; change root automatically when running `projectile-switch-project`
(setq projectile-switch-project-action 'neotree-projectile-action)

;; Don't allow neotree to be the only open window
(setq-default neo-dont-be-alone t)

(global-set-key [f8] 'neotree-toggle)

(setq neo-theme (if window-system 'icons 'nerd)) ; 'classic, 'nerd, 'ascii, 'arrow

(setq neo-vc-integration '(face char))

(setq neo-show-hidden-files t)

(setq neo-toggle-window-keep-p t)

(setq neo-force-change-root t)


;; face customizations

(set-face-attribute 'neo-vc-edited-face nil
                    :foreground "#E2C08D")

(set-face-attribute 'neo-vc-added-face nil
                    :foreground "green4")

(provide 'setup-neotree)
