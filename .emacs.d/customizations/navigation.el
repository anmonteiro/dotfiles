;; These customizations make it easier for you to navigate files,
;; switch buffers, and choose options from the minibuffer.


;; "When several buffers visit identically-named files,
;; Emacs must give the buffers distinct names. The usual method
;; for making buffer names unique adds ‘<2>’, ‘<3>’, etc. to the end
;; of the buffer names (all but one of them).
;; The forward naming method includes part of the file's directory
;; name at the beginning of the buffer name
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Uniquify.html
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Turn on recent file mode so that you can more easily switch to
;; recently edited files when you first start emacs
(setq recentf-save-file (concat user-emacs-directory ".recentf"))
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 40)

;; Shows a list of buffers
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; projectile everywhere!
(projectile-global-mode)

(require 'ace-window)
(global-set-key (kbd "M-p") 'ace-window)

;; Most configuration taken from https://tuhdo.github.io/helm-intro.html
(require 'helm-config)
(helm-mode 1)

(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x b") 'helm-mini)

(setq helm-truncate-lines t)

;; Enable fuzzy matching for selected commands
;; https://github.com/emacs-helm/helm/wiki/Fuzzy-matching
(setq helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match    t
      helm-M-x-fuzzy-match        t
      helm-mode-fuzzy-match       t)

;; open the helm buffer inside the current window
(setq helm-split-window-in-side-p t)

(setq helm-ff-file-name-history-use-recentf t)

;; rebind tab to run persistent action
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
;; make TAB work in terminal
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-z")  'helm-select-action)

(require 'helm-projectile)
(helm-projectile-on)

;; TODO:
;; - helm-kill-ring
;; - helm-occur / swoop
;; - helm-google-suggest C-x c C-c g is too long
;; - helm-ag
