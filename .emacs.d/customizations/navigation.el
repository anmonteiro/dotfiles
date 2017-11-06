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
(global-set-key (kbd "M-P") 'ace-window)

;; Most configuration taken from https://tuhdo.github.io/helm-intro.html
(require 'helm)
(require 'helm-config)
(require 'helm-buffers)

(helm-mode 1)
(helm-autoresize-mode 1)

(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)

;; Enable fuzzy matching for selected commands
;; https://github.com/emacs-helm/helm/wiki/Fuzzy-matching
(setq helm-candidate-number-limit 50
      helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match    t
      helm-M-x-fuzzy-match        t
      helm-mode-fuzzy-match       t
      helm-truncate-lines         t
      helm-autoresize-max-height 30
      helm-autoresize-min-height 30)

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

(setq helm-projectile-truncate-lines t)
(define-key projectile-mode-map (kbd "C-c p s g") 'helm-do-ag-project-root)

(require 'helm-swoop)
(setq helm-swoop-split-with-multiple-windows t
      helm-swoop-split-direction 'split-window-vertically
      helm-swoop-use-line-number-face t
      helm-swoop-speed-or-color t)

(setq helm-swoop-pre-input-function
      (lambda ()
        (if (region-active-p)
            (buffer-substring-no-properties (region-beginning)
                                            (region-end))
          "")))

(define-key helm-swoop-map (kbd "C-r") 'helm-previous-line)
(define-key helm-swoop-map (kbd "C-s") 'helm-next-line)
(define-key helm-multi-swoop-map (kbd "C-r") 'helm-previous-line)
(define-key helm-multi-swoop-map (kbd "C-s") 'helm-next-line)

;; Interactive search key bindings. By default, C-s/C-M-s and C-r/C-M/r run
;; isearch, this swaps those bindings for `helm-swoop`.
(global-set-key (kbd "C-s") 'helm-swoop)
(global-set-key (kbd "C-r") 'helm-swoop)
(global-set-key (kbd "C-M-s") 'helm-swoop)
(global-set-key (kbd "C-M-r") 'helm-swoop)

;; TODO:
;; - helm-ag: search in hidden dirs by default, better keybindings, better CLI defaults
;; - helm-google-suggest C-x c C-c g is too long
;; - eldoc / is there a helm thing?
