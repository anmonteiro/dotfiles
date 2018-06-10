;; Customizations relating to editing a buffer.

;; Key binding to use "hippie expand" for text autocompletion
;; http://www.emacswiki.org/emacs/HippieExpand
;; (global-set-key (kbd "M-/") 'hippie-expand)

;; Lisp-friendly hippie expand
;; (setq hippie-expand-try-functions-list
;;       '(try-expand-dabbrev
;;         try-expand-dabbrev-all-buffers
;;         try-expand-dabbrev-from-kill
;;         try-complete-lisp-symbol-partially
;;         try-complete-lisp-symbol))

(require 'company)
(require 'company-lsp)
(push 'company-lsp company-backends)

(add-hook 'after-init-hook (lambda ()
                             (global-company-mode)
                             (company-quickhelp-mode 1)))

(eval-after-load 'company
  '(progn
     ;; (define-key company-active-map (kbd "M-n") nil)
     ;; (define-key company-active-map (kbd "M-p") nil)
     (define-key company-active-map (kbd "C-n") 'company-select-next)
     (define-key company-active-map (kbd "C-p") 'company-select-previous)
     (define-key company-mode-map (kbd "C-M-i") 'company-complete)))

(setq company-idle-delay 0.3
      company-tooltip-idle-delay 0.3)

(global-set-key (kbd "C-M-?") 'xref-find-references)

(require 'color)

;; (let ((bg (face-attribute 'default :background)))
;;   (custom-set-faces
;;    `(company-tooltip ((t (:inherit default :background ,(color-lighten-name bg 17)))))
;;    `(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))

;;    `(company-scrollbar-bg ((t (:background ,(color-lighten-name bg 10)))))
;;    `(company-scrollbar-fg ((t (:background ,(color-lighten-name bg 5)))))

;;    `(company-tooltip-common ((t (:inherit font-lock-constant-face))))
;;    `(company-tooltip-common-selection ((t (:inherit font-lock-function-name-face))))

;;    `(company-tooltip-annotation ((t (:inherit font-lock-string-face))))
;;    `(company-tooltip-annotation-selection ((t (:inherit font-lock-string-face))))))

;; (defun on-off-fci-before-company (command)
;;   (when (string= "show" command)
;;     (turn-off-fci-mode))
;;   (when (string= "hide" command)
;;     (turn-on-fci-mode)))

;; https://github.com/company-mode/company-mode/issues/180
;; (advice-add 'company-call-frontends :before #'on-off-fci-before-company)

(setq ffap-machine-p-known 'reject)

;; Highlights matching parenthesis
(show-paren-mode 1)

;; Highlight current line
(global-hl-line-mode 1)

;; Don't use hard tabs
(setq-default indent-tabs-mode nil)

;; When you visit a file, point goes to the last place where it
;; was when you previously visited the same file.
;; http://www.emacswiki.org/emacs/SavePlace
(require 'saveplace)
(setq-default save-place t)
;; keep track of saved places in ~/.emacs.d/places
(setq save-place-file (concat user-emacs-directory "places"))

;; Emacs can automatically create backup files. This tells Emacs to
;; put all backups in ~/.emacs.d/backups. More info:
;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Backup-Files.html
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory
                                               "backups"))))
(setq auto-save-default nil)

;; comments
(defun toggle-comment-on-line ()
  "comment or uncomment current line"
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))

(global-set-key (kbd "C-;") 'toggle-comment-on-line)

;; auto-indent by pressing the return key
(add-hook 'prog-mode-hook
          (lambda ()
            (local-set-key (kbd "RET") 'newline-and-indent)))

;; auto-indent with paredit-newline when using paredit
(eval-after-load 'paredit
  '(define-key paredit-mode-map [remap newline-and-indent] 'paredit-newline))

;; yay rainbows!
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; Whitespace

(add-hook 'prog-mode-hook
          (lambda ()
            (setq c-basic-offset 2
                  tab-width 2
                  indent-tabs-mode nil)))

(add-hook 'css-mode-hook
          (lambda ()
            (setq css-indent-offset 2
                  tab-width 2
                  indent-tabs-mode nil)))

(add-hook 'makefile-mode-hook
          (lambda ()
            (setq indent-tabs-mode t)
            (setq-default indent-tabs-mode t)
            (setq tab-width 2)))

(add-hook 'makefile-gmake-mode-hook
          (lambda ()
            (setq indent-tabs-mode t)
            (setq-default indent-tabs-mode t)
            (setq tab-width 2)))

;; use 2 spaces for tabs
(defun die-tabs ()
  (interactive)
  (set-variable 'tab-width 2)
  (mark-whole-buffer)
  (untabify (region-beginning) (region-end))
  (keyboard-quit))

;; show trailing whitespace by default
(setq-default show-trailing-whitespace t)

;; delete trailing whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; highlight-symbol

(add-hook 'prog-mode-hook 'auto-highlight-symbol-mode)

(setq ahs-idle-interval 0.4)

;; fix weird os x kill error
(defun ns-get-pasteboard ()
  "Returns the value of the pasteboard, or nil for unsupported formats."
  (condition-case nil
      (ns-get-selection-internal 'CLIPBOARD)
    (quit nil)))

(setq electric-indent-mode nil)

;; disable "Use Option as Meta key" and set Mac Cmd key as Meta
(setq default-input-method "MacOSX")
(when window-system
  (setq mac-option-modifier 'super
        mac-command-modifier 'meta))
(setq x-select-enable-clipboard t)

;; truncate lines by default
(setq-default truncate-lines t)

;; display a vertical rule at column 80
(setq-default fill-column 80)

(add-hook 'prog-mode-hook 'turn-on-fci-mode)
(add-hook 'text-mode-hook 'turn-on-fci-mode)

;; diff-hl setup

(require 'diff-hl)

(set-face-attribute 'diff-hl-change nil
                    :foreground "#E2C08D"
                    :background "#E2C08D")
(set-face-attribute 'diff-hl-insert nil
                    :foreground "green4"
                    :background "green4")
(set-face-attribute 'diff-hl-delete nil
                    :foreground "red3"
                    :background "red3")

(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
(add-hook 'after-change-major-mode-hook 'turn-on-diff-hl-mode)
(add-hook 'after-change-major-mode-hook 'diff-hl-flydiff-mode)

;; smooth-scrolling

(smooth-scrolling-mode 1)
(setq smooth-scroll-margin 5)

;; Flycheck

;; http://www.flycheck.org/manual/latest/index.html
(require 'flycheck)

;; turn on flychecking globally
(add-hook 'after-init-hook 'global-flycheck-mode)

(require 'snoopy)
;; turn on snoopy mode globally
(add-hook 'prog-mode-hook 'snoopy-mode)
