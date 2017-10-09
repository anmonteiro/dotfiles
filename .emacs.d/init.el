;;;;
;; Packages
;;;;

;; Turn off mouse interface early in startup to avoid momentary display
;; (when (fboundp 'menu-bar-mode)
;;   (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
;;(when (fboundp 'scroll-bar-mode)
;;  (scroll-bar-mode -1))


;; Define package repositories
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/") t)

;; (setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
;;                          ("marmalade" . "http://marmalade-repo.org/packages/")
;;                          ("melpa" . "http://melpa-stable.milkbox.net/packages/")))


;; Load and activate emacs packages. Do this first so that the
;; packages are loaded before you start trying to modify them.
;; This also sets the load path.
(package-initialize)

;; Download the ELPA archive description if needed.
;; This informs Emacs about the latest versions of all packages, and
;; makes them available for download.
(when (not package-archive-contents)
  (package-refresh-contents))

;; The packages you want installed. You can also install these
;; manually with M-x package-install
;; Add in your own as you wish:
(defvar my-packages
  '(paredit
    clojure-mode
    ;; extra syntax highlighting for clojure
    clojure-mode-extra-font-locking
    cider
    projectile
    ;; colorful parenthesis matching
    rainbow-delimiters
    ;; edit html tags like sexps
    tagedit
    magit
    ;; sidebar / file explorer
    neotree
    markdown-mode
    ;; s string manipulation library
    s
    yaml-mode
    haskell-mode
    hindent
    less-css-mode
    markdown-preview-mode
    diff-hl
    fill-column-indicator
    smooth-scrolling
    flycheck
    rjsx-mode
    json-mode
    flycheck-flow
    all-the-icons
    go-mode
    smartparens
    purescript-mode
    terraform-mode
    dockerfile-mode
    prettier-js
    auto-highlight-symbol
    helm
    helm-projectile
    helm-ag
    helm-swoop
    ace-window
    snoopy
    tuareg
    company
    rainbow-mode
    base16-theme
    inf-clojure
    lsp-mode))

;; On OS X, an Emacs instance started from the graphical user
;; interface will have a different environment than a shell in a
;; terminal window, because OS X does not run a shell during the
;; login. Obviously this will lead to unexpected results when
;; calling external utilities like make from Emacs.
;; This library works around this problem by copying important
;; environment variables from the user's shell.
;; https://github.com/purcell/exec-path-from-shell
(if (eq system-type 'darwin)
    (add-to-list 'my-packages 'exec-path-from-shell))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; Place downloaded elisp files in ~/.emacs.d/vendor. You'll then be able
;; to load them.
;;
;; For example, if you download yaml-mode.el to ~/.emacs.d/vendor,
;; then you can add the following code to this file:
;;
;; (require 'yaml-mode)
;; (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
;;
;; Adding this code will make Emacs enter yaml mode whenever you open
;; a .yml file
(add-to-list 'load-path "~/.emacs.d/vendor")

;;;;
;; Customization
;;;;

;; Add a directory to our load path so that when you `load` things
;; below, Emacs knows where to look for the corresponding file.
(add-to-list 'load-path "~/.emacs.d/customizations")

;; Sets up exec-path-from-shell so that Emacs will use the correct
;; environment variables
(load "shell-integration.el")

;; These customizations make it easier for you to navigate files,
;; switch buffers, and choose options from the minibuffer.
(load "navigation.el")

;; These customizations change the way emacs looks and disable/enable
;; some user interface elements
(load "ui.el")

;; Set up fonts
(load "fonts.el")

;; For Neotree sidebar
(load "setup-neotree.el")

;; These customizations make editing a bit nicer.
(load "editing.el")

;; Hard-to-categorize customizations
(load "misc.el")

;; For editing lisps
(load "elisp-editing.el")

;; Language-specific
(load "setup-clojure.el")
(load "setup-js.el")
(load "setup-java.el")
(load "setup-markdown.el")
(load "setup-haskell.el")
(load "setup-latex.el")
(load "setup-reason.el")
(load "setup-smartparens.el")
(load "setup-css.el")
(load "setup-prettier.el")
(load "hacks.el")
