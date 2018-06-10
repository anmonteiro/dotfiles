(require 'reason-mode)

;; Merlin
(let ((opam-share (ignore-errors (car (process-lines "opam" "config" "var" "share")))))
  (when (and opam-share (file-directory-p opam-share))
    ;; Register Merlin
    (add-to-list 'load-path (expand-file-name "emacs/site-lisp" opam-share))
    (autoload 'merlin-mode "merlin" "Merlin mode" t)
    ;; Use opam switch to lookup ocamlmerlin binary
    (setq merlin-command 'opam)))

(setq merlin-ac-setup 'easy)

(defun shell-cmd (cmd)
  "Returns the stdout output of a shell command or nil if the command returned
   an error"
  (car (ignore-errors (apply 'process-lines (split-string cmd)))))

(let* ((refmt-bin (or (shell-cmd "refmt ----where")
                      (shell-cmd "which refmt"))))
  (when refmt-bin
    (setq refmt-command refmt-bin)))

(require 'merlin)
(require 'ocp-indent)
;; (require 'utop)

(setq utop-command "opam config exec -- utop -emacs")

(defun setup-ocaml-reason ()
  (company-mode)
  ;; (company-quickhelp-mode)
  ;; (setq-local merlin-completion-with-doc t)
  ;; (merlin-mode)
  (smartparens-mode)
  (auto-highlight-symbol-mode -1))

(defun setup-ocaml ()
  (setup-ocaml-reason)
  (setq-local indent-line-function 'ocp-indent-line)
  (setq-local indent-region-function 'ocp-indent-region)
  ;; (utop-minor-mode)
  )

(defun setup-reason ()
  (setup-ocaml-reason)
  (add-hook 'before-save-hook 'refmt-before-save))

(add-hook 'reason-mode-hook 'setup-reason)

(add-hook 'tuareg-mode-hook 'setup-ocaml)

(add-hook 'tuareg-jbuild-mode 'paredit-mode)

(setq refmt-width-mode 'fill)

;; (with-eval-after-load 'lsp-mode
;;   (require 'lsp-flycheck))
(require 'lsp-mode)
(require 'lsp-ocaml)

(setq lsp-ocaml-reason-lang-server-command
      '("/Users/anmonteiro/Downloads/bin.native"))

(add-hook 'reason-mode-hook 'lsp-ocaml-enable)
(add-hook 'tuareg-mode-hook 'lsp-ocaml-enable)
(add-hook 'caml-mode-hook 'lsp-ocaml-enable)
(add-hook 'tuareg-dune-mode 'paredit-mode)

