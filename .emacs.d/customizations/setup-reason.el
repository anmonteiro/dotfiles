(require 'reason-mode)

;; Merlin
(let ((opam-share (ignore-errors (car (process-lines "opam" "config" "var" "share")))))
  (when (and opam-share (file-directory-p opam-share))
    ;; Register Merlin
    (add-to-list 'load-path (expand-file-name "emacs/site-lisp" opam-share))
    (autoload 'merlin-mode "merlin" nil t nil)
    ;; Use opam switch to lookup ocamlmerlin binary
    (setq merlin-command 'opam)))

(setq merlin-ac-setup 'easy)

(defun shell-cmd (cmd)
  "Returns the stdout output of a shell command or nil if the command returned
   an error"
  (car (ignore-errors (apply 'process-lines (split-string cmd)))))

(let* ((refmt-bin (or (shell-cmd "refmt ----where")
                      (shell-cmd "which refmt")))
       (merlin-bin (or (shell-cmd "ocamlmerlin ----where")
                       (shell-cmd "which ocamlmerlin")))
       (merlin-base-dir (when merlin-bin
                          (replace-regexp-in-string "bin/ocamlmerlin$" "" merlin-bin))))
  ;; Add npm merlin.el to the emacs load path and tell emacs where to find ocamlmerlin
  ;; (when merlin-bin
  ;;   (add-to-list 'load-path (concat merlin-base-dir "share/emacs/site-lisp/"))
  ;;   (setq merlin-command merlin-bin))

  (when refmt-bin
    (setq refmt-command refmt-bin)))

(require 'merlin)
(require 'ocp-indent)

(defun setup-ocaml-reason ()
  (company-mode)
  ;; (company-quickhelp-mode)
  (setq-local merlin-completion-with-doc t)
  (setq-local indent-line-function 'ocp-indent-line)
  (setq-local indent-region-function 'ocp-indent-region)
  (merlin-mode)
  (smartparens-mode))

(add-hook 'reason-mode-hook (lambda ()
                              (add-hook 'before-save-hook 'refmt-before-save)
                              (setup-ocaml-reason)))

(add-hook 'tuareg-mode-hook 'setup-ocaml-reason)

(setq refmt-width-mode 'fill)
