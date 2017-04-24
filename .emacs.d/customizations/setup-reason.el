;;----------------------------------------------------------------------------
;; Reason setup
;;----------------------------------------------------------------------------

(setq opam (substring (shell-command-to-string "opam config var prefix 2> /dev/null") 0 -1))
(add-to-list 'load-path (concat opam "/share/emacs/site-lisp"))
(setq refmt-command (concat opam "/bin/refmt"))

(require 'reason-mode)
(require 'merlin)
(setq merlin-ac-setup t)
(add-hook 'reason-mode-hook (lambda ()
                              (add-hook 'before-save-hook 'refmt-before-save)
                              (merlin-mode)))

(add-hook 'reason-mode-hook 'smartparens-mode)


;; (let ((opam-share (ignore-errors (car (process-lines "opam" "config" "var" "share")))))
;;   (when (and opam-share (file-directory-p opam-share))
;;     ;; Register Merlin
;;     (add-to-list 'load-path (expand-file-name "emacs/site-lisp" opam-share))
;;     (autoload 'merlin-mode "merlin" nil t nil)
;;     ;; Automatically start it in OCaml buffers
;;     (add-hook 'tuareg-mode-hook 'merlin-mode t)
;;     (add-hook 'caml-mode-hook 'merlin-mode t)
;;     ;; Use opam switch to lookup ocamlmerlin binary
;;     (setq merlin-command 'opam)))
