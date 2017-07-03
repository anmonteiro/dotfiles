;;----------------------------------------------------------------------------
;; Reason setup
;; Expects reason-cli to be installed:
;; npm install -g git://github.com/reasonml/reason-cli.git
;;----------------------------------------------------------------------------

(defun chomp-end (str)
  "Chomp tailing whitespace from STR."
  (replace-regexp-in-string (rx (* (any " \t\n")) eos)
                            ""
                            str))

(let* ((refmt-bin (chomp-end (shell-command-to-string "refmt ----where")))
       (merlin-bin (chomp-end (shell-command-to-string "ocamlmerlin ----where")))
       (merlin-base-dir (replace-regexp-in-string "bin/ocamlmerlin$" "" merlin-bin)))
  ;; Add npm merlin.el to the emacs load path and tell emacs where to find ocamlmerlin
  (add-to-list 'load-path (concat merlin-base-dir "share/emacs/site-lisp/"))
  (setq merlin-command merlin-bin)

  (setq refmt-command refmt-bin))

(require 'reason-mode)
(require 'merlin)
(add-hook 'reason-mode-hook (lambda ()
                              (add-hook 'before-save-hook 'refmt-before-save)
                              (merlin-mode)))

(setq merlin-ac-setup t)


(add-hook 'reason-mode-hook 'smartparens-mode)
