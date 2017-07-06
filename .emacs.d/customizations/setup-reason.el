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

(defun shell-cmd (cmd)
  "Returns the stdout output of a shell command or nil if the command returned
   an error"
  (let ((stdoutput (chomp-end
                    (with-output-to-string
                      (with-current-buffer
                          standard-output
                        (process-file shell-file-name nil
                                      '(t nil)  nil
                                      shell-command-switch cmd))))))
    (when (not (= (length stdoutput) 0))
      stdoutput)))

(let* ((refmt-bin (shell-cmd "refmt ----where"))
       (merlin-bin (shell-cmd "ocamlmerlin ----where"))
       (merlin-base-dir (when merlin-bin (replace-regexp-in-string "bin/ocamlmerlin$" "" merlin-bin))))
  ;; Add npm merlin.el to the emacs load path and tell emacs where to find ocamlmerlin
  (when merlin-bin
    (add-to-list 'load-path (concat merlin-base-dir "share/emacs/site-lisp/"))
    (setq merlin-command merlin-bin))

  (when refmt-bin
    (setq refmt-command refmt-bin)))

(require 'reason-mode)
(require 'merlin)

(add-hook 'reason-mode-hook (lambda ()
                              (add-hook 'before-save-hook 'refmt-before-save)
                              (merlin-mode)))

(setq merlin-ac-setup t)
(setq refmt-width-mode 'fill)


(add-hook 'reason-mode-hook 'smartparens-mode)
