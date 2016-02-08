;; Sets up exec-path-from shell
;; https://github.com/purcell/exec-path-from-shell
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-envs
   '("PATH")))


(defun instashell (pre)
  "Opens a shell buffer based in the current directory and auto-named
   to prevent buffer name clashing on the *shell* buffer name.

   With a prefix argument (`C-u`), uses projectile to navigate to the project
   root and open the shell buffer from there."
  (interactive "P")
  (if pre
      (let* ((default-directory (projectile-project-root))
             (project-name (nth 1 (reverse
                                   (s-split "/" (projectile-project-root))))))
        (shell (concat "*shell:" project-name "*")))
      (shell (concat "*shell:" default-directory "*"))))

(global-set-key (kbd "C-c s") 'instashell)
