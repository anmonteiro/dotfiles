;; Neotree
;; https://github.com/jaypei/emacs-neotree

;;  every time when the neotree window is  opened, it will try to find current
;;  file and jump to node.
;;(setq-default neo-smart-open t)

;; Don't allow neotree to be the only open window
(setq-default neo-dont-be-alone t)


(require 'neotree)
(global-set-key [f8] 'neotree-toggle)


(setq neo-theme 'nerd) ; 'classic, 'nerd, 'ascii, 'arrow

(setq neo-vc-integration '(face char))

;; Patch to fix vc integration
;; as per https://github.com/jaypei/emacs-neotree/pull/156
(defun neo-vc-for-node (node)
  (let* ((backend (ignore-errors
                    (vc-responsible-backend node)))
         (vc-state (when backend (vc-state node backend))))
    ;; (message "anm: %s %s %s" node backend vc-state)
    (cons (cdr (assoc vc-state neo-vc-state-char-alist))
          (cl-case vc-state
            (up-to-date       neo-vc-up-to-date-face)
            (edited           neo-vc-edited-face)
            (needs-update     neo-vc-needs-update-face)
            (needs-merge      neo-vc-needs-merge-face)
            (unlocked-changes neo-vc-unlocked-changes-face)
            (added            neo-vc-added-face)
            (removed          neo-vc-removed-face)
            (conflict         neo-vc-conflict-face)
            (missing          neo-vc-missing-face)
            (ignored          neo-vc-ignored-face)
            (unregistered     neo-vc-unregistered-face)
            (user             neo-vc-user-face)
            (otherwise        neo-vc-default-face)))))

(provide 'setup-neotree)
