(define-minor-mode snoopy-mode
  "Toggle snoopy mode."
  nil
  " Snoopy"
  '(("1" . (lambda () (interactive) (insert-char ?! 1)))
    ("2" . (lambda () (interactive) (insert-char ?@ 1)))
    ("3" . (lambda () (interactive) (insert-char ?# 1)))
    ("4" . (lambda () (interactive) (insert-char ?$ 1)))
    ("5" . (lambda () (interactive) (insert-char ?% 1)))
    ("6" . (lambda () (interactive) (insert-char ?^ 1)))
    ("7" . (lambda () (interactive) (insert-char ?& 1)))
    ("8" . (lambda () (interactive) (insert-char ?* 1)))
    ("9" . paredit-open-round)
    ("0" . paredit-close-round)

    ("!" . (lambda () (interactive) (insert-char ?1 1)))
    ("@" . (lambda () (interactive) (insert-char ?2 1)))
    ("#" . (lambda () (interactive) (insert-char ?3 1)))
    ("$" . (lambda () (interactive) (insert-char ?4 1)))
    ("%" . (lambda () (interactive) (insert-char ?5 1)))
    ("^" . (lambda () (interactive) (insert-char ?6 1)))
    ("&" . (lambda () (interactive) (insert-char ?7 1)))
    ("*" . (lambda () (interactive) (insert-char ?8 1)))
    ("(" . (lambda () (interactive) (insert-char ?9 1)))
    (")" . (lambda () (interactive) (insert-char ?0 1)))))

;; I deserve to be made fun of for this:
(global-set-key (kbd "C-c 0") (lambda () (interactive) (insert-char ?0 1)))
(global-set-key (kbd "C-c 9") (lambda () (interactive) (insert-char ?9 1)))

(provide 'snoopy-mode)
;;; snoopy-mode.el ends here
