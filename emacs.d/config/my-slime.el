(defun my-lisp-hook ()
  (elisp-slime-nav-mode)
  (turn-on-eldoc-mode))
(add-hook 'emacs-lisp-mode-hook 'my-lisp-hook)

;; EVIL override keybinds
(evil-define-key 'normal emacs-lisp-mode-map
  (kbd "K") 'elisp-slime-nav-describe-elisp-thing-at-point)

(provide 'my-slime)
