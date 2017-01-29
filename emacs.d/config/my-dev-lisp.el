(provide 'my-dev-lisp)

(use-package elisp-slime-nav
  :ensure t
  :diminish elisp-slime-nav-mode
  :mode (("\\.el\\'" . emacs-lisp-mode))
  :init
  
  (defun my-lisp-hook ()
    (elisp-slime-nav-mode)
    (turn-on-eldoc-mode)
    (evil-define-key 'normal emacs-lisp-mode-map
      (kbd "K") 'elisp-slime-nav-describe-elisp-thing-at-point))

  (add-hook 'emacs-lisp-mode-hook 'my-lisp-hook))
