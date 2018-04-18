(provide 'my-dev-lisp)

;------------------------------------------------------------------------------
; ELisp
;------------------------------------------------------------------------------

(use-package elisp-slime-nav
  :ensure t
  :mode (("\\.el\\'" . emacs-lisp-mode))
  :init
  
  (defun my-lisp-hook ()
    (elisp-slime-nav-mode)
    (turn-on-eldoc-mode)
    (company-mode)
    (evil-define-key 'normal emacs-lisp-mode-map
      (kbd "K") 'elisp-slime-nav-describe-elisp-thing-at-point))

  (add-hook 'emacs-lisp-mode-hook 'my-lisp-hook))

(use-package slime
  :ensure t
  :mode (("\\.el\\'" . emacs-lisp-mode))
  :init
  (setq inferior-lisp-program "/usr/bin/sbcl")
  (setq slime-contribs '(slime-fancy))
  )

