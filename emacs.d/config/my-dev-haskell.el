(provide 'my-dev-haskell)

(use-package intero
  :ensure t
  :config
  (setq exec-path (append exec-path '("/home/nuchs/.local/bin")))
  (defun my-haskell-mode-hook ()
    "Hook for `emacs-haskell-mode'"
    ;; (set (make-local-variable 'company-backends)
    ;;      '((company-capf company-dabbrev-code)))
    (company-mode))
    (add-hook 'haskell-mode-hook 'my-haskell-mode-hook)
    (add-hook 'haskell-mode-hook 'intero-mode)
  )

