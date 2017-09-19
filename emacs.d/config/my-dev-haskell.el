(provide 'my-dev-haskell)

(use-package intero
  :ensure t
  :mode ("\\.hs\\'")
  :init
  (add-hook 'haskell-mode-hook 'intero-mode))
