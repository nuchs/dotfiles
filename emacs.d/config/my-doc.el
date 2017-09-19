(provide 'my-doc)

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package nov
             :ensure t
             :defer t
             :config
             (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
             (setq nov-text-width 80))
