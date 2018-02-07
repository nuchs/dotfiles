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
             :mode (("\\.epub\\'" . nov-mode))
             :config
             (setq nov-text-width 80))

(use-package pdf-tools
             :ensure t
             :config
             (pdf-tools-install))

(use-package pocket-reader
             :ensure t)

