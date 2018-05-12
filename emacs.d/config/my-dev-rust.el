(provide 'my-dev-rust)

(use-package toml-mode
  :ensure t
  :mode (("\\.toml\\'" . toml-mode))
  :init
  (add-to-list 'auto-mode-alist '("Cargo\\.lock$" . toml-mode))
  )

(use-package rust-mode
  :ensure t
  :mode (("\\.rs\\'" . rust-mode))
  :config
  (setq path-to-rusty-tags "/usr/bin/rusty-tags")

  (general-define-key
   :prefix my-major-leader
   :states '(normal)
   "f" '(rust-format-buffer :which-key "rust format buffer")
   "m" '(rust-promote-module-into-dir :which-key "make module into dir")
   "d" '(racer-find-definition :which-key "find definition of symbol")
   "h" '(racer-describe :which-key "open help for symbol")
   )

  (use-package flycheck-rust
    :ensure t
    :init
    (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
    )

  (use-package cargo
    :ensure t
    :defer t
    :diminish cargo-mode
    :commands (cargo-process-build
               cargo-process-clean
               cargo-process-test
               cargo-process-run
               cargo-process-update
               cargo-process-fmt
               cargo-process-doc)
    :init
    (add-hook 'rust-mode-hook 'cargo-minor-mode)
    (setq cargo-process--command-clippy "+nightly clippy")
    )

  (use-package racer
    :ensure t
    :defer t
    :diminish racer-mode
    :init
    (add-hook 'rust-mode-hook #'racer-mode)
    (add-hook 'racer-mode-hook #'eldoc-mode)
    (add-hook 'racer-mode-hook #'company-mode)
    :config
    (company-mode)
    )

  (defun my-rust-mode-hook ()
    "Hook for `emacs-haskell-mode'"
    (set (make-local-variable 'company-backends)
         '((company-yasnippet company-capf company-dabbrev-code)))
    (company-mode))
  
    (add-hook 'rust-mode-hook 'my-rust-mode-hook)
  )
