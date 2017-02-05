(provide 'my-dev-rust)

(use-package toml-mode
  :ensure t)

(use-package rust-mode
  :ensure t
  :init
  (setq exec-path (append exec-path '("/home/nuchs/.cargo/bin")))
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)

    ; Leader keys
    (general-define-key
     :prefix my-major-leader
     :states '(normal)
     "f" '(rust-format-buffer :which-key "rust format buffer")
     "m" '(rust-promote-module-into-dir :which-key "make module into dir")
     )
  )

(use-package flycheck-rust
  :ensure t
  :mode (("\\.rs\\'" . rust-mode))
  :init
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
  )

(use-package cargo
  :ensure t
  :commands (cargo-process-build
             cargo-process-clean
             cargo-process-test
             cargo-process-run
             cargo-process-update
             cargo-process-fmt
             cargo-process-doc)
  :mode (("\\.rs\\'" . rust-mode))
  :init
  (add-hook 'rust-mode-hook 'cargo-minor-mode)
  (add-hook 'rust-mode-hook #'racer-mode)
  :config
  (setq cargo-process--custom-path-to-bin "/home/nuchs/.cargo/bin/")
  )

(use-package racer
  :ensure t
  :defer t
  :mode (("\\.rs\\'" . rust-mode))
  :init
  (add-hook 'racer-mode-hook #'eldoc-mode)
  (add-hook 'racer-mode-hook #'company-mode)
  (setq racer-cmd "~/.cargo/bin/racer")
  (setq racer-rust-src-path "/home/nuchs/.multirust/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/src")
    ; Leader keys
    (general-define-key
     :prefix my-major-leader
     :states '(normal)
     "d" '(racer-find-definition :which-key "jump to definition")
     "k" '(racer-describe :which-key "describe item")
     )
  )
