(provide 'my-dev-common)

(use-package yaml-mode)

(use-package magit
  :ensure t
  :init
  (setq magit-diff-options (quote ("--word-diff")))
  (setq magit-diff-refine-hunk 'all)
  :config
  (use-package evil-magit
    :ensure t
    :config
    ; Default commit editor opening in insert mode
    (add-hook 'with-editor-mode-hook 'evil-insert-state)
    (general-define-key
     :states '(normal)
     :keymaps 'with-editor-mode-map
      (kbd "RET") 'with-editor-finish
      [escape] 'with-editor-cancel
      )
    (general-define-key
     :states '(normal)
     :keymaps 'git-rebase-mode-map
      "l" 'git-rebase-show-commit
      )
    (general-define-key
     :prefix my-global-leader
     :states '(normal visual)
     "z" '(magit-status :which-key "git status")
    )
    )
  )

(use-package evil-nerd-commenter
  :ensure t
  :commands (evilnc-comment-or-uncomment-lines)
  :init
    (general-define-key
     :prefix my-global-leader
     :states '(normal visual)
     "c" 'evilnc-comment-or-uncomment-lines
    )
  )

(use-package yasnippet
  :ensure t
  :defer t
  :init
  (yas-global-mode 1)
    (general-define-key
     :prefix my-global-leader
     :states '(normal)
     "n" 'yas-new-snippet
     "u" 'yas-describe-tables
    )
    :config
  (yas-reload-all)
)

(use-package company
  :ensure t
  :defer t
  :config
  (setq company-tooltip-align-annotations t)
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 2)
  (use-package company-flx
    :config
    (company-flx-mode +1))
  )

; autocomplete paired brackets
(electric-pair-mode 1)

(use-package flycheck
  :ensure t
  :defer t
  :init
  (global-flycheck-mode))

(use-package smex
  :ensure t
  :init
  (smex-initialize))

