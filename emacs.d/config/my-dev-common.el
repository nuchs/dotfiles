(provide 'my-dev-common)

(use-package magit
  :ensure t
  :defer t
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
    )
  )

(general-define-key
    :prefix my-global-leader
    :states '(normal visual)
    "z" '(magit-status :which-key "git status")
)

(use-package evil-nerd-commenter
  :ensure t
  :defer t
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
  :diminish yas-minor-mode
  :init
  (yas-global-mode 1)
  :config
  (defun my-extract-method ()
    "Extract selection into new method"
    (interactive)
    (evil-delete (region-beginning) (region-end))
    (evil-exit-visual-state)
    (evil-forward-section-end)
    (yas-expand-snippet (yas-lookup-snippet "function-yank"))
    (evil-insert 0)
    )

  (general-define-key
   :prefix my-global-leader
   :states '(normal)
   "n" 'yas-new-snippet
   "u" 'yas-describe-tables
   )

  (general-define-key
   :prefix "M-r"
   :states '(normal)
   "s" 'yas-insert-snippet
   "m" 'my-extract-method
   )

  (yas-reload-all)
)

(use-package company
  :ensure t
  :defer t
  :diminish company-mode
  :config
  (setq company-tooltip-align-annotations t)
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 2)
  (use-package company-flx
    :config
    (company-flx-mode +1))
  (add-to-list 'company-backends 'company-dabbrev-code)
  (add-to-list 'company-backends 'company-yasnippet)
  )

; autocomplete paired brackets
(electric-pair-mode 1)

(use-package flycheck
  :ensure t
  :defer t
  :init
  (global-flycheck-mode)
    (general-define-key
     :states '(normal)
     "]c" 'flycheck-next-error
     "[c" 'flycheck-previous-error
    )

    (general-define-key
    :prefix my-global-leader
     :states '(normal)
     "E" 'flycheck-list-errors
     "e" 'flycheck-explain-error-at-point
    )

    (use-package flycheck-status-emoji
      :ensure t
      :defer t
      :init
      (flycheck-status-emoji-mode))
  )

(use-package smex
  :ensure t
  :defer t
  :init
  (smex-initialize))

