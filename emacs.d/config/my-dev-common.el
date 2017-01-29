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
    (evil-define-key 'normal with-editor-mode-map
      (kbd "RET") 'with-editor-finish
      [escape] 'with-editor-cancel
      )
    (evil-define-key 'normal git-rebase-mode-map
      "l" 'git-rebase-show-commit
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
  :diminish yas-mode
  :init
  (add-hook 'prog-mode-hook #'yas-minor-mode)
    (general-define-key
     :prefix my-global-leader
     :states '(normal)
     "y" 'yas-new-snippet
     "u" 'yas-describe-tables
    )
    :config
  (yas-reload-all)
)

; autocomplete paired brackets
(electric-pair-mode 1)
