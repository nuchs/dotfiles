(provide 'my-cosmetic)

; Themes
(load-theme 'solarized-dark t)
(load-theme 'solarized-light t)
(load-theme 'zenburn t)

; Hid the bits of gui fluff that I'm not interested in
(tool-bar-mode -1)
(menu-bar-mode 0)
(setq inhibit-splash-screen t
      inhibit-startup-echo-area-message t
      inhibit-startup-message t)

; Highlight column 80
(use-package column-enforce-mode
  :ensure t
  :diminish column-enforce-mode
  :init
  (add-hook 'prog-mode-hook 'column-enforce-mode))

; I like line numbers
(use-package linum-relative
  :ensure t
  :diminish linum-relative-mode
  :config
  (global-linum-mode 1)
  (linum-relative-mode)
  )

(use-package rainbow-delimiters
  :ensure t
  :mode (("\\.rs\\'" . rust-mode)
         ("\\.el\\'" . emacs-lisp-mode)
         ("\\.hs\\'" . haskell-mode))
  :init
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

; Turn off audible alerts
(setq ring-bell-function 'ignore)
(setq visible-bell t)

; Make mouse wheel scrolling a bit slower
(setq mouse-wheel-scroll-amount '(2 ((shift) . 5)))
(setq mouse-wheel-progressive-speed nil)

; Sane visual lines
(visual-line-mode 1)

; Syntax highlighting hints
(add-to-list 'auto-mode-alist '("zshrc" . sh-mode))
(add-to-list 'auto-mode-alist '("zprofile" . sh-mode))
(add-to-list 'auto-mode-alist '("zshenv" . sh-mode))
