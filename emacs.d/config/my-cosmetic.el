(provide 'my-cosmetic)

; Themes
(require 'spaceline-config)
(setq powerline-default-separator 'contour)
(spaceline-spacemacs-theme)
(spaceline-toggle-buffer-id-on)
(spaceline-toggle-buffer-modified-on)
(spaceline-toggle-evil-state-on)
(spaceline-toggle-flycheck-error-on)
(spaceline-toggle-flycheck-info-on)
(spaceline-toggle-flycheck-warning-on)
(spaceline-toggle-line-on)
(spaceline-toggle-column-on)
(spaceline-toggle-major-mode-on)
(spaceline-toggle-version-control-on)
(spaceline-toggle-which-function-on)
(spaceline-toggle-buffer-position-on)

(spaceline-toggle-anzu-off)
(spaceline-toggle-auto-compile-off)
(spaceline-toggle-battery-off)
(spaceline-toggle-buffer-encoding-abbrev-off)
(spaceline-toggle-buffer-size-off)
(spaceline-toggle-erc-track-off)
(spaceline-toggle-global-off)
(spaceline-toggle-hud-off)
(spaceline-toggle-input-method-off)
(spaceline-toggle-minor-modes-off)
(spaceline-toggle-nyan-cat-off)
(spaceline-toggle-org-clock-off)
(spaceline-toggle-org-pomodoro-off)
(spaceline-toggle-paradox-menu-off)
(spaceline-toggle-persp-name-off)
(spaceline-toggle-point-position-off)
(spaceline-toggle-process-off)
(spaceline-toggle-python-pyenv-off)
(spaceline-toggle-python-pyvenv-off)
(spaceline-toggle-remote-host-off)
(spaceline-toggle-remote-host-off)
(spaceline-toggle-selection-info-off)
(spaceline-toggle-window-number-off)
(spaceline-toggle-workspace-number-off)

(load-theme 'material t)

; Hid the bits of gui fluff that I'm not interested in
(tool-bar-mode -1)
(menu-bar-mode 0)
(setq inhibit-splash-screen t
      inhibit-startup-echo-area-message t
      inhibit-startup-message t)

; Highlight column 80
(use-package column-enforce-mode
  :ensure t
  :init
  (add-hook 'prog-mode-hook 'column-enforce-mode))

; I like line numbers
(use-package linum-relative
  :ensure t
  :init
  (add-hook 'prog-mode-hook 'linum-mode)
  (linum-relative-mode)
  (setq linum-relative-current-symbol "")
  )

; Make it easy to match up parenthesis
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

; Set font
(add-to-list 'default-frame-alist '(font . "Inconsolata" ))
(set-face-attribute 'default nil :font  "Inconsolata-14" )

; Highlight current line
(global-hl-line-mode 1)

