(provide 'my-cosmetic)

(load-theme 'dracula t)

; Hid the bits of gui fluff that I'm not interested in
(tool-bar-mode -1)
(menu-bar-mode 0)
(setq inhibit-splash-screen t
      inhibit-startup-echo-area-message t
      inhibit-startup-message t)
(diminish 'undo-tree-mode)
(diminish 'eldoc-mode)

; Highlight column 80
(use-package column-enforce-mode
  :ensure t
  :diminish column-enforce-mode
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
(column-number-mode 1)


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
(set-fontset-font "fontset-default" nil 
                  (font-spec :size 24 :name "Symbola"))
; Highlight current line
(global-hl-line-mode 1)

