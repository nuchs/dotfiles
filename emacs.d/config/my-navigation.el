(provide 'my-navigation)

;----------------------------------------------------------------------------
; General setting
;----------------------------------------------------------------------------

; Turn on tracking mru files
(require 'recentf)
(recentf-mode t)
(setq recentf-max-saved-items 50)

;----------------------------------------------------------------------------
; Ivy
;----------------------------------------------------------------------------
(use-package flx
  :ensure t)

(use-package ivy
  :ensure t
  :init (ivy-mode 1)        ; enable ivy globally at startup
  :config
  (setq ivy-use-virtual-buffers t)   ; extend searching to bookmarks and …
  (setq ivy-height 20)               ; set height of the ivy window
  (setq ivy-count-format "(%d/%d) ") ; count format, from the ivy help page
  (setq ivy-re-builders-alist
        '((t . ivy--regex-fuzzy)))
  )

(use-package counsel
  :ensure t
  :defer t)

;----------------------------------------------------------------------------
; I'm EVIL
;----------------------------------------------------------------------------
(setq my-global-leader "SPC")
(setq my-major-leader "\\")


(use-package evil
  :ensure t
  :config
  (evil-mode 1)

  (add-to-list 'evil-motion-state-modes 'notmuch-search-mode)
  (add-to-list 'evil-motion-state-modes 'notmuch-show-mode)
  (add-to-list 'evil-motion-state-modes 'notmuch-hello-mode)
  (add-to-list 'evil-motion-state-modes 'notmuch-message-mode)

  (use-package evil-surround
    :ensure t
    :config
    (global-evil-surround-mode 1))

  (use-package evil-matchit
    :ensure t
    :defer t
    :config
    (global-evil-matchit-mode 1))

  (use-package highlight
    :ensure t
    :commands (evil-search-forward evil-search-backward)
    :config
    (use-package evil-search-highlight-persist
      :ensure t
      :config
      (global-evil-search-highlight-persist t)))

  (use-package general
    :ensure t
    :config

    (use-package which-key :ensure t
      :init
      (which-key-mode)
      :config
      (which-key-setup-side-window-right-bottom)
      (setq which-key-sort-order 'which-key-key-order-alpha
            which-key-side-window-max-width 0.33
            which-key-idle-delay 0.5))

    (defun shell-other-window ()
      "Open a `shell' in a new window."
      (interactive)
      (let ((buf (shell)))
        (switch-to-buffer (other-buffer buf))
        (switch-to-buffer-other-window buf)))

    (defun my-replace ()
      "Open writable grep buffer from ivy rg search"
      (interactive)
      (ivy-occur)
      (ivy-wgrep-change-to-wgrep-mode))

    ; Leader keys
    (general-define-key
     :prefix "SPC"
     :states '(normal motion)
     "`" '(counsel-load-theme :which-key "load theme")
     "SPC" '(mode-line-other-buffer :which-key "Flip to previous buffer")
     "1" '(delete-other-windows :which-key "maximise current window")
     "a" '(align-regexp :which-key "align on regexp")
     "B" '(ivy-switch-buffer-other-window :which-key "open buffer elsewhere")
     "b" '(ivy-switch-buffer :which-key "open buffer")
     "d" '(evil-window-split :which-key "horizontal split")
     "F" '(counsel-find-file :which-key "find file")
     "f" '(counsel-git :which-key "find file in project")
     "g" '(counsel-rg :which-key "search across files")
     "h" '(help :which-key "emacs help")
     "k" 'general-describe-keybindings
     "l" '(counsel-locate :which-key "locate file")
     "q" 'evil-quit
     "r" '(counsel-recentf :which-key "find recent file")
     "s" '(evil-window-vsplit :which-key "vertical split")
     "t" '(shell-other-window :which-key "open shell in new split")
     "w" 'bury-buffer
     "x" '(counsel-M-x :which-key "select command to run")
     "v" '(my-save-ivy-view :which-key "persist window layout")
     "V" '(my-delete-ivy-view :which-key "delete persistant window layout")
     "p" '(ivy-push-view :which-key "save current window layout for session")
     "P" '(ivy-pop-view :which-key "delete saved window layout")
     "y" '(counsel-yank-pop :which-key "search kill ring")
     )

    ; Insert mode keybinds
    (general-define-key
     :states '(insert)
     "j" (general-key-dispatch 'self-insert-command
           :timeout 0.25
           "k" 'evil-normal-state))
    
    ; Standard keybinds
    (general-define-key
     :states '(normal motion)
     "/" '(swiper :which-key "search current buffer")
     "C-h" '(evil-window-left  :which-key "Switch focus left")
     "C-j" '(evil-window-down  :which-key "Switch focus down")
     "C-k" '(evil-window-up    :which-key "Switch focus up")
     "C-l" '(evil-window-right :which-key "Switch focus right")
     "j" '(evil-next-visual-line :which-key "down line")
     "k" '(evil-previous-visual-line :which-key "up line")
     "C-<up>" '(evil-window-increase-height :which-key "increase window height")
     "C-<down>" '(evil-window-decrease-height :which-key "decrease window height")
     "C-<left>" '(evil-window-increase-width :which-key "increase window width")
     "C-<right>" '(evil-window-decrease-width :which-key "decrease window width")
     "<f1>" '(counsel-describe-function :which-key "describe lisp function")
     "<f2>" '(counsel-describe-variable :which-key "describe lisp variable")
     "<f3>" '(counsel-unicode-char :which-key "search unicode characters")
     "<f4>" '(whitespace-mode :which-key "display whitespace characters")
     )

    ; keybinds for ivy windows
    (general-define-key
     :map ivy-minibuffer-map
     "C-#" '(my-replace)
     "M-y" '(ivy-next-line :which-key "next line"))
    )
  )

;----------------------------------------------------------------------------
; Dired
;----------------------------------------------------------------------------
(use-package dired
  :defer t
  :init
  (evil-set-initial-state 'dired-mode 'normal)
  (global-set-key (kbd "C-x C-d") 'dired)
  
  (use-package dired-x
    :init
    (put 'dired-find-alternate-file 'disabled nil))
  
  (defun my-dired-up-directory ()
    "Take dired up one directory, but behave like dired-find-alternate-file"
    (interactive)
    (let ((old (current-buffer)))
      (dired-up-directory)
      (kill-buffer old)
      ))

  (defun my-dired-next-line (count)
    "Move to next line, always staying on the dired filename."
    (interactive "p")
    (dired-next-line count)
    (dired-move-to-filename))

  (defun my-dired-previous-line (count)
    "Move to previous line, always staying on the dired filename."
    (interactive "p")
    (dired-previous-line count)
    (dired-move-to-filename))

  (defun my-dired-hook ()
    "Set up evil mode keybinds for dired mode"
    (general-define-key
     :states '(normal)
     :keymaps 'dired-mode-map
     "f" 'dired-do-find-marked-files
     "h" 'my-dired-up-directory
     "l" 'dired-find-alternate-file
     "j" 'my-dired-next-line
     "k" 'my-dired-previous-line
     "K" 'dired-prev-marked-file
     "J" 'dired-next-marked-file
     "n" 'evil-search-next
     "N" 'evil-search-previous
     "q" 'kill-this-buffer
     "/" 'evil-search-forward
     "H" 'evil-window-top
     "M" 'evil-window-middle
     "L" 'evil-window-bottom

     "c" 'dired-create-directory
     "C" 'dired-do-copy
     "D" 'dired-do-delete
     "R" 'dired-do-rename
     "%u" 'dired-upcase
     "%l" 'dired-downcase

     "=" 'dired-diff
     "r" 'dired-do-find-regexp
     "Q" 'dired-do-find-regexp-and-replace

     "*/" 'dired-mark-directories
     "*%" 'dired-mark-files-regexp
     "m" 'dired-mark
     "u" 'dired-unmark
     "U" 'dired-unmark-backward
     "**" 'dired-unmark-all-marks
     "t" 'dired-toggle-marks
     ))

  (add-hook 'dired-mode-hook 'my-dired-hook))


