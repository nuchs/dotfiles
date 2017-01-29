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
  :diminish (ivy-mode . "") ; does not display ivy in the modeline
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

(use-package evil
  :ensure t
  :diminish undo-tree-mode
  :config
  (evil-mode 1)
  
  (use-package evil-surround
    :ensure t
    :config
    (global-evil-surround-mode 1))

  (use-package evil-matchit
    :ensure t
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
      :diminish which-key-mode
      :init
      (which-key-mode)
      :config
      (which-key-setup-side-window-right-bottom)
      (setq which-key-sort-order 'which-key-key-order-alpha
            which-key-side-window-max-width 0.33
            which-key-idle-delay 0.5))

    (defun switch-to-previous-buffer ()
      "Switch to previously open buffer."
      (interactive)
      (switch-to-buffer (other-buffer (current-buffer) 1)))

    (defun shell-other-window ()
      "Open a `shell' in a new window."
      (interactive)
      (let ((buf (shell)))
        (switch-to-buffer (other-buffer buf))
        (switch-to-buffer-other-window buf)))

    ; Leader keys
    (general-define-key
     :prefix my-global-leader
     :states '(normal)
     "k" 'general-describe-keybindings
     "q" 'evil-quit
     "w" 'bury-buffer
     "s" 'evil-window-split
     "d" 'evil-window-vsplit
     "SPC" 'switch-to-previous-buffer
     "td" '(shell-other-window :which-key "open shell in new split")
     "f" '(counsel-git :which-key "find file in project")
     "F" '(counsel-find-file :which-key "find file")
     "l" '(counsel-locate :which-key "locate file")
     "g" '(counsel-rg :which-key "search across files")
     "b" '(ivy-switch-buffer :which-key "open buffer")
     "B" '(ivy-switch-buffer-other-window :which-key "open buffer elsewhere")
     "/" '(swiper :which-key "search current buffer")
     "t" '(counsel-load-theme :which-key "load theme")
     "a" '(align-regexp :which-key "align on regexp")
     )

    ; Insert mode keybinds
    (general-define-key
     :states '(insert)
     "j" (general-key-dispatch 'self-insert-command
           :timeout 0.25
           "k" 'evil-normal-state))
    
    ; Standard keybinds
    (general-define-key
     :states '(normal)
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
     "M-y" '(counsel-yank-pop :which-key "search unicode characters")
     )

    ; keybinds for ivy windows
    (general-define-key
     :map ivy-minibuffer-map
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

;----------------------------------------------------------------------------
; IBuffer
;----------------------------------------------------------------------------
(use-package ibuffer
  :ensure t
  :defer t
  :init
  (evil-set-initial-state 'ibuffer-mode 'normal)
  (global-set-key (kbd "C-x b") 'ibuffer) ;; Use Ibuffer for Buffer List
  (setq ibuffer-default-sorting-mode 'major-mode)
  (setq ibuffer-expert t)
  (setq ibuffer-show-empty-filter-groups nil)

  (setq ibuffer-saved-filter-groups
        '(("home"
           ("Emacs-config" (or (filename . ".emacs.d") (filename . "emacs-config")))
           ("Org" (or (mode . org-mode) (filename . "OrgMode")))
           ("Programming" (or (mode . emacs-lisp-mode) (mode . rust-mode) (mode . haskell-mode))) 
           ("Magit" (name . "\*magit"))
           ("ERC" (mode . erc-mode))
           ("Help" (or (name . "\*Help\*") (name . "\*Apropos\*") (name . "\*info\*"))))))

  (defun my-ibuffer-hook ()
    "Set up evil mode keybinds for ibuffer mode"
    (general-define-key
     :states '(normal)
     :keymaps 'ibuffer-mode-map

     ; Movement
     "j" 'evil-next-line
     "k" 'evil-previous-line
     "J" 'ibuffer-forward-next-marked
     "K" 'ibuffer-backwards-next-marked
     
     ; Marking
     "m" 'ibuffer-mark-forward
     "u" 'ibuffer-unmark-forward
     "t" 'ibuffer-toggle-marks
     
     "* *" 'ibuffer-unmark-all
     "* M" 'ibuffer-mark-by-mode
     "* m" 'ibuffer-mark-modified-buffers
     "* u" 'ibuffer-mark-unsaved-buffers
     "* s" 'ibuffer-mark-special-buffers
     "* r" 'ibuffer-mark-read-only-buffers
     "* /" 'ibuffer-mark-dired-buffers
     "* e" 'ibuffer-mark-dissociated-buffers
     "* h" 'ibuffer-mark-help-buffers
     "* z" 'ibuffer-mark-compressed-file-buffers
     "." 'ibuffer-mark-old-buffers
     "% n" 'ibuffer-mark-by-name-regexp
     "% f" 'ibuffer-mark-by-file-name-regexp

     ; Actions on marked files
     "=" 'ibuffer-diff-with-file
     "d" 'ibuffer-mark-for-delete
     "x" 'ibuffer-do-kill-on-deletion-marks
     "g" 'ibuffer-update

     ; Sorting
     "s a" 'ibuffer-do-sort-by-alphabetic
     "s t" 'ibuffer-do-sort-by-recency
     "s s" 'ibuffer-do-sort-by-size
     "s f" 'ibuffer-do-sort-by-filename/process
     "s m" 'ibuffer-do-sort-by-major-mode

     ; Filters
     "/ m" 'ibuffer-filter-by-used-mode
     "/ f" 'ibuffer-filter-by-filename
     "/ n" 'ibuffer-filter-by-name
     "/ >" 'ibuffer-filter-by-size-gt
     "/ <" 'ibuffer-filter-by-size-lt
     "/ /" 'ibuffer-filter-disable
     
     ; marked operations
     "D" 'ibuffer-do-delete
     "F" 'ibuffer-do-shell-command-file
     "X" 'ibuffer-do-shell-command-pipe
     "I" 'ibuffer-do-query-replace-regexp
     "O" 'ibuffer-do-occur
     "S" 'ibuffer-do-save
     "V" 'ibuffer-do-revert

     ; Opening files
     "l" 'ibuffer-visit-buffer
     "f" 'ibuffer-do-view
     "C-x C-f" 'ibuffer-find-file
     "L" 'ibuffer-visit-buffer-other-window
     ))
  
  (add-hook 'ibuffer-mode-hook '(lambda () (ibuffer-switch-to-saved-filter-groups "home")))
  (add-hook 'ibuffer-mode-hook 'my-ibuffer-hook))

