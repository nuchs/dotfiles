;----------------------------------------------------------------------------
; Bootstap package management
;----------------------------------------------------------------------------
(require 'package)
(setq package-archives '(("melpa" . "http://melpa.org/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(require 'diminish)

;----------------------------------------------------------------------------
; Load Configuration
;----------------------------------------------------------------------------

(add-to-list 'load-path (concat user-emacs-directory "config"))

(require 'my-navigation)
(require 'my-core)
(require 'my-cosmetic)
(require 'my-dev-common)
(require 'my-dev-lisp)
(require 'my-dev-rust)
(require 'my-doc)
(require 'my-org)

;----------------------------------------------------------------------------
; Custom variables
;----------------------------------------------------------------------------

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(package-selected-packages
   (quote
    (evil-search-highlight-persist highlight evil-org auto-org-md swiper-helm counsel ivy evil-nerd-commenter rainbow-delimiters evil-matchit evil-surround General linum-relative zenburn-theme evil-magit magit use-package diminish general solarized-theme slime evil elisp-slime-nav))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
