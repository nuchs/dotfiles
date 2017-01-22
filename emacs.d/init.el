;----------------------------------------------------------------------------
; Package management
;----------------------------------------------------------------------------
(setq package-archives '(("melpa" . "http://melpa.milkbox.net/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")))

(require 'package)
(package-initialize)

;----------------------------------------------------------------------------
; Load Packages
;----------------------------------------------------------------------------

(require 'elisp-slime-nav)
(require 'dired-x)
(require 'evil) ;; Must be last

;----------------------------------------------------------------------------
; Load Configuration
;----------------------------------------------------------------------------

(add-to-list 'load-path (concat user-emacs-directory "config"))

(require 'my-evil)
(require 'my-core)
(require 'my-slime)
(require 'my-ibuffer)
(require 'my-dired)
(require 'my-cosmetic)
