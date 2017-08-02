(provide 'my-core)

; Package management
(use-package paradox--backups
  :config
  (paradox-enable))

; Set backup handling
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq delete-old-versions 10)
(setq version-control t)
(setq vc-make-backup-files t)
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))

; Tabs
(setq-default indent-tabs-mode nil)
(setq tab-width 2)
(setq standard-indent 2)
(setq tab-stop-list (number-sequence 2 120 2))

; Newline at end of files
(setq require-final-newline t)

; Wait longer for garbage collection (for flx)
(setq gc-cons-threshold 20000000)

(use-package wgrep
  :ensure
  :commands (ivy-wgrep-change-to-wgrep-mode))

; Set qutebrowser as the default browser
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "qutebrowser")

                                        ; Turn off the mouse
(use-package disable-mouse
  :ensure
  :config
  (global-disable-mouse-mode))
