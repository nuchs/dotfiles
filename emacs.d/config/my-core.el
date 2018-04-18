(provide 'my-core)

; Package management
(use-package paradox
  :defer t
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
  :defer t
  :commands (ivy-wgrep-change-to-wgrep-mode))

; Set firefox as the default browser
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "firefox-nightly")

(defun my-disconnect-hook ()
  (interactive)
  (message "Running disconnect tasks")
  (recentf-save-list))

(run-at-time (current-time) 300 'recentf-save-list)

(add-hook 'server-done-hook 'my-disconnect-hook)
