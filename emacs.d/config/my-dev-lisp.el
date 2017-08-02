(provide 'my-dev-lisp)

;------------------------------------------------------------------------------
; ELisp
;------------------------------------------------------------------------------

(use-package elisp-slime-nav
  :ensure t
  :mode (("\\.el\\'" . emacs-lisp-mode))
  :init
  
  (defun my-lisp-hook ()
    (elisp-slime-nav-mode)
    (turn-on-eldoc-mode)
    (company-mode)
    (evil-define-key 'normal emacs-lisp-mode-map
      (kbd "K") 'elisp-slime-nav-describe-elisp-thing-at-point))

  (add-hook 'emacs-lisp-mode-hook 'my-lisp-hook))

(use-package slime
  :ensure t
  :mode (("\\.el\\'" . emacs-lisp-mode))
  :init
  (setq inferior-lisp-program "/usr/bin/sbcl")
  (setq slime-contribs '(slime-fancy))
  )

;------------------------------------------------------------------------------
; Scheme
;------------------------------------------------------------------------------
(setq scheme-program-name
      (concat
       "/usr/bin/mit-scheme "
       "--library " "/usr/lib/mit-scheme-x86-64 "
       "--band " "/usr/lib/mit-scheme-x86-64/all.com "
       "-heap 10000"))

; Use the Edwin-like MIT/Scheme interpreter:
(load "xscheme")

; generic scheme completion
(require 'scheme-complete)
(autoload 'scheme-smart-complete "scheme-complete" nil t)
(autoload 'scheme-get-current-symbol-info "scheme-complete" nil t)
(setq lisp-indent-function 'scheme-smart-indent-function)

;; mit-scheme documentation
(require 'mit-scheme-doc)

(general-define-key
 :states '(normal insert)
 :keymaps '(scheme-mode-map
            inferior-scheme-mode-map
            scheme-interaction-mode-map)
 "C-SPC" '(scheme-complete-or-indent :which-key "invoke autocomplete")
 "K" '(mit-scheme-doc-lookup "Scheme documentation lookup"))

(defun my-scheme-hook ()
  (require 'flash-paren)
  (flash-paren-mode 1)
  (setq flash-paren-delay 0.4))

(setq x-select-enable-clipboard 't)
(setq auto-mode-alist (cons '("README" . text-mode) auto-mode-alist))
;; activate auto-fill-mode for various other modes
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'scheme-mode-hook 'turn-on-auto-fill)
(add-hook 'scheme-mode-hook 'my-scheme-hook)
(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Prevent annoying \"Active processes exist\" query when you quit Emacs."
  (cl-flet ((process-list ())) ad-do-it))
(setq-default ispell-program-name "aspell")

(defun xscheme-prompt-for-expression-exit ()
  (interactive)
  (let (
	;; In Emacs 21+, during a minibuffer read the minibuffer
	;; contains the prompt as buffer text and that text is
	;; read only.  So we can no longer assume that (point-min)
	;; is where the user-entered text starts and we must avoid
	;; modifying that prompt text.  The value we want instead
	;; of (point-min) is (minibuffer-prompt-end).
	(point-min (if (fboundp 'minibuffer-prompt-end)
		              (minibuffer-prompt-end)
		            (point-min))))
    (if (eq (xscheme-region-expression-p point-min (point-max)) 'one)
        (exit-minibuffer)
      (error "input must be a single, complete expression"))))
