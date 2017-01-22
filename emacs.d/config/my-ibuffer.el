(global-set-key (kbd "C-x C-b") 'ibuffer) ;; Use Ibuffer for Buffer List

(setq ibuffer-default-sorting-mode 'major-mode)
(setq ibuffer-expert t)
(setq ibuffer-show-empty-filter-groups nil)
(setq ibuffer-saved-filter-groups
      '(("home"
	 ("Emacs-config" (or (filename . ".emacs.d")
			     (filename . "emacs-config")))
	 ("Org" (or (mode . org-mode)
		    (filename . "OrgMode")))
	 ("Programming"
	  (or
	   (mode . emacs-lisp-mode)
	   (mode . rust-mode)
	   (mode . haskell-mode)
	   (mode . python-mode)
	   )) 
	 ("Magit" (name . "\*magit"))
	 ("ERC" (mode . erc-mode))
	 ("Help" (or (name . "\*Help\*")
		     (name . "\*Apropos\*")
		     (name . "\*info\*"))))))

(add-hook 'ibuffer-mode-hook
	  '(lambda ()
	     (ibuffer-switch-to-saved-filter-groups "home")))

;; EVIL override ibuffer keybinds
(eval-after-load 'ibuffer
  '(progn
     (evil-set-initial-state 'ibuffer-mode 'normal)
     (evil-define-key 'normal ibuffer-mode-map
	    (kbd "=") 'ibuffer-diff-with-file

            ;; Marking
	    (kbd "m") 'ibuffer-mark-forward
	    (kbd "u") 'ibuffer-unmark-forward
	    (kbd "t") 'ibuffer-toggle-marks
	    
	    (kbd "* *") 'ibuffer-unmark-all
	    (kbd "* M") 'ibuffer-mark-by-mode
	    (kbd "* m") 'ibuffer-mark-modified-buffers
	    (kbd "* u") 'ibuffer-mark-unsaved-buffers
	    (kbd "* s") 'ibuffer-mark-special-buffers
	    (kbd "* r") 'ibuffer-mark-read-only-buffers
	    (kbd "* /") 'ibuffer-mark-dired-buffers
	    (kbd "* e") 'ibuffer-mark-dissociated-buffers
	    (kbd "* h") 'ibuffer-mark-help-buffers
	    (kbd "* z") 'ibuffer-mark-compressed-file-buffers
	    (kbd ".") 'ibuffer-mark-old-buffers
	    (kbd "% n") 'ibuffer-mark-by-name-regexp
	    (kbd "% m") 'ibuffer-mark-by-mode-regexp
	    (kbd "% f") 'ibuffer-mark-by-file-name-regexp

	    (kbd "d") 'ibuffer-mark-for-delete
	    (kbd "C-d") 'ibuffer-mark-for-delete-backwards
	    (kbd "x") 'ibuffer-do-kill-on-deletion-marks

	    ;; Movement
	    (kbd "j") 'evil-next-line
	    (kbd "k") 'evil-previous-line
	    (kbd "J") 'ibuffer-forward-next-marked
	    (kbd "K") 'ibuffer-backwards-next-marked
	    (kbd "g") 'ibuffer-update
	    
	    ;; Sorting
	    (kbd "s i") 'ibuffer-invert-sorting
	    (kbd "s a") 'ibuffer-do-sort-by-alphabetic
	    (kbd "s t") 'ibuffer-do-sort-by-recency
	    (kbd "s s") 'ibuffer-do-sort-by-size
	    (kbd "s f") 'ibuffer-do-sort-by-filename/process
	    (kbd "s m") 'ibuffer-do-sort-by-major-mode

	    ;; Filters
	    (kbd "/ m") 'ibuffer-filter-by-used-mode
	    (kbd "/ M") 'ibuffer-filter-by-derived-mode
	    (kbd "/ n") 'ibuffer-filter-by-name
	    (kbd "/ c") 'ibuffer-filter-by-content
	    (kbd "/ e") 'ibuffer-filter-by-predicate
	    (kbd "/ f") 'ibuffer-filter-by-filename
	    (kbd "/ >") 'ibuffer-filter-by-size-gt
	    (kbd "/ <") 'ibuffer-filter-by-size-lt
	    (kbd "/ /") 'ibuffer-filter-disable
	    (kbd "/ \\") 'ibuffer-clear-filter-groups
	    
	    ;; marked operations
	    (kbd "D") 'ibuffer-do-delete
	    (kbd "E") 'ibuffer-do-eval
	    (kbd "F") 'ibuffer-do-shell-command-file
	    (kbd "I") 'ibuffer-do-query-replace-regexp
	    (kbd "H") 'ibuffer-do-view-other-frame
	    (kbd "N") 'ibuffer-do-shell-command-pipe-replace
	    (kbd "O") 'ibuffer-do-occur
	    (kbd "S") 'ibuffer-do-save
	    (kbd "U") 'ibuffer-do-replace-regexp
	    (kbd "V") 'ibuffer-do-revert
	    (kbd "W") 'ibuffer-do-view-and-eval
	    (kbd "X") 'ibuffer-do-shell-command-pipe

	    (kbd "l") 'ibuffer-visit-buffer
	    (kbd "v") 'ibuffer-do-view
	    (kbd "C-x C-f") 'ibuffer-find-file
	    (kbd "o") 'ibuffer-visit-buffer-other-window
	    (kbd "M-o") 'ibuffer-visit-buffer-1-window
	    (kbd "C-x v") 'ibuffer-do-view-horizontally
	  )
  )
)

(provide 'my-ibuffer)
