(eval-after-load 'dired
  '(progn
     (evil-set-initial-state 'dired-mode 'normal)
     (evil-define-key 'normal dired-mode-map
       "h" 'my-dired-up-directory
       "l" 'dired-find-alternate-file
       "n" 'evil-search-next
       "N" 'evil-search-previous
       "q" 'kill-this-buffer
       "o" 'dired-sort-toggle-or-edit
       "c" 'dired-create-directory
       "C" 'dired-do-copy
       "D" 'dired-do-delete
       "R" 'dired-do-rename
       "%u" 'dired-upcase
       "%l" 'dired-downcase
       "=" 'dired-diff
       "A" 'dired-do-find-regexp
       "Q" 'dired-do-find-regexp-and-replace
       "J" 'dired-prev-marked-file
       "K" 'dired-next-marked-file
       "%d" 'dired-flag-files-regexp
       "%g" 'dired-mark-files-containing-regexp
       "*e" 'dired-mark-executables
       "*/" 'dired-mark-directories
       "*@" 'dired-mark-symlinks
       "*%" 'dired-mark-files-regexp
       "*s" 'dired-mark-subdir-files
       "m" 'dired-mark
       "u" 'dired-unmark
       "U" 'dired-unmark-backward
       "**" 'dired-unmark-all-marks
       "t" 'dired-toggle-marks
       "j" 'dired-goto-file
     )
   )
)

(put 'dired-find-alternate-file 'disabled nil)

(defun my-dired-up-directory ()
  "Take dired up one directory, but behave like dired-find-alternate-file"
  (interactive)
  (let ((old (current-buffer)))
    (dired-up-directory)
    (kill-buffer old)
    ))

(provide 'my-dired)
