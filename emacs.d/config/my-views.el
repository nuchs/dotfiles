(provide 'my-views)

(setq my-ivy-views-file "~/etc/emacs.d/ivy-views")

(defun my-load-ivy-views ()
  (interactive)
  (setq ivy-views
        (with-temp-buffer
         (insert-file-contents my-ivy-views-file)
         (read (current-buffer))))
  (message "load ivy-views"))

(defun my-save-current-ivy-views()
  (interactive)
  (with-temp-file my-ivy-views-file
    (prin1 ivy-views (current-buffer)))
  (message "wrote ivy-views"))

(defun my-save-ivy-view ()
  (interactive)
  (ivy-push-view)
  (my-save-current-ivy-views))

(defun my-delete-ivy-view ()
  (interactive)
  (ivy-pop-view)
  (my-save-current-ivy-views))

(my-load-ivy-views)
