(provide 'my-views)

(defun my-create-views-file ()
  (interactive)
  (with-temp-file "~/.emacs.d/ivy-views"
    (prin1 ivy-views (current-buffer))))

(defun my-save-ivy-view ()
  (interactive)
  (with-temp-file "~/.emacs.d/ivy-views"
    (ivy-push-view)
    (prin1 ivy-views (current-buffer))))

(defun my-delete-ivy-view ()
  (interactive)
  (with-temp-file "~/.emacs.d/ivy-views"
    (ivy-pop-view)
    (prin1 ivy-views (current-buffer))))

(defun my-load-ivy-views ()
  (interactive)
  (setq ivy-views
        (with-temp-buffer
         (insert-file-contents "~/.emacs.d/ivy-views")
         (read (current-buffer))))
  (message "load ivy-views"))

(my-load-ivy-views)
