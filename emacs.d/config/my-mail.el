(provide 'my-mail)

(use-package notmuch
    :config

    (setq mail-user-agent 'message-user-agent)
    (setq user-mail-address "quadturtle@gmail.com"
          user-full-name "Simon Brown")
    (setq smtpmail-stream-type 'ssl
          smtpmail-smtp-server "smtp.gmail.com"
          smtpmail-smtp-service 465)
    (setq message-auto-save-directory "~/mail/drafts")
    (setq message-kill-buffer-on-exit t)

    (defun my-toggle-show-tag (tag)
    "Toggle tag for show mode"
    (if (member tag (notmuch-show-get-tags))
	(notmuch-show-tag (list "+inbox" (concat "-" tag)))
	(notmuch-show-tag (list "-inbox" "-unread" "-draft" (concat "+" tag)))))

    (defun my-toggle-search-tag (tag beg end)
    "Toggle tag on range for search mode"
    (if (member tag (notmuch-search-get-tags))
	(notmuch-search-tag (list "+inbox" (concat "-" tag)) beg end)
	(notmuch-search-tag (list "-inbox" "-unread" "-draft" (concat "+" tag)) beg end)))

    (defun my-toggle-search-archive (&optional beg end)
      "Toggle archive tag"
      (interactive (notmuch-search-interactive-region))
      (my-toggle-search-tag "archive" beg end))

    (defun my-toggle-search-spam (&optional beg end)
      "Toggle spam tag"
      (interactive (notmuch-search-interactive-region))
      (my-toggle-search-tag "spam" beg end))

    (defun my-toggle-search-deleted (&optional beg end)
      "Toggle deleted tag"
      (interactive(notmuch-search-interactive-region))
      (my-toggle-search-tag "deleted" beg end))

    (defun my-toggle-show-archive ()
      "Toggle archive tag"
      (interactive)
      (my-toggle-show-tag "archive"))

    (defun my-toggle-show-spam ()
      "Toggle spam tag"
      (interactive)
      (my-toggle-show-tag "spam"))

    (defun my-toggle-show-deleted () "Toggle deleted tag" (interactive)
      (my-toggle-show-tag "deleted"))

    (general-define-key
     :prefix my-global-leader
     :states '(normal motion)
     "m" '(notmuch :which-key "Start mail mode")
     )

    (general-define-key
     :states '(normal motion visual)
     :keymaps 'notmuch-search-mode-map
     "d"  '(my-toggle-search-deleted :which-key "mark mail as deleted")
     "s"  '(my-toggle-search-spam :which-key "mark mail as spam")
     "a"  '(my-toggle-search-archive :which-key "mark mail as archived")
     "j"  '(notmuch-search-next-thread)
     "k"  '(notmuch-search-previous-thread)
     "q"  '(notmuch-bury-or-kill-this-buffer)
     "'"  '(notmuch-jump-search)
     "+"  '(notmuch-search-add-tag)
     "-"  '(notmuch-search-remove-tag)
     "="  '(notmuch-refresh-this-buffer)
     "RET"  '(notmuch-search-show-thread)
     )
    
    (general-define-key
     :states '(normal motion)
     :keymaps 'notmuch-show-mode-map
     "d"     '(my-toggle-show-deleted :which-key "mark mail as deleted")
     "s"     '(my-toggle-show-spam :which-key "mark mail as spam")
     "a"     '(my-toggle-show-archive :which-key "mark mail as archived")
     "n"     '(notmuch-show-next-thread)
     "p"     '(notmuch-show-previous-thread)
     "["     '(notmuch-show-next-open-message)
     "]"     '(notmuch-show-previous-open-message)
     "q"     '(notmuch-bury-or-kill-this-buffer)
     "+"     '(notmuch-show-add-tag)
     "-"     '(notmuch-show-remove-tag)
     "'"     '(notmuch-jump-search)
     "e"     '(notmuch-show-resume-message)
     )

    (general-define-key
     :states '(normal motion)
     :keymaps 'notmuch-hello-mode-map
     "'"     '(notmuch-jump-search)
     "TAB"   '(widget-forward)
     "S-TAB" '(widget-backward)
     "RET"   '(widget-button-press)
     )

    (general-define-key
     :states '(normal motion insert visual)
     :keymaps 'notmuch-message-mode-map
     "C-c C-c" '(notmuch-mua-send-and-exit)
     "C-c C-s" '(notmuch-draft-save)
     "C-c C-a" '(mml-attach-file)
     "C-c C-k" '(message-kill-buffer)
     )

    (setq notmuch-search-oldest-first nil)

    (setq notmuch-saved-searches '(

        (:name "unread"  :key "u" :sort-order newest-first
         :query "tag:unread")

        (:name "inbox"  :key "i" :sort-order newest-first
         :query "tag:inbox not tag:archive and not tag:deleted")
        (:name "dev"  :key "d" :sort-order newest-first
         :query "tag:dev and not tag:archive and not tag:deleted")
        (:name "go"  :key "g" :sort-order newest-first
         :query "tag:go and not tag:archive and not tag:deleted")
        (:name "accounts"  :key "a" :sort-order newest-first
         :query "tag:accounts and not tag:archive and not tag:deleted")
        (:name "money"  :key "m" :sort-order newest-first
         :query "tag:money and not tag:archive and not tag:deleted")
        (:name "purchases"  :key "p" :sort-order newest-first
         :query "tag:purchases and not tag:archive and not tag:deleted")
        (:name "bills"  :key "b" :sort-order newest-first
         :query "tag:bills and not tag:archive and not tag:deleted")

        (:name "sent"  :key "s" :sort-order newest-first
         :query "tag:sent and not tag:archive and not tag:deleted")
        (:name "drafts"  :key "t" :sort-order newest-first
         :query "tag:draft and not tag:deleted")
        (:name "archived"  :key "v" :sort-order newest-first
         :query "tag:archive and not tag:deleted")
        (:name "junk"  :key "j" :sort-order newest-first
         :query "tag:spam and not tag:deleted")
        (:name "deleted"  :key "x" :sort-order newest-first
         :query "tag:deleted")
        ))
    )
