(provide 'my-mail)

(use-package notmuch
    :config
    (general-define-key
     :prefix my-global-leader
     :states '(normal motion)
     "m" '(notmuch :which-key "load theme"))
    (setq notmuch-search-oldest-first nil)
    (setq notmuch-saved-searches '(
        (:name "inbox" :query "tag:inbox" :key "i" :sort-order newest-first)
        (:name "unread" :query "tag:unread" :key "u")
        (:name "sent" :query "tag:sent" :key "s")
        (:name "drafts" :query "tag:draft" :key "t")
        (:name "purchases" :query "tag:purchases" :key "p")
        (:name "bills" :query "tag:bills" :key "b")
        (:name "dev" :query "tag:dev" :key "d")
        (:name "personal" :query "tag:friendsandfamily" :key "f")
        (:name "go" :query "tag:go" :key "g")
        (:name "all mail" :query "*" :key "a")
        ))
    )

