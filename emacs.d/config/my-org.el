(provide 'my-org)

(use-package org
  :ensure t
  :mode (("\\.\\(org\\|org_archive\\|txt\\)\\'" . org-mode))
  :config
  (setq org-directory "~/Dropbox/org")

  ;----------------------------------------------------------------------
  ; Journal
  ;----------------------------------------------------------------------
  (use-package org-journal
    :ensure t
    :config
    (setq org-journal-dir "~/Dropbox/org/journal"))

  ;----------------------------------------------------------------------
  ; Key bindings
  ;----------------------------------------------------------------------
  (setq my-org-leader "C-c")
  (general-define-key :prefix my-org-leader
                      "a" 'org-agenda
                      "b" 'org-iswitchb
                      "c" 'org-capture
                      "l" 'org-store-link
                      "j" 'org-journal-new-entry
                      "s" 'org-journal-search
                      "C-s" 'org-schedule)

  ;----------------------------------------------------------------------
  ; TODO Options
  ;----------------------------------------------------------------------
  (setq org-enforce-todo-dependencies t)
  (setq org-todo-keywords
        (quote ((sequence "TODO(t)"
                          "HOLD(h@/!)"
                          "DELGATED(g@/!)"
                          "|"
                          "DONE(d!/@)"
                          "CANCELLED(c@/@)"))))

  (setq org-todo-keyword-faces
    (quote (
        ("TODO"      :foreground "light green" :background "dark red"        :weight bold)
        ("HOLD"      :foreground "light green" :background "dark orange"     :weight bold)
        ("DELEGATED" :foreground "light green" :background "dark orange"     :weight bold)
        ("DONE"      :foreground "light green" :background "dark green"      :weight bold)
        ("CANCELLED" :foreground "light green" :background "dark slate blue" :weight bold))))

  ;----------------------------------------------------------------------
  ; Tag Options
  ;----------------------------------------------------------------------
  (setq org-tag-alist '(("@APPOINTMENT")
                        ("@HOME")
                        ("@HEALTH")
                        ("@MONEY")
                        ("@FOOD")
                        ("@CAR")
                        ("@WORK")
                        (:startgrouptag) ("@FAMILY") (:grouptags)
                            ("@EXTENDED")
                            ("@IMMEDIATE")
                        (:endgrouptag)
                            (:startgrouptag) ("@IMMEDIATE") (:grouptags)
                                ("@ME")
                                ("@LAUREN")
                                ("@OWEN")
                                ("@RORY")
                            (:endgrouptag)
                        (:startgrouptag) ("@DEV") (:grouptags)
                            ("@RUST")
                            ("@HASKELL")
                            ("@LISP")
                            ("@CSHARP")
                            ("@EMACS")
                        (:endgrouptag)
                        (:startgrouptag) ("@COMMS") (:grouptags)
                            ("@PHONE")
                            ("@TEXT")
                            ("@MAIL")
                            ("@EMAIL")
                            ("@WHATSAPP")
                        (:endgrouptag)))

  ;----------------------------------------------------------------------
  ; Archive Options
  ;----------------------------------------------------------------------
  (setq org-archive-location "~/Dropbox/org/archive/%s_archive::")

  ;----------------------------------------------------------------------
  ; Agenda
  ;----------------------------------------------------------------------
  (setq org-agenda-files '("~/Dropbox/org/projects.org"
                           "~/Dropbox/org/autoevents.txt"
                           "~/Dropbox/org/finance.org"
                           "~/Dropbox/org/someday.org"
                           "~/Dropbox/org/todo.org"))

  ;----------------------------------------------------------------------
  ; Refile
  ;----------------------------------------------------------------------
  (setq org-refile-targets '((nil :maxlevel . 9)
                             (org-agenda-files :maxlevel . 9)
                             ("reference.org" :maxlevel . 9)))
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-use-outline-path t)        

  ;----------------------------------------------------------------------
  ; Capture Settings
  ;----------------------------------------------------------------------
  (setq org-default-notes-file (concat org-directory "/capture.org"))
  (setq org-capture-templates
    '(("t" "Todo" entry (file+headline "~/Dropbox/org/capture.org" "Capture")
       "* TODO %?\n  %i\n  %a")
      ("a" "Appointment" entry (file+headline "~/Dropbox/org/autoevents.txt" "Tasks")
       "* %^{Description} :@APPOINTMENT:\nSCHEDULED: %^t\n%?"))))
