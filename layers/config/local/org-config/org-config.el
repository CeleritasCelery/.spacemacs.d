;;; org mode config

(setq org-src-tab-acts-natively t)
(setq org-priority-faces '((?A . (:foreground "OrangeRed" :weight 'bold))
                           (?B . (:foreground "yellow3" :weight 'bold))
                           (?C . (:foreground "ForestGreen" :weight 'bold))))

(setq org-todo-keywords '((sequence "TODO(t)" "DOING(g)" "WAIT(w@)" "|" "DONE(d)")
                          (sequence "|" "CANCELED(c@)")))

(defun cel/org-truncate-line ()
  (let ((inhibit-message t))
    (toggle-truncate-lines)))
(add-hook 'org-mode-hook 'cel/org-truncate-line)
(add-hook 'org-mode-hook 'org-indent-mode)

(defun cel/evil-org-todo-enter-insert-state (&rest args)
  (and (featurep 'evil-states)
       (evil-insert-state)))
(advice-add 'org-insert-todo-heading-respect-content :after #'cel/evil-org-todo-enter-insert-state)

(setq org-agenda-todo-ignore-scheduled 'future)
(setq org-enforce-todo-dependencies t)
(setq org-agenda-dim-blocked-tasks 'invisible)
(setq org-startup-folded nil)

(setq user-full-name "Troy Hinckley")

;;; commenting this out so I can add these settings to the export file
;;; I will keep them here till I am sure I don't need them
;; (setq org-export-with-section-numbers 2
;;       org-export-with-toc nil
;;       org-export-with-sub-superscripts "{}"
;;       org-export-with-priority t
;;       org-export-preserve-breaks t
;;       org-startup-folded nil
;;       org-insert-heading-respect-content t
;;       org-html-table-default-attributes '(:border "2" :rules "all" :frame "border"))


(add-hook 'org-mode-hook 'smartparens-mode)

(with-eval-after-load 'smartparens-org
  (sp-with-modes 'org-mode
    (sp-local-pair "~" "~"
                   :unless '(sp-point-after-word-p)
                   :post-handlers '(("[d1]" "SPC" "/")))
    (sp-local-pair ":" ":"
                   :unless '(sp-point-after-word-p)
                   :post-handlers '(("[d1]" "SPC")))))

(setq org-journal-file-format "%Y-%m-%d")
(setq org-journal-carryover-items nil)
(setq org-directory "~/org")
(setq org-default-notes-file (concat org-directory "/dev/notes.org"))
(setq org-journal-dir (expand-file-name "journal" org-directory)) ;; keep all techical journals here
(setq org-agenda-file-regexp (rx bos
                                 (or (1+ (in "-" digit)) ;; numeric journal files
                                     (and (not (any ".")) ;; regular org files (foo.org)
                                          (0+ nonl) ".org"))
                                 eos))

(defun org-archive-done-tasks ()
  (interactive)
  (org-map-entries
   (lambda ()
     (org-archive-subtree)
     (setq org-map-continue-from (outline-previous-heading)))
   "/DONE" 'file)
  (org-map-entries
   (lambda ()
     (org-archive-subtree)
     (setq org-map-continue-from (outline-previous-heading)))
   "/CANCELED" 'file))
(spacemacs/set-leader-keys-for-major-mode 'org-mode "oa" 'org-archive-done-tasks)

(add-to-list 'auto-mode-alist
             (cons (rx "/journal/20"
                       (= 2 (any digit)) ;; year
                       "-" (= 2 (any digit)) ;; month
                       "-" (= 2 (any digit)) eos) ;; day
                   'org-journal-mode))

(defun cel/filter-buffers (buffer-list)
  "ignore certain types of buffers in helm mini"
  (delq nil (mapcar
             (lambda (buffer)
               (cond
                ((eq (with-current-buffer buffer major-mode)  'org-agenda-mode) nil)
                ((eq (with-current-buffer buffer major-mode)  'org-journal-mode) nil)
                (t buffer)))
             buffer-list)))
(advice-add 'helm-skip-boring-buffers :filter-return 'cel/filter-buffers)

(setq org-agenda-files (--remove (or (string-match-p "journal" it)
                                     (string-match-p "org-html-themes" it))
                                 (f-directories org-directory nil t))) ;; where to search for TODO's

(spacemacs|define-transient-state org-journal
  :title "navigate org journals"
  :doc "
 [_p_/_N_] go to previous journal entry, [_n_] goto next journal entry"
  :bindings
  ("p" org-journal-open-previous-entry)
  ("N" org-journal-open-previous-entry)
  ("n" org-journal-open-next-entry)
  ("q" nil :exit t))
(spacemacs/set-leader-keys-for-major-mode 'org-journal-mode "jn" 'spacemacs/org-journal-transient-state/org-journal-open-next-entry)
(spacemacs/set-leader-keys-for-major-mode 'org-journal-mode "jp" 'spacemacs/org-journal-transient-state/org-journal-open-previous-entry)

(setq org-capture-templates
      '(("t" "Todo" entry (file+headline org-default-notes-file "Tasks")
         "* TODO %?\n %t\n" :empty-lines 1)
        ("l" "Todo Link" entry (file+headline org-default-notes-file "Tasks")
         "* TODO %?\n %i\n %a" :empty-lines 1)
        ("s" "Scheduled TODO" entry (file+headline org-default-notes-file "Tasks")
         "* TODO %?\n  SCHEDULED: %^T\n" :empty-lines 1)
        ("T" "Todo from Clipboard" entry (file+headline org-default-notes-file "Tasks")
         "* TODO %?\n  %c" :empty-lines 1)
        ("n" "Note" entry (file+headline org-default-notes-file "Notes")
         "* %?" :empty-lines 1)
        ("N" "Note with Clipboard" entry (file+headline org-default-notes-file "Notes")
         "* %?\n   %c" :empty-lines 1)
        ("e" "Email" plain (file+headline org-default-notes-file "email")
         "%?" :empty-lines 1)))
(spacemacs/set-leader-keys "oo" 'org-capture)

(provide 'org-config)
