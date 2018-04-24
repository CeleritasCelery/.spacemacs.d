;;; org mode config

(setq org-src-tab-acts-natively t)
(setq org-priority-faces '((?A . (:foreground "OrangeRed" :weight 'bold))
                           (?B . (:foreground "yellow3" :weight 'bold))
                           (?C . (:foreground "ForestGreen" :weight 'bold))))

(setq org-todo-keywords '((sequence "TODO(t)" "BLOCK(b@!)" "DOING(g)" "|" "DONE(d)")
                          (sequence "|" "CANCELED(c@)")))

(setq org-agenda-sorting-strategy '((agenda habit-down time-up priority-down category-keep)
                                    (todo user-defined-down priority-down timestamp-down category-keep)
                                    (tags priority-down category-keep)
                                    (search category-keep)))

(add-hook 'org-insert-heading-hook 'evil-insert-state)

(setq org-todo-sort-order '("BLOCK" "TODO" "DOING" "CANCELED" "DONE"))

(defun $user-todo-sort (a b)
  "Sort todo based on which I want to see first"
  (when-let ((state-a (get-text-property 14 'todo-state a))
             (state-b (get-text-property 14 'todo-state b))
             (cmp (--map (cl-position-if (lambda (x)
                                           (equal x it))
                                         org-todo-sort-order)
                         (list state-a state-b))))
    (cond ((apply '> cmp) 1)
          ((apply '< cmp) -1)
          (t nil))))
(setq org-agenda-cmp-user-defined '$user-todo-sort)


(defun $org-truncate-line ()
  (let ((inhibit-message t))
    (toggle-truncate-lines)))
(add-hook 'org-mode-hook '$org-truncate-line)
(add-hook 'org-mode-hook 'org-indent-mode)

(setq org-agenda-todo-ignore-scheduled 'future)
(setq org-enforce-todo-dependencies t)
(setq org-agenda-dim-blocked-tasks 'invisible)
(setq org-startup-folded nil)

(setq user-full-name "Troy Hinckley")

;; org export settings
(setq org-export-with-section-numbers nil
      org-export-with-toc nil
      org-export-with-sub-superscripts '{}
      org-export-with-priority t
      org-export-preserve-breaks t
      org-insert-heading-respect-content t)

;; org-startup-folded nil
;; org-html-table-default-attributes '(:border "2" :rules "all" :frame "border")

(setq org-html-postamble nil)

(add-hook 'org-mode-hook 'smartparens-mode)

(with-eval-after-load 'smartparens-org
  (sp-with-modes 'org-mode
    (sp-local-pair "~" "~"
                   :unless '(sp-point-after-word-p)
                   :post-handlers '(("[d1]" "SPC" "/")))
    (sp-local-pair ":" ":"
                   :unless '(sp-point-after-word-p)
                   :post-handlers '(("[d1]" "SPC")))))

(with-eval-after-load 'org
  (setq org-journal-file-format "%Y-%m-%d")
  (setq org-journal-carryover-items nil)
  (setq org-directory "~/org")
  (setq org-default-notes-file (concat org-directory "/dev/notes.org"))
  (setq org-default-email-file (concat org-directory "/dev/email.org"))
  (setq org-default-journal-file (concat org-directory "/dev/journal.org"))
  (setq org-journal-dir (expand-file-name "journal" org-directory)) ;; keep all techical journals here
  (setq org-agenda-file-regexp (rx bos
                                   (or (1+ (in "-" digit)) ;; numeric journal files
                                       (and (not (any ".")) ;; regular org files (foo.org)
                                            (0+ nonl) ".org"))
                                   eos)))


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

(defun $filter-buffers (buffer-list)
  "ignore certain types of buffers in helm mini"
  (delq nil (mapcar
             (lambda (buffer)
               (cond
                ((eq (with-current-buffer buffer major-mode)  'org-agenda-mode) nil)
                ((eq (with-current-buffer buffer major-mode)  'org-journal-mode) nil)
                (t buffer)))
             buffer-list)))
(advice-add 'helm-skip-boring-buffers :filter-return '$filter-buffers)

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
         "* TODO [#B] %?\n %t\n" :empty-lines 1)
        ("l" "Todo Link" entry (file+headline org-default-notes-file "Tasks")
         "* TODO [#B] %?\n %i\n %a" :empty-lines 1)
        ("s" "Scheduled TODO" entry (file+headline org-default-notes-file "Tasks")
         "* TODO [#B] %?\n  SCHEDULED: %^T\n" :empty-lines 1)
        ("T" "Todo from Clipboard" entry (file+headline org-default-notes-file "Tasks")
         "* TODO [#B] %?\n  %c" :empty-lines 1)
        ("n" "Note" entry (file+headline org-default-notes-file "Notes")
         "* %?" :empty-lines 1)
        ("N" "Note with Clipboard" entry (file+headline org-default-notes-file "Notes")
         "* %?\n   %c" :empty-lines 1)
        ("e" "Email" plain (file org-default-email-file)
         "%?" :empty-lines 1)
        ("j" "Journal" entry (file org-default-journal-file)
         "* %<%a %b %e, %l:%M> -  %?" :empty-lines 1)))
(spacemacs/set-leader-keys "oo" 'org-capture)


(setq org-refile-targets '((nil :maxlevel . 4)
                           (org-agenda-files :maxlevel . 3)
                           ("~/org/dev/.development.org" :maxlevel . 1)))

(setq org-outline-path-complete-in-steps nil)   ;; Refile in a single go
(setq org-refile-use-outline-path t)            ;; Show full paths for refiling

(defun $org-create-css-html-email-head ()
  "Create the header with CSS for use with email"
  (interactive)
  (setq org-html-head
        (concat
         "<style type=\"text/css\">\n"
         "<!--/*--><![CDATA[/*><!--*/\n"
         (with-temp-buffer
           (insert-file-contents
            "~/org/org-html-themes/styles/email/css/email.css")
           (buffer-string))
         "/*]]>*/-->\n"
         "</style>\n"))
  t)
($org-create-css-html-email-head)

(defun $export-org-email ()
  "Export the current org email and copy it to the clipboard"
  (interactive)
  (let ((org-export-show-temporary-export-buffer nil))
    (org-html-export-as-html)
    (with-current-buffer "*Org HTML Export*"
      (kill-new (buffer-string)))
    (message "HTML copied to clipboard")))
(with-eval-after-load 'org-capture
  (spacemacs/set-leader-keys-for-minor-mode 'org-capture-mode
    "ee" #'$export-org-email
    "ed" 'org-export-dispatch))

(with-eval-after-load 'org-src
  (define-key org-src-mode-map (kbd "C-c C-c") 'org-edit-src-exit)
  (define-key org-src-mode-map (kbd "C-c C-k") 'org-edit-src-abort)
  (spacemacs/set-leader-keys-for-minor-mode 'org-src-mode
    dotspacemacs-major-mode-leader-key nil))

(provide 'org-config)
