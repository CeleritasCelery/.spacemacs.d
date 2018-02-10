;;; org mode config -*- lexical-binding: t -*-

(setq org-src-tab-acts-natively t)
(setq org-priority-faces '((?A . (:foreground "OrangeRed" :weight 'bold))
                           (?B . (:foreground "yellow3" :weight 'bold))
                           (?C . (:foreground "ForestGreen" :weight 'bold))))

(setq org-todo-keywords '((sequence "TODO(t)" "DOING(g)" "|" "DONE(d)")
                          (sequence "|" "CANCELED(c)")))

(add-hook 'org-mode-hook (lambda ()
                           (spacemacs/toggle-truncate-lines-off)
                           (org-indent-mode 1)))

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

(when (configuration-layer/package-usedp 'smartparens)
  (add-hook 'org-mode-hook 'smartparens-mode)
  (sp-local-pair 'org-mode "~" "~") ;; org code
  (sp-local-pair 'org-mode "=" "=") ;; org literal
  (sp-local-pair 'org-mode "*" "*"));; org bold

(setq org-journal-carryover-items nil)
(setq org-directory "~/org")
(setq org-journal-dir (f-join org-directory "journal")) ;; keep all techical journals here
(setq org-agenda-file-regexp (rx bos
                                 (or (1+ (in "0-9")) ;; numeric journal files
                                     (and (not (in ".")) ;; regular org files (foo.org)
                                          (0+ nonl) ".org"))
                                 eos))

(setq org-agenda-files (cons org-directory (f-directories org-directory))) ;; where to search for TODO's

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

(provide 'org-config)
