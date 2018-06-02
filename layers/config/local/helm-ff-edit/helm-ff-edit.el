;;; helm-ff-edit.el --- occur for helm  -*- lexical-binding: t; -*-

(defvar helm-occur-cmd "ls -a | ag --nocolor '%s' | tr '\\n' '\\0' | xargs -0 ls -d --group-directories-first")
(defvar helm-dired-listing-switches "-alh")

(defun helm-ff-edit--get-file-re ()
  (let* ((submatchs (thread-last helm-pattern
                      (s-chop-prefix helm-ff-default-directory)
                      string-trim
                      split-string))
         (pattern (mapconcat (lambda (x) (format "(?=.*%s.*)" x))
                             submatchs "")))
    (if (string-blank-p pattern)
        ".*"
      pattern)))

(defun helm-ff-edit--cmd-to-dired (full-cmd candidates)
  "Adapted from `find-dired'."
  (require 'find-dired)
  (let ((buffer (get-buffer-create "*helm dired edit*"))
        (inhibit-read-only t))
    (select-window (get-buffer-window helm-current-buffer))
    (set-window-buffer (select-window (split-window)) buffer)
    (set-buffer buffer)
    (erase-buffer)
    (dired-mode default-directory helm-dired-listing-switches)
    (cd helm-ff-default-directory)
    (insert "  " default-directory ":\n")
    (let ((point (point)))
      (insert "  " full-cmd "\n")
      (dired-insert-set-properties point (point)))
    (setq-local dired-sort-inhibit t)
    (setq-local revert-buffer-function
                (lambda (_1 _2) (helm-ff-edit--cmd-to-dired full-cmd)))
    (setq-local dired-subdir-alist
                (list (cons default-directory (point-min-marker))))
    (set-process-sentinel
     (start-process-shell-command
      "helm-cmd" buffer full-cmd)
     (lambda (_ state)
       (when (equal state "finished\n")
         (goto-char (point-min))
         (forward-line 2)
         (dired-goto-file (car candidates))
         (wdired-change-to-wdired-mode))))))

;;;###autoload
(defun helm-find-files-edit ()
  (interactive)
  (with-helm-alive-p
    (helm-run-after-exit
     'helm-ff-edit--cmd-to-dired
     (concat (format helm-occur-cmd (helm-ff-edit--get-file-re))
             " " helm-dired-listing-switches
             " | sed -e 's/^/  /'")
     (helm-marked-candidates))))

(provide 'helm-ff-edit)
