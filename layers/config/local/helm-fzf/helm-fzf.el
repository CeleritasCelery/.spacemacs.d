;;; helm-fzf.el --- helm binding for FZF

;; Copyright (C) 2011 Free Software Foundation, Inc.

;; Author: Ivan Buda Mandura (ivan.mandura93@gmail.com)

;; Version: 0.1
;; Package-Requires: ((emacs "24.4"))
;; Keywords: helm fzf

;;; Commentary:

;;; Code:

(require 'helm)
(require 'helm-files)

(defcustom helm-fzf-executable "fzf"
  "Default executable for fzf"
  :type 'stringp
  :group 'helm-fzf)

(defun helm-fzf--project-root ()
  (cl-loop for dir in '(".git/" ".hg/" ".svn/" ".git")
           when (locate-dominating-file default-directory dir)
           return it))

(setq helm-fzf-source
      (helm-build-async-source "fzf"
        :candidates-process 'helm-fzf--do-candidate-process
        :requires-pattern 3
        :action 'helm-type-file-actions
        :persistent-action 'helm-fzf-kill-or-find-buffer-fname
        :action-transformer 'helm-transform-file-load-el
        :keymap helm-generic-files-map
        :candidate-number-limit 9999))

(defun helm-fzf-kill-or-find-buffer-fname (candidate)
  (let ((default-directory (helm-default-directory)))
    (helm-ff-kill-or-find-buffer-fname candidate)))

(defun helm-fzf--do-candidate-process ()
  (let* ((cmd-args (list helm-fzf-executable
                         "--no-sort"
                         "--exact"
                         "-f"
                         helm-pattern))
         (proc (apply 'start-file-process "helm-fzf" helm-buffer cmd-args)))
    (prog1 proc
      (set-process-sentinel
       (get-buffer-process helm-buffer)
       (lambda (process event)
         (helm-process-deferred-sentinel-hook
          process event (helm-default-directory)))))))

;;;###autoload
(defun helm-fzf (directory)
  (interactive "D")
  (let ((default-directory directory))
    (helm :sources '(helm-fzf-source)
          :buffer "*helm-fzf*")))

(defun helm-fzf-project-root ()
  (interactive)
  (if-let ((dir (helm-fzf--project-root)))
      (helm-fzf dir)
    (error "Could not find the project root.")))

;;;###autoload
(defun helm-fzf-from-session ()
  "Lanuch helm-fzf from within a `helm-find-files' session"
  (interactive)
  (with-helm-alive-p
    (helm-run-after-exit
     'helm-fzf
     helm-ff-default-directory)))

(provide 'helm-fzf)

;;; helm-fzf.el ends here
