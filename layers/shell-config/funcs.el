;;; funcs.el --- Shell Layer functions File

(defun cel/helm-ff-run-switch-to-shell ()
  "Run switch to shell action from `helm-source-find-files'."
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action 'cel/helm-ff-switch-to-shell)))

(defun cel/helm-ff-switch-to-shell (_)
  (if (equal (buffer-name helm-current-buffer)
             shell-pop-last-shell-buffer-name)
      (shell-pop--cd-to-cwd helm-ff-default-directory)
  (let ((default-directory helm-ff-default-directory))
      (spacemacs/default-pop-shell))))


(defun cel/get-path-at-point ()
  (replace-regexp-in-string
   "\"" ""
   (apply 'buffer-substring-no-properties
          (--map (save-excursion
                   (funcall it "-[:alnum:]$/._~\"")
                   (point))
                 '(skip-chars-backward skip-chars-forward)))))

(defun shx-send-input-or-copy-path ()
  (interactive)
  (if (shx-point-on-input-p)
      (shx-send-input)
    (let ((path (cel/get-path-at-point)))
      (goto-char (point-max))
      (insert path))))

(defun cel/find-file-at-point ()
  "A better replacement for `find-file-at-point' that gives me
the full power of helm"
  (interactive)
  (let ((file (cel/get-path-at-point)))
    (helm-find-files-1 file (file-name-base file))))
