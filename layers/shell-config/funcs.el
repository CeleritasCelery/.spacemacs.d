;;; funcs.el --- Shell Layer functions File

(defun cel/helm-ff-run-switch-to-shell ()
  "Run switch to shell action from `helm-source-find-files'."
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action 'cel/helm-ff-switch-to-shell)))

(defun cel/helm-ff-switch-to-shell (_)
  (let ((default-directory helm-ff-default-directory))
    (spacemacs/default-pop-shell)))


(defun cel/get-path-at-point ()
  (apply 'buffer-substring-no-properties
         (--map (save-excursion
                  (funcall it "-[:alnum:]$/._~")
                  (point))
                '(skip-chars-backward skip-chars-forward))))

(defun shx-send-input-or-copy-path ()
  (interactive)
  (if (shx-point-on-input-p)
      (shx-send-input)
    (let ((path (cel/get-path-at-point)))
      (goto-char (point-max))
      (insert path))))

(defun cel/find-file-at-point ()
  (interactive)
  (helm-find-files-1 (cel/get-path-at-point)))
