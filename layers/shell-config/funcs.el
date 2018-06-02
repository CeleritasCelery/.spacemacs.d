;;; funcs.el --- Shell Layer functions File

(defun $helm-ff-run-switch-to-shell ()
  "Run switch to shell action from `helm-source-find-files'."
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action '$helm-ff-switch-to-shell)))

(defun $helm-ff-switch-to-shell (_)
  (if (and (boundp 'shell-pop-last-shell-buffer-name)
           (equal (buffer-name helm-current-buffer)
                  shell-pop-last-shell-buffer-name))
      (shell-pop--cd-to-cwd helm-ff-default-directory)
    (let ((default-directory helm-ff-default-directory))
      (spacemacs/default-pop-shell))))


(defun $get-path-at-point ()
  (replace-regexp-in-string
   "/+" "/"
   (replace-regexp-in-string
    "\"" ""
    (apply 'buffer-substring-no-properties
           (--map (save-excursion
                    (funcall it "-[:alnum:]$/._~\"")
                    (point))
                  '(skip-chars-backward skip-chars-forward))))))

(defun shx-send-input-or-copy-path ()
  (interactive)
  (if (shx-point-on-input-p)
      (shx-send-input)
    (let ((path ($get-path-at-point)))
      (goto-char (point-max))
      (insert path))))

(defun $find-file-at-point ()
  "A better replacement for `find-file-at-point' that gives me
the full power of helm"
  (interactive)
  (let ((file ($get-path-at-point)))
    (helm-find-files-1 (concat (or (file-remote-p default-directory) "")
                               file)
                       (file-name-base file))))
