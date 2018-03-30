;;; find-protocol-file --- switch between espf and itpp  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Troy Hinckley

;; Author: Troy Hinckley <tjhinckl@sccj015054>
;; Keywords: files

(defvar cel/itpp-espf-search-path '(""))

;;;###autoload
(defun cel/switch-itpp-espf ()
  (interactive)
  (if-let ((ext (if (equal "itpp" (file-name-extension (buffer-file-name)))
                    ".espf"
                  ".itpp"))
           (other-file (concat (file-name-base) ext))
           (dir (seq-find (lambda (x)
                            (file-exists-p
                             (expand-file-name other-file x)))
                          cel/itpp-espf-search-path)))
      (switch-to-buffer (find-file-noselect (expand-file-name other-file dir)))
    (error "unable to find matching itpp/espf file")))

(provide 'find-protocol-file)
