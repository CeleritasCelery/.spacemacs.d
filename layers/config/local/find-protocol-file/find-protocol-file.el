;;; find-protocol-file --- switch between espf and itpp  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Troy Hinckley

;; Author: Troy Hinckley <tjhinckl@sccj015054>
;; Keywords: files

(defvar find-protocol-file-search-path '("" "../../espf_tests/*" "../../tests/*")
  "The search path to find a matching ITPP or ESPF. Supports wild card expansion.")

;;;###autoload
(defun find-protocol-file ()
  "Switch between the current ITPP and ESPF files."
  (interactive)
  (if-let ((ext (if (equal "itpp" (file-name-extension (buffer-file-name)))
                    ".*spf"
                  ".itpp"))
           (other-file (concat (file-name-base) ext))
           (path (cl-some (lambda (x)
                            (car (file-expand-wildcards
                                  (expand-file-name other-file x))))
                          find-protocol-file-search-path)))
      (switch-to-buffer (find-file-noselect path))
    (error "unable to find matching itpp/espf file")))

(provide 'find-protocol-file)
