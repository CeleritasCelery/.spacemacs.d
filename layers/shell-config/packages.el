;;; packages.el --- shell-config layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: troy.j.hinckley <tjhinckl@sccj004228>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `shell-config-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `shell-config/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `shell-config/pre-init-PACKAGE' and/or
;;   `shell-config/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst shell-config-packages
  '(
    shx
    (shell-config :location local)
    (company-fish :location local)
    (company-async-files :location local)
    ))

(defun shell-config/init-shx ()
  (use-package shx
    :defer t
    :init
    (spacemacs|diminish shx-mode " ⓧ" " x")
    (add-hook 'shell-mode-hook #'shx-mode)
    (add-hook 'shx-mode-hook (lambda () (setq comint-prompt-read-only t)))
    :config
    (defun shx-send-input-or-copy-path ()
      (interactive)
      (if (shx-point-on-input-p)
          (shx-send-input)
        (let ((path (apply 'buffer-substring-no-properties
                           (--map (save-excursion
                                    (funcall it "[:alnum:]$/._-")
                                    (point))
                                  '(skip-chars-backward skip-chars-forward)))))
          (goto-char (point-max))
          (insert path))))
    (define-key shx-mode-map (kbd "C-<return>") #'shx-send-input-or-copy-path)))

(defun shell-config/init-shell-config ()
  (use-package shell-config))

(defun shell-config/init-company-fish ()
  (use-package company-fish
    :defer t))

(defun shell-config/init-company-async-files ()
  (use-package company-async-files
    :defer t))


;;; packages.el ends here
