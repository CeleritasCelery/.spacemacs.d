;;; packages.el --- config layer packages file for Spacemacs.
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
;; added to `config-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `config/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `config/pre-init-PACKAGE' and/or
;;   `config/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst config-packages
  '(
    (general-config :location local)
    (org-config :location local)
    evil-lion
    helm
    (find-protocol-file :location local)
    ))

(defun config/pre-init-helm ()
  (spacemacs|use-package-add-hook helm
    :post-config
    (define-key helm-map (kbd "C-S-k") 'helm-beginning-of-buffer)
    (define-key helm-map (kbd "C-S-j") 'helm-end-of-buffer)
    ))

(defun config/init-evil-lion ()
  (use-package evil-lion
    :ensure t
    :bind (:map evil-normal-state-map
                ("g l " . evil-lion-left)
                ("g L " . evil-lion-right)
                :map evil-visual-state-map
                ("g l " . evil-lion-left)
                ("g L " . evil-lion-right))))

(defun config/init-general-config ()
  (use-package general-config))

(defun config/init-org-config ()
  (use-package org-config))

(defun config/init-find-protocol-file ()
  (use-package find-protocol-file
    :commands cel/switch-itpp-espf
    :init
    (dolist (mode '(itpp-mode cperl-mode perl-mode))
      (spacemacs/set-leader-keys-for-major-mode mode "s" #'cel/switch-itpp-espf))))


;;; packages.el ends here
