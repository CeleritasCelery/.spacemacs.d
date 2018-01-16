;;; packages.el --- shell-config layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: troy hinckley <troy.hinckley@gmail.com>
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
    shell
    shx
    company
    (shell-config :location local)
    (company-fish :location local)
    (company-async-files :location local)
    ))

(defun shell-config/post-init-shell ()
  )

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

(defun shell-config/post-init-company ()
  ;; messing with removing ret to select candiates because it makes it very hard to use
  ;; company in REPL's and org mode. Now just use `C-l'
  (spacemacs|use-package-add-hook company
    :post-config
    (define-key company-active-map (kbd "RET") nil)
    (define-key company-active-map [return] nil))

  (setq company-backends-shell-mode '((company-env company-command) company-async-files company-fish company-dabbrev-code company-dabbrev))
  (spacemacs|add-company-hook shell-mode)

  (defun cel/company-dabbrev--skip-numbers (ret-val)
    (unless (and ret-val
                 (s-matches? (rx bos (+ digit) eos) ret-val))
      ret-val))
  (advice-add 'company-dabbrev--prefix :filter-return #'cel/company-dabbrev--skip-numbers)

  (defun cel/company-select-prev-or-comint-match-input (&optional _)
    (when (and (eq major-mode 'shell-mode)
               (eq company-selection 0))
      (company-abort)
      (call-interactively 'comint-previous-matching-input-from-input)))
  (advice-add 'company-select-previous :before-until #'cel/company-select-prev-or-comint-match-input)
  (with-eval-after-load "company-dabbrev-code"
    (add-to-list 'company-dabbrev-code-modes 'comint-mode))
  )

(defun shell-config/init-shell-config ()
  (use-package shell-config))

(defun shell-config/init-company-fish ()
  (use-package company-fish
    :defer t))

(defun shell-config/init-company-async-files ()
  (use-package company-async-files
    :defer t))


;;; packages.el ends here
