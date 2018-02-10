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

;;; Code:

(defconst shell-config-packages
  '(
    shell
    shx
    company
    helm
    yasnippet
    (shell-config :location local)
    (company-fish :location local)
    (company-async-files :location local)
    evil
    ))

(defun shell-config/post-init-evil ()
  (evil-global-set-key 'normal (kbd "gf") 'cel/find-file-at-point))

(defun shell-config/init-shx ()
  (use-package shx
(spacemacs|extend-package yasnippet
  :post-init
  (remove-hook 'shell-mode-hook #'spacemacs/force-yasnippet-off))

(spacemacs|extend-package helm
  :post-config
  (progn
    (put 'cel/helm-ff-run-switch-to-shell 'helm-only t)
    (define-key helm-find-files-map (kbd "M-e") 'cel/helm-ff-run-switch-to-shell)))

(spacemacs|extend-package shell-pop
  :pre-init
  ;; overload this function so that it is layout local
  (defmacro make-shell-pop-command (func &optional shell)
    "Create a function to open a shell via the function FUNC.
  SHELL is the SHELL function to use (i.e. when FUNC represents a terminal)."
    (let* ((name (symbol-name func)))
      `(defun ,(intern (concat "spacemacs/shell-pop-" name)) (index)
         ,(format (concat "Toggle a popup window with `%S'.\n"
                          "Multiple shells can be opened with a numerical prefix "
                          "argument. Using the universal prefix argument will "
                          "open the shell in the current buffer instead of a "
                          "popup buffer.") func)
         (interactive "P")
         (require 'shell-pop)
         (when (equal '(4) index)
           (setq index nil))
         (shell-pop--set-shell-type
          'shell-pop-shell-type
          (list ,name
                (concat "*" (spacemacs//current-layout-name) "-" (if (file-remote-p default-directory) "remote-" "") ,name "*")
                (lambda nil (,func ,shell))))
         (shell-pop index))))
  :post-init
  (progn
    (defun cel/strip-tramp-cmd (args)
      (-let [(cwd) args]
        (list (s-chop-prefix (car (s-match
                                   (rx bos "/" (1+ (any word "\\:@")) ":")
                                   cwd))
                             cwd))))
    (advice-add 'shell-pop--cd-to-cwd :filter-args #'cel/strip-tramp-cmd)))

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
                                    (funcall it "[:alnum:]$/._-~")
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
