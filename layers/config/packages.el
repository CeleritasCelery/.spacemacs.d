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
    (helm-ff-edit :location local)
    (helm-fzf :location local)
    evil-lion
    helm
    (find-protocol-file :location local)
    evil-lisp-state
    org-fancy-priorities
    (highlight-quoted :location (recipe :fetcher github :repo "Fanael/highlight-quoted"))
    nameless
    yatemplate
    magit
    ivy
    ivy-hydra
    swiper
    counsel
    smex
    eval-in-repl
    alert
    compile
    ))

(defun config/init-compile ()
  (use-package compile
    :diminish (compilation-minor-mode . "Ⓒ")))

(defun config/init-alert ()
  (use-package alert
    :custom (alert-default-style 'fringe)))

(defun config/init-eval-in-repl ()
  (use-package eval-in-repl :ensure t
    :config
    (define-key sh-mode-map (kbd "<C-return>") 'eir-eval-in-shell)))

(defun config/init-helm-fzf ()
  (use-package helm-fzf
    :defer t))

(defun config/init-helm-ff-edit ()
  (use-package helm-ff-edit
    :defer t
    :init
    (with-eval-after-load 'helm-files
      (define-key helm-find-files-map (kbd "C-c C-e") 'helm-find-files-edit))))

(defun config/init-ivy ()
  (use-package ivy
    :defer t
    :init
    (setq ivy-height 15)
    :config
    (with-eval-after-load 'recentf
      ;; merge recentf and bookmarks into buffer switching. If we set this
      ;; before recentf loads, then ivy-mode loads recentf for us,
      ;; which messes up the spacemacs version of recentf.
      (setq ivy-use-virtual-buffers t))
    (require 'ivy-hydra)
    (evil-make-overriding-map ivy-occur-mode-map 'normal)
    ;; https://github.com/syl20bnr/spacemacs/issues/7516
    (define-key ivy-minibuffer-map (kbd "TAB") 'ivy-alt-done)))

(defun config/init-ivy-hydra ()
  (use-package ivy-hydra
    :defer t))

(defun config/init-swiper ()
  (use-package swiper
    :defer t
    :init
    (spacemacs/set-leader-keys "oS" 'swiper)))

(defun config/init-counsel ()
  (use-package counsel
    :defer t
    :init
    (spacemacs/set-leader-keys "oF" 'counsel-find-file)
    :config
    (define-key counsel-find-file-map (kbd "TAB") 'ivy-alt-done)))

(defun config/init-smex ()
  (use-package smex
    :defer t
    :init
    (setq-default smex-save-file (expand-file-name ".smex-items"
                                                   spacemacs-cache-directory))))

(defun config/post-init-magit ()
  (with-eval-after-load 'magit
    (setq magit-blame-time-format "%yww%U.%u | %b,%d %H:%M") ;; use intel ww syntax
    (remove-hook 'magit-status-sections-hook 'magit-insert-recent-commits)))

(defun config/init-yatemplate ()
  (use-package yatemplate
    :ensure t
    :config
    (setq yatemplate-dir "~/.spacemacs.d/templates")
    (yatemplate-fill-alist)
    (auto-insert-mode)
    ;; start the template in insert state
    (add-hook 'yas-before-expand-snippet-hook #'evil-insert-state)))

(defun config/init-nameless ()
  (use-package nameless
    :hook (emacs-lisp-mode . nameless-mode-from-hook)
    :diminish "Ͽ"
    :config (setq nameless-prefix "Ͽ")))

(defun config/init-highlight-quoted ()
  (use-package highlight-quoted
    :hook (emacs-lisp-mode . highlight-quoted-mode)))

(defun config/init-org-fancy-priorities ()
  (use-package org-fancy-priorities
    :hook (org-mode . org-fancy-priorities-mode)
    :diminish org-fancy-priorities-mode
    :config
    (setq org-fancy-priorities-list '("⬆" "⬅" "⬇" "☕"))))

(defun config/post-init-evil-lisp-state ()
  (when (configuration-layer/package-usedp 'evil-cleverparens)
    (with-eval-after-load 'evil-lisp-state
      ;; remove this keybindings so they can be prefixs
      (define-key evil-lisp-state-map (kbd "y") nil)
      (define-key evil-lisp-state-map (kbd "c") nil)

      ;; rebind some functions that we overwrote
      (define-key evil-lisp-state-map (kbd "o") 'sp-absorb-sexp) ;; a
      (define-key evil-lisp-state-map (kbd "ys") 'sp-copy-sexp) ;; y
      (define-key evil-lisp-state-map (kbd "cu") 'sp-convolute-sexp) ;; c

      ;; add evil-cleverparens
      (define-key evil-lisp-state-map (kbd "a") 'evil-cp-append)
      (define-key evil-lisp-state-map (kbd "i") 'evil-cp-insert)

      (define-key evil-lisp-state-map (kbd "Y") 'evil-cp-yank-line)
      (define-key evil-lisp-state-map (kbd "D") 'evil-cp-delete-line)
      (define-key evil-lisp-state-map (kbd "C") 'evil-cp-change-line)

      (define-key evil-lisp-state-map (kbd "dd") 'evil-cp-delete)
      (define-key evil-lisp-state-map (kbd "yy") 'evil-cp-yank)
      (define-key evil-lisp-state-map (kbd "cc") 'evil-cp-change)

      (define-key evil-lisp-state-map (kbd "x") 'evil-cp-delete-char-or-splice)
      (define-key evil-lisp-state-map (kbd "X") 'evil-cp-delete-char-or-splice-backwards))))

(defun config/post-init-helm ()
  (with-eval-after-load 'helm
    (define-key helm-map (kbd "C-S-k") 'helm-beginning-of-buffer)
    (define-key helm-map (kbd "C-S-j") 'helm-end-of-buffer)))

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
    :commands find-protocol-file
    :init
    (dolist (mode '(itpp-mode cperl-mode perl-mode))
      (spacemacs/set-leader-keys-for-major-mode mode "s" #'find-protocol-file))))


;;; packages.el ends here
