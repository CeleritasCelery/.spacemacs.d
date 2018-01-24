;;; packages.el --- major-modes layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2017 Troy Hinckley
;;
;; Author: troy.j.hinckley
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3


;;; Code:

(defconst major-modes-packages
  '(
    (itpp-mode    :location local)
    (reglist-mode :location local)
    (sdgc-mode    :location local)
    (spfspec-mode :location local)
    (postsim-mode :location local)
    ))

(defun major-modes/init-itpp-mode ()
  (use-package itpp-mode))

(defun major-modes/init-reglist-mode ()
  (use-package reglist-mode))

(defun major-modes/init-sdgc-mode ()
  (use-package sgdc-mode))

(defun major-modes/init-spfspec-mode ()
  (use-package spfspec-mode
    :config
    (with-eval-after-load "highlight-numbers"
      (puthash
       'spfspec-mode "\\_<[[:digit:]].*?\\_>\\|'\\(?:h[[:xdigit:]]*?\\|b[01]*?\\|o[0-7]*?\\|d[[:digit:]]*?\\)\\_>"
       highlight-numbers-modelist))))

(defun major-modes/init-postsim-mode ()
  (use-package postsim-mode))


;;; packages.el ends here
