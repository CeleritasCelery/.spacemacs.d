;;; packages.el --- major-modes layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2017 Troy Hinckley
;;
;; Author: troy.j.hinckley <tjhinckl@sccj004228>
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
    ))

(defun major-modes/init-itpp-mode ()
  (use-package itpp-mode
    :defer t))

(defun major-modes/init-reglist-mode ()
  (use-package reglist-mode
    :defer t))

(defun major-modes/init-sdgc-mode ()
  (use-package sdgc-mode
    :defer t))

(defun major-modes/init-spfspec-mode ()
  (use-package spfspec-mode
    :defer t))


;;; packages.el ends here
