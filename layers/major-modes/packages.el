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
  (use-package itpp-mode
    :mode "\\.itpp\\'"))

(defun major-modes/init-reglist-mode ()
  (use-package reglist-mode
    :mode "\\.list\\'"
    :mode "\\.defaults\\'"
    :mode "\\.inc\\'"))

(defun major-modes/init-sdgc-mode ()
  (use-package sgdc-mode
    :mode "\\.sgdc\\'"
    :mode "\\.opt\\'"))

(defun major-modes/init-spfspec-mode ()
  (use-package spfspec-mode
    :mode "\\.spfspec\\'"
    :init
    (with-eval-after-load "smartparens"
      (sp-local-pair 'spfspec-mode "'" nil :actions nil))
    (with-eval-after-load "highlight-numbers"
      (puthash
       'spfspec-mode (rx (or (and symbol-start (char digit) (*? nonl) symbol-end)
                             (and "'" (or (and "h" (+? (in xdigit)))
                                          (and "b" (+? (in (?0 . ?1))))
                                          (and "o" (+? (in (?0 . ?7))))
                                          (and "d" (+? (in digit))))
                                  symbol-end)))
       highlight-numbers-modelist))))

(defun major-modes/init-postsim-mode ()
  (use-package postsim-mode
    :mode (rx "postsim.log" (optional ".gz") eos)))


;;; packages.el ends here
