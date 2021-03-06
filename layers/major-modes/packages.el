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
    (sgdc-mode    :location local)
    (spfspec-mode :location local)
    (postsim-mode :location local)
    (tracker-mode :location local)
    (spf-mode     :location local)
    (log-mode     :location local)
    ))

(defun najor-modes/init-log-mode ()
  (use-package log-mode))

(defun major-modes/init-itpp-mode ()
  (use-package itpp-mode))

(defun major-modes/init-reglist-mode ()
  (use-package reglist-mode
    :config
    (spacemacs/set-leader-keys-for-major-mode 'reglist-mode "c" #'reglist-creed-wrap)
    (spacemacs/set-leader-keys-for-major-mode 'reglist-mode "a" #'reglist-ace-wrap)))

(defun major-modes/init-sgdc-mode ()
  (use-package sgdc-mode))

(defun major-modes/init-spfspec-mode ()
  (use-package spfspec-mode
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
  (use-package postsim-mode))

(defun major-modes/init-tracker-mode ()
  (use-package tracker-mode))

(defun major-modes/init-spf-mode ()
  (use-package spf-mode))
;;; packages.el ends here
