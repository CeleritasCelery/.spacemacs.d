;;; config.el --- Perl5 Layer config File for Spacemacs
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Troy Hinckley <troyhinckley@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(spacemacs|defvar-company-backends cperl-mode)
(spacemacs|define-jump-handlers cperl-mode)
(spacemacs|define-jump-handlers perl-mode)

(defvar perl5-perltidy-executable "perltidy"
  "Program name of perltidy")

(defvar perl5-perltidy-options '()
  "Command line options to pass to perltidy")
