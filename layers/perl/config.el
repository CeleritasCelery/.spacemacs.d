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

(defvar perl5--prettify-symbols-alist
  `(("&&" . (?\s (Br . Bl) ?\s (Br . Br) ,(decode-char 'ucs #XE100)))
    ("||" . (?\s (Br . Bl) ?\s (Br . Br) ,(decode-char 'ucs #XE104)))
    ("::" . (?\s (Br . Bl) ?\s (Br . Br) ,(decode-char 'ucs #XE106)))
    ("==" . (?\s (Br . Bl) ?\s (Br . Br) ,(decode-char 'ucs #XE107)))
    ("=>" . (?\s (Br . Bl) ?\s (Br . Br) ,(decode-char 'ucs #XE10A)))
    ("->" . (?\s (Br . Bl) ?\s (Br . Br) ,(decode-char 'ucs #XE112)))
    ("<<" . (?\s (Br . Bl) ?\s (Br . Br) ,(decode-char 'ucs #XE11C)))
    (".." . (?\s (Br . Bl) ?\s (Br . Br) ,(decode-char 'ucs #XE11F)))
    ("!=" . (?\s (Br . Bl) ?\s (Br . Br) ,(decode-char 'ucs #XE123)))
    ("<=" . (?\s (Br . Bl) ?\s (Bc . Bc) ?< (Bc . Bc) ?_))
    (">=" . (?\s (Br . Bl) ?\s (Bc . Bc) ?> (Bc . Bc) ?_))
    (".=" . (?· (Br . Bl) ?=))
    ("."  . ?·))
  "ligatures for the Hasklig font. Mapped to unicode open glyphs")
