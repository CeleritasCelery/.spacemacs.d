;;; funcs.el --- Perl5 Layer functions File for Spacemacs
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun spacemacs/perltidy-format ()
  "Format Perl code with perltidy.
If region is active, operate on it, else operate on line."
  (interactive)
  (let ((old-point (point))
        (pos
         (if (use-region-p)
             (cons (region-beginning)
                   (if (char-equal ?\n (char-before (region-end)))
                       (region-end)
                     (save-excursion ;; must including terminating newline
                       (goto-char (region-end))
                       (1+ (line-end-position)))))
           (cons (line-beginning-position)
                 (1+ (line-end-position))))))
    (apply #'call-process-region (car pos) (cdr pos) perl5-perltidy-executable t '(t nil)
           "--quiet"
           "--standard-error-output"
           perl5-perltidy-options)
    (goto-char old-point)))
