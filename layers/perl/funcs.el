;;; funcs.el --- Perl Layer functions File for Spacemacs
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun perl-perltidy-format ()
  "Tidy Perl code
if region is active, operate on
 region, else operate on line."
  (interactive)
  (let ((pos
         (if (use-region-p)
             (list (region-beginning)
                   (if (= ?\n (char-before (region-end)))
                       (region-end)
                     (save-excursion ;; must including terminating newline
                       (goto-char (region-end))
                       (+ 1 (line-end-position)))))
           (list (line-beginning-position)
                 (+ 1 (line-end-position)))))) ;; get newline
    (let ((old-point (point)))
      (call-process-region (pop pos) (pop pos) perl5-perltidy-executable t '(t nil)
                           "--quiet"
                           "--standard-error-output")
      (goto-char old-point))))


;; (defun my-check-region ()
;;   "print whether region is active."
;;   (interactive)
;;   (if (use-region-p)
;;       (message "region is %i - %s" (region-beginning) (char-before (region-end)))
;;     (message "line is %i - %i" (line-beginning-position) (+ 1 (line-end-position)))))

