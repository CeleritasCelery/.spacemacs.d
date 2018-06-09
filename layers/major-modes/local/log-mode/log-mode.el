;;; log-mode.el --- major mode for log files -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Troy Hinckley

;;; Code:

(setq log-font-lock-keywords
      `((,(rx bol (0+ space)
              (optional "-I-:")
              (group (any "#*") (any space "#*") (0+ nonl)))
         1 'font-lock-comment-face)
        (,(rx bol
              (0+ space)
              (optional "-I-:")
              (group-n 2
                       (group-n 1 (any "="))
                       (1+ (backref 1)))
              eol)
         2 'font-lock-comment-face)
        (,(rx bol (0+ space)
              (group (1+ "-"))
              eol)
         1 'font-lock-comment-face)
        (,(rx bol (optional "-I-:") (group (or "-W-" "Warning-"))) 1 'font-lock-warning-face)))

;;;###autoload
(add-to-list 'auto-mode-alist `(,(rx ".log" (optional "." (1+ num)) eos) . log-mode))

;;;###autoload
(define-derived-mode log-mode fundamental-mode "log"
  (setq-local font-lock-defaults '(log-font-lock-keywords))
  (modify-syntax-entry ?\" "." log-mode-syntax-table)
  (ansi-color-apply-on-region (point-min) (point-max))
  (compilation-minor-mode))


(provide 'log-mode)
;;; log-mode.el ends here
