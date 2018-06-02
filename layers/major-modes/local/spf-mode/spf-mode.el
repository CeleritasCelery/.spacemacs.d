;;; spf-mode.el --- major mode for spf and espf -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Troy Hinckley

;;; Code:

(defalias 'spf-syntax-propertize-function
  (eval `(syntax-propertize-rules
          (,(rx (group-n 1 "'")) (1 ".")))))

(setq spf-font-lock-keywords
      `((,(rx symbol-start "repeat" symbol-end) . font-lock-type-face)
        (,(rx "@set") . font-lock-warning-face)
        ,(rx symbol-start (or "pass"
                              "set"
                              "comment"
                              "compare"
                              "flush"
                              "label"
                              "focus_tap"
                              "unfocus_tap"
                              "set_stf_packet"
                              "focus_stf"
                              "cycle")
             symbol-end)))

;;;###autoload
(add-to-list 'auto-mode-alist (cons (rx "." (optional "e") "spf" eos)  'spf-mode))

;;;###autoload
(define-derived-mode spf-mode perl-mode "spf"
  (font-lock-add-keywords 'spf-mode spf-font-lock-keywords)
  (add-function :before (local 'syntax-propertize-function) 'spf-syntax-propertize-function))

(provide 'spf-mode)
;;; spf-mode.el ends here
