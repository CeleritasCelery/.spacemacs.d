(defface reglist-ace-args-face
  '((t :background "#3d3446"))
  "face for ace_args"
  :group 'reglist-mode)
(defvar reglist-ace-args-face 'reglist-ace-args-face)

(setq reglist-font-lock-keywords
      `(("^[[:space:]]*[+]\\([[:alpha:]]+[[:alnum:]]*\\)\\_>" 1 font-lock-keyword-face)
        ("^[[:space:]]*[-]\\([[:alpha:]]+[[:alnum:]]*\\)\\_>" 1 font-lock-type-face)
        ("^[[:space:]]*[.]\\([[:alpha:]]+[[:alnum:]]*\\)\\_>" 1 font-lock-function-name-face)
        ("[$]\\([[:alnum:]][[:alnum:]_]*\\)\\>" 1 font-lock-variable-name-face)
        (".-\\([[:alpha:]][[:alnum:]_]*\\)-?" 1 font-lock-variable-name-face)
        (,(rx "-"
              (or "ace_args" "creed_uopt")
              " " (0+ nonl) " -"
              (or "ace_args" "creed_uopt")
              "-") 0
              (if (nth 4 (syntax-ppss))
                  'font-lock-comment-face
                'reglist-ace-args-face) prepend)))

(defun reglist-syntax-propertize-function (start end)
  (goto-char start)
  (funcall
   (syntax-propertize-rules
    ("'\\(\"'\\).*\\('\"\\)'" (1 ".")(2 ".")))
   (point) end))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.list\\'" . reglist-mode))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.inc\\'" . reglist-mode))

;;;###autoload
(define-derived-mode reglist-mode prog-mode "list"
  (modify-syntax-entry ?# "< b" reglist-mode-syntax-table)
  (modify-syntax-entry ?\n "> b" reglist-mode-syntax-table)
  (modify-syntax-entry ?' "\"" reglist-mode-syntax-table)
  (modify-syntax-entry ?- "." reglist-mode-syntax-table)
  (modify-syntax-entry ?+ "." reglist-mode-syntax-table)
  (modify-syntax-entry ?/ "." reglist-mode-syntax-table)

  ;; (color-identifiers-mode)
  (setq-local syntax-propertize-function #'reglist-syntax-propertize-function)
  (setq-local font-lock-defaults '(reglist-font-lock-keywords))
  (setq-local comment-start "# ")
  (setq-local comment-end ""))

(provide 'reglist-mode)
