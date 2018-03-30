;;; major mode for ITTP files
(defvar itpp--prettify-symbols-alist
  `(("=>" . (?\s (Br . Bl) ?\s (Br . Br) ,(decode-char 'ucs #XE10A))))
  "ligatures for the Hasklig font. Mapped to unicode open glyphs")

;; create the list for font-lock.
;; each class of keyword is given a particular face
(setq itpp-font-lock-keywords
      `((,(rx "STOPNAME = "
              (group-n 1 (1+ word))) 1 font-lock-type-face)
        (,(rx "REG = "
              (group-n 1 (1+ word))) 1 font-lock-constant-face)
        (,(rx bol
              (group-n 1 "rem") ":") 1 font-lock-function-name-face)
        (,(rx bol "label:" (0+ space)
              (group-n 1 (1+ word))) 1 font-lock-warning-face)
        (,(rx symbol-start
              (group-n 1 (or "Type" "Domain"))) 1 font-lock-constant-face)
        (,(rx "=" (optional ">") (0+ space)
              (group-n 1 (1+ word))) 1 font-lock-variable-name-face)
        (,(rx bol
              (group-n 1 "vector: " (1+ nonl)) eol) 1 font-lock-string-face)
        (,(rx bol
              (group-n 1 (1+ (any lower "_"))) ":") 1 font-lock-keyword-face)
        (,(rx (or "(" "X") (group-n 1 (1+ (any "LH")))) 1 font-lock-warning-face t)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.itpp\\'" . itpp-mode))

;; define the mode
;;;###autoload
(define-derived-mode itpp-mode fundamental-mode "ITPP"
  "Major mode for editing itpp files."
  (setq prettify-symbols-alist itpp--prettify-symbols-alist)
  (modify-syntax-entry ?# "< b" itpp-mode-syntax-table)
  (modify-syntax-entry ?\n "> b" itpp-mode-syntax-table)
  (modify-syntax-entry ?= "." itpp-mode-syntax-table)
  (modify-syntax-entry ?> "." itpp-mode-syntax-table)
  (modify-syntax-entry ?_ "w" itpp-mode-syntax-table)
  (setq-local font-lock-defaults '(itpp-font-lock-keywords))
  (setq-local comment-start "# ")
  (setq-local comment-end "")
  (linum-mode)
  (prettify-symbols-mode))

(provide 'itpp-mode)
