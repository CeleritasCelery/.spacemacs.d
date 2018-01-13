;; ;; major mode for ITTP files
;; (defface itpp-x-face
;;   '((t :foreground "#545b8d"))
;;   "Face for X's and Z's in ITPP"
;;   :group 'itpp-mode)
;; (defvar itpp-x-face 'itpp-x-face)

;; create the list for font-lock.
;; each class of keyword is given a particular face
(setq itpp-font-lock-keywords
      '(
        ;; ("\\_<\\([[:digit:]LHXZ]+?\\(\\_>\\|[[:digit:]LHXZ_]+\\_>\\)\\)" . font-lock-constant-face)
        ;; ("\\([01LH]+?[01LH_]*\\|\\_<\\)\\([xXzZ_]+?\\)\\([01LH]+?\\|\\_>\\)" 2 itpp-x-face prepend)
        ("^label:[[:space:]]*\\([[:alnum:]_]+\\)\\_>" 1 font-lock-warning-face)
        ("^\\([[:lower:]_]+\\)\\_>:" 1 font-lock-keyword-face)
        ("\\_<\\(Type\\|Domain\\)\\_>" 1 font-lock-constant-face)
        ("\\_<\\([[:alnum:]_]+\\)[[:space:]]*=>?" 1 font-lock-variable-name-face)
        (".\\_<\\([[:alnum:]_]+\\)\\_>[[:space:]]*:" 1 font-lock-type-face)
        ("\\_<\\([[:alnum:]_]+\\)\\_>([[:xdigit:]xXhHlLzZ_]+)" 1 font-lock-function-name-face)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.itpp\\'" . itpp-mode))

;; define the mode
;;;###autoload
(define-derived-mode itpp-mode prog-mode "ITPP"
  "Major mode for editing itpp files."
  (modify-syntax-entry ?# "< b" itpp-mode-syntax-table)
  (modify-syntax-entry ?\n "> b" itpp-mode-syntax-table)
  (setq-local font-lock-defaults '(itpp-font-lock-keywords))
  (setq-local comment-start "# ")
  (setq-local comment-end ""))

(provide 'itpp-mode)
