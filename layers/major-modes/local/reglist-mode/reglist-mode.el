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
    ("'\\(\"'\\).*\\('\"\\)'" (1 ".") (2 ".")))
   (point) end))

(defvar reglist-test-type "dft_SPF_ITPP_generic")

(defun reglist--wrap (format-str)
  "Wrap the itpp in region with FORMAT-STR."
  (let* ((bounds (if (use-region-p)
                     (cons (region-beginning)
                           (region-end))
                   (bounds-of-thing-at-point 'paragraph)))
         (start (car bounds))
         (end (copy-marker (cdr bounds))))
    (save-excursion
      (goto-char start)
      (while (and (re-search-forward (rx bol
                                         (group-n 1 ;; group 1 contains the entire itpp path
                                                  (+? nonl)
                                                  (group-n 2 ;; group 2 contains only the test basename
                                                           (1+ (any alnum "_")))
                                                  ".itpp")
                                         eol)
                                     end :no-error)
                  (< (match-beginning 1) end))
        (let ((file (match-string 1))
              (name (match-string 2)))
          (replace-match (format format-str
                                 reglist-test-type
                                 reglist-test-type
                                 file name)))
        (forward-line)))))

(defun reglist-creed-wrap ()
  "Wrap the itpp in region with the creed calling convetion.
Change `reglist-test-type' to change the test type."
  (interactive)
  (reglist--wrap "%s -ovm_test %s -creed_uopt ITPP_FILE=%s -creed_uopt- -dirtag %s"))

(defun reglist-ace-wrap ()
  "Wrap the itpp in region with the ace calling convetion.
Change `reglist-test-type' to change the test type."
  (interactive)
  (reglist--wrap "%s -ovm_test %s -ace_args -simv_args '\"'+ITPP_FILE=%s'\"' -ace_args- -dirtag %s"))

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
