;; Major mode sypglass related files such as sgdc and opt files.
;; Written by Troy Hinckley

(setq sgdc-highlights
      '(("\\<\\(clock\\|test_mode\\|no_scan\\|no_fault\\|scan_wrap\\|scan_ratio\\|current_design\\|set_parameter\\|current_goal\\|set_goal_option\\|set_option\\|read_file\\)\\>" . font-lock-function-name-face)
        ("\\<\\(value\\|name\\|scanshift\\|invertInCapture\\|testclock\\|register_suffix\\)\\>" . font-lock-variable-name-face)
        ("\\<\\(off\\|on\\|yes\\|no\\|Error\\|Warning\\|Info_latch\\|Info\\)\\>" . font-lock-keyword-face)))

(defvar sgdc-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?\n ">" st)
    (modify-syntax-entry ?/ ". 12" st)
    st)
  "Syntax table for sgdc-mode")

;; only match # style comments at start of line
(defun sgdc-syntax-propertize-function (start end)
  (goto-char start)
  (funcall
   (syntax-propertize-rules
    ("^[[:space:]]*\\(#\\)" (1 "<")))
   (point) end))

(defun sgdc-indent-line ()
  "Indent current line as SGDC code"
  (interactive)
  (beginning-of-line)
  (if (bobp)  ; check for begining of buffer
      (indent-line-to 0)
    (let ((not-indented t) cur-indent)
      (if (looking-at "^[[:space:]]*//}}} - End \\(Design Specific Constraints Section\\|Global Constraints Section\\)") ; Deindent if an End tag is seen
          (progn
            (save-excursion
              (forward-line -1)
              (setq cur-indent (- (current-indentation) tab-width)))
            (if (< cur-indent 0)
                (setq cur-indent 0)))
        (save-excursion
          (while not-indented
            (forward-line -1)
            (if (looking-at "^[[:space:]]*//}}} - End \\(Design Specific Constraints Section\\|Global Constraints Section\\)") ; Match End tag if previous line
                (progn
                  (setq cur-indent (current-indentation))
                  (setq not-indented nil))
              (if (looking-at "^[[:space:]]*//{{{ - \\(Design Specific Constraints Section\\|Global Constraints Section\\)") ; indent if start tag is found
                  (progn
                    (setq cur-indent (+ (current-indentation) tab-width))
                    (setq not-indented nil))
                (if (bobp) ; Check for rule 5
                    (setq not-indented nil)))))))
      (if cur-indent
          (indent-line-to cur-indent)
        (indent-line-to 0))))) ; If we didn't see an indentation hint, then allow no indentation

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.sgdc\\'" . sgdc-mode))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.opt\\'" . sgdc-mode))

;;;###autoload
(define-derived-mode sgdc-mode prog-mode "sgdc"
  "major mode for editing spyglass related files"
  (setq tab-width 3)
  (setq-local indent-line-function 'sgdc-indent-line)
  (setq-local syntax-propertize-function #'sgdc-syntax-propertize-function)
  (if (string-match "\\.opt" (buffer-name))
      (setq-local comment-start "#")
    (setq-local comment-start "//"))
  (setq-local font-lock-defaults '(sgdc-highlights)))

(provide 'sgdc-mode)
