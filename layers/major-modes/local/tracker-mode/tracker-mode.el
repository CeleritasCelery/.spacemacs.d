;; create the list for font-lock.
;; each class of keyword is given a particular face
(setq tracker-font-lock-keywords
      `((,(rx bol (group-n 1 (1+ "_")) eol) 1 font-lock-comment-face)
        (,(rx (group-n 1  "|")) 1 font-lock-comment-face)
        (,(rx symbol-start (group-n 1 "b-DATA") symbol-end) 1 font-lock-constant-face)
        (,(rx symbol-start (group-n 1 "a-CONTROL") symbol-end) 1 font-lock-variable-name-face)
        (,(rx symbol-start (group-n 1 "4-SELECT") symbol-end) 1 font-lock-string-face)
        (,(rx symbol-start (group-n 1 (any "1-3" "5-9" "c-f") "-" (1+ word)) symbol-end) 1 font-lock-keyword-face)))

;;;###autoload
(add-to-list 'auto-mode-alist `(,(rx "TRK_PID_0x" (1+ (any xdigit)) ".out" (optional ".gz") eos) . tracker-mode))

;; define the mode
;;;###autoload
(define-derived-mode tracker-mode fundamental-mode "Tracker"
  "Major mode for editing tracker files."
  (setq-local font-lock-defaults '(tracker-font-lock-keywords)))

(defun tracker-mode-truncate-lines ()
  (toggle-truncate-lines))
(add-hook 'tracker-mode-hook #'tracker-mode-truncate-lines)

(provide 'tracker-mode)
