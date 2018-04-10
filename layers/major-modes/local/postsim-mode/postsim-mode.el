;; postsim-mode.el
(defgroup postsim-mode nil
  "Custom faces for pass or fail status"
  :group 'faces)

(defface postsim-pass-face
  '((t :background "ForestGreen" :weight bold))
  "Face to highlight pass status"
  :group 'postsim-mode)

(defface postsim-fail-face
  '((t :background "OrangeRed" :weight bold))
  "Face to highlight fail status"
  :group 'postsim-mode)

(setq postsim-font-lock-keywords
      `((,(rx bol (group "[" (1+ (in alnum " ")) "]")) 1 font-lock-type-face)
        (,(rx bol (0+ space) (group (1+ (in alnum " _")) ":")) 1 font-lock-keyword-face)
        (,(rx bol (0+ space) "Status" (1+ space) ": " (group "PASS")) 1 postsim-pass-face)
        (,(rx bol (0+ space) "Status" (1+ space) ": " (group "FAIL")) 1 postsim-fail-face)
        (,(rx bol (group "#" (1+ nonl))) 1 font-lock-warning-face)))

;;;###autoload
(add-to-list 'auto-mode-alist (cons (rx "postsim.log" (optional ".gz") eos) 'postsim-mode))

;;;###autoload
(define-derived-mode postsim-mode prog-mode "postsim"
  (modify-syntax-entry ?= ". 12" postsim-mode-syntax-table)
  (modify-syntax-entry ?\n ">" postsim-mode-syntax-table)
  (modify-syntax-entry ?\/ "." postsim-mode-syntax-table) ;; for highlight numbers mode
  (setq-local font-lock-defaults '(postsim-font-lock-keywords)))

(provide 'postsim-mode)
;;; postsim-mode.el ends here
