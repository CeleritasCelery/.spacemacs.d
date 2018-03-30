;;; spfspec-mode.el --- support for spfspec object notation -*- lexical-binding: t; -*-

;; Troy Hinckley troy.j.hinckley@intel.com

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Put a description of the package here

;;; Code:

(require 's)

(defvar spfspec--prettify-symbols-alist
  `(("&&" . (?\s (Br . Bl) ?\s (Br . Br) ,(decode-char 'ucs #XE100)))
    ("||" . (?\s (Br . Bl) ?\s (Br . Br) ,(decode-char 'ucs #XE104)))
    ("==" . (?\s (Br . Bl) ?\s (Br . Br) ,(decode-char 'ucs #XE107)))
    ("->" . (?\s (Br . Bl) ?\s (Br . Br) ,(decode-char 'ucs #XE112)))
    ("!=" . (?\s (Br . Bl) ?\s (Br . Br) ,(decode-char 'ucs #XE123)))
    ("<=" . (?\s (Br . Bl) ?\s (Bc . Bc) ?< (Bc . Bc) ?_))
    (">=" . (?\s (Br . Bl) ?\s (Bc . Bc) ?> (Bc . Bc) ?_)))
  "ligatures for the Hasklig font. Mapped to open unicode glyphs")

(setq spfspec-font-lock-keywords
      '(("^[[:space:]]*\\(@[[:alnum:]_]+\\)\\_>" 1 font-lock-variable-name-face)
        ("^[[:space:]]*\\([[:alnum:]_%]+\\)[[:space:]]*:" 1 font-lock-keyword-face)
        ("^[[:space:]]*\\([[:alnum:]_]+\\)[[:space:]]*{" 1 font-lock-type-face)
        ("^[[:space:]]*\\([[:alnum:][:space:]_]+\\)[[:space:]]*{" 1 font-lock-function-name-face)))


(defun spfspec-indent-line ()
  "Indent current line using sfpspec convention"
  (interactive)
  (beginning-of-line)
  (if (bobp)  ; check for begining of buffer
      (indent-line-to 0)
    (let ((not-indented t) cur-indent)
      (if (looking-at "^[[:space:]]*}") ; Deindent if an End tag is seen
          (progn
            (save-excursion
              (forward-line -1)
              (setq cur-indent (- (current-indentation) tab-width)))
            (if (< cur-indent 0)
                (setq cur-indent 0)))
        (save-excursion
          (while not-indented
            (forward-line -1)
            (if (looking-at "^[[:space:]]*}") ; Match End tag if previous line
                (progn
                  (setq cur-indent (current-indentation))
                  (setq not-indented nil))
              (if (looking-at "^[[:space:]]*[[:alnum:][:space:]_]+{[[:space:]]*$") ; indent if start tag is found
                  (progn
                    (setq cur-indent (+ (current-indentation) tab-width))
                    (setq not-indented nil))
                (if (bobp) ; Check for rule 5
                    (setq not-indented nil)))))))
      (if cur-indent
          (indent-line-to cur-indent)
        (indent-line-to 0))))) ; If we didn't see an indentation hint, then allow no indentation

(defun spfspec-goto-definition ()
  "Goto definition."
  (interactive)
  (let* ((symbol (thing-at-point 'symbol t))
         (type (if (s-contains? "stf.spfspec" (buffer-file-name))
                   (concat "STF_STOP " symbol)
                 (if (s-matches? (rx (+ (any word "_")) (+ space) ": 'b" (+ (any "01")) " :")
                                 (thing-at-point 'line t))
                     (concat (rx (+ space)) symbol " {")
                   (concat "TAP " symbol)))))
    (when (search-forward-regexp (concat "^" type))
      (recenter 1))))
(spacemacs/set-leader-keys-for-major-mode 'spfspec-mode "g" #'spfspec-goto-definition)

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.spfspec\\'" . spfspec-mode))

;;;###autoload
(define-derived-mode spfspec-mode fundamental-mode "SPF Spec"
  (setq tab-width 2)
  (setq prettify-symbols-alist spfspec--prettify-symbols-alist)
  (setq-local indent-line-function 'spfspec-indent-line)
  (modify-syntax-entry ?\/ ". 12b" spfspec-mode-syntax-table)
  (modify-syntax-entry ?\n "> b" spfspec-mode-syntax-table)
  (modify-syntax-entry ?- "." spfspec-mode-syntax-table)
  (modify-syntax-entry ?> "." spfspec-mode-syntax-table)
  (setq-local font-lock-defaults '(spfspec-font-lock-keywords))
  (setq-local comment-start "// ")
  (setq-local comment-end "")
  (prettify-symbols-mode)
  (linum-mode)
  (when (featurep 'highlight-numbers)
    (highlight-numbers-mode)))

(provide 'spfspec-mode)
;;; spfspec-mode.el ends here
