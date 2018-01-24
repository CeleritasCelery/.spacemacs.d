;; a collection of functions I wrote to assist in coding

(defun highlight-lines ()
  (interactive)
  (set-face-attribute 'highlight nil :background "#293235" :foreground "#67b11d") ;; green
  (let ((start (line-beginning-position))
		(end (line-end-position)))
	(when (or (not transient-mark-mode) (region-active-p))
	  (setq start (save-excursion
					(goto-char (region-beginning))
					(beginning-of-line)
					(point))
			end (save-excursion
				  (goto-char (region-end))
				  (end-of-line)
				  (point))))
	(hlt-highlight-region start end)))
(spacemacs/set-leader-keys "oh" 'highlight-lines) ;; highlight the current line

(defun unhighlight-lines ()
  (interactive)
  (let ((start (line-beginning-position))
		(end (line-end-position)))
	(when (or (not transient-mark-mode) (region-active-p))
	  (setq start (save-excursion
					(goto-char (region-beginning))
					(beginning-of-line)
					(point))
			end (save-excursion
				  (goto-char (region-end))
				  (end-of-line)
				  (point))))
	(hlt-unhighlight-region start end)))
(spacemacs/set-leader-keys "oj" 'unhighlight-lines) ;; unhighlight the current line

;; remove duplicate lines in region
(defun delete-duplicate-lines (start end)
  "Find duplicate lines in region START to END keeping first occurrence."
  (interactive "*r")
  (save-excursion
	(let ((end (copy-marker end)))
	  (while
		  (progn
			(goto-char start)
			(re-search-forward "^\\(.*\\)\n\\(\\(.*\n\\)*\\)\\1\n" end t))
		(replace-match "\\1\n\\2")))))
(spacemacs/set-leader-keys "xld" 'delete-duplicate-lines) ;; remove all duplicates

(defun set-tab-width (x)
  "set the tab width for the current buffer"
  (interactive "ntab-width: ")
  (setq tab-width x))
(spacemacs/set-leader-keys "ot" 'set-tab-width)

(defun tab-mode ()
  "Toggle between space and tab mode"
  (interactive)
  (if (eq (symbol-value 'indent-tabs-mode) nil)
	  (progn (setq indent-tabs-mode t)
			 (message "tab-mode enabled for buffer"))
	(progn (setq indent-tabs-mode nil)
		   (message "tab-mode disabled for buffer"))))
(spacemacs/set-leader-keys "tt" 'tab-mode) ;; switch between tabs and spaces

(defun my/evil-next-line (orig-fun &rest args)
  "check to see if we are in visual line mode"
  (if visual-line-mode
	  (apply 'evil-next-visual-line args)
	(apply orig-fun args)))
(advice-add 'evil-next-line :around 'my/evil-next-line)

(defun my/evil-previous-line (orig-fun &rest args)
  "check to see if we are in visual line mode"
  (if visual-line-mode
	  (apply 'evil-previous-visual-line args)
	(apply orig-fun args)))
(advice-add 'evil-previous-line :around 'my/evil-previous-line)

(require 's)
(defun my/open-file-in-clipboard ()
  (interactive)
  (helm-find-files-1 (replace-regexp-in-string "\\s-+" "" (substring-no-properties (current-kill 0)))))
(spacemacs/set-leader-keys "of" #'my/open-file-in-clipboard)

(defvar window-sizes
  '((large  . "1920x1200")
    (wide   . "1920x1080")
    (mobile . "1536x864"))
  "list of valid VNC dimensions")

(defvar prev-vnc-size (caar window-sizes)
  "default VNC size")

(defun vnc-resize (size)
  (setq prev-vnc-size size)
  (shell-command (concat "xrandr --size " (cdr (assoc size window-sizes)))))

(defun vnc-prev ()
  (interactive)
  (vnc-resize prev-vnc-size))

(dolist (size window-sizes)
  (let ((name (car size)))
    (eval `(defun ,(intern (concat "vnc-window-" (symbol-name name))) ()
             (interactive)
             (vnc-resize ',name)))))



(spacemacs/set-leader-keys "oii" #'vnc-prev)
(spacemacs/set-leader-keys "oil" #'vnc-window-large)
(spacemacs/set-leader-keys "oim" #'vnc-window-mobile)
(spacemacs/set-leader-keys "oiw" #'vnc-window-wide)

(setenv "SPF_ROOT" "/p/hdk/cad/spf/latest")
(setenv "IDS_HOME" "/p/hdk/rtl/cad/x86-64_linux26/dteg/ideas_shell/0.7.0")
(setenv "GLOBAL_TOOLS" "/nfs/site/proj/dpg/tools")

(setq ec-hdk (concat "/p/hdk/rtl/hdk.rc -cfg shdk" (if (eq (getenv "EC_SITE") "fc") "73" "74")))

(setq spf-root (concat (getenv "SPF_ROOT") "/bin/spf_setup_env"))

(setq ec-variables
      `(("IP_RELEASES"  . ,ec-hdk)
        ("IP_MODELS"    . ,ec-hdk)
        ("GIT_REPOS"    . ,ec-hdk)
        ("GLOBAL_TOOLS" . ,ec-hdk)
        ("RTLMODELS"    . ,ec-hdk)
        ("SPF_PERL_LIB" . ,spf-root)))

(dolist (var ec-variables)
  (--if-let (shell-command-to-string (format "/usr/intel/bin/tcsh -c 'source %s >/dev/null; echo -n $%s'" (cdr var) (car var)))
      (setenv (car var) it)))

;; (defun cel/get-spm-model-names)

;; (defcustom setmodel-sources
;;   '(("/p/hdk/rtl/ip_models/shdk74" f-directories t)
;;     ("/p/hdk/rtl/models/shdk74/config_spm" f-directories nil))
;;   "sources for setting the model"
;;   :type '(list (list directory function boolean)))

;; (setq setmodel-sources
;;       '(("/p/hdk/rtl/ip_models/shdk74" f-directories t)
;;         ("/p/hdk/rtl/models/shdk74/config_spm" f-directories nil)))

;; (defun shx-cmd-setmodel ()
;;   (interactive)
;;   (let* ((models (-flatten (--map (funcall (cadr it) (car it)) setmodel-sources)))
;;          (model (completing-read "Select Model: " models))
;;          (base (f-filename model))
;;          ;; (version (completing-read "Select Version: " (-map 'f-filename (f-directories model-path))))
;;          ;; (model-root (f-join model-path version)))
;;          ;; (setenv "MODEL_ROOT" (concat root "/" model))
;;          ;; (message "%s" model)
;;          )
;;     (when (f-parent-of? "/p/hdk/rtl/ip_models/shdk74" model)
;;       (setq model (f-join model (completing-read "Select Version: " (-map 'f-filename (f-directories model))))))
;;     (comint-simple-send nil (format "export MODEL_ROOT=%s" model))
;;     ))

;; /p/hdk/rtl/models/shdk74/config_spm/config_spm-srvr10nm-snr_0p8-17ww49a

(defun set-model ()
  (interactive)
  (let (models model versions version duts dut model-root)
    (setq models (s-lines (shell-command-to-string (format "find %s/ -maxdepth 1 2>/dev/null" (getenv "IP_MODELS")))))
    (setq model (completing-read "Select Model: " (mapcar 'file-name-nondirectory models)))
    (setq versions (s-lines (shell-command-to-string (concat "find " (getenv "IP_MODELS") "/" model "/ -maxdepth 1 2>/dev/null"))))
    (setq version (completing-read "Select Version: " (mapcar 'file-name-nondirectory versions)))
    (setq model-root (concat (getenv "IP_MODELS") "/" model "/" version))
    (setenv "MODEL_ROOT" model-root)

    (setq duts (s-lines (shell-command-to-string (concat "find " model-root "/tools/ipgen/ -maxdepth 1 -type d 2>/dev/null"))))
    (setq dut (completing-read "Select DUT: " (mapcar 'file-name-nondirectory duts)))
    (setenv "STF_SPFSPEC" (concat model-root "/tools/ipgen/" dut "/output/dft/verif/rtl/spf/" dut ".stf.spfspec"))
    (setenv "TAP_SPFSPEC" (concat model-root "/tools/ipgen/" dut "/output/dft/verif/rtl/spf/" dut ".tap.spfspec"))))

(defun mc-column--col-at-point (point)
  (save-excursion (goto-char point) (current-column)))

(defun mc-column--make-cursor-at-col-append (_startcol endcol orig-line)
  (end-of-line)
  (when (> endcol (current-column))
    (insert-char ?\s (- endcol (current-column))))
  (move-to-column (- endcol 1))
  (unless (= (line-number-at-pos) orig-line)
    (evil-mc-make-cursor-here)))

(defun mc-column--make-cursor-at-col-insert (startcol _endcol orig-line)
  (end-of-line)
  (move-to-column startcol)
  (unless (or (= (line-number-at-pos) orig-line) (> startcol (current-column)))
    (evil-mc-make-cursor-here)))

(defun mc-column--make-vertical-cursors (beg end func)
  (evil-mc-pause-cursors)
  (apply-on-rectangle func
                      beg end (line-number-at-pos (point)))
  (evil-mc-resume-cursors)
  (evil-normal-state))

(defun evil-mc-insert-vertical-cursors (beg end)
  (interactive (list (region-beginning) (region-end)))
  (mc-column--make-vertical-cursors beg end 'mc-column--make-cursor-at-col-insert)
  (move-to-column (min (mc-column--col-at-point beg) (mc-column--col-at-point end))))

(defun evil-mc-append-vertical-cursors (beg end)
  (interactive (list (region-beginning) (region-end)))
  (let ((final-column (- (max (mc-column--col-at-point beg) (mc-column--col-at-point end)) 1)))
    (mc-column--make-vertical-cursors beg end 'mc-column--make-cursor-at-col-append)
    (mc-column--col-at-point beg)
    (move-to-column final-column)))

(evil-define-key 'visual global-map "gI" 'evil-mc-insert-vertical-cursors)
(evil-define-key 'visual global-map "gA" 'evil-mc-append-vertical-cursors)

(require 'calc-bin) ;; converting radicies
(require 'calc-ext) ;; big numbers
(defun convert-hex-binary ()
  "Converts hex to binary or vice versa and copies the results to the kill ring"
  (interactive)
  (let (inputStr numStr p1 p2 radix)
    (save-excursion
      (search-backward-regexp "[^0-9A-Fa-fx#'h]" nil t)
      (forward-char)
      (setq p1 (point))
      (re-search-forward "[^0-9A-Fa-fx#'h]" nil t)
      (backward-char)
      (setq p2 (point)))
    (setq inputStr (buffer-substring-no-properties p1 p2))
    (setq radix ;; auto detect the radix of the number
          (cond
           ((string-prefix-p "'b" inputStr) 'bin)
           ((string-prefix-p "'h" inputStr) 'hex)
           ((string-prefix-p "0b" inputStr) 'bin)
           ((string-prefix-p "0x" inputStr) 'hex)
           ((string-prefix-p "#b" inputStr) 'bin)
           ((string-prefix-p "#x" inputStr) 'hex)
           ((string-match-p "[2-9a-fA-F]" inputStr) 'hex)
           (t 'bin)))
    (let ((case-fold-search nil)) ;; remove prefix radix identifier
      (setq numStr (replace-regexp-in-string "^'b" "" inputStr)) ; SPF binary
      (setq numStr (replace-regexp-in-string "^'h" "" numStr )) ; SPF hex
      (setq numStr (replace-regexp-in-string "^0b" "" numStr )) ; C, Perl binary
      (setq numStr (replace-regexp-in-string "^0x" "" numStr )) ; C, Perl hex
      (setq numStr (replace-regexp-in-string "^#b" "" numStr )) ; elisp binary
      (setq numStr (replace-regexp-in-string "^#x" "" numStr ))); elisp hex
    (if (eq radix 'bin) ;; print, copy, and convert number
        (message "Binary %s = Hex %s" numStr
                 (let ((calc-number-radix 16))
                   (kill-new (math-format-radix (string-to-number numStr 2)))))
      (message "Hex %s = Binary %s" numStr
               (let ((calc-number-radix 2))
                 (kill-new (math-format-radix (string-to-number numStr 16))))))))
(spacemacs/set-leader-keys "oc" 'convert-hex-binary)


(defun name-radix (radix)
  (cond
   ((eql radix 2)  "Binary")
   ((eql radix 3)  "Ternary")
   ((eql radix 4)  "Quaternary")
   ((eql radix 5)  "Quinary")
   ((eql radix 6)  "Senary")
   ((eql radix 7)  "Heptary")
   ((eql radix 8)  "Octal")
   ((eql radix 9)  "Nonary")
   ((eql radix 10) "Decimal")
   ((eql radix 11) "Undecimal")
   ((eql radix 12) "Duodecimal")
   ((eql radix 13) "Tridecimal")
   ((eql radix 14) "Tetradecimal")
   ((eql radix 15) "Pentadecimal")
   ((eql radix 16) "Hexadecimal")
   (t (format      "(base %d)" radix))))

(defun convert-radix (r1 r2)
  "Convert one radix to another and copy the result to the kill ring"
  (interactive "ncurrent radix: \nndesired radix: ")
  (let (inputStr p1 p2)
    (if (use-region-p)
        (setq p1 (region-beginning)
              p2 (region-end))
      (save-excursion
        (let (max-char valid-chars)
          (setq
           valid-chars
           (if (<= r1 10)
               (format "[^0-%d]" (- r1 1))
             (progn
               (setq
                max-char
                (cond
                 ((eql r1 11) "a")
                 ((eql r1 12) "b")
                 ((eql r1 13) "c")
                 ((eql r1 14) "d")
                 ((eql r1 15) "e")
                 ((eql r1 16) "f")))
               (format "[^0-9a-%sA-%s]" max-char (upcase max-char)))))
          (search-backward-regexp valid-chars nil t)
          (forward-char)
          (setq p1 (point))
          (re-search-forward valid-chars nil t)
          (backward-char)
          (setq p2 (point)))))
    (setq inputStr (buffer-substring-no-properties p1 p2))
    (message "%s %s = %s %s"
             (name-radix r1)
             inputStr
             (name-radix r2)
             (let ((calc-number-radix r2))
               (kill-new (math-format-radix (string-to-number inputStr r1)))))))
(spacemacs/set-leader-keys "ox" 'convert-radix)

(add-hook 'c-mode-common-hook
		  ;; prefered comment style
		  (lambda ()
			(setq comment-start "// "
				  comment-end "")))

(add-hook 'json-mode-hook (lambda () (setq indent-tabs-mode t
										   tab-width 3)))

(add-hook 'verilog-mode-hook (lambda () (setq indent-tabs-mode nil
											  tab-width 3)))

(add-hook 'java-mode-hook (lambda ()
                            (setq c-basic-offset 3)))

(spacemacs|define-transient-state imagex
  :title "Image Manipulation Transient State"
  :bindings
  ("+" imagex-sticky-zoom-in)
  ("=" imagex-sticky-zoom-in)
  ("-" imagex-sticky-zoom-out)
  ("m" imagex-sticky-maximize)
  ("o" imagex-sticky-restore-original)
  ("r" imagex-sticky-rotate-right)
  ("l" imagex-sticky-rotate-left)
  ("q" nil :exit t))
(spacemacs/set-leader-keys-for-major-mode 'image-mode "m" 'spacemacs/imagex-transient-state/body)

(defun messages-auto-tail (&rest _)
  "Make *Messages* buffer auto-scroll to the end after each message."
    (let* ((buf-name "*Messages*")
           (buf (get-buffer-create buf-name)))
      (when (not (string= buf-name (buffer-name)))
        (dolist (win (get-buffer-window-list buf-name nil :all-frames))
          (with-selected-window win
            (goto-char (point-max))))
        (with-current-buffer buf
          (goto-char (point-max))))))

(define-minor-mode messages-auto-scroll-mode
  "Make Messages buffer auto scroll when new input arrives"
  :init-value nil
  :global t
  (if messages-auto-scroll-mode
      (advice-add 'message :after #'messages-auto-tail)
    (advice-remove 'message #'messages-auto-tail)))

(defun cel/helm-ff-not-hardlink-p (file)
  (not (s-ends-with? ".." file)))

(defun cel/helm-ff-up-one-level (fcn &rest args)
  (cl-flet ((helm-file-completion-source-p (&rest _) t))
    (apply fcn args)))

(defun cel/helm-ff-dots-at-bottom (ret-val)
  (if (listp ret-val)
      (-rotate (- (--count (s-ends-with? "." it) (-take 2 ret-val)))
               ret-val)
    ret-val))

(with-eval-after-load 'helm-files
  (advice-add 'helm-ff-filter-candidate-one-by-one
              :before-while 'cel/helm-ff-not-hardlink-p)
  (advice-add 'helm-find-files-up-one-level
              :around 'cel/helm-ff-up-one-level)
  (advice-add 'helm-find-files-get-candidates
              :filter-return 'cel/helm-ff-dots-at-bottom))

(provide 'general-config)
