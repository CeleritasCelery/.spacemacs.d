;;; general-config.el --- a collection of functions I wrote to assist in coding  -*- lexical-binding: t -*-
(defun highlight-lines ()
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

(defun $open-file-in-clipboard ()
  (interactive)
  (helm-find-files-1 (string-trim (current-kill 0))))
(spacemacs/set-leader-keys "of" #'$open-file-in-clipboard)

(defvar $ediff-targets nil
  "The last two files that were diffed")
(defun $save-ediff-targets (&rest args)
  "Save the last two ediffed files"
  (setq $ediff-targets (car args)))
(advice-add 'ediff-files-internal :filter-args #'$save-ediff-targets)

(defun $run-last-ediff ()
  "Run ediff with the last used files"
  (interactive)
  (apply 'ediff-files-internal $ediff-targets))
(spacemacs/set-leader-keys "oe" #'$run-last-ediff)

(defun set-tab-width (x)
  "set the tab width for the current buffer"
  (interactive "ntab-width: ")
  (setq tab-width x))
(spacemacs/set-leader-keys "ot" 'set-tab-width)

(defun $evil-next-line (orig-fun &rest args)
  "check to see if we are in visual line mode"
  (if visual-line-mode
	  (apply 'evil-next-visual-line args)
	(apply orig-fun args)))
(advice-add 'evil-next-line :around '$evil-next-line)

(defun $evil-previous-line (orig-fun &rest args)
  "check to see if we are in visual line mode"
  (if visual-line-mode
	  (apply 'evil-previous-visual-line args)
	(apply orig-fun args)))
(advice-add 'evil-previous-line :around '$evil-previous-line)

(defvar window-sizes
  '((large  . "1920x1200")
    (mobile . "1536x864")
    (wide . "3840x1200")
    (other . "1920x1080")
    (short . "1920x1080"))
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


(defhydra vnc-resize (:columns 2 :exit t :idle 0.5)
  "VNC Resize"
  ("i" vnc-prev "prev size")
  ("l" vnc-window-large "single monitor (large)")
  ("m" vnc-window-mobile "mobile")
  ("w" vnc-window-wide "double monitor (wide)")
  ("s" vnc-window-short "standard res"))
(spacemacs/set-leader-keys "oi" #'vnc-resize/body)

(setenv "SPF_ROOT" "/p/hdk/cad/spf/latest")
(setenv "IDS_HOME" "/p/hdk/rtl/cad/x86-64_linux26/dteg/ideas_shell/0.7.0")
(setenv "GLOBAL_TOOLS" "/nfs/site/proj/dpg/tools")
(setenv "DFT_GLOBAL_DIR" "/nfs/site/home/tjhinckl/workspace/chassis_dft_val_global")

(setq ec-hdk (concat "/p/hdk/rtl/hdk.rc -cfg shdk" (if (equal (getenv "EC_SITE") "fc")
                                                       "73"
                                                     "74")))

(setq spf-root (concat (getenv "SPF_ROOT") "/bin/spf_setup_env"))

(setq ec-variables
      `(("IP_RELEASES"  . ,ec-hdk)
        ("IP_MODELS"    . ,ec-hdk)
        ("GIT_REPOS"    . ,ec-hdk)
        ("GLOBAL_TOOLS" . ,ec-hdk)
        ("RTLMODELS"    . ,ec-hdk)
        ("SPF_PERL_LIB" . ,spf-root)))

(cl-loop for var in ec-variables
         do (when-let ((value (shell-command-to-string
                               (format "/usr/intel/bin/tcsh -c 'source %s >/dev/null; echo -n $%s'"
                                       (cdr var)
                                       (car var)))))
              (setenv (car var) value)))

;; the column feature does not look good with the spacemacs key-doc function
(setq hydra-key-doc-function 'hydra-key-doc-function-default)

(defhydra helm-nav (:foreign-keys run :hint nil :idle 1)
  "Helm Navigation"
  ("1" (helm-select-nth-action 1)  :exit t)
  ("2" (helm-select-nth-action 2)  :exit t)
  ("3" (helm-select-nth-action 3)  :exit t)
  ("4" (helm-select-nth-action 4)  :exit t)
  ("5" (helm-select-nth-action 5)  :exit t)
  ("6" (helm-select-nth-action 6)  :exit t)
  ("7" (helm-select-nth-action 7)  :exit t)
  ("8" (helm-select-nth-action 8)  :exit t)
  ("9" (helm-select-nth-action 9)  :exit t)
  ("0" (helm-select-nth-action 10) :exit t)
  ("<RET>" helm-maybe-exit-minibuffer :exit t)
  ("j" helm-next-line "next line" :column "Lines")
  ("k" helm-previous-line "prev line")
  ("g" helm-beginning-of-buffer "first line")
  ("G" helm-end-of-buffer "last line")
  ("h" helm-previous-source "previous source" :column "Scroll")
  ("l" helm-next-source "next source")
  ("K" helm-scroll-other-window-down "source window down")
  ("J" helm-scroll-other-window "source window up")
  ("m" helm-toggle-visible-mark "mark" :column "Mode")
  ("t" helm-toggle-all-marks "toggle all marks")
  ("f" helm-follow-mode "follow mode")
  ("w" helm-toggle-resplit-and-swap-windows "swap window")
  ("a" (call-interactively 'helm-select-action) "actions" :column "Execute" :exit t)
  ("e" helm-edit "edit" :exit t)
  ("y" helm-copy-to-kill-ring "yank" :exit t)
  ("v" helm-execute-persistent-action "view")
  ("H" helm-help "help" :column "Other" :exit t)
  ("q" nil "quit" :exit t))

(defhydra helm-ff-nav (:foreign-keys run :inherit (helm-nav/heads) :exit t :columns 3 :idle 1)
  "Helm Find Files"
  ("c" helm-ff-run-copy-file "copy")
  ("D" helm-ff-run-delete-file "delete and quit")
  ("d" helm-ff-persistent-delete "delete")
  ("s" helm-ag-from-session "search")
  ("/" helm-ff-run-find-sh-command "find")
  ("z" helm-fzf-from-session "fzf")
  ("x" helm-ff-run-ediff-file "ediff")
  ("o" $helm-ff-run-switch-to-shell "shell")
  ("i" helm-ff-file-name-history "file history")
  ("r" helm-find-files-history "directory history"))

(defun helm-edit ()
  "Switch in edit mode depending on the current helm buffer."
  (interactive)
  (cond
   ((equal "*helm-ag*" helm-buffer)
    (helm-ag-edit))
   ((equal "*helm find files*" helm-buffer)
    ;; TODO: make this respect candidates
    (helm-find-files-edit))
   ((equal "*Helm Swoop*" helm-buffer)
    (helm-swoop-edit))
   (t (error "No editing function bound"))))

(defun helm-ag-from-session ()
  "Launch `helm-ag' from within a helm session"
  (interactive)
  (with-helm-alive-p
    (helm-run-after-exit
     'helm-do-ag
     helm-ff-default-directory
     (let ((cand (helm-marked-candidates)))
       ;; if we have not marked anything we want to search the current directory
       (unless (equal (list (helm-get-selection))
                      cand)
         cand)))))

(defun helm-copy-to-kill-ring ()
  "Copy selection or marked candidates to the kill ring.
Note that the real values of candidates are copied and not the
display values.
If a file name, copy the full path"
  (interactive)
  (with-helm-alive-p
    (helm-run-after-exit
     (lambda (cands)
       (with-helm-current-buffer
         (kill-new (mapconcat
                    (lambda (c)
                      (format "%s" (if (and (stringp c)
                                            (file-exists-p c))
                                       (file-truename c)
                                     c)))
                    cands "\n"))))
     (helm-marked-candidates))))

(with-eval-after-load "helm"
  (define-key helm-map (kbd "C-o") 'helm-nav/body)
  (define-key helm-map (kbd "C-c y") 'helm-copy-to-kill-ring)
  (put 'helm-copy-to-kill-ring 'helm-only t))
(with-eval-after-load 'helm-files
  (define-key helm-find-files-map (kbd "C-c s") 'helm-ag-from-session)
  (define-key helm-find-files-map (kbd "C-o") 'helm-ff-nav/body)
  (define-key helm-read-file-map (kbd "C-o") 'helm-ff-nav/body)
  (put 'helm-ag-from-session 'helm-only t))

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

(defun radix-name (radix)
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

(defun $convert-radix-internal (str old-radix new-radix)
  "internal function to convert between two radices"
  (message "%s %s = %s %s"
           (radix-name old-radix)
           str
           (radix-name new-radix)
           (let ((calc-number-radix new-radix))
             (kill-new (math-format-radix (string-to-number str old-radix))))))

(defun convert-hex-binary ()
  "Converts hex to binary or vice versa and copies the results to the kill ring"
  (interactive)
  (let* ((str (apply 'buffer-substring-no-properties
                     (--map (save-excursion
                              (funcall it "[:xdigit:]#'hx")
                              (point))
                            '(skip-chars-backward skip-chars-forward))))
         (radix (--if-let (car (s-match (rx bos (or (and (0+ (any digit)) "'" (any "hb"))
                                                    (and (any "0#") (any "bx")))) str))
                    (progn (setq str (s-chop-prefix it str))
                           (if (s-contains? "b" it) 'bin 'hex))
                  'bin)))
    (apply '$convert-radix-internal str (if (eq radix 'bin) '(2 16) '(16 2)))))
(spacemacs/set-leader-keys "oc" 'convert-hex-binary)

(defun convert-radix (r1 r2)
  "Convert one radix to another and copy the result to the kill ring"
  (interactive "ncurrent radix: \nndesired radix: ")
  (let* ((valid-chars (if (<= r1 10)
                          (format "0-%d" (- r1 1))
                        (let ((max-char (cond
                 ((eql r1 11) "a")
                 ((eql r1 12) "b")
                 ((eql r1 13) "c")
                 ((eql r1 14) "d")
                 ((eql r1 15) "e")
                                         ((eql r1 16) "f"))))
                          (format "0-9a-%sA-%s" max-char (upcase max-char)))))
         (str (apply 'buffer-substring-no-properties
                     (--map (save-excursion
                              (funcall it valid-chars)
                              (point))
                            '(skip-chars-backward skip-chars-forward)))))
    ($convert-radix-internal str r1 r2)))

(spacemacs/set-leader-keys "ox" 'convert-radix)

(add-hook 'c-mode-common-hook
		  ;; prefered comment style
		  (lambda ()
			(setq comment-start "// "
				  comment-end "")))

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

(defun $helm-ff-not-hardlink-p (file)
  (not (s-ends-with? ".." file)))

(defun $helm-ff-up-one-level (fcn &rest args)
  (cl-flet ((helm-file-completion-source-p (&rest _) t))
    (apply fcn args)))

(defun $helm-ff-dots-at-bottom (ret-val)
  (if (and (listp ret-val)
           (not (equal (with-helm-buffer (buffer-name))
                       "*Helm file completions*")))
      (-rotate (- (--count (s-ends-with? "." it) (-take 2 ret-val)))
               ret-val)
    ret-val))

;; *helm file completions*
(with-eval-after-load 'helm-files
  (advice-add 'helm-ff-filter-candidate-one-by-one
              :before-while '$helm-ff-not-hardlink-p)
  (advice-add 'helm-find-files-up-one-level
              :around '$helm-ff-up-one-level)
  (advice-add 'helm-find-files-get-candidates
              :filter-return '$helm-ff-dots-at-bottom))

(defun $plist-get (list prop)
  (when-let ((index (1+ (seq-position list prop))))
    (nth index list)))

(defun $truename-files (args)
  "make helm dired actions use absolute paths"
  (let* ((index (1+ (seq-position args :files)))
         (files (mapcar #'file-truename (nth index args))))
    (setf (nth index args) files)
    args))
(advice-add 'helm-dired-action :filter-args #'$truename-files)

(defvar evil-v$-gets-eol nil)

(evil-define-motion evil-end-of-line (count)
  "Move the cursor to the end of the current line.
If COUNT is given, move COUNT - 1 lines downward first."
  :type inclusive
  (move-end-of-line count)
  (when evil-track-eol
    (setq temporary-goal-column most-positive-fixnum
          this-command 'next-line))
  (unless (and (evil-visual-state-p) evil-v$-gets-eol)
    (evil-adjust-cursor)
    (when (eolp)
      ;; prevent "c$" and "d$" from deleting blank lines
      (setq evil-this-type 'exclusive))))

(evil-define-text-object evil-filename (count &rest args)
  (let ((bounds (bounds-of-thing-at-point 'filename)))
    (list (car bounds) (cdr bounds))))
(define-key evil-inner-text-objects-map "F" 'evil-filename)

(defvar $bookmarked-dirs '((?r . "/nfs/site/home/tjhinckl/workspace/models/snr/tests/scan/custom/snr/snr/")
                           (?s . "/nfs/site/home/tjhinckl/workspace/models/snr/scan_tests/1p0/")
                           (?l . "/nfs/site/home/tjhinckl/workspace/models/snr/tests/scan/custom/")))
(defun $goto-bookmarked-dir ()
  (interactive)
  (if-let ((key (read-char "Directory letter: "))
           (dir (alist-get key $bookmarked-dirs)))
      (helm-find-files-1 dir)
    (user-error "Directory letter does not exist")))
(spacemacs/set-leader-keys "od" #'$goto-bookmarked-dir)

(defun call-keymap (map &optional prompt)
  "Read a key sequence and call the command it's bound to in MAP.
https://stackoverflow.com/questions/24914202/elisp-call-keymap-from-code"
  (let* ((key (read-key-sequence prompt))
         (cmd (lookup-key map key t)))
    (cond ((functionp cmd) (call-interactively cmd))
          ((keymapp cmd) (call-keymap cmd prompt))
          ((equal key (kbd "C-g"))) ;; don't error out on keyboard quit
          (t (user-error "%s is undefined" key)))))

(defmacro $create-split-fn (direction)
  (let ((func-name (concat "split-window-" (symbol-name direction))))
    `(defun ,(intern (concat "$" func-name)) ()
       (interactive)
       (let ((old-buf (current-buffer)) new-buf)
         (call-keymap spacemacs-cmds "Enter keybinding: ")
         (setq new-buf (current-buffer))
         (unless (equal old-buf new-buf)
           (,(intern func-name))
           (set-window-buffer (selected-window) old-buf)
           (select-window (window-in-direction ',direction)))))))

(defun $run-ipgen (arg)
  "run dft ipgen in the current model"
  (interactive "P")
  (let* ((model-root (vc-git-root default-directory))
         (ipgen (f-join model-root "tools/ipgen/"))
         (dut (completing-read "select DUT: "
                               (-map 'f-filename
                                     (f--directories ipgen (member "setup" (directory-files it))))))
         (default-directory (f-join ipgen dut))
         (compilation-environment (list (concat "MODEL_ROOT=" model-root)))
         (compilation-buffer-name-function
          (lambda (major-mode)
            (concat "*" (f-filename model-root) " " dut " " (downcase major-mode) "*"))))
    (compile (concat "source setup && $DFT_REPO_ROOT/DFTNetworkGen/run_dft_ipgen"
                     (if arg "" " -B")))))

(defun $run-bman ()
  "run dft ipgen in the current model"
  (interactive)
  (let* ((model-root (file-truename (vc-git-root default-directory)))
         (compilation-environment (list (concat "MODEL_ROOT=" model-root)))
         (compilation-buffer-name-function
          (lambda (major-mode)
            (concat "*" (f-filename model-root) " bman " (downcase major-mode) "*"))))
    ;; TODO: need to remove this hardcoded value
    (compile "source /p/hdk/rtl/hdk.rc -cfg shdk74 && bman -dut mdf_10nm")))
(spacemacs/set-leader-keys "ci" '$run-ipgen)

(defun $compile-with-tcsh (fn &rest args)
  (let ((shell-file-name "tcsh"))
    (apply fn args)))
(advice-add 'compile :around #'$compile-with-tcsh)
(advice-add 'recompile :around #'$compile-with-tcsh)

(with-eval-after-load 'compile
  (font-lock-add-keywords 'compilation-mode
                  `((,(rx bol (0+ space)
                          (group "#" (0+ nonl)))
                     1 'font-lock-comment-face)
                    (,(rx bol
                          (group-n 2
                                   (group-n 1 (any "*="))
                                   (1+ (backref 1)))
                          eol)
                     2 'font-lock-comment-face)
                    (,(rx bol (0+ space)
                          (group (1+ "-"))
                          eol)
                     1 'font-lock-comment-face))))

(defun $notfiy-compile-done (buffer exit-string)
  "notfiy the user that compliation is finished"
  (alert "compliation finished"
         :severity (if (string-prefix-p "exited abnormally" exit-string)
                       'high
                     'normal)))
(setq compilation-finish-functions '($notfiy-compile-done))

(add-to-list 'compilation-error-regexp-alist-alist
             `(ipgen-gmake ,(rx bol "gmake" (optional "[1]") ": *** [" (group-n 1 (1+ nonl)) "] Error " digit) 1))
(add-to-list 'compilation-error-regexp-alist-alist
             `(bman-line ,(rx bol "-I-:Error: " (group-n 3 (1+ nonl))
                              "\n-I-:-" (or "E" "F") "-: [CRT-" (1+ digit) "] Error in "
                              (1+ nonl) " file " (group-n 1 (1+ (not (any space)))) (0+ space)
                              "\n-I-: Error at line# " (group-n 2 (1+ digit)) (1+ nonl))
                         1 2 nil nil nil (3 font-lock-warning-face)))
(add-to-list 'compilation-error-regexp-alist-alist
             `(bman-verilog ,(rx bol "-I-:Error-" (1+ nonl)
                                 "\n-I-:" (group-n 1 (1+ (not space))) ", " (group-n 2 (1+ digit))) 1 2))
(add-to-list 'compilation-error-regexp-alist-alist `(bman-stage ,(rx bol "-E-: FAILED: " (1+ (not space)) " : LOG : " (group (1+ (not space)))) 1))
(setq compilation-error-regexp-alist '(ipgen-gmake bman-line bman-stage bman-verilog))

(spacemacs/set-leader-keys-for-minor-mode 'compilation-minor-mode
  "e" 'compile-errors/body)

(defhydra compile-errors ()
  ("n" compilation-next-error "next")
  ("j" compilation-next-error)
  ("p" compilation-previous-error "previous")
  ("k" compilation-previous-error))


(defun $normalize-region (beg end)
  "normalize characters used in Microsoft formatting"
  (interactive "r")
  (save-excursion
    (goto-char beg)
    (when (re-search-forward (rx (char "–‘’“”")) end 'no-error)
      (let* ((orig-text (buffer-substring beg end))
             (normalized-text
              (thread-last orig-text
                (replace-regexp-in-string "–" "--") ;; Em-dash
                (replace-regexp-in-string (rx (char "‘’")) "'")
                (replace-regexp-in-string (rx (char "“”")) "\""))))
        (goto-char beg)
        (delete-region beg end)
        (insert normalized-text)))))

(defun $normalize-evil-paste (&rest _)
  "Normalize last evil paste"
  ($normalize-region (save-excursion (evil-goto-mark ?\[) (point))
                     (1+ (save-excursion (evil-goto-mark ?\]) (point)))))
(advice-add 'evil-paste-after :after #'$normalize-evil-paste)
(advice-add 'evil-paste-before :after #'$normalize-evil-paste)

(defun $normalize-yank (&rest _)
  "normalize last emacs yank"
  (apply #'$normalize-region (cl-sort (list (point) (mark t)) '<)))
(advice-add 'yank :after #'$normalize-yank)


(setq json-encoding-default-indentation "    ")

(defvar $json-array-max-cuddle-size 3
  "the max size of a JSON array that will be cuddled")

(defun $json-format-buffer ()
  "format the buffer, keeping some elements more compact"
  (interactive)
  (json-pretty-print-buffer)
  ($json-cuddle-obj-array-delimiter)
  ($json-cuddle-array $json-array-max-cuddle-size))

(defun $json-cuddle-obj-array-delimiter ()
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward (rx ": [" eol) nil 'noerror)
      (let ((array-start (1- (point)))
            obj-start)
        (forward-line)
        (when (looking-at-p (rx (1+ space) "{" (optional ",") eol))
          (setq obj-start (point))
          (goto-char array-start)
          (forward-list)
          (forward-line -1)
          (end-of-line)
          (indent-rigidly obj-start
                          (point)
                          (- (length json-encoding-default-indentation)))
          (join-line 1)
          (goto-char obj-start)
          (join-line))))))

(defun $json-cuddle-array (max-size)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward (rx "[" eol) nil 'noerror)
      (let ((start (point))
            (col (progn (back-to-indentation)
                        (current-column)))
            (line 1)
            (max (1+ max-size)))
        (cl-loop while (<= line max) do
                 (forward-line)
                 (move-to-column col )
                 (when (looking-at-p (rx (1+ (char "]")) (optional ",") eol))
                   (dotimes (_ line)
                     (join-line))
                   (cl-return))
                 (setq line (1+ line)))
        (goto-char start)))))

(defun $magit-status-current-directory ()
  "limit magit status the current directory"
  (interactive)
  (let* ((root (vc-git-root default-directory))
         (dir (list (file-relative-name default-directory root)))
         (old-hook magit-status-mode-hook))
    (add-hook 'magit-status-mode-hook
              (lambda () (setq-local magit-diff-section-file-args dir)))
    (magit-status-internal root)
    (setq magit-status-mode-hook old-hook)))
(spacemacs/set-leader-keys "gd" #'$magit-status-current-directory)

(defun $magit-status-clear-local-dir (fn &rest args)
  (let ((old-hook magit-status-mode-hook))
    (add-hook 'magit-status-mode-hook
              (lambda ()
                (when (--none? (eq (car it) 'magit-diff-section-file-args)
                               dir-local-variables-alist)
                  (setq-local magit-diff-section-file-args nil))))
    (apply fn args)
    (setq magit-status-mode-hook old-hook)))
(advice-add 'magit-status :around #'$magit-status-clear-local-dir)

(spacemacs/set-leader-keys "gfb" #'magit-checkout-file)
(spacemacs/set-leader-keys "gfd" #'magit-diff-buffer-file)

(defun $git-work-user ()
  "Set my work credentials"
  (interactive)
  (shell-command "git config --local user.name \"Hinckley, Troy J\" &&
     git config --local user.email troy.j.hinckley@intel.com"))

(defun $git-private-user ()
  "Set my private credentials"
  (interactive)
  (shell-command "git config --local user.name CeleritasCelery &&
     git config --local user.email t.macman@gmail.com"))


(provide 'general-config)
