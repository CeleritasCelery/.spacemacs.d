;;; config for `shell' -*- lexical-binding: t -*-

(require 'dash-functional)

;; https://github.com/kyagi/shell-pop-el/issues/51
(add-to-list 'display-buffer-alist (cons "\\*shell\\*" display-buffer--same-window-action))

(setq shell-file-name "bash")
(setq comint-scroll-to-bottom-on-input t)
(setq comint-process-echoes t)

(defvar cel/dir-history nil
  "previous shell directories")
(make-variable-buffer-local 'cel/dir-history)

(defun track-shell-directory/procfs (str)
  "use the proc filesytem to get the current directory.
Works on remote shells as well if `shx' and `shx-cmd-set-pid' are used. The
remote shell will need to echo it's PID in the rc file in the form of `shx'
markup."
  (when (string-match comint-prompt-regexp str)
    (--when-let (-some->> (cel/get-shell-pid)
                          (format "/proc/%s/cwd")
                          (concat (file-remote-p default-directory))
                          file-symlink-p
                          cd)
      (unless (equal it (car cel/dir-history))
        (push it cel/dir-history))))
  str)

(defun cel/supress-hostkey-warning (str)
  "EC machines issue a benign but really annoying warning that the EC people
don't have the technical competence to fix"
  ;; add_host_to_hostkeys: failed to open <missing path> - reason Permission denied
  (if (string-match "add_host_to_hostkeys: failed to open" str) "" str))

(defun cel/select-shell-history ()
  (interactive)
  (goto-char (point-max))
  (insert (concat "cd " (s-chop-prefix (file-remote-p default-directory)
                                       (completing-read "directory:" cel/dir-history)))))

(define-key shell-mode-map (kbd "C-c C-j") 'cel/select-shell-history)

(defvar cel/layout-local-variables nil)

(defvar cel/layout-local-map (ht-create))

(defun cel/make-variable-layout-local (&rest vars)
  (cl-loop for (symbol default-value) on vars by 'cddr
           do (add-to-list 'cel/layout-local-variables (cons symbol default-value))))

(defun cel/load-layout-local-vars (persp-name &rest _)
  (let ((layout-local-vars (-filter 'boundp
                                    (-map 'car cel/layout-local-variables))))
    (ht-set! cel/layout-local-map
             (spacemacs//current-layout-name)
             (--map (cons it (symbol-value it))
                    layout-local-vars))
    ;; load the default values
    (--each layout-local-vars
      (set it (alist-get it cel/layout-local-variables)))
    ;; override with the previously bound values for the new layout
    (--when-let (ht-get cel/layout-local-map persp-name)
        (-each it
        (-lambda ((var . val)) (set var val))))))

(cel/make-variable-layout-local 'shell-pop-last-shell-buffer-index 1
                                'shell-pop-last-shell-buffer-name ""
                                'shell-pop-last-buffer nil)


(defun cel/tcsh-remote-shell (fn &rest args)
  (if (file-remote-p default-directory)
      (let ((shell-file-name "tcsh"))
        (apply fn args))
    (apply fn args)))

(advice-add 'shell-pop :around #'cel/tcsh-remote-shell)
(advice-add 'shell :around #'cel/tcsh-remote-shell)

(defun cel/shell-pop-restore-window ()
  (unless (buffer-live-p shell-pop-last-buffer)
    (setq shell-pop-last-buffer (window-buffer (get-mru-window (not :all-frames) :dedicated :not-selected))))
  (unless (window-live-p shell-pop-last-window)
    (setq shell-pop-last-window (get-buffer-window shell-pop-last-buffer))))

(add-hook 'shell-pop-out-hook 'cel/shell-pop-restore-window)

(advice-add 'persp-switch :before #'cel/load-layout-local-vars)

(defun cel/create-persistant-shell (name)
  "Create a presistent named NAME shell to layout."
  (interactive "sShell Name: ")
  (let ((shell-buf (s-lex-format "*${name}*")))
    (shell shell-buf)
    (persp-add-buffer shell-buf)))
(spacemacs/set-leader-keys "os" #'cel/create-persistant-shell)

(defun cel/kill-backward-shell ()
  (interactive)
  (--when-let (-some->> (buffer-substring-no-properties (point) (point-at-bol))
                        (s-match (rx (or bos (any space "=/"))
                                     (0+ (not (any space "=/")))
                                     (0+ (any space "=/"))
                                     eos))
                        (car)
                        (length)
                        (- (point)))
    (delete-region (if (eq (point-at-bol) it) it (1+ it)) (point))))
(evil-define-key 'insert shell-mode-map (kbd "C-w") #'cel/kill-backward-shell)

(substitute-key-definition 'comint-previous-input 'comint-previous-matching-input-from-input shell-mode-map)
(substitute-key-definition 'comint-next-input 'comint-next-matching-input-from-input shell-mode-map)
(define-key shell-mode-map (kbd "C-S-k") 'comint-previous-prompt)
(define-key shell-mode-map (kbd "C-S-j") 'comint-next-prompt)

(defun cel/shell-mode-hook ()
  (shell-dirtrack-mode 0)
  (setq-local comint-prompt-regexp "^╰─→ \\'")
  (modify-syntax-entry ?= ".")
  (add-hook 'comint-preoutput-filter-functions
            'cel/supress-hostkey-warning nil t)
  (add-hook 'comint-preoutput-filter-functions
            'track-shell-directory/procfs nil t))
(add-hook 'shell-mode-hook 'cel/shell-mode-hook)

(defun cel/add-shell-to-layout ()
  (persp-add-buffer (current-buffer)))
(add-hook 'shell-pop-in-after-hook 'cel/add-shell-to-layout)

(defun cel/shell-in-cwd-p (cwd)
  (f-same? default-directory cwd))
(advice-add 'shell-pop--cd-to-cwd :before-until 'cel/shell-in-cwd-p)

(with-eval-after-load 'comint
  (define-key comint-mode-map
    [remap comint-dynamic-list-input-ring] #'helm-comint-input-ring))

(defun company-command--prefix ()
  (when (member major-mode company-env-enabled-modes)
    (when-let (prefix (company-grab-symbol))
      (when (and (not (s-contains? "/" prefix))
                 (not (s-prefix? "$" prefix))
                 (s-equals? prefix
                            (buffer-substring
                             (line-beginning-position)
                             (point))))
        prefix))))

(setq company-command--types '((builtin . "-b")
                               (keyword . "-k")
                               (command . "-c")))

(defun company-command--get-type (type)
  (-if-let* ((var (intern (concat "company-command--" (symbol-name type))))
             (value (and (boundp var) (symbol-value var))))
      value
    (set var (company-command--fetch type))))

;; IFS=':';for i in $PATH; do test -d "$i" && find "$i" -maxdepth 1 -executable -type f -name 'd*' -exec basename {} \;; done
(defun company-command--fetch (type &optional prefix)
  (let ((flag (alist-get type company-command--types))
        (buffer (generate-new-buffer "command-types")))
    (call-process "bash" nil buffer nil "-c" (s-join " " (list "compgen" flag prefix)))
    (let ((candidates (s-trim (with-current-buffer buffer
                                (buffer-string)))))
      (kill-buffer buffer)
      (when (s-present? candidates)
        (s-lines candidates)))))

(defun cel/proc-to-string (proc &rest args)
  (let ((buffer (generate-new-buffer "proc")))
    (apply #'call-process proc nil buffer nil args)
    (let ((output (s-trim (with-current-buffer buffer
                            (buffer-string)))))
      (kill-buffer buffer)
      (when (s-present? output)
        output))))

(defun company-command--candidates (prefix)
  (shell-env-sync 'func)
  (when (shell-env-sync 'alias)
    (setq process-aliases (--map (->> it
                                      (s-chop-prefix "alias ")
                                      (s-chop-suffix "'")
                                      (s-replace "='" "=")
                                      (s-replace "'\\''" "'"))
                                 process-aliases)))

  (cl-flet ((annotate (annot list)
                      (--map (progn (put-text-property 0 1 'annotation annot it) it)
                             list)))
    (->> (list process-aliases
               process-functions
               (company-command--get-type 'builtin)
               (company-command--get-type 'keyword)) ;; all the competion sources
         (--map (--filter (s-prefix? prefix it) it)) ;; filter by those matching the prefix
         (funcall (-flip #'-snoc) ;; add the command compeletion (already filtered)
                  (company-command--fetch 'command prefix))
         (-zip-with #'annotate '("alias" "function" "built-in" "keyword" "executable")) ;; add annotation
         (--map-first t (--map (-let [(cand meta) (s-split-up-to "=" it 1 t)]
                                 (when meta
                                   (put-text-property 0 1 'meta meta cand))
                                 cand)
                               it)) ;; added meta data to the aliases
         (-remove #'null) ;; remove empty sources
         (-flatten) ;; flaten into a single list
         (-distinct)))) ;; remove duplicates

(defun company-command--meta (cand)
  (if (s-equals? "executable" (get-text-property 0 'annotation cand))
      (cel/proc-to-string "which" cand)
    (get-text-property 0 'meta cand)))

(defun company-command (command &optional arg &rest ignored)
  "Complete shell commands and options using Fish shell. See `company's COMMAND ARG and IGNORED for details."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-command))
    (prefix (company-command--prefix))
    (candidates (company-command--candidates arg))
    (meta (company-command--meta arg))
    (annotation (format " (%s)" (get-text-property 0 'annotation arg)))))

(defvar company-env-commands '("unset" "unsetenv" "munge"))
(defvar company-env-enabled-modes '(shell-mode) "enabled modes.")

(defun company-env--annotation (candidate)
  (-let [annotation (get-text-property 0 'annotation candidate)]
    (when annotation
      (format " (%s)" annotation))))

(defun company-env--prefix ()
  (when (member major-mode company-env-enabled-modes)
    (when-let (prefix (with-syntax-table (make-syntax-table (syntax-table))
                      (modify-syntax-entry ?{ "_")
                      (company-grab-symbol)))
      (unless (s-contains? "/" prefix)
        (-let [(cmd arg)
               (s-split (rx (+ space))
                        (buffer-substring (line-beginning-position)
                                          (point))
                        t)]
        (cond ((s-prefix? "$" prefix) (let ((var (s-chop-prefixes '("$" "{") prefix)))
                                        (cons var (1+ (length var))))) ;; expansion
                ;; When using unset export etc the variable name does not have a
                ;; `$' so we need to make sure to watch for this senario
              ((and (s-equals? arg prefix)
                      (-contains? company-env-commands cmd))
                 prefix)
                ;; used in asigment (i.e. FOO=bar). Force update the environment
                ;; to ensure we are not using this backend when
                ;; `company-command' would be better
                ((and (s-equals? cmd prefix) ;; used in assignment
                    (progn (shell-env-sync 'env)
                           (--any? (s-prefix? prefix it)
                                     process-environment)))
                 prefix)))))))

(defun company-env--candidates (prefix)
  (shell-env-sync 'env)
  (--map  (-let [(cand annot) (s-split-up-to "=" it 1 t)]
            (when annot
              (put-text-property 0 1 'annotation annot cand))
            cand)
          (--filter (s-prefix? prefix it)
                    process-environment)))

(defun company-env (command &optional arg &rest ignored)
  "Complete shell commands and options using Fish shell. See `company's COMMAND ARG and IGNORED for details."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-env))
    (prefix (company-env--prefix))
    (candidates (company-env--candidates arg))
    (annotation " (env)")
    (meta (get-text-property 0 'annotation arg))
    ))

(defvar env-dump-dir "/tmp")
(defvar shell-env-timestamps '((env)
                               (alias)
                               (func)))

(defvar shell-env-vars '((env . process-environment)
                         (alias . process-aliases)
                         (func . process-functions)))

(defun shell-env-sync (type)
  (-when-let* ((src-file (shell-env-get-file type))
               (time-stamp (->> src-file
                                (file-attributes)
                                (nth 5))))
    (unless (equal time-stamp (alist-get type shell-env-timestamps))
      (shell-env-update type)
      (setf (alist-get type shell-env-timestamps) time-stamp))))

(advice-add 'getenv :before (lambda (&rest _) (shell-env-sync 'env)))

(defun cel/get-shell-pid ()
  (or cel/shell-pid
      ;; we can only use the buffer process PID
      ;; with local shells
      (unless (file-remote-p default-directory)
  (-some->> (current-buffer)
                  get-buffer-process
                  process-id))))

(defvar cel/shell-pid nil
  "Set this variable when the buffer process PID is not the shell PID.")
(make-variable-buffer-local 'cel/shell-pid)

(defun shx-cmd-set-pid (pid)
  "(SAFE) sets env local shell PID.
Add the following lines to (or equvilent) to your shell starup file

echo \"<set-pid $$>\""
  (setq cel/shell-pid pid))

(defun shell-env-get-file (type)
  (-some->> (cel/get-shell-pid)
            (format (concat "%s." (symbol-name type)))
            (f-join env-dump-dir)
            (concat (file-remote-p default-directory))))

(defun shell-env-update (type)
  (let ((var (alist-get type shell-env-vars)))
    (make-local-variable var)
    (-some->> (shell-env-get-file type)
              f-read-text
              s-trim
              s-lines
              (set var))))

(provide 'shell-config)
