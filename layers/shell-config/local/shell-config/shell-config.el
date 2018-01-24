;;; config for `shell' -*- lexical-binding: t -*-

(require 'dash-functional)

;; https://github.com/kyagi/shell-pop-el/issues/51
(push (cons "\\*shell\\*" display-buffer--same-window-action) display-buffer-alist)

(setq shell-file-name "bash")
(setq comint-scroll-to-bottom-on-input t)
(setq comint-process-echoes t)

(defun track-shell-directory/procfs (str)
  (when (and (string-match comint-prompt-regexp str)
             (not (file-remote-p default-directory)))
      (cd (file-symlink-p
           (format "/proc/%s/cwd" (process-id
                                   (get-buffer-process
                                  (current-buffer)))))))
  str)


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
                                     (0+ (any space "/"))
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

(spacemacs/make-layout-local 'shell-pop-last-shell-buffer-index
                             'shell-pop-last-shell-buffer-name)

(defun company-command--prefix ()
  (when (-contains? company-env-enabled-modes major-mode)
    (let ((prefix (company-grab-symbol)))
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

(defvar company-env-commands '("export" "set" "unset" "setenv" "unsetenv" "munge"))
(defvar company-env-enabled-modes '(shell-mode) "enabled modes.")

(defun company-env--annotation (candidate)
  (-let [annotation (get-text-property 0 'annotation candidate)]
    (when annotation
      (format " (%s)" annotation))))

(defun company-env--prefix ()
  (when (-contains? company-env-enabled-modes major-mode) ;; not inside string
    (-let* ((prefix (with-syntax-table (make-syntax-table (syntax-table))
                      (modify-syntax-entry ?{ "_")
                      (company-grab-symbol)))
            (line (buffer-substring-no-properties
                   (line-beginning-position)
                   (point)))
            ((cmd arg) (s-split (rx (+ space)) line t)))
      (unless (s-contains? "/" prefix)
        (cond ((s-prefix? "$" prefix) (let ((var (s-chop-prefixes '("$" "{") prefix)))
                                        (cons var (1+ (length var))))) ;; expansion
              ((and (s-equals? arg prefix)
                    (-contains? company-env-commands cmd)) prefix) ;; export setenv etc
              ((and (s-equals? cmd prefix) ;; used as command (or set)
                    (progn (shell-env-sync 'env)
                           (--any? (s-prefix? prefix it)
                                   process-environment))) prefix))))))

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

(defun shell-env-get-file (type)
  (-some->> (current-buffer)
            (get-buffer-process)
            (process-id)
            (format (concat "%d." (symbol-name type)))
            (f-join env-dump-dir)))

(defun shell-env-update (type)
  (let ((var (alist-get type shell-env-vars)))
    (make-local-variable var)
    (-some->> (shell-env-get-file type)
              (f-read-text)
              (s-trim)
              (s-lines)
              (set var))))

(provide 'shell-config)
