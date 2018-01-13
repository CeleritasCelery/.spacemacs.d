;;; config for `shell' -*- lexical-binding: t -*-

(require 'dash-functional)

(setq shell-file-name "bash")
(setq comint-scroll-to-bottom-on-input t)

(when (configuration-layer/package-usedp 'evil-cleverparens)
  (add-hook 'emacs-lisp-mode-hook 'evil-cleverparens-mode))

(defun track-shell-directory/procfs (str)
  (prog1 str
    (when (string-match comint-prompt-regexp str)
      (cd (file-symlink-p
           (format "/proc/%s/cwd" (process-id
                                   (get-buffer-process
                                    (current-buffer)))))))))

;; (defvar company-files--min-depth 4
;;   "Minimum depth from root before using `company-files'
;; because is is painfully slow sometimes.")

;; (defun company-files--skip-root-dir (ret-val)
;;   (unless (and (s-prefix? (f-root) ret-val)
;;                (> company-files--min-depth (f-depth ret-val)))
;;     ret-val))
;; (advice-add 'company-files--grab-existing-name :filter-return #'company-files--skip-root-dir)

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


(defun cel/company-dabbrev--skip-numbers (ret-val)
  (unless (and ret-val
               (s-matches? (rx bos (+ digit) eos) ret-val))
    ret-val))
(advice-add 'company-dabbrev--prefix :filter-return #'cel/company-dabbrev--skip-numbers)

(defun cel/company-select-prev-or-comint-match-input (&optional _)
  (when (and (eq major-mode 'shell-mode)
             (eq company-selection 0))
    (company-abort)
    (call-interactively 'comint-previous-matching-input-from-input)))
(advice-add 'company-select-previous :before-until #'cel/company-select-prev-or-comint-match-input)

;; messing with removing ret to select candiates because it makes it very hard to use
;; company in REPL's and org mode. Now just use `C-l'
(with-eval-after-load "company"
  (define-key company-active-map (kbd "RET") nil)
  (define-key company-active-map [return] nil))

(substitute-key-definition 'comint-previous-input 'comint-previous-matching-input-from-input shell-mode-map)
(substitute-key-definition 'comint-next-input 'comint-next-matching-input-from-input shell-mode-map)
(define-key shell-mode-map (kbd "C-S-k") 'comint-previous-prompt)
(define-key shell-mode-map (kbd "C-S-j") 'comint-next-prompt)

(with-eval-after-load 'company-dabbrev-code
  (add-to-list 'company-dabbrev-code-modes 'comint-mode))

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

;; (use-package pcomplete-extension
;;   :ensure t)


;; (use-package bash-completion
;;   :init
;;   (add-hook 'shell-dynamic-complete-functions
;;             'bash-completion-dynamic-complete))

(spacemacs|defvar-company-backends shell-mode)
(setq company-backends-shell-mode '((company-env company-command) company-async-files company-fish company-dabbrev-code))
(spacemacs|add-company-hook shell-mode)

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
