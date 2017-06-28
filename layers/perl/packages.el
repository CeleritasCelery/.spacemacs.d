;; Layer by troy.j.hinckley@intel.com
(setq perl-packages
      '(
        (perl-mode :location built-in)
        (cperl-mode :location built-in)
        smartparens
        highlight-numbers
        flycheck
        ;; (perl-completion :toggle (configuration-layer/package-usedp 'auto-complete))
        ;; (anything :toggle (configuration-layer/package-usedp 'perl-completion))
        ;; (plsense :toggle (configuration-layer/package-usedp 'auto-complete))
        ))

(defun perl/init-perl-mode ()
  "Perl mode init function."
  (use-package perl-mode
    :defer t
    :mode "\\.ip_info\\'"
    :mode "\\.spf\\'"
    :mode "\\.espf\\'"
    :mode "\\.espflist\\'"
    :mode "\\.tdr\\'"
    :mode "\\.udf\\'"
    :mode "\\.hdl\\'"
    :mode "\\.rc\\'"
    :mode "\\.cfg.template\\'"
    :mode "\\.txt.template\\'"

    :config
    (progn
      ;; correct for Intel-style binary and hex notation
      (defalias 'my/perl-syntax-propertize-function
        (syntax-propertize-rules
         ("\\('\\)FILL:[0-9a-fA-FXZLH]+" (1 ".")) ;; FILL
         ("\\('\\)DEFAULT\\([]}),;[:space:]]\\|$\\)" (1 ".")) ;; DEFAULT
         ("\\('b\\)\\([01XZLH]+\\|[$][a-zA-Z_\\{][a-zA-Z0-9_]*\\)\\([]}),;[:space:]]\\|$\\)"        (1 "."))   ;; binary
         ("\\('o\\)\\([0-7XZLH]+\\|[$][a-zA-Z_\\{][a-zA-Z0-9_]*\\)\\([]}),;[:space:]]\\|$\\)"       (1 "."))   ;; octal
         ("\\('d\\)\\([0-9XZLH]+\\|[$][a-zA-Z_\\{][a-zA-Z0-9_]*\\)\\([]}),;[:space:]]\\|$\\)"       (1 "."))   ;; decimal
         ("\\('h\\)\\([0-9a-fA-FXZLH]+\\|[$][a-zA-Z_\\{][a-zA-Z0-9_]*\\)\\([]}),;[:space:]]\\|$\\)" (1 ".")))) ;; hex

      (add-hook 'perl-mode-hook
                (lambda ()
                  (add-function :before (local 'syntax-propertize-function)
                                #'my/perl-syntax-propertize-function)))

      (font-lock-add-keywords
       'perl-mode
       '(("\\<repeat\\>" . font-lock-type-face)
         ("\\_<\\(pass\\|set\\|comment\\|compare\\|flush\\|label\\|focus_tap\\|unfocus_tap\\|set_stf_packet\\|focus_stf\\|cycle\\)\\_>" . font-lock-keyword-face))))))

(defun perl/init-cperl-mode ()
  (use-package cperl-mode
    :defer t
    :mode "\\.\\(p[lm]x?\\|P[LM]X?\\)\\'"
    :interpreter "perl"
    :interpreter "perl5"

    :config
    (progn
      (font-lock-remove-keywords
       'cperl-mode
       '(("\\(\\([@%]\\|\\$#\\)[a-zA-Z_:][a-zA-Z0-9_:]*\\)" 1
          (if (eq (char-after (match-beginning 2)) 37)
              'cperl-hash-face 'cperl-array-face)
          t)
         ("\\(\\([$@]+\\)[a-zA-Z_:][a-zA-Z0-9_:]*\\)[ \t]*\\([[{]\\)" 1
          (if (= (- (match-end 2) (match-beginning 2)) 1)
              (if (eq (char-after (match-beginning 3)) 123)
                  'cperl-hash-face 'cperl-array-face)
            font-lock-variable-name-face)
          t)
         ("\\([]}\\\\%@>*&]\\|\\$[a-zA-Z0-9_:]*\\)[ \t]*{[ \t]*\\(-?[a-zA-Z0-9_:]+\\)[ \t]*}"
          (2 font-lock-string-face t)
          ("\\=[ \t]*{[ \t]*\\(-?[a-zA-Z0-9_:]+\\)[ \t]*}" nil nil
           (1 font-lock-string-face t)))
         ("[[ \t{,(]\\(-?[a-zA-Z0-9_:]+\\)[ \t]*=>" 1 font-lock-string-face t)))

      (font-lock-add-keywords
       'cperl-mode
       '(("\\(\\([@%]\\|\\$#\\)[a-zA-Z_:][a-zA-Z0-9_:]*\\)" 1
          (if (nth 4 (syntax-ppss))
              'font-lock-comment-face
            (if (eq (char-after (match-beginning 2)) ?%)
                'cperl-hash-face
              'cperl-array-face))
          t)
         ("\\(\\([$@]+\\)[a-zA-Z_:][a-zA-Z0-9_:]*\\)[ \t]*\\([[{]\\)" 1
          (if (nth 4 (syntax-ppss))
              'font-lock-comment-face
            (if (= (- (match-end 2) (match-beginning 2)) 1)
                (if (eq (char-after (match-beginning 3)) ?{)
                    'cperl-hash-face
                  'cperl-array-face)
              font-lock-variable-name-face))
          t)
         ("\\([]}\\\\%@>*&]\\|\\$[a-zA-Z0-9_:]*\\)[ \t]*{[ \t]*\\(-?[a-zA-Z0-9_:]+\\)[ \t]*}"
          (2 (if (nth 4 (syntax-ppss))
                 'font-lock-comment-face
               'font-lock-string-face) t)
          ("\\=[ \t]*{[ \t]*\\(-?[a-zA-Z0-9_:]+\\)[ \t]*}" nil nil
           (1 (if (nth 4 (syntax-ppss))
                  'font-lock-comment-face
                'font-lock-string-face) t)))
         ("[[ \t{,(]\\(-?[a-zA-Z0-9_:]+\\)[ \t]*=>" 1
          (if (nth 4 (syntax-ppss))
              'font-lock-comment-face
            'font-lock-string-face) t)))

      ;; tab key will ident all marked code when tab key is pressed
      (add-hook 'cperl-mode-hook
                (lambda () (local-set-key (kbd "<tab>") 'indent-for-tab-command)))

      ;; Use less horrible colors for cperl arrays and hashes
      (set-face-attribute 'cperl-array-face nil :foreground  "#DD7D0A"    :background  'unspecified :weight 'unspecified)
      (set-face-attribute 'cperl-hash-face nil  :foreground  "OrangeRed3" :background  'unspecified :weight 'unspecified)
      (setq cperl-highlight-variables-indiscriminately t)

      ;; cperl default settings
      (setq cperl-indent-level 4) ;; 4 spaces is the standard indentation
      (setq cperl-close-paren-offset -4) ;; indent the closing paren back four spaces
      (setq cperl-continued-statement-offset 4) ;; if a statement continues indent it to four spaces
      (setq cperl-indent-parens-as-block t) ;; parentheses are indented with the block and not with scope

      (spacemacs/declare-prefix "mh" "perldoc")
      (spacemacs/declare-prefix "mg" "find-symbol")
      (spacemacs/set-leader-keys-for-major-mode 'cperl-mode "hp" 'cperl-perldoc-at-point)
      (spacemacs/set-leader-keys-for-major-mode 'cperl-mode "hd" 'cperl-perldoc)
      (spacemacs/set-leader-keys-for-major-mode 'cperl-mode "v" 'cperl-select-this-pod-or-here-doc)

      (font-lock-add-keywords 'cperl-mode
                              '(("\\_<const\\|croak\\|carp\\|confess\\|cluck\\_>" . font-lock-keyword-face)))
      (font-lock-add-keywords 'cperl-mode
                              '(("\\_<say\\|any\\_>" . cperl-nonoverridable-face)))
      )))

(defun perl/post-init-flycheck ()
  (spacemacs/add-flycheck-hook 'cperl-mode)
  (setq exec-path (append exec-path '("/usr/intel/pkgs/perl/5.14.1-threads/bin")))
  (setenv "SPF_ROOT" "/p/hdk/cad/spf/latest")
  (setenv "GLOBAL_TOOLS" "/nfs/site/proj/dpg/tools")
  (setenv "SPF_PERL_LIB" "/p/hdk/cad/spf/latest/lib/perl5")
  (setenv "XWEAVE_REPO_ROOT" "/p/hdk/rtl/ip_releases/shdk74/xweave/v17ww14a")
  (setq flycheck-perl-executable "/usr/intel/pkgs/perl/5.14.1/bin/perl")
  (push "/p/hdk/rtl/ip_releases/shdk74/xweave/v17ww14a/lib/perl5" flycheck-perl-include-path)
  (push "/p/hdk/cad/spf/latest/lib/perl5" flycheck-perl-include-path)
  (push "/nfs/site/proj/dpg/tools" flycheck-perl-include-path))

(defun perl/post-init-smartparens ()
  (sp-local-pair 'perl-mode "'" nil :actions nil)
  ;; fix a bug with electric mode and smartparens https://github.com/syl20bnr/spacemacs/issues/480
  (with-eval-after-load "cperl-mode"
    (add-hook 'smartparens-enabled-hook  (lambda () (define-key cperl-mode-map "{" nil)))
    (add-hook 'smartparens-disabled-hook  (lambda () (define-key cperl-mode-map "{" 'cperl-electric-lbrace)))))

(defun perl/post-init-highlight-numbers ()
  (with-eval-after-load "highlight-numbers"
    (puthash
     'perl-mode "\\_<[[:digit:]].*?\\_>\\|'\\(?:h[[:xdigit:]]*?\\|b[01]*?\\|o[0-7]*?\\|d[[:digit:]]*?\\)\\_>"
     highlight-numbers-modelist)))

;; (defun perl/init-anything ()
;;   (use-package anything))

;; (defun perl/init-perl-completion ()
;;   (use-package perl-completion
;;     :config
;;     (progn
;;       (setq plcmp-default-lighter  "")
;;       (add-hook
;;        'cperl-mode-hook
;;        (lambda ()
;;          (auto-complete-mode t)
;;          (perl-completion-mode t)
;;          (make-variable-buffer-local 'ac-sources)
;;          (setq ac-sources
;;                '(ac-source-perl-completion)))))))

;; (defun perl/init-plsense ()
;;   (use-package plsense
;;     :defer t
;;     :init
;;     (progn
;;       (setenv "PERL5LIB" "/nfs/site/home/tjhinckl/perl5/lib/perl5:/p/hdk/rtl/ip_releases/shdk74/xweave/v17ww14a/lib/perl5:/p/hdk/cad/spf/latest/lib/perl5:/nfs/site/proj/dpg/tools")
;;       (setenv "PATH" (concat (getenv "PATH") ":/nfs/site/home/tjhinckl/perl5/bin"))
;;       (setq plsense--config-path "~/perl5/bin")
;;       (plsense-config-default)
;;       (spacemacs/set-leader-keys-for-major-mode 'cperl-mode
;;         "p" 'plsense-popup-help
;;         "h" 'plsense-display-help-buffer
;;         "d" 'plsense-jump-to-definition))))
