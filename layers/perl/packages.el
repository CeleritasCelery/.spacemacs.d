;; Layer by troy.j.hinckley@intel.com
(defconst perl-packages
  '(
    (perl-mode :location built-in)
    (cperl-mode :location built-in)
    smartparens
    highlight-numbers
    flycheck
    realgud
    company
    (company-plsense :toggle (configuration-layer/package-usedp 'company) :location local)
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
         ("\\('\\)" (1 "."))))

      (add-hook 'perl-mode-hook
                (lambda ()
                  (setq prettify-symbols-alist perl5--prettify-symbols-alist)
                  (prettify-symbols-mode)
                  (add-function :before (local 'syntax-propertize-function)
                                #'my/perl-syntax-propertize-function)))

      (font-lock-add-keywords
       'perl-mode
       `((,(rx symbol-start "repeat" symbol-end) . font-lock-type-face)
         ,(rx symbol-start (or "pass"
                               "set"
                               "comment"
                               "compare"
                               "flush"
                               "label"
                               "focus_tap"
                               "unfocus_tap"
                               "set_stf_packet"
                               "focus_stf"
                               "cycle")
              symbol-end))))))

(defun perl/init-cperl-mode ()
  (use-package cperl-mode
    :defer t
    :mode "\\.\\(p[lm]x?\\|P[LM]X?\\)\\'"
    :interpreter "perl"
    :interpreter "perl5"
    :init
    (progn
      ;; ligatures for hasklig

      ;; ("==" . (?═ (Br . Bl) ?═))
      ;; ("=>" . (?═ (tr . tc) ?═ (Br Br 70 0) ?>))

      (setq
       ;; highlight all scalar variables not just the instantiation
       cperl-highlight-variables-indiscriminately t
       cperl-indent-level 4        ; 4 spaces is the standard indentation
       cperl-close-paren-offset -4 ; indent the closing paren back four spaces
       cperl-continued-statement-offset 4 ; if a statement continues indent it to four spaces
       cperl-indent-parens-as-block t)) ; parentheses are indented with the block and not with scope

    :config
    (progn
      ;; don't highlight arrays and hashes inside comments
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
       `((,(rx (group-n 1
                        (group-n 2 (or (any "@%")
                                       "$#"))
                        (any alpha "_:")
                        (0+ (any alnum "_:"))))
          1
            (if (eq (char-after (match-beginning 2)) ?%)
                'cperl-hash-face
              'cperl-array-face))
         ("\\(\\([$@]+\\)[a-zA-Z_:][a-zA-Z0-9_:]*\\)[ \t]*\\([[{]\\)" 1
            (if (= (- (match-end 2) (match-beginning 2)) 1)
                (if (eq (char-after (match-beginning 3)) ?{)
                    'cperl-hash-face
                  'cperl-array-face)
              font-lock-variable-name-face))
         (,(rx (group-n 1 "@") "{")
          1
          (if (eq (char-after (match-beginning 1)) ?%)
              'cperl-hash-face
            'cperl-array-face))
         ("\\([]}\\\\%@>*&]\\|\\$[a-zA-Z0-9_:]*\\)[ \t]*{[ \t]*\\(-?[a-zA-Z0-9_:]+\\)[ \t]*}"
          (2 'font-lock-string-face)
          ("\\=[ \t]*{[ \t]*\\(-?[a-zA-Z0-9_:]+\\)[ \t]*}" nil nil
           (1 'font-lock-string-face)))
         ("[[ \t{,(]\\(-?[a-zA-Z0-9_:]+\\)[ \t]*=>" 1 'font-lock-string-face)))

      ;; tab key will ident all marked code when tab key is pressed
      (add-hook 'cperl-mode-hook (lambda ()
                                   (local-set-key (kbd "<tab>") 'indent-for-tab-command)
                                   (modify-syntax-entry ?: "." (syntax-table))
                                   (setq prettify-symbols-alist perl5--prettify-symbols-alist)
                                   (prettify-symbols-mode)))

      ;; Use less horrible colors for cperl arrays and hashes
      (set-face-attribute 'cperl-array-face nil :foreground  "#DD7D0A"    :background 'unspecified :weight 'unspecified)
      (set-face-attribute 'cperl-hash-face nil  :foreground  "OrangeRed3" :background 'unspecified :weight 'unspecified)

      (spacemacs/declare-prefix "m=" "format")
      (spacemacs/declare-prefix "mg" "find-symbol")
      (spacemacs/declare-prefix "mh" "perldoc")
      (spacemacs/set-leader-keys-for-major-mode 'cperl-mode
        "==" 'spacemacs/perltidy-format
        "=b" 'spacemacs/perltidy-format-buffer
        "=f" 'spacemacs/perltidy-format-function
        "hh" 'cperl-perldoc-at-point
        "hd" 'cperl-perldoc
        "v" 'cperl-select-this-pod-or-here-doc)

      (font-lock-add-keywords
       'cperl-mode
       `(,(rx symbol-start (or "pass"
                               "set"
                               "comment"
                               "compare"
                               "flush"
                               "label"
                               "focus_tap"
                               "unfocus_tap"
                               "set_stf_packet"
                               "focus_stf"
                               "cycle"
                               "const"
                               "croak"
                               "carp"
                               "confess"
                               "cluck")
              symbol-end)
         (,(rx symbol-start "repeat" symbol-end) . font-lock-type-face)
         (,(rx symbol-start (or "say" "any") symbol-end) . cperl-nonoverridable-face))))))

(defun perl/post-init-flycheck ()
  (spacemacs/add-flycheck-hook 'cperl-mode)
  (setenv "SPF_ROOT" "/p/hdk/cad/spf/latest")
  (setenv "VALID_ROOT" "/p/hdk/rtl/valid/shdk74")
  (setenv "VTLIB" "/p/hdk/rtl/valid/shdk74/lib")
  (setenv "SPF_PERL_LIB" "/p/hdk/cad/spf/latest/lib/perl5")
  (setenv "GLOBAL_TOOLS" "/nfs/site/proj/dpg/tools")
  (setenv "XWEAVE_REPO_ROOT" "/p/hdk/rtl/ip_releases/shdk74/xweave/v17ww43a")
  (setenv "IDS_HOME" "/p/hdk/rtl/cad/x86-64_linux26/dteg/ideas_shell/0.15.1")

  ;; ISC required variables
  (setenv "RTL_CAD_ROOT" "/p/hdk/rtl/cad/x86-64_linux26")
  (setenv "RTL_PROJ_CFG" "/p/hdk/rtl/proj_tools/proj_cfg")
  (setenv "CFG_PROJECT" "shdk74")
  (setenv "RTL_PROJ_BIN" "/p/hdk/rtl/proj_tools/proj_binx/shdk74/latest")
  (setenv "RTL_PROJ_TOOLS" "/p/hdk/rtl/proj_tools")

  (setq flycheck-perl-executable "/usr/intel/pkgs/perl/5.14.1/bin/perl")
  (setq flycheck-perl-perlcritic-executable "/usr/intel/pkgs/perl/5.14.1-threads/bin/perlcritic")
  (setq flycheck-perl-include-path '("/p/hdk/cad/spf/latest/lib/perl5" ;; SPF library
                                     "../lib/perl5" ;; DTEG ultiscan
                                     "../../lib/perl5" ;; DTEG STF
                                     ".."))) ;; library files need to see the library ¯\_(ツ)_/¯

(defun perl/post-init-smartparens ()
  (sp-local-pair 'perl-mode "'" nil :actions nil)
  ;; fix a bug with electric mode and smartparens https://github.com/syl20bnr/spacemacs/issues/480
  (with-eval-after-load "cperl-mode"
    (add-hook 'smartparens-enabled-hook  (lambda () (define-key cperl-mode-map "{" nil)))
    (add-hook 'smartparens-disabled-hook  (lambda () (define-key cperl-mode-map "{" 'cperl-electric-lbrace)))))

(defun perl/post-init-highlight-numbers ()
  (with-eval-after-load "highlight-numbers"
    (puthash
     'perl-mode "\\_<[[:digit:]].*?\\_>\\|'\\(?:h[[:xdigit:]xX]*?\\|b[01xX]*?\\|o[0-7xX]*?\\|d[[:digit:]xX]*?\\)\\_>"
     highlight-numbers-modelist)))

(defun perl/pre-init-realgud()
  (spacemacs/add-realgud-debugger 'cperl-mode "trepan.pl"))

(defun perl/init-company-plsense()
  (use-package company-plsense
    :defer t
    :init
    (push 'company-plsense company-backends-cperl-mode)))

(defun perl/post-init-company ()
  (spacemacs|add-company-hook cperl-mode))
