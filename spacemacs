;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distrubutions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs
   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation 'unused
   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t
   ;; If non-nil layers with lazy install support are lazy installed.
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()
   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press <SPC f e R> (Vim style) or
     ;; <M-m f e R> (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     markdown
     csv
     python
     debug
     graphviz
     vimscript
     shell-scripts
     perl
     c-c++
     helm
     tmux
     imenu-list
     syntax-checking
     (auto-completion :variables
                      auto-completion-enable-snippets-in-popup t)
     better-defaults
     evil-cleverparens
     ranger
     emacs-lisp
     git
     version-control
     no-dots
     colors
     (org :variables org-enable-github-support t)
     (evil-snipe :variables evil-snipe-enable-alternate-f-and-t-behaviors t)
     (shell :variables shell-default-height 30 shell-default-position 'bottom)
     (spell-checking :variables spell-checking-enable-by-default nil)
     )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages
   '(
     json-mode
     json-snatcher
     eimp
     image+
     vlf
     (itpp-mode    :location local)
     (reglist-mode :location local)
     (specman-mode :location local)
     (sgdc-mode    :location local)
     (spfspec-mode :location local)
     (functions    :location local)
     )
   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '(org-projectile projectile)
   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '()
   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and uninstall any
   ;; unused packages as well as their unused dependencies.
   ;; `used-but-keep-unused' installs only the used packages but won't uninstall
   ;; them if they become unused. `all' installs *all* packages supported by
   ;; Spacemacs and never uninstall them. (default is `used-only')
   dotspacemacs-install-packages 'used-but-keep-unused))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https nil
   ;; Maximum allowed time in seconds to contact an ELPA repository.
   dotspacemacs-elpa-timeout 10
   ;; If non nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update t
   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'.
   dotspacemacs-elpa-subdirectory nil
   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'vim
   ;; If non nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'official
   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `bookmarks' `projects' `agenda' `todos'."
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '((recents . 5) (projects . 7))
   ;; True if the home buffer should respond to resize events.
   dotspacemacs-startup-buffer-responsive t
   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(spacemacs-dark spacemacs-light)
   ;; If non nil the cursor color matches the state color in GUI Emacs.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.
   dotspacemacs-default-font '("Source Code Pro" :size 11 :powerline-scale 1.1)
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The key used for Emacs commands (M-x) (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"
   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"
   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m")
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs C-i, TAB and C-m, RET.
   ;; Setting it to a non-nil value, allows for separate commands under <C-i>
   ;; and TAB or <C-m> and RET.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil
   ;; If non nil `Y' is remapped to `y$' in Evil states. (default nil)
   dotspacemacs-remap-Y-to-y$ t
   ;; If non-nil, the shift mappings `<' and `>' retain visual state if used
   ;; there. (default t)
   dotspacemacs-retain-visual-state-on-shift t
   ;; If non-nil, J and K move lines up and down when in visual mode.
   ;; (default nil)
   dotspacemacs-visual-line-move-text t
   ;; If non nil, inverse the meaning of `g' in `:substitute' Evil ex-command.
   ;; (default nil)
   dotspacemacs-ex-substitute-global nil
   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"
   ;; If non nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil
   ;; If non nil then the last auto saved layouts are resume automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil
   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 50
   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache
   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5
   ;; If non nil, `helm' will try to minimize the space it uses. (default nil)
   dotspacemacs-helm-resize nil
   ;; if non nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header nil
   ;; define the position to display `helm', options are `bottom', `top',
   ;; `left', or `right'. (default 'bottom)
   dotspacemacs-helm-position 'bottom
   ;; Controls fuzzy matching in helm. If set to `always', force fuzzy matching
   ;; in all non-asynchronous sources. If set to `source', preserve individual
   ;; source settings. Else, disable fuzzy matching in all sources.
   ;; (default 'always)
   dotspacemacs-helm-use-fuzzy 'always
   ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
   ;; several times cycle between the kill ring content. (default nil)
   dotspacemacs-enable-paste-transient-state t
   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4
   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom
   ;; If non nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t
   ;; If non nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup t
   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90
   ;; If non nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t
   ;; If non nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t
   ;; If non nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols t
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t
   ;; If non nil line numbers are turned on in all `prog-mode' and `text-mode'
   ;; derivatives. If set to `relative', also turns on relative line numbers.
   ;; (default nil)
   dotspacemacs-line-numbers t
   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode t
   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc…
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil
   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all
   ;; If non nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server t
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
   ;; (default '("ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now. (default nil)
   dotspacemacs-default-package-repository nil
   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed'to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup nil
   ))

(defun dotspacemacs/user-init ()
  ;;   "Initialization function for user code.
  ;; It is called immediately after `dotspacemacs/init', before layer configuration
  ;; executes.
  ;;  This function is mostly useful for variables that need to be set
  ;; before packages are loaded. If you are unsure, you should try in setting them in
  ;; `dotspacemacs/user-config' first."

  (add-to-list 'auto-mode-alist '("\\.summary\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.do\\'"      . tcl-mode))
  (add-to-list 'auto-mode-alist '("\\.vs\\'"      . verilog-mode))
  (add-to-list 'auto-mode-alist '("\\.svh\\'"     . verilog-mode))
  (add-to-list 'auto-mode-alist '("\\.vf\\'"      . verilog-mode))
  (add-to-list 'auto-mode-alist '("\\.hier\\'"    . verilog-mode))
  (add-to-list 'auto-mode-alist '("rc\\'"         . conf-unix-mode))
  (add-to-list 'interpreter-mode-alist '("gmake"  . makefile-mode))

  ;; get around old magit git version problem
  (setq magit-git-executable "/usr/intel/pkgs/git/2.8.4/bin/git")
  (setq git-gutter+-git-executable "/usr/intel/pkgs/git/2.8.4/bin/git")
  (setq evil-want-abbrev-expand-on-insert-exit nil) ;; don't preform an abbrev expansion on insert state exit
  (setq exec-path-from-shell-check-startup-files nil)

  )

(defun dotspacemacs/user-config ()
  ;;   "Configuration function for user code.
  ;; This function is called at the very end of Spacemacs initialization after
  ;; layers configuration.
  ;; This is the place where most of your configurations should be done. Unless it is
  ;; explicitly specified that a variable should be set before a package is loaded,
  ;; you should place your code here."

  (defconst lisp--prettify-symbols-alist
    '(("lambda" . ?λ)   ; Shrink this
      ("."      . ?•))) ; Enlarge this
  (add-hook 'emacs-lisp-mode-hook 'prettify-symbols-mode)
  (add-hook 'emacs-lisp-mode-hook 'aggressive-indent-mode)
  (spacemacs|diminish orgtbl-mode)

  (add-to-list 'load-path "/nfs/site/home/tjhinckl/personal/github/company-plsense/")
  (setenv "PERL5LIB" "/nfs/site/home/tjhinckl/perl5/lib/perl5:/p/hdk/rtl/ip_releases/shdk74/xweave/v17ww14a/lib/perl5:/p/hdk/cad/spf/latest/lib/perl5:/nfs/site/proj/dpg/tools")
  (require 'company-plsense)
  (setq company-plsense-enabled-modes '(cperl-mode))
  (company-plsense-setup)

  (use-package spfspec-mode
    :mode "\\.spfspec\\'"
    :config
    (with-eval-after-load "highlight-numbers"
      (puthash
       'spfspec-mode "\\_<[[:digit:]].*?\\_>\\|'\\(?:h[[:xdigit:]]*?\\|b[01]*?\\|o[0-7]*?\\|d[[:digit:]]*?\\)\\_>"
       highlight-numbers-modelist)))

  (use-package reglist-mode
    :mode "\\.list\\'"
    :config
    (progn
      (add-hook 'reglist-mode-hook 'color-identifiers-mode)
      (with-eval-after-load "color-identifiers-mode"
        (add-to-list
         'color-identifiers:modes-alist
         `(reglist-mode
           . ("^[[:space:]]*[-.+]" "\\_<\\([[:alpha:]]+[[:alnum:]]*\\)\\_>"
              (nil font-lock-keyword-face font-lock-function-name-face))))

        (defun color-identifiers:colorize (limit)
          (color-identifiers:scan-identifiers
           (lambda (start end)
             (let* ((identifier (buffer-substring-no-properties start end))
                    (hex (color-identifiers:color-identifier identifier)))
               (when hex
                 (put-text-property start end 'face `(:foreground ,hex))
                 ;; (add-face-text-property start end '(:underline t))
                 (add-face-text-property start end '(:weight bold))
                 (put-text-property start end 'color-identifiers:fontified t))))
           limit)))))

  (use-package itpp-mode
    :mode "\\.itpp\\'"
    )

  (use-package sgdc-mode
    :mode "\\.sgdc\\'"
    :mode "\\.opt\\'"
    )

  (use-package specman-mode
    :mode "\\.e\\'"
    :mode "\\.e3\\'"
    :mode "\\.load\\'"
    :mode "\\.ecom\\'"
    :mode "\\.etst\\'"
    )

  (setq minimap-window-location 'right)
  (setq minimap-enlarge-certain-faces 'always)
  (setq minimap-width-fraction 0.1)

  (setq-default tab-width 4) ;; colunms per "tab"
  (setq-default indent-tabs-mode nil) ;; use spaces instead of tabs by default
  (setq-default require-final-newline t) ;; always require a newline at end of the file for compatibility
  (setq-default visual-line-mode t) ;; cursor keys move by visual lines, not logical ones
  (setq-default evil-snipe-scope 'line) ;; snipe current line only
  (setq-default evil-snipe-repeat-scope 'line) ;; repeated snipes will stay on line
  (setq evil-search-module 'evil-search) ;; use evil search so we can have very-magic mode enabled
  (setq evil-magic 'very-magic) ;; very-magic mode matches perl regex syntax
  (setq evil-ex-search-vim-style-regexp t) ;; translate vim escape characters to emacs regex
  (setq mouse-drag-copy-region nil) ;; stops selection with a mouse being immediately injected to the kill ring
  (setq select-enable-clipboard t) ;; tell emacs to use the system clipboard ...
  (setq select-enable-primary nil) ;; and don't use the primary
  (setq interprogram-paste-function 'x-cut-buffer-or-selection-value) ;; make inter-program paste work correctly
  (setq recentf-max-saved-items 300) ;; I want a lot of recent items
  (setq projectile-enable-caching t) ;; cache the project so it doesn't take an eternity to load
  (setq delete-by-moving-to-trash nil) ;; don't move deleted files to my trash, which is in my home disk
  (setq dired-recursive-deletes 'always) ;; don't ask for confirmation to delete files
  (setq vc-follow-symlinks t) ; follow symlinks
  (setq large-file-warning-threshold nil) ;; Don't warn me when opening large files
  (setq no-dots-whitelist '("*Helm file completions*")) ;; show directory when selecting a directory
  (setq read-quoted-char-radix 16) ;; show unpritable chars in hex
  (setq helm-buffer-max-length 60) ;; max buffer column larger for helm
  (setq perl5-perltidy-executable "/nfs/site/home/tjhinckl/perl5/bin/perltidy")
  (add-hook 'emacs-lisp-mode-hook 'evil-cleverparens-mode)

  (fset 'evil-visual-update-x-selection 'ignore) ;; don't update the primary when in evil
  (define-key global-map (kbd "<S-down-mouse-1>") 'mouse-save-then-kill) ;; use shift-click to mark a region
  (global-linum-mode) ; Show line numbers by default

  ;; C-i can be used to move forward in cursor jumps, but
  ;; Emacs binds it to TAB, so we rebinding it to H-i. Though
  ;; this probably won't work in the terminal
  (define-key input-decode-map (kbd "C-i") (kbd "H-i"))
  (global-set-key (kbd "H-i") 'evil-jump-forward)
  (push "/nfs/site/home/tjhinckl/personal/verilator-3.884/include" flycheck-clang-include-path)

  (add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p nil t) ;; make scripts executable on save

  (add-hook 'focus-in-hook 'redraw-display) ;; display may be out of focus when switching workspaces

  ;; set arrow keys in isearch and evil search. up/down is history. press Return to exit
  ;; (define-key isearch-mode-map (kbd "<up>") 'isearch-ring-retreat)
  ;; (define-key isearch-mode-map (kbd "<down>") 'isearch-ring-advance)

  (define-key evil-insert-state-map "\C-e" 'mwim-end-of-code-or-line);; make end-of-line work in insert
  (define-key evil-insert-state-map "\C-a" 'mwim-beginning-of-code-or-line);; make end-of-line work in insert

  (spacemacs/declare-prefix "o" "user-defined")
  (spacemacs/set-leader-keys "ou" 'untabify) ;; replace tabs with spaces
  (spacemacs/set-leader-keys "ow" 'whitespace-mode) ;; toggle whitespace mode

  (define-key evil-normal-state-map "gh" 'backward-list) ; sp-backward-down-sexp
  (define-key evil-normal-state-map "gj" 'down-list) ; sp-up-sexp
  (define-key evil-normal-state-map "gk" 'backward-up-list) ; sp-backward-up-sexp
  (define-key evil-normal-state-map "gl" 'forward-list) ; sp-down-sexp

  (spacemacs/set-leader-keys "jj" 'evil-avy-goto-word-or-subword-1)
  (spacemacs/set-leader-keys "jc" 'evil-avy-goto-char)
  (spacemacs/set-leader-keys "jC" 'evil-avy-goto-char-2)

  (spacemacs/set-leader-keys "ws" 'split-window-below-and-focus)
  (spacemacs/set-leader-keys "wS" 'split-window-below)
  (spacemacs/set-leader-keys "wv" 'split-window-right-and-focus)
  (spacemacs/set-leader-keys "wV" 'split-window-right)

  (add-hook 'emacs-lisp-mode-hook 'rainbow-mode)

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

  ;; Don't auto-pair single quote in verilog mode
  (when (configuration-layer/package-usedp 'smartparens)
    (sp-local-pair 'verilog-mode "'" nil :actions nil))

  (setq git-gutter+-diff-options '("-w")) ;; ignore whitespace in gutter diffs

  (spacemacs/set-leader-keys-for-major-mode 'json-mode "p" 'jsons-print-path)

  (spacemacs/add-flycheck-hook 'verilog-mode)

  (require 'private-functions)

  )

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(magit-diff-section-arguments (quote ("--no-ext-diff")))
 '(package-selected-packages
   (quote
    (eros evil-cleverparens paredit mmm-mode markdown-toc markdown-mode gh-md stickyfunc-enhance srefactor sublimity minimap realgud-pry realgud test-simple loc-changes load-relative winum powerline spinner hydra parent-mode hide-comnt projectile request pkg-info epl flx evil-nerd-commenter smartparens iedit anzu evil goto-chg undo-tree highlight diminish bind-map bind-key helm avy helm-core async popup helm-perldoc deferred unfill json-snatcher json-reformat insert-shebang fuzzy disaster company-c-headers cmake-mode clang-format f s dash pyenv-mode company-anaconda anaconda-mode yapfify pyvenv pytest py-isort pip-requirements live-py-mode hy-mode helm-pydoc cython-mode pythonic skewer-mode simple-httpd multiple-cursors js2-mode dash-functional tern anything perl-completion exec-path-from-shell evil-mc plsense yaxception org gitignore-mode fringe-helper git-gutter+ git-gutter magit magit-popup git-commit with-editor packed rainbow-mode rainbow-identifiers color-identifiers-mode python-mode vlf ox-gfm imenu-list flyspell-correct-helm flyspell-correct auto-dictionary eimp image+ graphviz-dot-mode org-projectile org-present org-pomodoro alert log4e gntp org-download htmlize gnuplot pos-tip flycheck company yasnippet auto-complete smart-tabs-mode xterm-color ws-butler window-numbering which-key wgrep web-beautify volatile-highlights vimrc-mode vi-tilde-fringe uuidgen use-package toc-org tabbar spacemacs-theme spaceline smex smeargle shell-pop restart-emacs ranger rainbow-delimiters quelpa popwin persp-mode pcre2el paradox orgit org-plus-contrib org-bullets open-junk-file neotree mwim multi-term move-text magit-gitflow macrostep lorem-ipsum livid-mode linum-relative link-hint json-mode js2-refactor js-doc ivy-hydra info+ indent-guide ido-vertical-mode hungry-delete hl-todo highlight-parentheses highlight-numbers highlight-indentation help-fns+ helm-themes helm-swoop helm-projectile helm-mode-manager helm-make helm-gitignore helm-flx helm-descbinds helm-company helm-c-yasnippet helm-ag google-translate golden-ratio gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe git-gutter-fringe+ flycheck-pos-tip flx-ido fish-mode fill-column-indicator fancy-battery eyebrowse expand-region evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-snipe evil-search-highlight-persist evil-numbers evil-nerd-commenter-mc evil-matchit evil-magit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-ediff evil-args evil-anzu eval-sexp-fu eshell-z eshell-prompt-extras esh-help elisp-slime-nav dumb-jump diff-hl define-word dactyl-mode csv-mode counsel-projectile company-tern company-statistics company-shell column-enforce-mode coffee-mode clean-aindent-mode auto-yasnippet auto-highlight-symbol auto-compile aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line ac-ispell)))
 '(safe-local-variable-values
   (quote
    ((eval font-lock-add-keywords nil
           (\`
            (((\,
               (concat "("
                       (regexp-opt
                        (quote
                         ("sp-do-move-op" "sp-do-move-cl" "sp-do-put-op" "sp-do-put-cl" "sp-do-del-op" "sp-do-del-cl"))
                        t)
                       "\\_>"))
              1
              (quote font-lock-variable-name-face)))))
     (cperl-indent-parens-as-block . t)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ediff-current-diff-A ((t (:background "#32322c" :foreground "#b1951d"))))
 '(ediff-current-diff-B ((t (:background "#32322c" :foreground "#b1951d"))))
 '(ediff-current-diff-C ((t (:background "#32322c" :foreground "#b1951d"))))
 '(ediff-even-diff-A ((t (:background "#424245" :foreground "Gray70"))))
 '(ediff-even-diff-B ((t (:background "#424245" :foreground "Gray70"))))
 '(ediff-even-diff-C ((t (:background "#424245" :foreground "Gray70"))))
 '(ediff-fine-diff-A ((t (:background "#293235" :foreground "#67b11d"))))
 '(ediff-fine-diff-B ((t (:background "#293235" :foreground "#67b11d"))))
 '(ediff-fine-diff-C ((t (:background "#293235" :foreground "#67b11d"))))
 '(ediff-odd-diff-A ((t (:background "#454242" :foreground "Gray70"))))
 '(ediff-odd-diff-B ((t (:background "#454242" :foreground "Gray70"))))
 '(ediff-odd-diff-C ((t (:background "#454242" :foreground "Gray70"))))
 '(minimap-active-region-background ((t :inherit highlight)))
 '(minimap-font-face ((t (:height 20 :family "DejaVu Sans Mono")))))
