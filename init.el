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
     html
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
     imenu-list
     syntax-checking
     (auto-completion :variables auto-completion-enable-sort-by-usage t
                      spacemacs-default-company-backends '(company-async-files
                                                           (company-dabbrev-code company-keywords)
                                                           company-dabbrev))
     better-defaults
     evil-cleverparens
     ranger
     emacs-lisp
     git
     version-control
     colors
     journal
     org
     (evil-snipe :variables evil-snipe-enable-alternate-f-and-t-behaviors t)
     (shell :variables shell-default-shell 'shell)
     (spell-checking :variables spell-checking-enable-by-default nil)
     ;; custom layers
     major-modes
     config
     shell-config
     )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages
   '(
     ox-twbs
     (rsw-elisp :location (recipe :fetcher github :repo "rswgnu/rsw-elisp"))
     elisp-def
     quickrun
     dash-functional
     suggest
     helpful
     json-mode
     json-snatcher
     eimp
     image+
     vlf
     )
   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '(org-projectile projectile)
   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '(company-plsense origami bug-reference hl-todo link-hint)
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
   dotspacemacs-startup-lists '((recents . 5) (projects . 4) )
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
   dotspacemacs-default-font '("Hasklig" :size 11 :powerline-scale 1.1)
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
   dotspacemacs-distinguish-gui-tab t
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
   dotspacemacs-ex-substitute-global t
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
   dotspacemacs-enable-paste-transient-state nil
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
   dotspacemacs-line-numbers nil
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
  (add-to-list 'auto-mode-alist '("\\.rdl\\'"     . verilog-mode))
  (add-to-list 'auto-mode-alist '("rc\\'"         . conf-unix-mode))
  (add-to-list 'interpreter-mode-alist '("gmake"  . makefile-mode))

  (setq custom-file "~/.emacs.d/private/local/custom.el")
  (load custom-file 'noerror)

  (setq magit-git-executable "/usr/intel/pkgs/git/2.12.0/bin/git" ;; get around old magit git version problem
        git-gutter+-git-executable "/usr/intel/pkgs/git/2.12.0/bin/git" ;; get around old magit git version problem
        evil-want-abbrev-expand-on-insert-exit nil ;; don't preform an abbrev expansion on insert state exit
        exec-path-from-shell-check-startup-files nil)
  )

(defun dotspacemacs/user-config ()
  ;;   "Configuration function for user code.
  ;; This function is called at the very end of Spacemacs initialization after
  ;; layers configuration.
  ;; This is the place where most of your configurations should be done. Unless it is
  ;; explicitly specified that a variable should be set before a package is loaded,
  ;; you should place your code here."

  (setq company-plsense-executable "/nfs/site/home/tjhinckl/perl5/bin/plsense")
  (defconst lisp--prettify-symbols-alist
    '(("lambda" . ?λ)                   ; Shrink this
      ("."      . ?•)))                 ; Enlarge this
  (add-hook 'emacs-lisp-mode-hook 'prettify-symbols-mode)
  (add-hook 'emacs-lisp-mode-hook 'aggressive-indent-mode)
  (spacemacs|diminish orgtbl-mode)

  (setq prettify-symbols-unprettify-at-point t)

  (setq-default require-final-newline t ;; always require a newline at end of the file for compatibility
                visual-line-mode t ;; cursor keys move by visual lines, not logical ones
                evil-snipe-scope 'line         ;; snipe current line only
                evil-snipe-repeat-scope 'line) ;; repeated snipes will stay on line

  (setq evil-search-module 'evil-search ;; use evil search so we can have very-magic mode enabled
        evil-magic 'very-magic ;; very-magic mode matches perl regex syntax
        evil-ex-search-vim-style-regexp t ;; translate vim escape characters to emacs regex
        interprogram-paste-function 'x-cut-buffer-or-selection-value ;; make inter-program paste work correctly
        recentf-max-saved-items 300 ;; I want a lot of recent items
        projectile-enable-caching t ;; cache the project so it doesn't take an eternity to load
        delete-by-moving-to-trash nil ;; don't move deleted files to my trash, which is in my home disk
        dired-recursive-deletes 'always ;; don't ask for confirmation to delete files
        vc-follow-symlinks t            ; follow symlinks
        large-file-warning-threshold 60000000 ;; above 60M prompt to open file literally or with vlf
        read-quoted-char-radix 16  ;; show unpritable chars in hex
        evil-escape-key-sequence "jk" ;; just mash both of these
        evil-escape-unordered-key-sequence t ;; lets me use jk or kj
        helm-buffer-max-length 60) ;; increase max buffer column for helm

  (dired-async-mode)

  (fset 'evil-visual-update-x-selection 'ignore) ;; don't update the primary when in evil
  (define-key global-map (kbd "<S-down-mouse-1>") 'mouse-save-then-kill) ;; use shift-click to mark a region

  ;; C-i can be used to move forward in cursor jumps, but
  ;; Emacs binds it to TAB, so we rebinding it to H-i. Though
  ;; this probably won't work in the terminal
  (define-key input-decode-map (kbd "C-i") (kbd "H-i"))
  (global-set-key (kbd "H-i") 'evil-jump-forward)

  (when (configuration-layer/package-usedp 'flycheck)
    (add-to-list 'flycheck-clang-include-path "/nfs/site/home/tjhinckl/personal/verilator-3.884/include")
    (add-to-list 'flycheck-clang-include-path "/nfs/site/home/tjhinckl/workspace/dteg_tools-ultiscan/template/verif/tests/ids")
    (add-to-list 'flycheck-clang-include-path "/p/hdk/rtl/cad/x86-64_linux26/dteg/ideas_shell/0.15.1/ISC/include")
    (add-to-list 'flycheck-clang-args "-std=c++11")
    (setq flycheck-gcc-language-standard "c++11")
    (spacemacs/add-flycheck-hook 'verilog-mode)
    (put 'flycheck-perl-executable 'safe-local-variable 'stringp))

  (add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p) ;; make scripts executable on save

  (add-hook 'focus-in-hook 'redraw-display) ;; display may be out of focus when switching workspaces

  ;; set arrow keys in isearch and evil search. up/down is history. press Return to exit
  (define-key isearch-mode-map (kbd "C-k") 'isearch-ring-retreat)
  (define-key isearch-mode-map (kbd "C-j") 'isearch-ring-advance)

  (evil-global-set-key 'insert (kbd "C-h") 'delete-backward-char)
  (evil-global-set-key 'insert (kbd "C-y") 'yank)
  (evil-global-set-key 'insert (kbd "C-e") 'mwim-end-of-code-or-line);; make end-of-line work in insert
  (evil-global-set-key 'insert (kbd "C-a") 'mwim-beginning-of-code-or-line) ;; make end-of-line work in insert
  (evil-global-set-key 'normal (kbd "C-f") 'forward-char) ;; allow forward and backwards in normal state
  (evil-global-set-key 'normal (kbd "C-b") 'backward-char) ;; allow backward char in normal state
  (evil-global-set-key 'normal (kbd "0"  ) 'evil-first-non-blank) ;; swap `^' and `0'
  (evil-global-set-key 'normal (kbd "^"  ) 'evil-digit-argument-or-evil-beginning-of-line)
  (evil-global-set-key 'visual (kbd "0"  ) 'evil-first-non-blank)
  (evil-global-set-key 'visual (kbd "^"  ) 'evil-digit-argument-or-evil-beginning-of-line)
  (evil-global-set-key 'normal (kbd "C-s") 'evil-search-next)
  (evil-global-set-key 'normal (kbd "gR") 'apply-macro-to-region-lines)

  (spacemacs/set-leader-keys "ii" 'aya-create)
  (evil-global-set-key 'insert (kbd "C-'") 'aya-expand)

  (add-hook 'makefile-mode-hook (lambda () (modify-syntax-entry ?$ "_")))

  (global-evil-mc-mode)
  ;; changing the volume on my mic triggers these bindings.
  (dolist (key '("<XF86AudioLowerVolume>" "<XF86AudioRaiseVolume>"))
    (define-key global-map (kbd key) (lambda () (interactive))))

  (defun spacemacs/show-and-copy-buffer-filename (arg)
    "Show and copy the full path to the current file in the minibuffer."
    (interactive "P")
    ;; list-buffers-directory is the variable set in dired buffers
    (let ((file-name (or (buffer-file-name)
                         list-buffers-directory
                         default-directory)))
      (if file-name
          (message (kill-new (if (null arg)
                                 (file-truename file-name)
                               file-name)))
        (error "Buffer not visiting a file"))))

  (setq helm-ff-auto-update-initial-value nil) ;; testing this out for helm ff
  (setq helm-ff-candidate-number-limit 1500)
  (setq helm-ff-fuzzy-matching t)
  (setq avy-background t) ;; leaving this on for now to test the latency
  (spacemacs/declare-prefix "o" "user-defined")
  (spacemacs/set-leader-keys
    "ou" 'untabify        ;; replace tabs with spaces

    "jj" 'avy-goto-char-timer
    "jc" 'evil-avy-goto-word-or-subword-1
    "jC" 'evil-avy-goto-char-2

    "ws" 'split-window-below-and-focus
    "wS" 'split-window-below
    "wv" 'split-window-right-and-focus
    "wV" 'split-window-right

    "hs" 'profiler-start
    "ha" 'profiler-report

    "br" 'rename-buffer

    "fa" 'save-buffer ;; i often miss type `SPC f s' and type `SPF s f'
    "af" 'save-buffer ;; so add them both
    "gg" 'spacemacs/vcs-transient-state/body)

  (setq avy-timeout-seconds 0.3)
  (when (configuration-layer/package-usedp 'rainbow-mode)
    (add-hook 'emacs-lisp-mode-hook 'rainbow-mode)) ;; color names are highlighted in that color

  ;; Don't auto-pair single quote in verilog mode
  (when (configuration-layer/package-usedp 'smartparens)
    (sp-local-pair 'verilog-mode "'" nil :actions nil))

  (setq git-gutter+-diff-options '("-w")) ;; ignore whitespace in gutter diffs

  (spacemacs/set-leader-keys-for-major-mode 'json-mode "p" 'jsons-print-path) ;; print the json path of object

  ;; (set-face-attribute 'font-lock-comment-face nil :background "#293038") ;; this looks better with 15 or 16 bit color
  (blink-cursor-mode)
  ;; TRAMP
  (with-eval-after-load "tramp"
    (setq tramp-default-method "ssh")
    (setq tramp-default-user "tjhinckl")
    ;; set TRAMP verbosity to warnings and errors only. default is level 3 which
    ;; sends a message every time we connect to a remote host
    (setq tramp-verbose 3)
    (setq helm-tramp-connection-min-time-diff 1)

    ;; https://emacs.stackexchange.com/questions/29286/tramp-unable-to-open-some-files
    (setq tramp-inline-compress-start-size 1000000)
    (setq tramp-copy-size-limit 1000000)
    (defun $tcsh-remote-shell (fn &rest args)
      (if (file-remote-p default-directory)
          (let ((shell-file-name "tcsh"))
            (apply fn args))
        (apply fn args)))
    (tramp-set-completion-function "ssh" '((tramp-parse-hosts "~/.ssh2/ssh2_config"))))

  (setq helm-split-window-inside-p t)
  (advice-add 'shell-pop :around #'$tcsh-remote-shell)
  (advice-add 'shell :around #'$tcsh-remote-shell)

  (when (configuration-layer/package-usedp 'evil-cleverparens)
    (add-hook 'emacs-lisp-mode-hook 'evil-cleverparens-mode))

  (setq sp-echo-match-when-invisible nil)
  (setq dired-listing-switches "-alth")

  (defun $disable-modes-json ()
    (highlight-numbers-mode -1)
    (rainbow-delimiters-mode -1))
  (add-hook 'json-mode-hook '$disable-modes-json)
  (add-hook 'css-mode-hook 'rainbow-mode)

  (font-lock-add-keywords 'sh-mode
                  `((,(rx word-start
                          (group (repeat 1 2 "-")
                                 (0+ (any alnum "-_")))) 1 'font-lock-constant-face t)
                    (,(rx "$" (group (1+ (any alnum "_")))) 1 'font-lock-builtin-face t)))

  (put 'cperl-indent-parens-as-block 'safe-local-variable 'booleanp)

  ;; use less horrible ediff faces
  (with-eval-after-load "ediff"
    (dolist (face '((current :background "#32322c" :foreground "#b1951d")
                    (fine    :background "#293235" :foreground "#67b11d")
                    (even    :background "#424245" :foreground "Gray70")
                    (odd     :background "#454242" :foreground "Gray70")))
      (dolist (num '(A B C))
        (apply 'set-face-attribute (intern-soft (format "ediff-%s-diff-%s" (car face) num)) nil (cdr face)))))

  ;; https://github.com/syl20bnr/spacemacs/tree/master/layers/%2Bcompletion/auto-completion#improved-faces
  (with-eval-after-load 'company
    (define-key company-active-map (kbd "TAB") 'company-complete-common)
    (define-key company-active-map (kbd "<tab>") 'company-complete-common)
    (set-face-attribute 'company-tooltip-common nil
                        :inherit 'company-tooltip
                        :weight 'bold
                        :underline nil)
    (set-face-attribute 'company-tooltip-common-selection nil
                        :inherit 'company-tooltip-selection
                        :weight 'bold
                        :underline nil))

  (spacemacs/set-leader-keys "bp" 'spacemacs/buffer-transient-state/previous-buffer)
  (spacemacs/set-leader-keys "bn" 'spacemacs/buffer-transient-state/next-buffer)

  (setq default-input-method "TeX")

  (defun $copy-file ()
    (interactive)
    (let* ((destination (read-file-name "Write File: "))
           (dir (file-name-directory destination)))
      (unless (file-exists-p dir)
        (make-directory dir :parents))
      (write-file destination :confirm)))
  (spacemacs/set-leader-keys "fc" #'$copy-file)

  (helm-adaptive-mode)
  (global-linum-mode -1)

  (spacemacs/set-leader-keys "tq" #'toggle-debug-on-quit)
  (spacemacs/set-leader-keys "te" #'toggle-debug-on-error)

  ;; fixes https://github.com/syl20bnr/spacemacs/pull/10585
  (global-set-key (kbd "M-x") 'helm-M-x)
  (spacemacs/set-leader-keys dotspacemacs-command-key 'helm-M-x))
