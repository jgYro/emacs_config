(setq
 backup-directory-alist '(("." . "~/.config/emacs/backup"))
 backup-by-copying t ; Don't delink hardlinks
 version-control t ; Use version numbers on backups
 delete-old-versions t ; Automatically delete excess backups
 kept-new-versions 20 ; how many of the newest versions to keep
 kept-old-versions 5 ; and how many of the old
 )

;; Install and set up exec-path-from-shell to load environment variables from .zshrc
(use-package
 exec-path-from-shell
 :ensure t
 :config
 (when (memq window-system '(mac ns x))
   (setq exec-path-from-shell-variables
         '("PATH" "MANPATH" "ZSH" "SHELL"))
   (exec-path-from-shell-initialize)))

;; Package manager setup
(require 'package)
(add-to-list
 'package-archives '("melpa" . "https://melpa.org/packages/")
 t)
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;; Use system clipboard
(setq select-enable-clipboard t)
(defun copy-from-osx ()
  (shell-command-to-string "pbpaste"))
(defun paste-to-osx (text &optional push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))
(setq interprogram-cut-function 'paste-to-osx)
(setq interprogram-paste-function 'copy-from-osx)

;; Disable annoying backup files
(setq make-backup-files nil)

(use-package
 doom-themes
 :ensure t
 :config
 ;; Global Settings (defaults)
 (setq
  doom-themes-enable-bold t
  doom-themes-enable-italic t)
 ;; Load theme
 (load-theme 'doom-bluloco-dark t)

 ;; Enable flashing mode-line on errors
 (doom-themes-visual-bell-config)

 ;; Corrects and improves org-mode's native fontification
 (doom-themes-org-config))

(use-package dired-preview)
(dired-preview-global-mode 1)

;; Example configuration for Consult
(use-package
 consult
 ;; Replace bindings. Lazily loaded by `use-package'.
 :bind
 ( ;; C-c bindings in `mode-specific-map'
  ("C-c M-x" . consult-mode-command)
  ("C-c h" . consult-history)
  ("C-c k" . consult-kmacro)
  ("C-c m" . consult-man)
  ("C-c i" . consult-info)
  ([remap Info-search] . consult-info)
  ;; C-x bindings in `ctl-x-map'
  ("C-x M-:" . consult-complex-command) ;; orig. repeat-complex-command
  ("C-x b" . consult-buffer) ;; orig. switch-to-buffer
  ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
  ("C-x 5 b" . consult-buffer-other-frame) ;; orig. switch-to-buffer-other-frame
  ("C-x t b" . consult-buffer-other-tab) ;; orig. switch-to-buffer-other-tab
  ("C-x r b" . consult-bookmark) ;; orig. bookmark-jump
  ("C-x p b" . consult-project-buffer) ;; orig. project-switch-to-buffer
  ;; Custom M-# bindings for fast register access
  ("M-#" . consult-register-load)
  ("M-'" . consult-register-store) ;; orig. abbrev-prefix-mark (unrelated)
  ("C-M-#" . consult-register)
  ;; Other custom bindings
  ("M-y" . consult-yank-pop) ;; orig. yank-pop
  ;; M-g bindings in `goto-map'
  ("M-g e" . consult-compile-error)
  ("M-g f" . consult-flycheck) ;; Alternative: consult-flycheck
  ("M-g g" . consult-goto-line) ;; orig. goto-line
  ("M-g M-g" . consult-goto-line) ;; orig. goto-line
  ("M-g o" . consult-outline) ;; Alternative: consult-org-heading
  ("M-g m" . consult-mark)
  ("M-g k" . consult-global-mark)
  ("M-g i" . consult-imenu)
  ("M-g I" . consult-imenu-multi)
  ;; M-s bindings in `search-map'
  ("M-s d" . consult-find) ;; Alternative: consult-fd
  ("M-s c" . consult-locate)
  ("M-s g" . consult-grep)
  ("M-s G" . consult-git-grep)
  ("M-s r" . consult-ripgrep)
  ("M-s l" . consult-line)
  ("M-s L" . consult-line-multi)
  ("M-s k" . consult-keep-lines)
  ("M-s u" . consult-focus-lines)
  ;; Isearch integration
  ("M-s e" . consult-isearch-history)
  :map
  isearch-mode-map
  ("M-e" . consult-isearch-history) ;; orig. isearch-edit-string
  ("M-s e" . consult-isearch-history) ;; orig. isearch-edit-string
  ("M-s l" . consult-line) ;; needed by consult-line to detect isearch
  ("M-s L" . consult-line-multi) ;; needed by consult-line to detect isearch
  ;; Minibuffer history
  :map
  minibuffer-local-map
  ("M-s" . consult-history) ;; orig. next-matching-history-element
  ("M-r" . consult-history)) ;; orig. previous-matching-history-element

 ;; Enable automatic preview at point in the *Completions* buffer. This is
 ;; relevant when you use the default completion UI.
 :hook (completion-list-mode . consult-preview-at-point-mode)

 ;; The :init configuration is always executed (Not lazy)
 :init

 ;; Tweak the register preview for `consult-register-load',
 ;; `consult-register-store' and the built-in commands.  This improves the
 ;; register formatting, adds thin separator lines, register sorting and hides
 ;; the window mode line.
 (advice-add #'register-preview :override #'consult-register-window)
 (setq register-preview-delay 0.5)

 ;; Use Consult to select xref locations with preview
 (setq
  xref-show-xrefs-function #'consult-xref
  xref-show-definitions-function #'consult-xref)

 ;; Configure other variables and modes in the :config section,
 ;; after lazily loading the package.
 :config

 ;; Optionally configure preview. The default value
 ;; is 'any, such that any key triggers the preview.
 ;; (setq consult-preview-key 'any)
 ;; (setq consult-preview-key "M-.")
 ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
 ;; For some commands and buffer sources it is useful to configure the
 ;; :preview-key on a per-command basis using the `consult-customize' macro.
 (consult-customize
  consult-theme
  :preview-key
  '(:debounce 0.2 any)
  consult-ripgrep
  consult-git-grep
  consult-grep
  consult-man
  consult-bookmark
  consult-recent-file
  consult-xref
  consult--source-bookmark
  consult--source-file-register
  consult--source-recent-file
  consult--source-project-recent-file
  ;; :preview-key "M-."
  :preview-key '(:debounce 0.4 any))

 ;; Optionally configure the narrowing key.
 ;; Both < and C-+ work reasonably well.
 (setq consult-narrow-key "<") ;; "C-+"

 ;; Optionally make narrowing help available in the minibuffer.
 ;; You may want to use `embark-prefix-help-command' or which-key instead.
 ;; (keymap-set consult-narrow-map (concat consult-narrow-key " ?") #'consult-narrow-help)
 )

;; Enable rich annotations using the Marginalia package
(use-package
 marginalia
 ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
 ;; available in the *Completions* buffer, add it to the
 ;; `completion-list-mode-map'.
 :bind (:map minibuffer-local-map ("M-A" . marginalia-cycle))

 ;; The :init section is always executed.
 :init

 ;; Marginalia must be activated in the :init section of use-package such that
 ;; the mode gets enabled right away. Note that this forces loading the
 ;; package.
 (marginalia-mode))

;; Enable Vertico.
(use-package
 vertico
 :custom
 ;; (vertico-scroll-margin 0) ;; Different scroll margin
 ;; (vertico-count 20) ;; Show more candidates
 ;; (vertico-resize t) ;; Grow and shrink the Vertico minibuffer
 (vertico-cycle t) ;; Enable cycling for `vertico-next/previous'
 :init (vertico-mode))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist :init (savehist-mode))

;; Emacs minibuffer configurations.
(use-package
 emacs
 :custom
 ;; Support opening new minibuffers from inside existing minibuffers.
 (enable-recursive-minibuffers t)
 ;; Hide commands in M-x which do not work in the current mode.  Vertico
 ;; commands are hidden in normal buffers. This setting is useful beyond
 ;; Vertico.
 (read-extended-command-predicate
  #'command-completion-default-include-p)
 ;; Do not allow the cursor in the minibuffer prompt
 (minibuffer-prompt-properties
  '(read-only t cursor-intangible t face minibuffer-prompt)))

;; Optionally use the `orderless' completion style.
(use-package
 orderless
 :custom
 ;; Configure a custom style dispatcher (see the Consult wiki)
 ;; (orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch))
 ;; (orderless-component-separator #'orderless-escapable-split-on-space)
 (completion-styles '(orderless basic))
 (completion-category-defaults nil)
 (completion-category-overrides
  '((file (styles partial-completion)))))

(use-package
 embark
 :ensure t

 :bind
 (("M-." . embark-act) ;; pick some comfortable binding
  ("M-;" . embark-dwim) ;; good alternative: M-.
  ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

 :init

 ;; Optionally replace the key help with a completing-read interface
 (setq prefix-help-command #'embark-prefix-help-command))
;; Consult users will also want the embark-consult package.
(use-package
 embark-consult
 :ensure t ; only need to install it, embark loads it after consult if found
 :hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package which-key :config (which-key-mode))

;; Tool bar settings
(tool-bar-mode -1)

;; Rebind `C-h` (help-command) to `C-?`
(global-set-key (kbd "C-?") 'help-command)

(global-git-gutter-mode +1)

;; Line numbers
(global-display-line-numbers-mode 1)
(setq display-line-numbers-type 'relative)

;; Keep track of cursor easier
(beacon-mode 1)

(require 'ansi-color)
(defun my/colorize-compilation-buffer ()
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region compilation-filter-start (point))))
(add-hook 'compilation-filter-hook 'my/colorize-compilation-buffer)

;; Rainbow delimiters
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; Remove trailing whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Ensure proper mouse and key handling in terminal
(setq xterm-mouse-mode t)

;; Easy wrapping of matching characters
(global-set-key (kbd "M-[") 'insert-pair)
(global-set-key (kbd "M-{") 'insert-pair)
(global-set-key (kbd "M-\"") 'insert-pair)
(global-set-key (kbd "M-\'") 'insert-pair)
(global-set-key (kbd "M-]") 'delete-pair)

;; Awesome delete
(global-set-key (kbd "M-z") 'ace-jump-zap-to-char)

(use-package
 dape
 :config
 ;; Info buffers to the right
 (setq dape-buffer-window-arrangement 'right)

 ;; Info buffers like gud (gdb-mi)
 (setq dape-buffer-window-arrangement 'gud)

 ;; Pulse source line (performance hit)
 (add-hook
  'dape-display-source-hook 'pulse-momentary-highlight-one-line)

 ;; Showing inlay hints
 (setq dape-inlay-hints t))

(use-package repeat :config (repeat-mode))

(use-package
 company
 :config
 (setq
  company-minimum-prefix-length 1
  company-selection-wrap-around t
  company-frontends '(company-pseudo-tooltip-frontend company-echo-metadata-frontend)))
(global-company-mode)

;; With use-package:
(use-package company-box :hook (company-mode . company-box-mode))

(global-set-key (kbd "M-q") 'company-complete)

(use-package tree-sitter)
(use-package tree-sitter-langs)

(global-tree-sitter-mode)

(defun toggle-eldoc ()
  "Toggle display of the eldoc documentation buffer.
If the buffer (returned by `eldoc-doc-buffer`) is visible, delete its window;
otherwise, display it."
  (interactive)
  (if (not (fboundp 'eldoc-doc-buffer))
      (message
       "eldoc-doc-buffer is not defined in this Emacs version.")
    (let ((buf (eldoc-doc-buffer)))
      (if (get-buffer-window buf)
          (delete-window (get-buffer-window buf))
        (display-buffer buf)))))

;; Bind the toggle function to a key of your choice.
(global-set-key (kbd "M-e") 'toggle-eldoc)

;; Rust development setup
(add-to-list 'load-path "~/Yro/repos/emacs_packages/rust-mode/")
(autoload 'rust-mode "rust-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))
;
;;;(add-hook 'rust-mode-hook 'eglot-ensure)
(setq rust-format-on-save t)
(setq rust-mode-treesitter-derive t)
(add-hook 'rust-mode-hook (lambda () (setq indent-tabs-mode nil)))
(add-hook 'rust-mode-hook (lambda () (prettify-symbols-mode)))
(add-hook 'rust-mode-hook #'lsp)

(use-package
 lsp-mode
 :init
 ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
 (setq lsp-keymap-prefix "s-l")
 :bind (("s-a" . lsp-execute-code-action))
 :hook
 ((python-mode . lsp)
  (js-ts-mode . lsp)
  (tsx-ts-mode . lsp)
  (typescript-mode . lsp)
  (php-mode . lsp)
  (lsp-mode . lsp-enable-which-key-integration))
 :commands lsp
 :ensure t)

;; optionally
(use-package lsp-ui :commands lsp-ui-mode :ensure t)

(use-package
 lsp-ui
 :ensure t
 :after lsp-mode
 :hook (lsp-mode . lsp-ui-mode)
 :bind
 (("C-c k" . lsp-ui-peek-find-definitions)
  ("C-c C-SPC" . lsp-ui-doc-toggle)
  ("C-c ." . lsp-ui-doc-focus-frame)
  ("C-c ," . lsp-ui-doc-unfocus-frame))
 :config
 (setq
  lsp-ui-doc-enable t
  lsp-ui-doc-use-childframe t
  lsp-ui-doc-position 'at-point
  lsp-ui-doc-include-signature t
  lsp-ui-sideline-enable t
  lsp-ui-sideline-show-hover t
  lsp-ui-sideline-show-diagnostics t
  lsp-ui-sideline-show-code-actions t
  lsp-ui-imenu-enable t
  lsp-ui-imenu-kind-position 'top
  lsp-ui-peek-enable t
  lsp-ui-peek-always-show t
  lsp-ui-peek-peek-height 20
  lsp-ui-peek-list-width 50))
(add-to-list 'auto-mode-alist '("\\.php\\'" . php-mode))
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
(add-hook 'before-save-hook #'lsp-format-buffer)

(use-package
 flycheck
 :ensure t
 :config (add-hook 'after-init-hook #'global-flycheck-mode))

;; Compile
(global-set-key (kbd "M-c") 'compile)

(setq compilation-scroll-output t)
(setq compilation-auto-jump-to-first-error t)
(add-hook 'compilation-finish-functions
          (lambda (buf str)
            (when (string-match "finished" str)
              (switch-to-buffer-other-window buf))))

;; Expand region with M-n and contract with M-p
(use-package
 expand-region
 :bind
 (("M-n" . er/expand-region) ("M-p" . er/contract-region)))

;; Multiple cursors setup
(use-package visual-regexp :bind (("s-." . vr/mc-mark)))
(use-package
 multiple-cursors
 :bind
 (("s-n" . mc/mark-next-like-this)
  ("s-p" . mc/mark-previous-like-this)
  ("s-8" . mc/mark-all-like-this)))

;; Visual replacement in real-time
(use-package
 visual-replace
 :defer t
 :bind
 (("C-c r" . visual-replace)
  :map
  isearch-mode-map
  ("C-c r" . visual-replace-from-isearch)))

;; Enable natural selection behavior
(delete-selection-mode t)

(global-set-key (kbd "C-c b i") 'next-buffer)
(global-set-key (kbd "C-c b o") 'previous-buffer)
(global-set-key (kbd "C-c b h") 'windmove-left)
(global-set-key (kbd "C-c b l") 'windmove-right)
(global-set-key (kbd "C-c b j") 'windmove-down)
(global-set-key (kbd "C-c b k") 'windmove-up)

(use-package
 avy
 :bind ("C-;" . avy-goto-char-timer) ("M-u" . avy-goto-line))

(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
(setq avy-all-windows t)

(use-package
 drag-stuff
 :bind
 ("M-j" . drag-stuff-down)
 ("M-k" . drag-stuff-up)
 ("M-h" . drag-stuff-left)
 ("M-l" . drag-stuff-right))
(drag-stuff-global-mode 1)

;; Vim-like keybindings for inserting lines
(defun vim-o ()
  "Insert a new line below the current line and move to it."
  (interactive)
  (end-of-line)
  (newline-and-indent))
;; Bind Alt-o to `vim-o`
(global-set-key (kbd "M-o") 'vim-o)

;; Vim-like keybindings for inserting lines above the current line
(defun vim-O ()
  "Insert a new line above the current line and move to it."
  (interactive)
  (beginning-of-line)
  (newline-and-indent)
  (forward-line -1)
  (indent-according-to-mode))

;; Bind Alt-o to `vim-O`
(global-set-key (kbd "M-O") 'vim-O)

;; Allow C-n to add newlines at the end of the buffer
(setq next-line-add-newlines t)

(setq scroll-error-top-bottom t)

(defun refresh-config ()
  "Refresh emacs config"
  (interactive)
  (load-file user-init-file))

(global-set-key (kbd "s-p") 'refresh-config)

(global-set-key
 (kbd "C-c i")
 (lambda ()
   (interactive)
   (find-file "~/.config/emacs/init.org")))

(use-package
 elisp-autofmt
 :commands (elisp-autofmt-mode elisp-autofmt-buffer)
 :hook (emacs-lisp-mode . elisp-autofmt-mode)
 :bind ("s-e" . elisp-autofmt-region))
(setq elisp-autofmt-python-bin "python3")

(add-hook 'org-mode-hook 'org-bullets-mode)

;; #+ATTR_HTML: width="100px"
;; #+ATTR_ORG: :width 100
(setq org-image-actual-width nil)

(add-hook
 'org-mode-hook
 (lambda ()
   (add-hook 'after-save-hook #'org-babel-tangle :append :local)))

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-M-l") 'org-metaright)
  (define-key org-mode-map (kbd "C-M-h") 'org-metaleft))

(add-hook 'org-mode-hook #'flycheck-mode)

;; Custom variables and faces
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(inhibit-startup-echo-area-message "")
 '(inhibit-startup-screen t)
 '(initial-scratch-message nil)
 '(package-selected-packages
   '(projectile
     rainbow-delimiters
     visual-replace
     ox-pandoc
     org-bullets
     ace-jump-zap
     beacon
     drag-stuff
     magit
     ace-window-mode
     ace-window
     avy
     doom-themes
     exec-path-from-shell
     which-key
     company
     flycheck
     org-make-toc
     tree-sitter-langs
     tree-sitter

     multiple-cursors
     expand-region)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
