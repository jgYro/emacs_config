;;; package -- Summary
;;; Commentary:


;;; Code:

;; Install and set up exec-path-from-shell to load environment variables from .zshrc
(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
    (setq exec-path-from-shell-variables '("PATH" "MANPATH" "ZSH" "SHELL"))
    (exec-path-from-shell-initialize)))

;; Package manager setup
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;; Use system clipboard
(setq select-enable-clipboard t)(defun copy-from-osx ()
   (shell-command-to-string "pbpaste"))
(defun paste-to-osx (text &optional push)
   (let ((process-connection-type nil))
      (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
         (process-send-string proc text)
         (process-send-eof proc))))
(setq interprogram-cut-function 'paste-to-osx)
(setq interprogram-paste-function 'copy-from-osx)

;; Stop making ugly back up files
(setq make-backup-files nil)

;; #+ATTR_HTML: width="100px"
;; #+ATTR_ORG: :width 100
(setq org-image-actual-width nil)


;; Theme
(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-snazzy t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;; Tool bar settings
(tool-bar-mode -1)

;; Line numbers
(global-display-line-numbers-mode 1)
(setq display-line-numbers-type 'relative)

;; Ensure proper mouse and key handling in terminal
(setq xterm-mouse-mode t)

;; Disable annoying backup files
(setq make-backup-files nil)

;; Allow C-n to add newlines at the end of the buffer
(setq next-line-add-newlines t)

;; Enable natural selection behavior
(delete-selection-mode t)

;; Relative line numbers
(setq display-line-numbers-type 'relative)

;; Remove trailing whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Easier Splits
(global-set-key (kbd "M-|") 'split-window-right)
(global-set-key (kbd "M-\-") 'split-window-below)

;; Rebind `C-h` (help-command) to `C-?`
(global-set-key (kbd "C-?") 'help-command)

;; Easy wrapping of matching characters
(global-set-key (kbd "M-[") 'insert-pair)
(global-set-key (kbd "M-{") 'insert-pair)
(global-set-key (kbd "M-\"") 'insert-pair)
(global-set-key (kbd "M-\'") 'insert-pair)
(global-set-key (kbd "M-]") 'delete-pair)


;; Compile
(global-set-key (kbd "M-c") 'compile)
(setq compilation-scroll-output t)
(add-hook 'compilation-finish-functions 'switch-to-buffer-other-window 'compilation)
(setq compilation-auto-jump-to-first-error t)

;; Keep track of cursor easier
(beacon-mode 1)

;; Awesome delete
(global-set-key (kbd "M-z") 'ace-jump-zap-to-char)


;; Expand region with M-n and contract with M-p
(use-package expand-region
  :bind
  (("M-n" . er/expand-region)
   ("M-p" . er/contract-region)))

;; Multiple cursors setup
(use-package multiple-cursors
  :bind
  (("C-J" . mc/mark-next-like-this)
   ("C-K" . mc/mark-previous-like-this)
   ("M-8" . mc/mark-all-like-this)
   ("C-." . mc/mark-all-in-region-regexp)))

;; Visual replacement in real-time
(use-package visual-replace
   :defer t
   :bind (("C-c r" . visual-replace)
          :map isearch-mode-map
          ("C-c r" . visual-replace-from-isearch)))

;; Rainbow delimiters
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; Vim-like keybindings for inserting lines
(defun vim-o ()
  "Insert a new line below the current line and move to it."
  (interactive)
  (end-of-line)
  (newline-and-indent))

(defun vim-O ()
  "Insert a new line above the current line and move to it."
  (interactive)
  (beginning-of-line)
  (newline-and-indent)
  (forward-line -1)
  (indent-according-to-mode))

;; Bind Alt-o to `vim-o`
(global-set-key (kbd "M-o") 'vim-o)
;; Bind Alt-o to `vim-O`
(global-set-key (kbd "M-O") 'vim-O)

(global-set-key (kbd "C-x w h") 'previous-buffer)
(global-set-key (kbd "C-x w l") 'next-buffer)

(setq scroll-error-top-bottom t)

(use-package tree-sitter)
(use-package tree-sitter-langs)

(global-tree-sitter-mode)

;; Rust development setup
(add-to-list 'load-path "~/Yro/repos/emacs_packages/rust-mode/")
(autoload 'rust-mode "rust-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))

;;(add-hook 'rust-mode-hook 'eglot-ensure)
(setq rust-format-on-save t)
(add-hook 'rust-mode-hook
          (lambda () (setq indent-tabs-mode nil)))
(add-hook 'rust-mode-hook
          (lambda () (prettify-symbols-mode)))

(add-to-list 'load-path "~/Yro/repos/emacs_packages/python-mode/")
(autoload 'python-mode "python-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))

(defun toggle-eldoc ()
  "Toggle display of the eldoc documentation buffer.
If the buffer (returned by `eldoc-doc-buffer`) is visible, delete its window;
otherwise, display it."
  (interactive)
  (if (not (fboundp 'eldoc-doc-buffer))
      (message "eldoc-doc-buffer is not defined in this Emacs version.")
    (let ((buf (eldoc-doc-buffer)))
      (if (get-buffer-window buf)
          (delete-window (get-buffer-window buf))
        (display-buffer buf)))))

;; Bind the toggle function to a key of your choice.
(global-set-key (kbd "M-e") 'toggle-eldoc)

(use-package eglot
  :ensure t
  :hook ((python-mode . eglot-ensure)
         (rust-mode   . eglot-ensure))
  :bind (:map eglot-mode-map
              ("C-c C-a" . eglot-code-actions)
	      ("C-c C-r" . eglot-rename)
	      ))

(use-package flycheck
  :ensure t
  :bind
  ("C-c C-d l" . flymake-goto-next-error)
  ("C-c C-d h" . flymake-goto-prev-error)
  ("C-c C-c d" . flymake-show-buffer-diagnostics)
  ("C-c C-c D" . flymake-show-project-diagnostics)
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package company
  :config
  (setq company-idle-delay 0
        company-minimum-prefix-length 2
        company-selection-wrap-around t))
(global-company-mode)(use-package company)
(add-hook 'after-init-hook 'global-company-mode)

(use-package which-key
:config (which-key-mode))

(ido-mode)
(ido-everywhere 1)
(setq ido-enable-flex-matching t)
    (global-set-key
     "\M-x"
     (lambda ()
       (interactive)
       (call-interactively
        (intern
         (ido-completing-read
          "M-x "
          (all-completions "" obarray 'commandp))))))


(use-package avy
  :bind
  ("C-;" . avy-goto-char-timer)
  ("M-u" . avy-goto-line))

(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
(setq avy-all-windows t)

(use-package drag-stuff
  :bind
  ("M-j" . drag-stuff-down)
  ("M-k" . drag-stuff-up)
  ("M-h" . drag-stuff-left)
  ("M-l" . drag-stuff-right)
  )

(drag-stuff-global-mode 1)

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
   '(projectile rainbow-delimiters visual-replace ox-pandoc org-bullets ace-jump-zap beacon drag-stuff magit ace-window-mode ace-window avy doom-themes exec-path-from-shell which-key company flycheck tree-sitter-langs tree-sitter eglot multiple-cursors expand-region)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
