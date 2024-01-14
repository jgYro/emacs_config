(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

(menu-bar-mode -1)
(tool-bar-mode -1)

(setq make-backup-files nil)

(setq use-package-always-ensure 't)

(setq next-line-add-newlines t)

(delete-selection-mode t)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(global-set-key (kbd "C-c c") 'kill-ring-save)
(global-set-key (kbd "C-c y") (shell-command-on-region (point) (mark) "x"))


(global-set-key (kbd "C-c v") 'yank)


(global-set-key (kbd "M-|") 'split-window-right)
(global-set-key (kbd "M-\-") 'split-window-below)

(global-set-key (kbd "M-z") 'recenter-top-bottom)

(defun end-of-line-and-newline ()
  "Move to the end of the line, then insert a newline."
  (interactive)
  (move-end-of-line nil)
  (newline))

(global-set-key (kbd "M-o") 'end-of-line-and-newline)

(defun open-line-above-and-move ()
  "Insert a new line above the current line and move the cursor to it."
  (interactive)
  (previous-line)
  (move-end-of-line nil)
  (newline)
  (indent-according-to-mode))

(global-set-key (kbd "M-O") 'open-line-above-and-move)


(use-package smartparens
:config
(add-hook 'prog-mode-hook 'smartparens-mode))
(add-hook 'prog-mode-hook 'electric-pair-mode)

(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-one t)
  (doom-themes-visual-bell-config))


(use-package multi
  ple-cursors)
(global-set-key (kbd "M-j") #'mc/mark-next-like-this)

(global-set-key
 (kbd "M-k") #'mc/mark-previous-like-this)
(global-set-key (kbd "M-*") #'mc/mark-all-like-this)
(global-set-key (kbd "M-A") #'mc/edit-beginnings-of-lines)
(global-set-key (kbd "M-E") #'mc/edit-ends-of-lines)
(global-set-key (kbd "M-,") #'mc/mark-pop)
(defun clear-mark-ring ()
  "Clear the mark ring."
  (interactive)
  (setq mark-ring ()))

(global-set-key (kbd "M-_") 'clear-mark-ring)

(use-package company
  :config
  (setq company-idle-delay 0
        company-minimum-prefix-length 2
        company-selection-wrap-around t))
(global-company-mode)

(use-package expand-region
  :bind
  ("M-n" . er/expand-region)
  ("M-p" . er/contract-region))

(use-package avy
  :bind
  ("M-SPC" . avy-goto-char)
  ("M-y" . avy-copy-region)
  ("M-;" . avy-goto-char-in-line)
  ("M-l" . avy-next)
  ("M-h" . avy-prev)
  ("M-/" . avy-goto-char-timer)
  ("M-u" . avy-goto-line))

(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package ivy
  :config
  (ivy-mode t))

(use-package ace-window
  :bind
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))
(global-set-key (kbd "M-w") 'ace-window)

(use-package counsel
  :bind (("M-x" . counsel-M-x)))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(smartparens counsel ivy rainbow-delimiters avy expand-region multiple-cursors doom-themes cmake-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
