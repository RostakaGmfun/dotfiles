(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;;
;; Appearance settings
;;
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist '(font . "DeJaVu Sans Mono-11"))

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(global-display-line-numbers-mode t)
;;(setq display-line-numbers "%4d \u2502 ")
(pixel-scroll-mode 1)
(setq scroll-margin 30)
(setq scroll-conservatively 101)

(setq-default show-trailing-whitespace t)

;;
;; Other Emacs settings
;;

(setq auto-save-default nil)
;; Disable exit hotkey
(global-unset-key (kbd "C-x C-c"))

;; Disable customize-
(setq-default custom-file null-device)

;;
;; Formatting/indentation
;;
(setq-default truncate-lines t)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default c-basic-offset 4)
(c-set-offset 'arglist-cont-nonempty '++)
(c-set-offset 'innamespace [0])
(c-set-offset 'inextern-lang [0])

;;
;; Packages
;;

(eval-when-compile
  (require 'use-package)
  (require 'use-package-ensure)
  (setq use-package-always-ensure t))

(use-package rainbow-delimiters
    :hook (prog-mode . rainbow-delimiters-mode))

(use-package evil
    :init
    (setq evil-want-keybinding nil)
    :config
    (evil-mode 1))

(use-package evil-collection
    :init
    (evil-collection-init))

(use-package magit
    :bind ("C-c m" . 'magit-status)
    :hook (magit-mode-hook . evil-magit-init))

(use-package key-chord
    :config
    (key-chord-mode 1)
    (key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
    (key-chord-define evil-insert-state-map "qw" 'evil-write)
    (key-chord-define evil-normal-state-map "qw" 'evil-write))

;; Powerful status line
(use-package powerline
    :config
    (powerline-center-evil-theme))

;; Highlight unstaged changes in files
(use-package git-gutter
    :config
    (global-git-gutter-mode 1)
    (setq-default git-gutter:modified-sign "~")
    (setq-default git-gutter:added-sign "+")
    (setq-default git-gutter:deleted-sign "\u2013") ;; en dash
    (set-face-foreground 'git-gutter:modified "orange")
    (set-face-foreground 'git-gutter:added "green")
    (set-face-foreground 'git-gutter:deleted "red"))

(use-package ivy
    :config
    (ivy-mode 1)
    (setq-default ivy-extra-directories nil)
    (setq-default ivy-initial-inputs-alist nil)
    :bind ("C-x p" . 'ivy-switch-buffer))

(use-package smex
    :config
    (smex-initialize))

;; counsel-fzf but search from the root of current git repository if possible
(defun my/counsel-fzf-from-git-root ()
    (interactive)
    (let ((initial-dir (or (counsel--git-root) default-directory)))
      (counsel-fzf nil initial-dir nil)))

;; ag (grep) with symbol at point as initial search text
(defun my/counsel-ag-at-point ()
  (interactive)
  (counsel-ag (ivy-thing-at-point)))

(use-package counsel
    :bind (("C-x f" . 'my/counsel-fzf-from-git-root)
           ("M-x" . 'counsel-M-x)
           ("C-x d" . 'counsel-dired)
           ("C-x g" . 'my/counsel-ag-at-point)))

(use-package dracula-theme)

(use-package org
    :config
    (setq org-todo-keywords '((sequence "TODO" "DOING" "|" "DONE")))
    (setq org-todo-keyword-faces '(("TODO" . org-warning)
                                   ("DOING" . (:foreground "green" :weight bold))
                                   ("DONE" . "cyan"))))

(use-package cmake-mode)
(use-package markdown-mode)
