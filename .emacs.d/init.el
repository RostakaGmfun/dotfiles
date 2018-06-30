(require 'package)

(setq package-list '(helm-gtags plantuml-mode company-nixos-options
                     evil-magit git-gutter ox-twbs org diff-hl
                     vimrc-mode slime rainbow-delimiters key-chord
                     anything cmake-mode company dracula-theme ess
                     evil-leader evil fiplr goto-chg grizzl helm
                     helm-core julia-mode magit git-commit
                     magit-popup nix-mode popup powerline
                     python-mode racer f rust-mode s
                     smooth-scrolling solarized-theme undo-tree
                     with-editor dash async go-mode flymake-go))

(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

(package-initialize)

(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(add-hook 'prog-mode-hook (lambda ()
                            (font-lock-add-keywords nil
                             '(("\\<\\(FIXME\\|TODO\\|BUG\\)" 1 font-lock-warning-face t)))))

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(setq shell-file-name "/bin/sh")

;; evil

(require 'evil)
(evil-mode 1)

(define-key evil-normal-state-map (kbd ";") 'evil-ex)

(require 'key-chord)
(setq key-chord-two-keys-delay 0.1)
(key-chord-mode 1)
(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
(key-chord-define evil-insert-state-map "qw" 'evil-write)
(key-chord-define evil-normal-state-map "qw" 'evil-write)

(add-to-list 'default-frame-alist '(font . "DeJaVu Sans Mono-12"))

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default c-basic-offset 4)
(c-set-offset 'arglist-cont-nonempty '++)
(c-set-offset 'innamespace 0)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(global-linum-mode 1)
(setq-default truncate-lines t)
(setq-default show-trailing-whitespace t)
(setq system-time-locale "C")
(setq split-width-threshold nil)

(require 'smooth-scrolling)
(smooth-scrolling-mode 1)

(load-theme 'dracula t)

(require 'powerline)
(powerline-default-theme)

(setq make-backup-files nil)
(setq auto-save-default nil)

(global-unset-key (kbd "C-x C-c"))

(global-git-gutter-mode 1)
(setq-default git-gutter:modified-sign "~")
(setq-default git-gutter:added-sign "+")
(setq-default git-gutter:deleted-sign "\u2013") ;; en dash

(set-face-foreground 'git-gutter:modified "orange")
(set-face-foreground 'git-gutter:added "green")
(set-face-foreground 'git-gutter:deleted "red")

;; Helm config

(require 'helm-config)
(helm-mode 1)

(global-set-key (kbd "M-x") 'helm-M-x)

;; Company config

(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
(add-to-list 'company-backends 'company-nixos-options 'company-clang)
(add-hook 'prog-mode-hook
          (lambda ()
            (company-mode)
            (local-set-key (kbd "C-SPC") #'company-complete-common)))

(add-hook 'prog-mode-hook 'helm-gtags-mode)
(add-hook 'prog-mode-hook (lambda () (global-set-key (kbd "C-c C-j") 'helm-gtags-dwim)))

;; fiplr

(require 'fiplr)
(global-set-key (kbd "C-x f") 'fiplr-find-file)
(global-set-key (kbd "C-c f") 'fiplr-find-file-other-window)
(setq-default fiplr-ignored-globs '((directories (".git" "build")) (files ("#*" "*~"))))

;; Racer config

(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)

;; SLIME
(require 'slime)
(add-hook 'lisp-mode-hook (lambda () (slime-mode t)))
(add-hook 'inferior-lisp-mode-hook (lambda () (inferior-slime-mode t)))
(setq inferior-lisp-program "sbcl")

;; Org-mode
(global-set-key (kbd "C-c c") 'org-capture)
(setq org-capture-templates
      '(("j" "Diary" entry (file "~/diary/diary.org")
         "** %^{prompt|%T} %?" :prepend :unnarrowed (:empty-lines 1))))
(setq org-todo-keywords '((sequence "TODO" "DOING" "|" "DONE")))
(setq org-todo-keyword-faces '(("TODO" . org-warning)
                               ("DOING" . (:foreground "green" :weight bold))
                               ("DONE" . "cyan")))
;; magit
(defun magit-split ()
  (interactive)
  (if (< (length (window-list)) 2)
      (split-window-horizontally)())
  (magit-status))
(global-set-key (kbd "C-c m") 'magit-status)
(add-hook 'magit-mode-hook 'evil-magit-init)

;; plantuml-mode
;; Use system `plantuml` executable from default Nix profile rather
;; than  plain JAR file
(setq plantuml-jar-path "")
(setq plantuml-java-command "plantuml")
(setq plantuml-java-args "")
