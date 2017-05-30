(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("ff7625ad8aa2615eae96d6b4469fcc7d3d20b2e1ebc63b761a349bebbb9d23cb" default)))
 '(package-selected-packages
   (quote
    (evil-magit git-gutter ox-twbs auto-org-md diff-hl diff-hl-mode vimrc-mode slime nyan-mode key-chord fiplr python-mode magit company racer cmake-mode rust-mode ess nix-mode powerline smooth-scrolling solarized-theme helm evil-leader dracula-theme))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(require 'package)

(setq package-list '(evil-magit git-gutter ox-twbs org diff-hl vimrc-mode slime rainbow-delimiters
                    key-chord anything cmake-mode
                    company dracula-theme ess
                    evil-leader evil fiplr
                    goto-chg grizzl helm helm-core
                    julia-mode magit git-commit magit-popup
                    nix-mode popup powerline python-mode
                    racer f rust-mode s smooth-scrolling
                    solarized-theme undo-tree with-editor dash async))

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
                             '(("\\<\\(FIXME\\|TODO\\|BUG\\):" 1 font-lock-warning-face t)))))

(add-to-list 'default-frame-alist '(fullscreen . maximized))

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

(set-default-font "DeJaVu Sans Mono-10")

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

(require 'smooth-scrolling)
(smooth-scrolling-mode 1)

(load-theme 'dracula)

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
(company-mode 1)
(global-set-key (kbd "C-SPC") #'company-complete-common)

;; fiplr

(require 'fiplr)
(global-set-key (kbd "C-x f") 'fiplr-find-file)

;; Racer config

(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)
(add-hook 'racer-mode-hook #'company-mode)

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
