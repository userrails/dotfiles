;; Check version compitability
(let ((minver "23.3"))
  (when (version<= emacs-version minver)
    (error "Your Emacs is too old -- this config requires v%s or higher" minver)))
(when (version<= emacs-version "24")
  (message "Your Emacs is old, and some functionality in this config will be disabled. Please upgrade if possible."))

;; package manager
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
			 ("melpa" . "http://melpa.org/packages/")))
(package-initialize)
;; (when (not package-archive-contents) (package-refresh-contents))
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; emacs functions that will helps us load
(load "~/.emacs.d/personal/defuns.el")

;; smex configuration
(package 'smex)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;;;;;;;;;;;;;;;;;;;;;;
;; EVIL (Emacs + VIM)
;;;;;;;;;;;;;;;;;;;;;;
(setq evil-want-C-u-scroll t)  ;; C-u to scrolL
(package 'evil)
(evil-mode t)

;;;;;;;;;;;;;;;;;;;;;
;; EVIL PACKAGES
;;;;;;;;;;;;;;;;;;;;;
(package 'evil-matchit)
(package 'evil-nerd-commenter)

;;;;;;;;;;;;;;;;;;;;
;; PROGRAMMING
;;;;;;;;;;;;;;;;;;;;
(package 'alchemist)      ;; elixir
(package 'bundler)        ;; ruby
(package 'company)        ;; autocomplete
(package 'dumb-jump)      ;; jump to definitations (requires silver searcher)
(package 'gist)           ;; github
(package 'git-gutter+)     ;; git gutter
(package 'haml-mode)      ;; haml
(package 'emmet-mode)     ;; html
(package 'enh-ruby-mode)  ;; ruby
(package 'flycheck)       ;; flycheck for warning and errors, don't configure yet
(package 'inf-ruby)       ;; ruby
(package 'json-mode)      ;; json
(package 'js2-mode)       ;; js
(package 'magit)          ;; git
(package 'markdown-mode)  ;; markdown
(package 'nodejs-repl)    ;; node repl
(package 'quickrun)       ;; bash
(package 'rbenv)          ;; ruby
(package 'rspec-mode)     ;; ruby
(package 'robe)           ;; ruby
(package 'stylus-mode)    ;; stylus
(package 'sass-mode)      ;; sass
(package 'scss-mode)      ;; scss
(package 'web-mode)       ;; web
(package 'yasnippet)      ;; snippets

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; WINDOWS, MOVEMENT, FILE MANUPLATION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(package 'ace-jump-mode)
(package 'exec-path-from-shell)
(package 'expand-region)
(package 'helm)
(package 'helm-ag)
(package 'helm-projectile)
(package 'multiple-cursors)
(package 'neotree)
(package 'powerline)
(package 'projectile)
(package 'projectile-rails)
(package 'restclient)
(package 'smartparens)
(package 'switch-window)
(package 'textmate)
(package 'undo-tree)
(package 'use-package)
(package 'yaml-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; THEMES
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(package 'blackboard-theme)
(package 'zenburn-theme)
(package 'oceanic-theme)
(package 'linum-relative)

;;;;;;;;;;;;;;;;;;;;
;; PERSONAL
;;;;;;;;;;;;;;;;;;;;
(personal 'theme)
(personal 'bindings)
(personal 'setting)

;;;;;;;;;;;;;;;;;;;;
;; OTHER SETTINGS
;;;;;;;;;;;;;;;;;;;;
;; set backup directory
(setq backup-directory-alist `(("." . "~/.saves")))
;; set delete mode for marked regions
(pending-delete-mode t)
(global-font-lock-mode 1)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("9d91458c4ad7c74cf946bd97ad085c0f6a40c370ac0a1cbeb2e3879f15b40553" default)))
 '(helm-follow-mode-persistent t)
 '(package-selected-packages
   (quote
    (git-gutter+ dumb-jump evil-nerd-commenter evil-matchit linum-relative evil neotree stylus-mode nodejs-repl zenburn-theme yasnippet yaml-mode web-mode use-package undo-tree textmate switch-window smex smartparens scss-mode sass-mode rspec-mode robe restclient rbenv quickrun projectile-rails powerline php-mode oceanic-theme multiple-cursors markdown-mode magit json-mode js2-mode helm-projectile helm-ag gist flycheck expand-region exec-path-from-shell enh-ruby-mode emmet-mode bundler blackboard-theme alchemist ace-jump-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'downcase-region 'disabled nil)
