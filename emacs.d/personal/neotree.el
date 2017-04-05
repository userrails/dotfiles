;; NeoTree setup
;;
;; NOTE: This package requires all-the-icons package
;; https://github.com/domtronn/all-the-icons.el
;; M-x package-install <RET> all-the-icons <RET>

(require 'neotree)

;; set neo-theme with file icons, requires 'all-the-icons package
;; make sure 'all-the-icons package is installed
(setq neo-theme (if (display-graphic-p) 'arrow))

;; everytime when the neotree window is open,
;; try to find current file and jump to node.
(setq-default neo-smart-open t)

;; change root automatically when running 'projectile-switch-project
;; enable it if you like it
;; (setq projectile-switch-project-action 'neotree-projectile-action)

(evil-define-key 'normal neotree-mode-map (kbd "TAB") 'neotree-enter)
(evil-define-key 'normal neotree-mode-map (kbd "SPC") 'neotree-enter)
(evil-define-key 'normal neotree-mode-map (kbd "q") 'neotree-hide)
(evil-define-key 'normal neotree-mode-map (kbd "RET") 'neotree-enter)
