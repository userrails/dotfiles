;; blackboard theme
;; (load-theme 'blackboard t)

;; zenburn
(load-theme 'zenburn t)

;; oceanic
;; (load-theme 'oceanic t)

;; set cursor color
(set-cursor-color 'brown)

;; disable toolbar
(tool-bar-mode -1)

;; disable scrollbar
(scroll-bar-mode -1)

;; display column number
;; (column-number-mode)

;; highlight matching parens
(show-paren-mode)

;; highlight current line
;; (global-hl-line-mode)

;; display line number
(global-linum-mode t)
(linum-mode)
(linum-relative-global-mode)
;; add padding on line number
(setq linum-format "%4d ")
;; don't display line number for terminal, shell
(add-hook 'term-mode-hook (lambda ()
			    (setq-local global-linum-mode nil)))
(add-hook 'ansi-term-mode-hook (lambda ()
				 (setq-local global-linum-mode nil)))
(add-hook 'shell-mode-hook (lambda ()
			     (setq-local global-linum-mode nil)))

;; powerline
(powerline-vim-theme)

;; Basic setup
(global-hl-line-mode)
(column-number-mode)
