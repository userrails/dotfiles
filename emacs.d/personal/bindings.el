;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PACKAGES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; magit
(global-set-key (kbd "C-c C-c C-m") 'magit-status)
;; ace-jump-mode
(define-key global-map (kbd "C-c .") 'ace-jump-mode)
(define-key global-map (kbd "C-.") 'ace-jump-char-mode)
(define-key global-map (kbd "C-c C-c .") 'ace-jump-line-mode)
;; helm
(global-set-key (kbd "s-p") 'projectile-find-file-dwim)
(global-set-key (kbd "s-e") 'helm-projectile)
;; undo tree
(global-set-key (kbd "C-/") 'undo-tree-undo)
(global-set-key (kbd "C-?") 'undo-tree-redo)
;; Multiple cursors
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
;; neotree
(global-set-key (kbd "s-\\") 'neotree-toggle)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CURSOR MOVEMENTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; insert new line
(global-set-key (kbd "C-<return>") 'insert-new-line-down)
(global-set-key (kbd "C-S-<return>") 'insert-new-line-up)
;; transpose marked region
(global-set-key (kbd "M-P") 'move-marked-lines-up)
(global-set-key (kbd "M-N") 'move-marked-lines-down)
;; duplicate current line
;; @todo: need to duplicate marked region just like sublime and other editors does
(global-set-key (kbd "s-<down>") 'duplicate-current-line)
;; toggle line-num mode
(global-set-key (kbd "C-c l") 'linum-mode)
;; shrink and enlarge window
(global-set-key (kbd "C-S-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "C-S-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "C-S-<down>") 'shrink-window)
(global-set-key (kbd "C-S-<up>") 'enlarge-window)
;; Expand-region
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "M-\\") 'switch-window)
