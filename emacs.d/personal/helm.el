;; helm
(require 'helm-config)
(helm-mode 1)
(global-set-key (kbd "C-c h") 'helm-command-prefix)

;; helm-ag
(custom-set-variables
 '(helm-follow-mode-persistent t))

;; disable helm-mode for find-file
(eval-after-load 'helm-mode
  '(add-to-list 'helm-completing-read-handlers-alist '(find-file)))
