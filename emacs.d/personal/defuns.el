;; For loading personal configurations
(defun personal (library)
  (load (concat "~/.emacs.d/personal/" (symbol-name library)) 'noerror))

;; For loading packages from the Emacs Lisp Package Archive (ELPA)
(defun package (package)
  (when (not (package-installed-p package))
    (package-install package))
  (personal package))

;; Transpose marked region
(defun move-text-internal (arg)
  (cond
   ((and mark-active transient-mark-mode)
    (if (> (point) (mark))
	(exchange-point-and-mark))
    (let ((column (current-column))
	  (text (delete-and-extract-region (point) (mark))))
      (forward-line arg)
      (move-to-column column t)
      (set-mark (point))
      (insert text)
      (exchange-point-and-mark)
      (setq deactivate-mark nil)))
   (t
    (beginning-of-line)
    (when (or (> arg 0) (not (bobp)))
      (forward-line)
      (when (or (< arg 0) (not (eobp)))
	(transpose-lines arg))
      (forward-line -1)))))

;; transpose marked region one step down on each recursive call
(defun move-marked-lines-down (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines down."
  (interactive "*p")
  (move-text-internal arg))

;; transpose marked region one step up on each recursive call
(defun move-marked-lines-up (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines up."
  (interactive "*p")
  (move-text-internal (- arg)))

;; Insert new line line up
;; indent it
;; set cursor in it
(defun insert-new-line-up ()
  "Insert new line up, indent it and activate cursor in it"
  (interactive)
  (previous-line)
  (end-of-line)
  (newline-and-indent))

;; Insert new line down
;; indent it
;; set cursor in it
(defun insert-new-line-down ()
  "Insert new line down, indent it and activate cursor in it"
  (interactive)
  (end-of-line)
  (newline-and-indent))

;; duplicate current line
(defun duplicate-current-line ()
  "Duplicate current line made easy"
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (next-line 1)
  (yank))
