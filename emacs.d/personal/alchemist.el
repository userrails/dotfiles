;; ;; read: http://alchemist.readthedocs.io/en/latest/configuration/

(alchemist-mode t)

;; ;; Use a different shell command for mix.
;; (setq alchemist-mix-command "/usr/local/bin/mix")

;; ;; Use a different task for running tests.
;; (setq alchemist-mix-test-task "espec")

;; ;; Use custom mix test task options.
;; (setq alchemist-mix-test-default-options '()) :: default

;; ;; Use a different environment variable in which mix tasks will run.
;; (setq alchemist-mix-env "prod")

;; ;; Use a different shell command for iex.
;; (setq alchemist-iex-program-name "/usr/local/bin/iex") :: default: iex

;; ;; Use a different shell command for elixir.
;; (setq alchemist-execute-command "/usr/local/bin/elixir") :: default: elixir

;; ;; Use a different shell command for elixirc.
;; (setq alchemist-compile-command "/usr/local/bin/elixirc") :: default: elixirc

;; ;; Disable the change of the modeline color with the last test run status.
;; (setq alchemist-test-status-modeline nil)

;; ;; Use a different keybinding prefix than C-c a
;; (setq alchemist-key-command-prefix (kbd "C-c .")) ;; default: (kbd "C-c a")

;; ;; Disable the use of a more significant syntax highlighting on functions like test, assert_* and refute_*
;; (setq alchemist-test-mode-highlight-tests nil) ;; default t

;; ;; Don't ask to save changed file buffers before running tests.
;; (setq alchemist-test-ask-above-save nil)

;; ;; Don't change the color of the mode-name when test run failed or passed.
;; (setq alchemist-test-status-modeline nil)

;; ;; Show compilation output in test report.
;; (setq alchemist-test-display-compilation-output t)

;; ;; Toggle truncating lines in test report.
;; (setq alchemist-test-truncate-lines nil) ;; default t

;; ;; Run the whole test suite with alchemist-mix-test after saving a buffer.
;; (setq alchemist-hooks-test-on-save t)

;; ;; Compile your project with alchemist-mix-compile after saving a buffer.
;; (setq alchemist-hooks-compile-on-save t)

