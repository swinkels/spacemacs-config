;;; lsp-mode - Use Zuban to replace mypy and as Python LSP server

(use-package lsp-mode
  :defer t
  :config
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection "zubanls")
    :activation-fn (lsp-activate-on "python")
    :server-id 'zuban)))

(use-package flycheck
  :defer t
  :config

  ;; let the Zuban type checker be known to flycheck
  ;;
  ;; This is a copy of the code to define the flycheck checker for mypy[^1]. It
  ;; works without changes because `zuban mypy' is compatible to mypy.
  ;;
  ;; [^1]: package version 20260224.1923
  (flycheck-define-checker python-zuban-mypy
    "Zuban syntax and type checker.

See URL `https://docs.zubanls.com/en/latest/'."
    :command ("zuban"
              "mypy"
              "--show-column-numbers"
              "--show-error-codes"
              "--no-pretty"
              (config-file "--config-file" flycheck-python-mypy-config)
              (option "--cache-dir" flycheck-python-mypy-cache-dir)
              (option "--python-executable" flycheck-python-mypy-python-executable)
              source-original)
    :error-patterns
    ((error line-start (file-name) ":" line (optional ":" column)
            ": error:" (message) line-end)
     (warning line-start (file-name) ":" line (optional ":" column)
              ": warning:" (message) line-end)
     (info line-start (file-name) ":" line (optional ":" column)
           ": note:" (message) line-end))
    :error-filter
    (lambda (errors)
      (dolist (err errors)
        (let ((msg (flycheck-error-message err)))
          (when (and msg (string-match "\\(.*?\\)  \\[\\([^]]+\\)\\]\\'" msg))
            (setf (flycheck-error-message err) (match-string 1 msg))
            (setf (flycheck-error-id err) (match-string 2 msg)))))
      errors)
    :working-directory flycheck-python-find-project-root
    :modes (python-mode python-ts-mode)
    ;; Ensure the file is saved, to work around
    ;; https://github.com/python/mypy/issues/4746.
    :predicate flycheck-buffer-saved-p)

  ;; Add the Zuban flycheck checker to the end of the list of checkers to give
  ;; it a lower priority than the other Python checkers. In this way it's not
  ;; automatically selected and catch an unsuspecting user by surprise.
  (add-to-list 'flycheck-checkers 'python-zuban-mypy t)

  ;; let the ruff flycheck checker appoint Zuban as next checker instead of mypy
  ;;
  ;; For my Python projects, this lets flycheck automatically run Zuban.
  (flycheck-remove-next-checker 'python-ruff 'python-mypy)
  (flycheck-add-next-checker 'python-ruff '(warning . python-zuban-mypy)))

;;; pydor - Run the doctests in the doctring at-point

(use-package pydor
  :commands (pydor-execute-doctest)
  :init
  (spacemacs/set-leader-keys-for-major-mode 'python-mode
    "td" 'pydor-execute-doctest))

;;; python-pytest - Integrate the Python test runner pytest

(use-package python-pytest
  :commands (python-pytest
             python-pytest-file
             python-pytest-dispatch
             python-pytest-repeat
             python-pytest-last-failed
             python-pytest-run-def-at-point-treesit
             python-pytest-run-class-at-point-treesit)
  :init
  (spacemacs/set-leader-keys-for-major-mode 'python-mode
    "ta" 'python-pytest
    "tb" 'python-pytest-file
    "tD" 'python-pytest-dispatch
    "tl" 'python-pytest-repeat
    "tf" 'python-pytest-last-failed
    "tt" 'python-pytest-run-def-at-point-treesit
    "ts" 'python-pytest-run-class-at-point-treesit)
  :config
  (defun my-display-pytest-buffer (orig-fun &rest args)
    "Let the pytest buffer honor the `compilation-window-height'."
    (let* ((buffer (car args))
           (window (get-buffer-window buffer)))
      (or window
          (progn
            ;; Before Emacs displays the pytest buffer for the first time, so
            ;; before the pytest process has started running, make the buffer
            ;; compilation buffer. This lets Emacs place the buffer in the
            ;; default position for compilation buffers.
            (unless (get-buffer-process buffer)
              (compilation-mode)
              (read-only-mode -1))
            ;; Display the buffer and let its window use the configured
            ;; compilation-window-height
            (let ((new-window (apply orig-fun args)))
              (compilation-set-window-height new-window)
              new-window)))))
  (advice-add 'python-pytest--display-pytest-buffer :around #'my-display-pytest-buffer))


;;; window-purpose - Dedicate Python buffers to specific windows

;; Spacemacs doesn't support python-pytest and its windows-purpose configuration
;; does not dedicate python-pytest compilation buffers. The next configuration
;; overrides (replaces) the Spacemacs one to dedicate them.
;;
;; The next configuration is smaller than the one it replaces because it only
;; dedicates Python buffers I use.

(purpose-set-extension-configuration
 :python-layer
 (purpose-conf
  :mode-purposes '((inferior-python-mode . repl))
  :name-purposes '(("*compilation*" . logs))
  :regexp-purposes '(("^\\*pytest\\*" . logs))))

(provide 'mpc-python)
