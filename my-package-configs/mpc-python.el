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
