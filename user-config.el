;; ---- Frame appearance


;; ---- Key bindings

(global-set-key [f4]       'next-error)
(setq tab-always-indent t)

;; ---- Miscellaneous

;; enable word wrap
(add-hook 'text-mode-hook 'turn-on-auto-fill)

(defun sks-buffers-nosort-transformer (_ candidates _)
  candidates)

(advice-add 'helm-buffers-sort-transformer :around 'sks-buffers-nosort-transformer)

;; ---- Individual-packages

;; -------- elm-mode

(with-eval-after-load 'elm-format
    (setq elm-format-on-save t))

;; -------- lsp-mode

(setq lsp-headerline-arrow ">")

;; -------- org-mode

(with-eval-after-load 'org
  (require 'ob-python)
  (org-babel-do-load-languages
    'org-babel-load-languages
    '((python . t))))

(with-eval-after-load 'org
  (dolist (face '(org-level-1
                  org-level-2
                  org-level-3
                  org-level-4
                  org-level-5))
    (set-face-attribute face nil :weight 'semi-bold :height 1.0)))

;; -------- tox-pyvenv

(use-package tox-pyvenv
  :commands (tox-pyvenv-activate)
  :init
    (spacemacs/set-leader-keys-for-major-mode 'python-mode "vt" 'tox-pyvenv-activate))

;; -------- terminals

(evil-set-initial-state 'term-mode 'emacs)
