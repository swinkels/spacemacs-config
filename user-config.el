;;; Frame appearance

;;; Key bindings

(global-set-key [f4] 'next-error)

;;; Miscellaneous

;; enable word wrap
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; use en_US to formatting time values, e.g. "[2022-06-14 Tue]"
(setq system-time-locale "en_US.UTF-8")

(defun sks-buffers-nosort-transformer (_ candidates _)
  candidates)

(advice-add 'helm-buffers-sort-transformer :around 'sks-buffers-nosort-transformer)

;;; Individual-packages

;;;; helm-dash

(use-package helm-dash
  ;; don't load the package until you explicitly trigger its load, for example,
  ;; when you call one of the autoloaded functions
  :defer t
  :init
  ;; bind the following helm-dash functions before the package is loaded - note
  ;; that these two functions are autoloaded
  (spacemacs/set-leader-keys-for-major-mode 'python-mode "hd" 'helm-dash-at-point)
  (spacemacs/set-leader-keys-for-major-mode 'python-mode "hD" 'helm-dash))

;;;; lsp-mode

(setq lsp-headerline-arrow ">")

;;;; org-mode

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

(require 'ox-extra)
(ox-extras-activate '(ignore-headlines))

;;;; projectile

(defun my-projectile-toggle-between-implementation-and-test-other-window ()
  (interactive)
  (split-window-right-and-focus)
  (projectile-toggle-between-implementation-and-test)
  )

(spacemacs/set-leader-keys
  "pA"
  'my-projectile-toggle-between-implementation-and-test-other-window)

;;;; pydor

(use-package pydor
  :commands (pydor-execute-doctest)
  :init
    (spacemacs/set-leader-keys-for-major-mode 'python-mode "td" 'pydor-execute-doctest))

;;;; spaceline

;; Hide the minor mode indicators from the Spacemacs modeline.
;;
;; The icons used for these indicators are difficult to read on a 22" full HD
;; monitor. And even if you can read them, their single-character abbreviations
;; are uninformative.

(with-eval-after-load 'spaceline
  (setq spaceline-minor-modes-p nil))

;;;; tox-pyvenv

(use-package tox-pyvenv
  :commands (tox-pyvenv-activate)
  :init
    (spacemacs/set-leader-keys-for-major-mode 'python-mode "vt" 'tox-pyvenv-activate))

;;;; terminals

(evil-set-initial-state 'term-mode 'emacs)
