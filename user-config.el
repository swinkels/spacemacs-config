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

;;;; helm-descbinds

;; from https://github.com/syl20bnr/spacemacs/issues/16276#issuecomment-1939865121
;;
;; helm-descbinds-mode is activated by being on the helm-mode-hook. However,
;; when activated, helm-descbinds-mode disables which-key-mode. So, adjust the
;; hook to avoid activating it. See
;; https://github.com/syl20bnr/spacemacs/issues/16276

(remove-hook 'helm-mode-hook 'helm-descbinds-mode)

;; Commit 889145b of helm-descbinds, "Make display more fancy", adds some
;; "fancy" formatting to display the key and its binding. For me, the formatting
;; makes the list of keys and their bindings look messy and less clear. The next
;; formatter restores the original, plain formatting.

(defun format-helm-descbinds-candidate-as-is (key binding)
  (format "%-10s\t%s" key binding))

(custom-set-variables
 '(helm-descbinds-candidate-formatter 'format-helm-descbinds-candidate-as-is))

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

;;;; magit

(defun magit-yadm()
  "Execute `magit-status' on the bare repo that yadm uses."
  (interactive)
  (magit-status "/yadm::"))

(use-package magit
  :defer t
  :init
  (spacemacs/set-leader-keys "gy" 'magit-yadm))

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

;;;; tox-pyvenv

(use-package tox-pyvenv
  :commands (tox-pyvenv-activate)
  :init
    (spacemacs/set-leader-keys-for-major-mode 'python-mode "vt" 'tox-pyvenv-activate))

;;;; terminals

(evil-set-initial-state 'term-mode 'emacs)

;;;; tramp

;; We define tramp method "yadm" so magit can show the bare repo that yadm uses.
;; Because I use Zsh, this only works as of yadm 3.0.0 due to yadm commit
;; 8a3fb1a, December 29, 2020.
;;
;; To use it, execute `(magit-status "/yadm::")'. Section "magit" in this file
;; defines function `magit-yadm' for that and binds it to a key.

(use-package tramp
  :defer t
  :config
  (add-to-list 'tramp-methods
               '("yadm"
                 (tramp-login-program "yadm")
                 (tramp-login-args (("enter")))
                 (tramp-login-env (("SHELL") ("/bin/sh")))
                 (tramp-remote-shell "/bin/sh")
                 (tramp-remote-shell-args ("-c")))))
