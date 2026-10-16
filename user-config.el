;;; Require personal configs and packages

(add-to-list 'load-path (file-name-concat dotspacemacs-directory "my-package-configs"))

(require 'mpc-denote)
(require 'mpc-flyspell)
(require 'mpc-python)
(require 'mpc-treesit)
(require 'mpc-vterm)

(add-to-list 'load-path (file-name-concat dotspacemacs-directory "my-lisp"))

(require 'ml-org)

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

;;;; clipetty
;;
;; Clipetty is a minor mode for terminal Emacs users that sends text you kill
;; also to the OS clipboard. For this to work you need a terminal (or terminal
;; emulator) that supports OSC 52 escape sequences.

(use-package clipetty :unless window-system :config (global-clipetty-mode))

;;;; consult

(use-package consult
  :defer t
  :config
  (consult-customize
   ;; only preview when using the consult version of occur
   ;;
   ;; If you preview everything, moving the candidate will visit uninteresting
   ;; files that may trigger expensive package initializations. This setting
   ;; activates the preview for existing buffers.
   consult-line
   ;; Spacemacs wraps consult-line and you also have to set the preview key on
   ;; the wrappers. The next wrappers are the ones I use most.
   spacemacs/consult-line
   spacemacs/consult-line-symbol :preview-key '(:debounce 0.2 any)
   ;; show the complete path for each reference
   ;;
   ;; This disables the grouping of xrefs by file. If consult groups the xrefs,
   ;; the minibuffer looks crowded and it's less clear where each reference
   ;; comes from. This might also depends on the color theme you're using.
   consult-xref :group nil))

;;;; helm-descbinds

;; Commit 889145b of helm-descbinds, "Make display more fancy", adds some
;; "fancy" formatting to display the key and its binding. For me, the formatting
;; makes the list of keys and their bindings look messy and less clear. The next
;; formatter restores the original, plain formatting.
;;
;; The use of this formatter is set as a custom variable in init.el.

(defun format-helm-descbinds-candidate-as-is (key binding)
  (format "%-10s\t%s" key binding))

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

(use-package lsp-mode
  :defer t
  :init
  ;; After I updated Spacemacs to `4d7c01d3e` (February 20, 2026), flycheck mode
  ;; wasn't enabled anymore in Python buffers. When I manually enabled flycheck
  ;; mode in such a buffer, the error list showed the errors were provided by
  ;; LSP instead of the expected command-line utilities `python-ruff` and
  ;; `python-mypy`.
  ;;
  ;; A web search lead to [a comment][1] on emacs-lsp/lsp-mode/#1413, "How to
  ;; disable lsp as flycheck's default checker". The user mentioned they had to
  ;; set `lsp-diagnostics-provider` *before* `lsp-mode` was loaded. Indeed, that
  ;; did the trick.
  ;;
  ;; [1]: https://github.com/emacs-lsp/lsp-mode/issues/1413#issuecomment-674325953
  (setq lsp-diagnostics-provider :none)
  (setq lsp-headerline-arrow ">")
  ;; By default, the breadcrumbs also show the "document symbols", for example
  ;; which class and method point is at. These symbols can change when you move
  ;; point and this leads to a more restless experience. To avoid that, only
  ;; show the path to the file in the project.
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file)))

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

;;;; windows-purpose

;; let windows-purpose use the same window for pytest compilation buffers as for
;; "ordinary" compilation buffers

(add-to-list 'purpose-user-regexp-purposes '("^\\*pytest-" . logs))
(purpose-compile-user-configuration)

                                        ; LocalWords:  lsp flycheck's
