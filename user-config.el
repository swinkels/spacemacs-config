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

;; -------- org-mode

(with-eval-after-load 'org
  (require 'ob-python)
  (org-babel-do-load-languages
    'org-babel-load-languages
    '((python . t))))

;; -------- org-journal

(setq system-time-locale "en_US.UTF-8")

(with-eval-after-load 'org-journal
  (custom-set-variables '(org-journal-dir "~/repos/bitbucket.org/journal/"))
  (custom-set-variables '(org-journal-file-format "%Y%m%d.org"))
  (setq org-journal-date-format "%F, %A")

  (add-to-list 'org-agenda-files org-journal-dir)

  (setq org-journal-time-format "")

  (defun add-time-as-orgmode-property()
    (progn
      (org-insert-property-drawer)
      (org-entry-put (point) "Time" (format-time-string "%H:%M"))
      (re-search-backward "^*")
      (end-of-line)
      )
    )
  (add-hook 'org-journal-after-entry-create-hook 'add-time-as-orgmode-property)
)

(load "~/repos/github.com/oje/oje.el")

;; -------- terminals

(evil-set-initial-state 'term-mode 'emacs)
