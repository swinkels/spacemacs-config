;; ---- Frame appearance

;; let the frame titlebar show the path to the current file and the buffer name
(setq frame-title-format "%f - %b (%*)")


;; ---- Python extensions

;; PEP8 recommends to "avoid trailing whitespace anywhere"
(eval-after-load 'python-mode
  '(add-hook 'python-mode-hook
             '(lambda () (add-hook 'before-save-hook 'delete-trailing-whitespace nil t)))
  )


;; enable word wrap
(add-hook 'text-mode-hook 'turn-on-auto-fill)

(setq system-time-locale "en_US.UTF-8")
(custom-set-variables '(org-journal-dir "~/repos/bitbucket.org/journal/"))
(setq org-journal-date-format "%F, %A")

(custom-set-variables '(org-journal-file-format "%Y%m%d.org"))

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

(defun sks-buffers-nosort-transformer (_ candidates _)
  candidates)

(advice-add 'helm-buffers-sort-transformer :around 'sks-buffers-nosort-transformer)
