;;; sks-org.el --- Improvements to my org-mode workflow -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Pieter Swinkels

;; Author: Pieter Swinkels <swinkels.pieter@yahoo.com>
;; Maintainer: Pieter Swinkels <swinkels.pieter@yahoo.com>
;; URL: https://github.com/swinkels/orgox
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.4")))
;; Keywords: org-mode

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides ...

;;; Code:

(provide 'ml-org)

(defun insert-subheading-inactive-date ()
  (interactive)
  (let ((column (current-column)))
    (beginning-of-line)
    (org-insert-subheading nil)
    (setq current-prefix-arg '(16))
    (org-insert-time-stamp (current-time) nil 'inactive nil "\n\n")
    (move-to-column column)))

(defun heading-for-today-p(heading)
  (string-prefix-p (concat "[" (format-time-string "%Y-%m-%d" (current-time))) heading))

(defun get-next-heading()
  (save-excursion
    (org-next-visible-heading 1)
    (when (org-at-heading-p)
      (org-back-to-heading)
      (org-get-heading))))

(defun move-line-below-next-heading ()
  "Move the current line below the next heading.

This function inserts the current line and its newline at the
start of the first non-empty line below the next heading. This
means the line at that position before the move, moves one line
down.

This function assumes there is a next heading.
"
  (let ((column (current-column))
        (line-text
         (delete-and-extract-region
          (line-beginning-position) (+ (line-end-position) 1))))
    (org-next-visible-heading 1)
    (end-of-line)
    (skip-chars-forward "\n")
    (insert line-text)
    (forward-line -1)
    (move-to-column column)))

(defun prepare-checkbox-for-toggle (&optional toggle-presence)
  (unless (or (heading-for-today-p (org-get-heading))
              (heading-for-today-p (get-next-heading)))
    (insert-subheading-inactive-date))
  (if (heading-for-today-p (get-next-heading))
      (move-line-below-next-heading)))

(advice-add 'org-toggle-checkbox :before 'prepare-checkbox-for-toggle)

(defun is-line-with-checkbox-p ()
  (string-prefix-p "- [" (thing-at-point 'line 'no-properties)))

(defun my-ctrl-c-ctrl-c (&optional arg)
  (if (is-line-with-checkbox-p)
      (prepare-checkbox-for-toggle)))

(advice-add 'org-ctrl-c-ctrl-c :before 'my-ctrl-c-ctrl-c)

(spacemacs/set-leader-keys-for-major-mode 'org-mode "iS" 'insert-subheading-inactive-date)

;;; sks-org.el ends here
