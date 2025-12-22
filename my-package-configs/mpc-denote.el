;;; Configure denote, "a simple note taking tool for Emacs"
;;
;; Repository: https://github.com/protesilaos/denote
;; Official manual: https://protesilaos.com/emacs/denote

;; I consider the following variables to be machine-dependent and so I don't
;; configure them here:
;;
;; - denote-directory :: directory that stores the notes
;; - denote-known-keywords :: list of predefined keywords (tags)

(use-package denote
  :defer t
  :init
  (spacemacs/declare-prefix "aD" "denote")
  (spacemacs/set-leader-keys
    "aDn" 'denote
    "aDd" 'denote-dired
    "aDg" 'denote-grep
    "aDr" 'denote-rename-file
    "aDR" 'denote-rename-file-using-front-matter)
  :config
  (denote-rename-buffer-mode 1)
  :hook (dired-mode . denote-dired-mode))

(provide 'mpc-denote)
