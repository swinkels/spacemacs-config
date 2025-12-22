;;; Configure flyspell, "minor mode that performs automatic spell-checking [..] as you type"
;;
;; flyspell is a built-in package

(use-package flyspell
  :defer t
  :custom
  ;; only highlight misspelled words that you edit, not as you go over them
  ;;
  ;; This also disables the highlighting of words that are incomplete because
  ;; you're still typing them.
  (flyspell-check-changes t))

(provide 'mpc-flyspell)
