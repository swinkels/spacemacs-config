;;; Configure treesit, "the Lisp counterpart of treesit.c [to] provide tree-sitter integration"

;; Repository: built-in since Emacs 29

;; For more information about tree-sitter, see
;; https://www.masteringemacs.org/article/how-to-get-started-tree-sitter

(use-package treesit
  :preface
  ;; for now I only need support for Python
  (setq treesit-language-source-alist
        '((python "https://github.com/tree-sitter/tree-sitter-python"))))

(provide 'mpc-treesit)
