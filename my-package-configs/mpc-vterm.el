;;; Configure emacs-libvterm, "a fully-fledged terminal emulator inside GNU Emacs"
;;
;; Repository: https://github.com/akermu/emacs-libvterm

(use-package vterm
  :defer t
  :init
  ;; If vterm-module.so is not available, vterm will ask you whether it should
  ;; compile it and it will ask it before it calls `vterm-module-config'. You
  ;; have to answer that question with a yes otherwise the next advice won't do
  ;; anything.
  ;;
  ;; Note that the advice is added before vterm is loaded (as indicated by the
  ;; :init). This is necessary because the function adviced is called during the
  ;; loading of the package, not afterwards.
  (advice-add 'vterm-module-compile :around #'mpc-link-vterm-module))

(defun mpc-link-vterm-module (orig-fun)
  "Link shared library vterm-module.so when possible.
This function is intended as an advice around `vterm-module-compile',
which compiles the libvterm library if it's not available. This advice
attempts to avoid that compilation step. It checks if vterm-module.so is
available in the main Guix profile and if so, creates a link to it in
the vterm package directory. If vterm-module.so is not available, it
calls the original `vterm-module-compile'."
  (let* ((vterm-lisp-file (locate-library "vterm.el" t))
         (vterm-package-dir (file-name-directory vterm-lisp-file))
         (vterm-module-filename "vterm-module.so")
         (vterm-module-in-guix-file
          (expand-file-name
           (file-name-concat "~/.guix-profile/lib/" vterm-module-filename))))
    (if (file-exists-p vterm-module-in-guix-file)
        (progn
          "No need to compile `vterm-module': can link to version provided by Guix"
          (make-symbolic-link vterm-module-in-guix-file vterm-package-dir))
      (apply orig-fun))))

(provide 'mpc-vterm)
