;;
;; Customization of lua-mode.
;;

(add-to-list 'load-path (concat oc/package-path "lua-mode"))

(defun oc/lua-mode-hook ()
  (auto-fill-mode 1)
  (make-local-variable 'oc/grep-files)
  (setq lua-indent-level 3)
  (setq oc/grep-files "-name \"*.lua\"")
  (local-set-key [return] 'newline-and-indent)
  (local-set-key [?\C-c ?\C-c] 'recompile))

(if (load "lua-mode.el" 'noerror)
    (progn
      (add-hook 'lua-mode-hook 'oc/lua-mode-hook)
      (add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
      (add-to-list 'auto-mode-alist '("\\.cid\\'" . lua-mode))
      (add-to-list 'auto-mode-alist '("\\.cld\\'" . lua-mode))
      (add-to-list 'interpreter-mode-alist '("lua" . lua-mode))))
