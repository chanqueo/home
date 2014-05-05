;;
;; Customization of cc-mode.
;;

(require 'xcscope nil 'noerror)

(defun oc/c-mode-hook ()
  (auto-fill-mode 1)
  (auto-insert-mode 1)
  (c-set-style "k&r")
  (make-local-variable 'oc/grep-files)
  (setq c-auto-newline nil
        c-basic-offset 4
        c-tab-always-indent t
        oc/grep-files "-name \"*.[ch]\"")
  (c-set-offset 'case-label 4)
  (c-toggle-hungry-state -1)
  ;;(add-hook 'after-save-hook 'oc/build-tags nil t)
  (add-to-list 'c-hanging-braces-alist '(class-open after))
  (add-to-list 'c-hanging-braces-alist '(brace-list-close))
  (local-set-key [return] 'newline-and-indent)
  (local-set-key [tab] 'oc/tab)
  (local-set-key [?\C-c ?\C-c] 'recompile)
  (local-set-key [?\C-d] 'oc/find-tag))

(add-hook 'c-mode-hook 'oc/c-mode-hook)

(add-to-list
 'auto-insert-alist
 '(("\\.h\\'" . "C header")
   (subst-char-in-string
    45 95 (upcase
           (concat
            "__"
            (file-name-nondirectory
             (substring buffer-file-name 0 (match-beginning 0)))
            "_H__")))
   "/**\n"
   " * @file\n"
   " * " _ ".\n"
   " * $Id: $\n"
   " */\n"
   "\n"
   "#ifndef " str "\n"
   "#define " str "\n"
   "\n"
   "/*******************************************************************************\n"
   " * PREPROCESSOR\n"
   " ******************************************************************************/\n"
   "\n"
   "\n"
   "\n"
   "#endif\n"))
(add-to-list
 'auto-insert-alist
 '(("\\.c\\'" . "C file")
   (concat
    (file-name-nondirectory (substring buffer-file-name 0 (match-beginning 0)))
    ".h")
   "/**\n"
   " * @file\n"
   " * " _ ".\n"
   " * $Id: $\n"
   " */\n"
   "\n"
   "/*******************************************************************************\n"
   " * PREPROCESSOR\n"
   " ******************************************************************************/\n"
   "\n"
   "#include \"" str "\"\n"
   "\n"))

;(pushnew `(iar2 ,(concat "^\"\\(.*\\)\",\\([0-9]+\\)\\s-+\\(?:\\(Fatal "
;                         "e\\|E\\)rror\\|Warnin\\(g\\)\\|Remar\\(k\\)\\)"
;                         "\\[[a-zA-Z0-9]+\\]:")
;                1 2 nil (4 . 5)) compilation-error-regexp-alist-alist)
;(add-to-list 'compilation-error-regexp-alist 'iar2)

(add-to-list 'auto-mode-alist '("\\.nc\\'" . c-mode))
(add-to-list 'auto-mode-alist '("\\.dox\\'" . c-mode))
