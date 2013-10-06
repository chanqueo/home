;;
;; Customization of text-mode.
;;

(defun oc/text-mode-hook ()
  (auto-fill-mode 1)
  (local-set-key [return] 'newline-and-indent))

(add-hook 'text-mode-hook 'oc/text-mode-hook)
