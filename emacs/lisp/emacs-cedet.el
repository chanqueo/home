;;
;; Customization of cedet.
;;

(add-to-list 'load-path (concat oc/package-path "cedet-1.1"))

;;(require 'cedet)
(require 'cedet-cscope)
;;(require 'cedet-global)
;;(require 'semantic)
;;(require 'semantic/db)
;;(require 'semantic/db-global)
(require 'semantic/analyze/refs)

;;
;; Semantic.
;;
;; Semantic parsers store results in a database for future Emacs sessions.
(add-to-list 'semantic-default-submodes 'global-semanticdb-minor-mode)
;; The idle scheduler will automatically reparse buffers in idle time.
(add-to-list 'semantic-default-submodes 'global-semantic-idle-scheduler-mode)
;; The echo area displays a summary of the lexical token at point whenever
;; Emacs is idle.
(add-to-list 'semantic-default-submodes 'global-semantic-idle-summary-mode)
;; Emacs displays a list of possible completions at idle time.
(add-to-list 'semantic-default-submodes 'global-semantic-idle-completions-mode)
;; Decoration mode turns on all active decorations as specified by
;; semantic-decoration-styles.
(add-to-list 'semantic-default-submodes 'global-semantic-decoration-mode)
;; Highlights current function.
;;(add-to-list 'semantic-default-submodes 'global-semantic-highlight-func-mode)

;; Display completions “inline” with the buffer text, as described above.
(setq semantic-complete-inline-analyzer-idle-displayor-class 'semantic-displayor-ghost)
;; Display completions in a tooltip.
;;(setq semantic-complete-inline-analyzer-idle-displayor-class 'semantic-displayor-tooltip)
;; Display completions in a separate window.
;;(setq semantic-complete-inline-analyzer-idle-displayor-class 'semantic-displayor-traditional)

;; Keymap to command completion.
;;(semantic-complete-inline-map)

;; New menu TAGS.
(defun oc/semantic-hook ()
  (imenu-add-to-menubar "TAGS"))

(add-hook 'semantic-init-hook 'oc/semantic-hook)

;;
;; CEDET
;;
(defun oc/cedet-hook ()
  ;; Enable Semantic parser.
  (semantic-mode t)
  ;; Intellisense menu.
  (local-set-key (read-kbd-macro "C-<return>")
                 'semantic-ia-complete-symbol-menu)
  ;; Jump to declaration of variable or function, whose name is under point.
  (local-set-key "\C-cj" 'semantic-ia-fast-jump)
  ;; Swith to/from declaration/implement.
  (local-set-key "\C-cp" 'semantic-analyze-proto-impl-toggle)
  ;; Visit the header file under point.
  (local-set-key "\C-ch" 'semantic-decoration-include-visit) ;
  ;; Shows documentation for function or variable, whose names is under point.
  (local-set-key "\C-cd" 'semantic-ia-show-doc)      ; in a separate buffer.
  (local-set-key "\C-cs" 'semantic-ia-show-summary)  ; in the mini-buffer.
  ;; Shows references to symbol at point.
  (local-set-key "\C-cf" 'semantic-symref))

(add-hook 'c-mode-common-hook 'oc/cedet-hook)

;;
;; CSCOPE
;;
(when (cedet-cscope-version-check t)
  (add-hook 'c-mode-common-hook 'oc/cedet-hook))

;;
;; GNU Global.
;;
;;(when (cedet-gnu-global-version-check t)
;;  (semanticdb-enable-gnu-global-databases 'c-mode)
;;  ;;(semanticdb-enable-gnu-global-databases 'c++-mode)
;;  )

;;
;; SRecode - insertion de templates
;;
;;(global-srecode-minor-mode 1)
