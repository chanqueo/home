;;; init.el --- Emacs init file for custom settings.

(require 'autoinsert)
(require 'cl)
(require 'compile)
(require 'etags)
(require 'thingatpt)
(require 'uniquify)

(defvar oc/lisp-path "~/.emacs.d/lisp" "Location of Emacs Lisp files.")
(defvar oc/package-path "~/.emacs.d/packages/" "Location of packages.")

(add-to-list 'load-path oc/package-path)
(add-to-list 'load-path oc/lisp-path)

;;
;; General Emacs Options
;;

(setq user-full-name    "Olivier Chanquelin")
(setq user-mail-address "<chanqueo@gmail.com>")

(if (fboundp 'menu-bar-mode) (menu-bar-mode t)) ; Enable Menu Bar.
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1)) ; Disable Tool Bar.
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode t)) ; Enable Scroll Bar.

(setq-default major-mode 'text-mode) ; Default major mode is text-mode.
(setq-default mode-require-final-newline t) ; Last line is a newline.
(setq-default require-final-newline t) ; Last line is a new line.
(setq-default vertical-scroll-bar 'right) ; Display vertical scroll bar.
(setq-default show-trailing-whitespace t) ; Display trailing whitespaces.
(setq-default fill-column 80) ; Buffer width.
(setq-default tab-width 4) ; Default tab width.
(setq-default indent-tabs-mode nil) ; Use spaces instead of tabs.

(setq inhibit-startup-screen t) ; Don't show startup screen.
(setq search-highlight t) ; Highlight search object.
(setq query-replace-highlight t) ; Highlight query object.
(setq mouse-wheel-progressive-speed nil) ; Don't accelerate scrolling.
(setq history-delete-duplicates t) ; No duplicates in history.
(setq uniquify-buffer-name-style 'forward) ; Buffer name based on directory.

; Default font.
(add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono-10"))

(line-number-mode t) ; Display line number.
(column-number-mode t) ; Display column number.
(show-paren-mode t) ; Highlight matching parenthesis.
(transient-mark-mode t) ; Highlight regions.
(global-auto-revert-mode t) ; Enable auto revert mode.
(savehist-mode t) ; Save minibuffer history.
(auto-fill-mode t) ; Enable automatic line breaking.

(prefer-coding-system 'utf-8) ; Prefer UTF-8.

;;
;; Miscellaneous functions and variables.
;;

(defun oc/beginning-of-line ()
  "Returns 't if there is nothing more than whitespace between
   point and the beginning of the line."
  (interactive)
  (let ((startpos (point))
        (matchpos 0)
        )
    (save-excursion
      (beginning-of-line)
      (setq matchpos (search-forward-regexp "[ \t]*" startpos 't))
      (if (<= startpos matchpos)
          't
        nil))))

(defun oc/end-of-line ()
  "Returns 't if there is nothing more than whitespace between
   point and the end of the line."
  (interactive)
  (let ((matchpos 0)
        (lep (line-end-position)))
    (save-excursion
      (setq matchpos (search-forward-regexp "[ \t]+" lep 't))
      (if matchpos
          (if (= matchpos lep)
              't
            nil)
        't))))

(defun oc/symbol-at-point ()
  "Returns symbol at point."
  (interactive)
  (let ((bounds (bounds-of-thing-at-point 'symbol)))
    (if bounds
        (buffer-substring-no-properties (car bounds) (cdr bounds)))))

(defun oc/symbol-before-point ()
  "Returns symbol before point or nil if beginning of line."
  (interactive)
  (if (not (bolp))
      (save-excursion
        (backward-char)
        (oc/symbol-at-point))))

(defun oc/symbol-after-point ()
  "Returns symbol after point or nil if end of line."
  (interactive)
  (if (not (eolp))
      (save-excursion
        (forward-char)
        (oc/symbol-at-point))))

(defun oc/end-of-symbol ()
  "Sets point at end of symbol."
  (interactive)
  (end-of-thing 'symbol))

(defun oc/dabbrev-expand ()
  "Makes completion."
  (condition-case nil
      (dabbrev-expand nil)
    (error nil)))

(defun oc/tab ()
  "Removes whitespaces at end of line, indents line,
   if there is a word under cursor sets cursor to end of word,
   otherwise makes completions."
  (interactive)
  (let ((symbol-before (oc/symbol-before-point))
        (symbol-at (oc/symbol-at-point))
        (symbol-after (oc/symbol-after-point)))
    (if (and symbol-at symbol-after (equal symbol-at symbol-after))
        (oc/end-of-symbol)
      (if (and symbol-before symbol-at (equal symbol-before symbol-at))
          (oc/dabbrev-expand))))
  (if (oc/end-of-line)
      (delete-horizontal-space))
  (indent-for-tab-command))

(defun oc/iwb ()
  "Indents whole buffer."
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))

(defalias 'iwb 'oc/iwb "Indents whole buffer.")

(defun oc/canonicalize-directory (dir)
  "Canonicalizes specified directory."
  (interactive "DDirectory: ")
  (or dir
      (setq dir default-directory))
  (setq dir (file-name-directory
             (expand-file-name (substitute-in-file-name dir))))
  dir)

(defvar oc/root-file-name ".root" "Default root file name.")

(defun oc/find-root-file (dir)
  "Recursively searches hierarchy for a root file."
  (interactive "FSearch root file from: ")
  (let* ((dir (oc/canonicalize-directory dir))
         (file (concat dir oc/root-file-name)))
    (cond ((file-exists-p file)
           file)
          ((equal dir "/")
           nil)
          (t
           (oc/find-root-file
            (file-name-directory (directory-file-name dir)))))))

(defun oc/find-file-r (file dir &OPTIONAL default-directory)
  "Recursively searches hierarchy for a file."
  (interactive "DSearch file: ")
  (let* ((dir (oc/canonicalize-directory dir))
         (file (concat dir oc/root-file-name)))
    (cond ((file-exists-p file)
           file)
          ((equal dir "/")
           nil)
          (t
           (oc/find-file-r
            (file-name-directory (directory-file-name dir)))))))

(defun oc/find-trunk ()
  "Recursively searches each parent directory for a directory named `trunk'
   and returns the path to it or nil if not found.
   Returns nil if the buffer is not visiting a file."
  (cl-labels ((oc/find-trunk-r
               (path)
               (let* ((parent (if path (file-name-directory path)
                                default-directory))
                      (possible-trunk (concat parent "trunk")))
                 (cond
                  ((file-exists-p possible-trunk)
                   (throw 'found-it possible-trunk))
                  ((string= "/trunk" possible-trunk)
                   (throw 'found-it nil))
                  (t
                   (oc/find-trunk-r (directory-file-name parent)))))))
    (catch 'found-it
      (oc/find-trunk-r (buffer-file-name)))))

(defun oc/build-tags ()
  "Builds tags table."
  (interactive)
  (let ((trunk (oc/find-trunk)))
    (setq trunk (if trunk trunk default-directory))
    (if (file-exists-p (concat trunk "Makefile"))
        (save-window-excursion
          (async-shell-command
           (concat "make -C " trunk " tags")))
      (save-window-excursion
        (async-shell-command
         (concat "find " trunk " -name \"*.[ch]\" -print0 | xargs -0 etags"))))))

(defun oc/grep (regexp trunk)
  "Searches files from trunk for regular expression."
  (interactive)
  (grep (concat "find " trunk " " oc/grep-files " -print0 | xargs -0 grep -nH -e \"" regexp "\"")))

(defun oc/find-in-files ()
  "Searches trunk files for symbol at point."
  (interactive)
  (save-some-buffers)
  (let ((symbol (oc/symbol-at-point))
        (trunk (oc/find-trunk)))
    (setq symbol (read-from-minibuffer "Find in files: " symbol))
    (setq trunk (if trunk trunk default-directory))
    (oc/grep symbol trunk)))

(defun oc/find-tags-file ()
  "Recursively searches each parent directory for a file named `TAGS'
   and returns the path to that file or nil if a tags file is not found.
   Returns nil if the buffer is not visiting a file."
  (cl-labels ((oc/find-tags-file-r
               (path)
               (let* ((parent (if path (file-name-directory path)
                                default-directory))
                      (possible-tags-file (concat parent "TAGS")))
                 (cond
                  ((file-exists-p possible-tags-file)
                   (throw 'found-it possible-tags-file))
                  ((string= "/TAGS" possible-tags-file)
                   (throw 'found-it nil))
                  (t
                   (oc/find-tags-file-r (directory-file-name parent)))))))
    (catch 'found-it
      (oc/find-tags-file-r (buffer-file-name)))))

(defun oc/find-tag ()
  "Searches tags for definition of symbol at point."
  (interactive)
  (save-some-buffers)
  (let ((symbol (oc/symbol-at-point)))
    (setq symbol (read-from-minibuffer "Find tag: " symbol))
    (find-tag symbol)))

(defun oc/next-window ()
  "Selects next window."
  (interactive)
  (other-window 1))

(defun oc/previous-window ()
  "Selects previous window."
  (interactive)
  (other-window -1))

;;
;; KEYS minor mode.
;;
(defvar oc/keys-minor-mode-map (make-keymap) "oc/keys-minor-mode keymap.")
(define-key oc/keys-minor-mode-map [delete] 'delete-char)
(define-key oc/keys-minor-mode-map [?\C-f] 'oc/find-in-files)
(define-key oc/keys-minor-mode-map [C-tab] 'oc/next-window)
(define-key oc/keys-minor-mode-map [C-S-iso-lefttab] 'oc/previous-window)
(define-minor-mode oc/keys-minor-mode
  "A minor mode so that my key settings override annoying major modes."
  t " Keys" 'oc/keys-minor-mode-map)
(oc/keys-minor-mode 1)

;;
;; Major-mode configurations
;;
(load "emacs-cc-mode")
(load "emacs-lua-mode")
(load "emacs-text-mode")
(load "emacs-vc-mode")

;;
;; Custom-file
;;
(setq custom-file "~/.emacs.d/custom")
(load custom-file 'noerror)

(provide 'init)

;;; init.el ends here
