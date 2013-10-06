;;
;; Miscellaneous.
;;

(require 'autoinsert)
(require 'cl)
(require 'compile)
(require 'etags)
(require 'thingatpt)

;; Functions.

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
  (c-indent-command))

(defun oc/iwb ()
  "Indents whole buffer."
  (interactive)
  (untabify (point-min) (point-max))
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil))

(defun oc/find-trunk ()
  "Recursively searches each parent directory for a directory named `trunk'
   and returns the path to it or nil if not found.
   Returns nil if the buffer is not visiting a file."
  (labels ((oc/find-trunk-r
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
  (labels ((oc/find-tags-file-r
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

;; Variables.

(defvar oc/keys-minor-mode-map (make-keymap) "oc/keys-minor-mode keymap.")
(define-key oc/keys-minor-mode-map [delete] 'delete-char)
(define-key oc/keys-minor-mode-map [?\C-f] 'oc/find-in-files)
(define-key oc/keys-minor-mode-map [C-tab] (lambda ()
                                             "Selects next window."
                                             (interactive)
                                             (other-window 1)))
(define-key oc/keys-minor-mode-map [C-S-iso-lefttab] (lambda ()
                                                       "Selects previous window."
                                                       (interactive)
                                                       (other-window -1)))
