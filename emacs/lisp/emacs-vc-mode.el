;;
;; Customization for version-control.
;;

(defvar vc-state-mark-modeline t)

(defun vc-mark-modeline-dot (color)
  (propertize "    "
              'display
              `(image :type xpm
                      :data ,(format "/* XPM */
static char * data[] = {
\"18 13 3 1\",
\"  c None\",
\"+ c #000000\",
\". c %s\",
\"                  \",
\"       +++++      \",
\"      +.....+     \",
\"     +.......+    \",
\"    +.........+   \",
\"    +.........+   \",
\"    +.........+   \",
\"    +.........+   \",
\"    +.........+   \",
\"     +.......+    \",
\"      +.....+     \",
\"       +++++      \",
\"                  \"};"
                                     color)
                      :ascent center)))

(defsubst vc-state->color (stat)
  "Interpret vc-state symbol to mode line color"
  (case stat
    ('edited "tomato"      )
    ('up-to-date "GreenYellow" )
    ;; what is missing here??
    ;; ('unknown  "gray"        )
    ;; ('added    "blue"        )
    ;; ('deleted  "red"         )
    ;; ('unmerged "purple"      )
    (t "red")))

(defun vc-install-state-mark-modeline (color)
  (push `(vc-state-mark-modeline, (vc-mark-modeline-dot color))
        mode-line-format))

(defun vc-uninstall-state-mark-modeline ()
  (setq mode-line-format
        (remove-if #'(lambda (mode) (eq (car-safe mode)
                                        'vc-state-mark-modeline))
                   mode-line-format)))

(defadvice vc-default-mode-line-string
  (before oc/before-vc-mode-line-string activate)
  (let* ((backend-name (symbol-name backend))
         (state (vc-state file backend))
         (color (vc-state->color state)))
    (vc-uninstall-state-mark-modeline)
    (vc-install-state-mark-modeline color)))
