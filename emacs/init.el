;;
;; Emacs init file for custom settings.
;;

(add-to-list 'load-path "~/.emacs.d/")
(add-to-list 'load-path "~/.emacs.d/packages/")
(add-to-list 'load-path "~/.emacs.d/lisp/")

;;
;; General Emacs Options
;;

(setq user-full-name    "Olivier Chanquelin")
(setq user-mail-address "<olivier.chanquelin@gmail.com>")

(if (fboundp 'menu-bar-mode) (menu-bar-mode t))     ; Enable Menu Bar.
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))    ; Disable Tool Bar.
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode t)) ; Enable Scroll Bar.

;; Load miscellaneous tools.
(load "emacs-misc")

;;
;; Defaults
;;
(setq-default major-mode 'text-mode)
(setq-default mode-require-final-newline t)
(setq-default require-final-newline t)
(setq-default vertical-scroll-bar 'right)
(setq-default show-trailing-whitespace t)

(setq inhibit-startup-screen t)  ; Don't show starupt screen.
(setq search-highlight t)        ; Highlight search object.
(setq query-replace-highlight t) ; Highlight query object.
(setq mouse-wheel-progressive-speed nil) ; Don't accelerate scrolling.

;; Use font DejaVu if installed.
(add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono-10"))

;;
;; Minor modes
;;
(line-number-mode t)
(column-number-mode t)
(show-paren-mode t)
(transient-mark-mode t)
(global-auto-revert-mode t)

;;
;; History.
;;
(setq history-delete-duplicates t)
(savehist-mode 1)

;;
;; Auto-save and backup files
;;
;;(setq auto-save-list-file-name nil)  ; No .saves files.
;;(setq auto-save-default t)           ; Auto-saving.
;;(setq make-backup-files t)           ; Make backup files.
;;(setq backup-by-copying t)           ; Don't clobber symlinks.
;;(setq backup-directory-alist '(("." . "~/.saves")))  ; Don't litter my fs tree.
;;(setq delete-old-versions t)         ; Delete excess backup versions silently.
;;(setq kept-new-versions 6)
;;(setq kept-old-versions 2)
;;(setq version-control t)             ; Make numeric backup versions.

;;
;; Language Environments (UTF-8)
;;
(prefer-coding-system 'utf-8)

;;
;; Auto-fill configuration
;;
(setq-default fill-column 80)
(setq auto-fill-mode t)

;;
;; Indenting configuration
;;
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)  ; Indentation can't insert tabs.

;; Turn on Common Lisp support.
(require 'cl)

;; Unique buffer names dependent on file name.
(require 'uniquify)
;; Style used for uniquifying buffer names with parts of directory name.
(setq uniquify-buffer-name-style 'forward)

;;
;; ELPA package manager
;;
;;(when
;;    (load
;;     (expand-file-name "~/.emacs.d/elpa/package.el"))
;;  (package-initialize)
;;  (require 'init-elpa))

;; Automatically compile .el files as they're loaded.
;; Keeps the compiled files in ~/.emacs.d/byte-cache by default, and will
;; automatically recompile any files that change
;;(require 'byte-code-cache)
;;(setq bcc-cache-directory "~/.emacs.d/.byte-cache")

;;;; ========================================================
;;;; Auto-insert: automatic insertion of text into new files
;;;; ========================================================
;;(require 'auto-insert-tkld)    ; see ~/.emacs.d/site-lisp/auto-insert-tkld.el
;;;; doc:  ~/.emacs.d/site-lisp/auto-insert-tkld.pdf
;;(setq auto-insert-path (cons "~/.emacs.d/insert" auto-insert-path))
;;;; trick to abstract the personal web page
;;(setq auto-insert-organisation "http://varrette.gforge.uni.lu")
;;(setq auto-insert-automatically t)
;;;; associate file extention to a template name
;;(setq auto-insert-alist
;;      '(
;;        ("\\.tex$"         . "LaTeX")            ; TeX or LaTeX
;;        ("\\.bib$"         . "BibTeX")           ; BibTeX
;;        ("\\.sty$"         . "LaTeX Style")      ; LaTeX Style
;;        ("\\.el$"          . "Emacs Lisp")       ; Emacs Lisp
;;        ("\\.java$"        . "Java")             ; Java
;;        ("\\App.java$"     . "JavaSwing")        ; Java Swing app
;;        ("[Tt]ools.h"      . "Tools C++")        ; Useful functions in C/C++
;;        ("\\Logs.cpp"      . "Logs C++")         ; Macros for logs/debugging
;;        ("\\Logs.h[+p]*"   . "Logs C++ Include") ; " header file
;;        ("\\.c$"           . "C")                ; C
;;        ("\\.h$"           . "C Include")        ; C header file
;;        ("\\.cxx$"         . "C++")              ; C++
;;        ("\\.c\\+\\+$"     . "C++")              ;
;;        ("\\.cpp$"         . "C++")              ;
;;        ("\\.cc$"          . "C++")              ;
;;        ("\\.C$"           . "C++")              ;
;;        ("[Mm]akefile$"    . "Makefile")         ; Makefile
;;        ("[Mm]akefile.am$" . "Makefile.am")      ; Makefile.am (Automake)
;;        ("\\.md$"          . "Text")             ; Text
;;        ("\\.txt$"         . "Text")             ; Text
;;        ("\\.gpg$"         . "GPG")              ; GPG 
;;        ("[Rr]eadme$"      . "Readme")           ; Readme
;;        ("README$"         . "Readme")           ;
;;        ("\\.sh$"          . "Shell")            ; Shell
;;        ("\\.csh$"         . "Shell")            ;
;;        ("\\.tcsh$"        . "Shell")            ;
;;        ("\\.html"         . "Html")             ; HTML
;;        ("\\.wml"          . "WML")              ; WML (Website Meta Language)
;;        ("\\.php"          . "PHP")              ; PHP
;;        ("\\.gnuplot"      . "Gnuplot")          ; Gnuplot
;;        ("\\.pl$"          . "Perl")             ; Perl
;;        ("\\.pm$"          . "Perl Module")      ; PerlModule
;;        ("\\.t$"           . "Perl Test")        ; Perl Test script
;;        ("\\.pp$"          . "Puppet")           ; Puppet manifest
;;        ("\\.rb$"          . "Ruby")             ; Ruby
;;        (""                . "Shell") ; Shell (by default: assume a shell template)
;;        ))
;;;; now associate a template name to a template file
;;(setq auto-insert-type-alist
;;      '(
;;        ("LaTeX"       . "insert.tex")
;;        ("BibTeX"      . "insert.bib")
;;        ("LaTeX Style" . "insert.sty")
;;        ("Emacs Lisp"  . "insert.el")
;;        ("Java"        . "insert.java")
;;        ("JavaSwing"   . "insertApp.java")
;;        ("C"           . "insert.c")
;;        ("C Include"   . "insert.h")
;;        ("C++"         . "insert.cpp")
;;        ("Tools C++"   . "insert.tools_cpp.h")
;;        ("Logs C++"    . "insert.logs.cpp")
;;        ("Logs C++ Include" . "insert.logs.h")
;;        ("Makefile"    . "insert.makefile")
;;        ("Makefile.am" . "insert.makefile.am")
;;        ("Text"        . "insert.md")
;;        ("GPG"         . "insert.gpg")
;;        ("Readme"      . "insert.readme")
;;        ("Shell"       . "insert.shell")
;;        ("Html"        . "insert.html")
;;        ("WML"         . "insert.wml")
;;        ("PHP"         . "insert.php")
;;        ("Gnuplot"     . "insert.gnuplot")
;;        ("Perl"        . "insert.pl")
;;        ("Perl Module" . "insert.pm")
;;        ("Perl Test"   . "insert.t")
;;        ("Puppet"      . "insert.pp")
;;        ("Ruby"        . "insert.rb")
;;        ))

;; ================================================================
;; Smart-compile
;; figures out the better compile command, based on filename or its
;; major-mode.
;; see http://www.emacswiki.org/emacs/SmartCompile
;; ================================================================
;;(require 'smart-compile)

;; Switch back to whatever buffer was in your other window if compilation is
;; successful
;; see http://www.emacswiki.org/emacs/ModeCompile
;; Close the compilation window if there was no error at all.
;; version that use the winner mode (see ~/.emacs.d/init-defuns.el)
;;(setq compilation-finish-functions 'compile-autoclose)

;;
;; TAGS file management
;;
(setq-default tags-revert-without-query t)
(defadvice find-tag (before oc/before-find-tag activate)
  (setq tags-file-name (oc/find-tags-file)))
(define-minor-mode oc/keys-minor-mode
  "A minor mode so that my key settings override annoying major modes."
  t " Keys" 'oc/keys-minor-mode-map)
(oc/keys-minor-mode 1)

;;
;; User variables
;;
(setq-default oc/grep-files "-name \"*.*\"")
(setq oc/emacs-path "~/.emacs.d/")
(setq oc/package-path "~/.emacs.d/packages/")

;;
;; Major-mode configurations
;;
(load "emacs-cc-mode")
(load "emacs-lua-mode")
(load "emacs-text-mode")
(load "emacs-vc-mode")

;;
;; Minor-mode configurations
;;
;;(load "emacs-cedet")

;;
;; Command aliases
;;
(defalias 'iwb 'oc/iwb)
(defalias 'tags 'oc/build-tags)

;;
;; Custom-file
;;
(setq custom-file "~/.emacs.d/custom")
(load custom-file 'noerror)
