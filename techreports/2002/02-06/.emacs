;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Filename: .emacs -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Description     : 
;; Created On      : Tue Jun 18 15:21:48 2002
;; Last Modified On: Tue Jun 18 15:22:43 2002
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load-file (concat (if (or (string-match "i386" (emacs-version))
                           (string-match "win32" (emacs-version)))
		       "s:/"
		     "~csdl/")
		   "emacs/dot.emacs.el"))


;; The following statements are for JDE
;;(add-to-list 'load-path (expand-file-name "~/add-on/jde/lisp"))
;;(add-to-list 'load-path (expand-file-name "~/add-on/semantic"))
;;(add-to-list 'load-path (expand-file-name "~/add-on/speedbar"))
;;(add-to-list 'load-path (expand-file-name "~/add-on/elib"))
;;(add-to-list 'load-path (expand-file-name "~/add-on/eieio"))
;;(require 'jde)

;; load the utilities path
(add-to-list 'load-path (expand-file-name "~/add-on/utilities"))
;; The following statement lets you make headers at top of files
(require 'header)
;; The following one sets up the color theme
(require 'color-theme)
;; sets up the word perfect color theme
;; (color-theme-word-perfect)
;; sets up the emacs 21 color theme
;; (color-theme-emacs-21)
;; sets up the sitaram solaris color theme
(color-theme-sitaramv-solaris)

;; The following loads HTML helper modes
(autoload 'html-helper-mode "html-helper-mode" "Yay HTML" t)
;; To invoke html-helper-mode automatically on .html files, do this:
(setq auto-mode-alist (cons '("\\.html$" . html-helper-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.htm$" . html-helper-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.jsp$" . html-helper-mode) auto-mode-alist))

;; The following statement turns off the automatic backup file feature
(setq make-backup-files nil)

;; The following statement turns off the autosave feature
(setq auto-save-default nil)

;; The following variable was defined for use in header script
(defvar csdl-login-name "Joy Agustin")

;; The following statement sets text mode as default major mode
(setq default-major-mode 'text-mode)

;; The following statement turns on the fill mode
;; (setq text-mode-hook 'turn-on-auto-fill)

;; The following statement turns on font lock mode
(global-font-lock-mode t)

;; The following statement turns on the transient mark mode
(transient-mark-mode t)

;; The following statements were set by emacs 
(custom-set-variables
 '(display-time-day-and-date t)
 '(speedbar-use-images t)
 '(speedbar-frame-parameters (quote ((minibuffer) (width . 35) (border-width . 0) (menu-bar-lines . 0) (unsplittable . t))))
 '(speedbar-show-unknown-files t))
(custom-set-faces)

;; The following statement displays the time
(display-time-mode t)

;; Setting up XML authoring enviornment. Used for docBook formatting.
;; add-to-list 'load-path (expand-file-name "~/add-on/xae/lisp"))
;; (require 'xae)

;; invoke batch file mode automatically on .bat files
;;(autoload 'bat-mode "bat-mode" nil t nil)
;;(setq auto-mode-alist (cons '("\\.[bB][aA][tT]$" . 'bat-mode) auto-mode-alist))

(autoload 'bat-mode "bat-mode" "bat-mode Editing Mode" t)
(setq auto-mode-alist (append '(("\\.bat\\b"  . bat-mode))  auto-mode-alist))
;; (add-hook 'bat-mode-hook 'turn-on-font-lock)

;; Hackystat sensor related code
(defvar hackystat*user-home "c:/Documents and Settings/jagustin/")
(load-file (concat hackystat*user-home ".hackystat/emacs/sensor-package.el"))
;; (defvar hackystat*user-home "C:\\Documents and Settings\\jitender.OPHELIA\\")
;; (add-to-list 'load-path (concat hackystat*user-home "hackystat\\emacs\\"))
;; (require 'sensor-hooks)

;; set sgml mode for xsl files
(setq auto-mode-alist (append '(("\\.xsl" . sgml-mode)) auto-mode-alist))





