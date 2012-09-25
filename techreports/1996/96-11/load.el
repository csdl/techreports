;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Emacs-Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;
;; load.el -- 
;; Author          : Carleton Moore
;; Created On      : Thu Nov  7 15:51:01 1996
;; Last Modified By: Carleton Moore
;; Last Modified On: Wed Nov 20 17:23:56 1996
;; RCS: $Id: load.el,v 1.2 1996/11/12 21:23:52 cmoore Exp cmoore $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   Copyright (C) 1996 Carleton Moore
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
(require 'tex)

(defvar ptp*proposal*LaTeX-files nil
  "Holds the LaTeX files for the proposal")
(defvar ptp*proposal*directory "/home/5/cmoore/PhD/proposal/"
  "Holds the directory where the LaTeX files are stored.")

(setq ptp*proposal*LaTeX-files (list
				"proposal"
				"titlepage"
				"abstract"
				"intro"
				"related"
				"method"
				"manual"
				"AFTR"
				"experiment"
				"plan"))

(defun ptp*proposal*spell-check ()
  "Spell checks all of the files in the proposal.
Returns nil."
  (interactive)
  (dolist (file ptp*proposal*LaTeX-files)
    (save-excursion
      (find-file (concat ptp*proposal*directory file ".tex"))
      (TeX-command-menu "Spell")))
  nil)

(defun ptp*proposal*LaTeX ()
  "Runs LaTeX on current buffer.
Returns nil."
  (interactive)
  (TeX-command-menu "LaTeX")
  nil)

(defun ptp*proposal*print ()
  "Prints the current LaTeX document.
Returns nil."
  (interactive)
  (TeX-command-menu-print "Local" "dvips -f %s | lp" "Print")
  nil)

(define-key LaTeX-mode-map [(control c) (control a)] 'ptp*proposal*LaTeX)
(define-key LaTeX-mode-map [(control c) (control p)] 'ptp*proposal*print)
