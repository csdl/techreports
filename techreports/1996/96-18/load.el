;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Emacs-Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;
;; load.el -- 
;; Author          : Carleton Moore
;; Created On      : Thu Nov  7 15:51:01 1996
;; Last Modified By: Carleton Moore
;; Last Modified On: Tue Nov 26 16:27:55 1996
;; RCS: $Id: load.el,v 1.1 1996/11/27 02:28:03 cmoore Exp $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   Copyright (C) 1996 Carleton Moore
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
(require 'tex)

(defvar ptp*proposal*LaTeX-files nil
  "Holds the LaTeX files for the proposal")
(defvar ptp*proposal*directory "/group/csdl/techreports/96-18/"
  "Holds the directory where the LaTeX files are stored.")

(setq ptp*proposal*LaTeX-files (list
				"AFTR-SRS"))

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
