This directory is intended for useful auxilary files that are not TeX inputs
and are therefore inappropriate for the texmf directory.

Makefile: the latex-makefile from <http://code.google.com/p/latex-makefile/>
	Should always contain the latest version, copy to your tech report
	directory if you want to use it.

Makefile.ini: an example configuration file for latex-makefile. If your tech
	report is broken into multiple .tex files, list the master file here
	to ensure that it will be LaTeXed correctly. The example also disables
	cleaning of *.pdf files so that when you have finished working on a
	report you can do "make clean" and still have the final PDF file
	available.

svn_ignore.txt:	a collection of file extensions for temporary LaTeX files
	created when preparing a document. All of these types of files should
	be kept out of the svn repository. To set up a directory with these
	ignore values, cd into the directory and do:
	svn propset svn:ignore --file ../etc/svn_ignore.txt .
	Note that the filename has "_" instead of ":" because Windows SVN
	appears to choke when it sees a filename with ":". Note also that now that
	we are storing the final PDF of the techreport in svn, "*.pdf" has been
	removed from the list.

upload-pdf: a simple shell script to upload a PDF to dasha for distribution.
	Can be copied to the tech report directory for use.
