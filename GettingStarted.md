This page provides step-by-step instructions on how to get started using this library.

## 1.0 Download and install Tex Live ##

If you have not done so already, you must download a distribution of TeX. We strongly recommend [Tex Live](http://www.tug.org/texlive/), which runs on Unix, Windows, and Macintosh.

If you are using Windows, you should download TeX Live using the installer package made available on the [TeX Live availability page](http://www.tug.org/texlive/acquire.html).  (See the section entitled, "TeX Live installation over the internet".)  This involves downloading a file named install-tl.zip, uncompressing it, then double-clicking the file named install-tl.bat to start the downloading process. (This results in downloading over 1800 files which takes several hours on my office machine.)

If you are using Macintosh, you should download the [MacTex](http://www.tug.org/mactex/) package for Tex Live. This involves downloading a file called MacTeX.mpkg.zip (over 1 GB) and double-clicking it to start the installation process.

## 2.0 Set up your local texmf/ directory ##

Different CSDL techreports use different styles, depending upon the requirements of the journal or conference to which they were submitted.  In addition, CSDL has developed a number of different LaTeX bibliography databases (i.e. .bib files) for the various research areas.  To facilitate reuse of these styles and bibliography databases, you should create a local texmf/ directory that will be referenced by the TeX Live system when it runs on your tech report.

Set up your local texmf/ directory in the following way.

First, create a directory inside your home directory called "texmf".  For example, on my Windows machine, this directory is C:\Documents and Settings\johnson\texmf.  On my Macintosh, this directory is /Users/johnson/Library/texmf.  (Note that on Macintosh, it must be in the Library/ directory.)

Next, SVN checkout the contents of https://csdl-techreports.googlecode.com/svn/trunk/texmf into your local texmf/ directory.  The resulting directory structure should look something like this:

```
/Users/johnson/Library/texmf/
                             bibtex/
                                    bib/
                                        csdl-trs.bib, tdd.bib, etc.
                                    bst/
                                       IEEE_CS_Latex/
                                                    latex8.bst
                                       svjour3/
                                               spbasic.bst
                             tex/
                                 latex/
                                       acm/
                                           acm_proc_article-sp.cls, etc.
                                       IEEE_CS_Latex/
                                                     latex8.sty, etc.
                                       svjour3/
                                               svjour3.cls, etc.
                                       latex-uhm-thesis-1.1.0/
                                                              uhthesis2e.cls, etc.
```

This is less complicated than it might seem:

  * The bibtex/bib/ directory contains CSDL bibtex database files.  For example, csdl-trs.bib contains citations for all CSDL techreports. Other files contain domain-specific bibtex databases. For example, tdd.bib contains references for test-driven design.

  * The IEEE\_CS\_Latex/ directories contain [IEEE conference style files](http://www.cs.uoregon.edu/events/icse2009/calls/format/?n=DR).

  * The svjour3/ directories contain [Springer-Verlag publication style files](http://www.springer.com/math/math+authors?SGWID=0-40017-2-94399-0).

  * The acm/ directory contains [ACM publications and conferences style files](http://www.acm.org/sigs/publications/proceedings-templates).

  * The latex-uhm-thesis-1.1.0/ directory contains [University of Hawaii thesis style files](http://code.google.com/p/latex-uhm-thesis/).

Feel free to add your own .bib files to the bibtex/bib directory.  This enables you to reuse the same bib file across multiple publications.  Remember to SVN add and commit your new file so others can reference it.

## 3.0 Set up your local techreports directory ##

Now create a directory called "techreports" in a location of your choosing, then SVN checkout https://csdl-techreports.googlecode.com/svn/trunk/techreports into this directory.  The resulting directory structure should look something like this:

```
/Users/johnson/techreports/
                           00-00/
                                 00-00.tex, etc.
                           09-01/
                                 09-01.tex, etc.
                           09-02/
                                 09-02.tex, etc.
```

As should be obvious, this directory contains the source files for techreports under development, except for 00-00/, which is a directory used for testing your installation, as discussed next.

## 5.0 Test your installation ##

### 5.1 Unix/Macintosh ###

Once you have set things up, you can see if it functions correctly by generating a sample CSDL techreport inside the 00-00 directory. (This is a slightly modified version of the 06-13 techreport).

Change directories to 00-00/ and run a shell script called "make", passing it the name of the tech report:

```
./make 00-00
```

You may need to invoke 'chmod 775 make' to make this file executable.

This shell script is quite simple and simply invokes the following:

```
latex 00-00
bibtex 00-00
latex 00-00
latex 00-00
dvipdfmx 00-00
```


### 5.2 Windows ###

On Windows, change directories to 00-00/ and invoke the make.bat batch file as follows:

```
make 00-00
```

This corresponds to the following sequence of commands as in the Unix script.


### 5.3 Sample test run output ###

Here is the slightly reformatted output from running 00-00/make.bat on Windows (the Unix/Macintosh output is very similar):

```
C:\csdl-techreports\00-00>latex 00-00
This is pdfTeXk, Version 3.1415926-1.40.9 (Web2C 7.5.7)
 %&-line parsing enabled.
entering extended mode
(./00-00.tex
LaTeX2e <2005/12/01>
Babel <v3.8l> and hyphenation patterns for english, usenglishmax, dumylang, noh
yphenation, german-x-2008-06-18, ngerman-x-2008-06-18, ancientgreek, ibycus, ar
abic, basque, bulgarian, catalan, pinyin, coptic, croatian, czech, danish, dutc
h, esperanto, estonian, farsi, finnish, french, galician, german, ngerman, mono
greek, greek, hungarian, icelandic, indonesian, interlingua, irish, italian, la
tin, lithuanian, mongolian, mongolian2a, bokmal, nynorsk, polish, portuguese, r
omanian, russian, sanskrit, serbian, slovak, slovenian, spanish, swedish, turki
sh, ukenglish, ukrainian, uppersorbian, welsh, loaded.
(c:/Program Files/texlive/2008/texmf-dist/tex/latex/base/article.cls
Document Class: article 2005/09/16 v1.4f Standard LaTeX document class
(c:/Program Files/texlive/2008/texmf-dist/tex/latex/base/size11.clo))
(c:/Documents and Settings/johnson/texmf/tex/latex/IEEE_CS_Latex/latex8.sty
IEEE 8.5 x 11-Inch Proceedings Style `latex8.sty'.
) (c:/Program Files/texlive/2008/texmf-dist/tex/latex/graphics/graphicx.sty
(c:/Program Files/texlive/2008/texmf-dist/tex/latex/graphics/keyval.sty)
(c:/Program Files/texlive/2008/texmf-dist/tex/latex/graphics/graphics.sty
(c:/Program Files/texlive/2008/texmf-dist/tex/latex/graphics/trig.sty)
(c:/Program Files/texlive/2008/texmf/tex/latex/config/graphics.cfg)
(c:/Program Files/texlive/2008/texmf-dist/tex/latex/graphics/dvips.def)))
No file 00-00.aux.
LaTeX Warning: Citation `Beck:03' on page 1 undefined on input line 55.
LaTeX Warning: Citation `Bhat:06' on page 1 undefined on input line 58.
LaTeX Warning: Citation `Maximilien:03' on page 1 undefined on input line 59.
LaTeX Warning: Citation `Muller:02' on page 1 undefined on input line 61.
LaTeX Warning: Citation `Erdogmus:05' on page 1 undefined on input line 63.
Underfull \vbox (badness 4316) has occurred while \output is active [1]
LaTeX Warning: Citation `Janzen:05' on page 2 undefined on input line 91.
LaTeX Warning: Citation `Wang:04' on page 2 undefined on input line 91.
LaTeX Warning: Reference `fig:ZorroArchitecture' on page 2 undefined on input line 134.
<zorro-architecture.eps>
LaTeX Warning: Citation `csdl2-04-11' on page 2 undefined on input line 163.
LaTeX Warning: Citation `csdl2-04-22' on page 2 undefined on input line 164.
LaTeX Warning: Citation `csdl2-03-12' on page 2 undefined on input line 165.
[2]
Underfull \vbox (badness 10000) has occurred while \output is active [3]
LaTeX Warning: Reference `fig:Categories' on page 4 undefined on input line 220.
<episode-classification.eps>
LaTeX Warning: Reference `fig:Categories' on page 4 undefined on input line 241.
Underfull \hbox (badness 1152) in paragraph at lines 246--256
\OT1/cmr/m/n/10.95 cer-tain Code Pro-duc-tions are am-bigu-ous: in
[4]
LaTeX Warning: Reference `sec:validation' on page 5 undefined on input line 285.
Underfull \vbox (badness 7168) has occurred while \output is active [5]
LaTeX Warning: Reference `fig:Analysis-Table' on page 6 undefined on input line 296.
<zorro-episode-interface.eps>
LaTeX Warning: Reference `fig:Analysis-Table' on page 6 undefined on input line 307.
LaTeX Warning: Reference `fig:Analysis-Table' on page 6 undefined on input line 317.
LaTeX Warning: Reference `fig:Analysis-Demography' on page 6 undefined on input line 320.
LaTeX Warning: Reference `fig:Analysis-Table' on page 6 undefined on input line 324.
<zorro-episode-demography.eps>
LaTeX Warning: Reference `fig:Analysis-Ratio' on page 6 undefined on input line 335.
<zorro-test-production-size-ratio.eps>
LaTeX Warning: Reference `fig:Analysis-Telemetry' on page 6 undefined on input line 352.
<zorro-tdd-coverage-2.eps> [6] [7]
Underfull \vbox (badness 10000) has occurred while \output is active [8]
Underfull \vbox (badness 10000) has occurred while \output is active [9]
No file 00-00.bbl.
[10] (./00-00.aux)
LaTeX Warning: There were undefined references.
LaTeX Warning: Label(s) may have changed. Rerun to get cross-references right.
 )
(see the transcript file for additional information)
Output written on 00-00.dvi (10 pages, 37204 bytes).
Transcript written on 00-00.log.



C:\csdl-techreports\00-00>bibtex 00-00
This is BibTeX, Version 0.99c (Web2C 7.5.7)
The top-level auxiliary file: 00-00.aux
The style file: latex8.bst
Database file #1: tdd.bib
Database file #2: zorro.bib
Database file #3: csdl-trs.bib
Database file #4: hackystat.bib
Database file #5: psp.bib



C:\csdl-techreports\00-00>latex 00-00
This is pdfTeXk, Version 3.1415926-1.40.9 (Web2C 7.5.7)
 %&-line parsing enabled.
entering extended mode
(./00-00.tex
LaTeX2e <2005/12/01>
Babel <v3.8l> and hyphenation patterns for english, usenglishmax, dumylang, noh
yphenation, german-x-2008-06-18, ngerman-x-2008-06-18, ancientgreek, ibycus, ar
abic, basque, bulgarian, catalan, pinyin, coptic, croatian, czech, danish, dutc
h, esperanto, estonian, farsi, finnish, french, galician, german, ngerman, mono
greek, greek, hungarian, icelandic, indonesian, interlingua, irish, italian, la
tin, lithuanian, mongolian, mongolian2a, bokmal, nynorsk, polish, portuguese, r
omanian, russian, sanskrit, serbian, slovak, slovenian, spanish, swedish, turki
sh, ukenglish, ukrainian, uppersorbian, welsh, loaded.
(c:/Program Files/texlive/2008/texmf-dist/tex/latex/base/article.cls
Document Class: article 2005/09/16 v1.4f Standard LaTeX document class
(c:/Program Files/texlive/2008/texmf-dist/tex/latex/base/size11.clo))
(c:/Documents and Settings/johnson/texmf/tex/latex/IEEE_CS_Latex/latex8.sty
IEEE 8.5 x 11-Inch Proceedings Style `latex8.sty'.
) (c:/Program Files/texlive/2008/texmf-dist/tex/latex/graphics/graphicx.sty
(c:/Program Files/texlive/2008/texmf-dist/tex/latex/graphics/keyval.sty)
(c:/Program Files/texlive/2008/texmf-dist/tex/latex/graphics/graphics.sty
(c:/Program Files/texlive/2008/texmf-dist/tex/latex/graphics/trig.sty)
(c:/Program Files/texlive/2008/texmf/tex/latex/config/graphics.cfg)
(c:/Program Files/texlive/2008/texmf-dist/tex/latex/graphics/dvips.def)))
(./00-00.aux)
LaTeX Warning: Citation `Beck:03' on page 1 undefined on input line 55.
LaTeX Warning: Citation `Bhat:06' on page 1 undefined on input line 58.
LaTeX Warning: Citation `Maximilien:03' on page 1 undefined on input line 59.
LaTeX Warning: Citation `Muller:02' on page 1 undefined on input line 61.
LaTeX Warning: Citation `Erdogmus:05' on page 1 undefined on input line 63.
Underfull \vbox (badness 4316) has occurred while \output is active [1]
LaTeX Warning: Citation `Janzen:05' on page 2 undefined on input line 91.
LaTeX Warning: Citation `Wang:04' on page 2 undefined on input line 91.
<zorro-architecture.eps>
LaTeX Warning: Citation `csdl2-04-11' on page 2 undefined on input line 163.
LaTeX Warning: Citation `csdl2-04-22' on page 2 undefined on input line 164.
LaTeX Warning: Citation `csdl2-03-12' on page 2 undefined on input line 165.
[2]
Underfull \vbox (badness 10000) has occurred while \output is active [3]
<episode-classification.eps>
Underfull \hbox (badness 1152) in paragraph at lines 246--256
\OT1/cmr/m/n/10.95 cer-tain Code Pro-duc-tions are am-bigu-ous: in
[4]
Underfull \vbox (badness 7168) has occurred while \output is active [5]
<zorro-episode-interface.eps> <zorro-episode-demography.eps>
<zorro-test-production-size-ratio.eps> <zorro-tdd-coverage-2.eps> [6] [7]
Underfull \vbox (badness 10000) has occurred while \output is active [8]
Underfull \vbox (badness 10000) has occurred while \output is active [9]
(./00-00.bbl
Underfull \hbox (badness 2503) in paragraph at lines 34--38
[]\OT1/cmr/m/n/10 P. M. John-son, H. Kou, M. G. Pauld-ing,
) [10] (./00-00.aux)
LaTeX Warning: There were undefined references.
LaTeX Warning: Label(s) may have changed. Rerun to get cross-references right.
 )
(see the transcript file for additional information)
Output written on 00-00.dvi (10 pages, 40268 bytes).
Transcript written on 00-00.log.



C:\csdl-techreports\00-00>latex 00-00
This is pdfTeXk, Version 3.1415926-1.40.9 (Web2C 7.5.7)
 %&-line parsing enabled.
entering extended mode
(./00-00.tex
LaTeX2e <2005/12/01>
Babel <v3.8l> and hyphenation patterns for english, usenglishmax, dumylang, noh
yphenation, german-x-2008-06-18, ngerman-x-2008-06-18, ancientgreek, ibycus, ar
abic, basque, bulgarian, catalan, pinyin, coptic, croatian, czech, danish, dutc
h, esperanto, estonian, farsi, finnish, french, galician, german, ngerman, mono
greek, greek, hungarian, icelandic, indonesian, interlingua, irish, italian, la
tin, lithuanian, mongolian, mongolian2a, bokmal, nynorsk, polish, portuguese, r
omanian, russian, sanskrit, serbian, slovak, slovenian, spanish, swedish, turki
sh, ukenglish, ukrainian, uppersorbian, welsh, loaded.
(c:/Program Files/texlive/2008/texmf-dist/tex/latex/base/article.cls
Document Class: article 2005/09/16 v1.4f Standard LaTeX document class
(c:/Program Files/texlive/2008/texmf-dist/tex/latex/base/size11.clo))
(c:/Documents and Settings/johnson/texmf/tex/latex/IEEE_CS_Latex/latex8.sty
IEEE 8.5 x 11-Inch Proceedings Style `latex8.sty'.
) (c:/Program Files/texlive/2008/texmf-dist/tex/latex/graphics/graphicx.sty
(c:/Program Files/texlive/2008/texmf-dist/tex/latex/graphics/keyval.sty)
(c:/Program Files/texlive/2008/texmf-dist/tex/latex/graphics/graphics.sty
(c:/Program Files/texlive/2008/texmf-dist/tex/latex/graphics/trig.sty)
(c:/Program Files/texlive/2008/texmf/tex/latex/config/graphics.cfg)
(c:/Program Files/texlive/2008/texmf-dist/tex/latex/graphics/dvips.def)))
(./00-00.aux)
Underfull \vbox (badness 4316) has occurred while \output is active [1]
<zorro-architecture.eps> [2]
Underfull \vbox (badness 10000) has occurred while \output is active [3]
<episode-classification.eps>
Underfull \hbox (badness 1152) in paragraph at lines 246--256
\OT1/cmr/m/n/10.95 cer-tain Code Pro-duc-tions are am-bigu-ous: in
[4]
Underfull \vbox (badness 7168) has occurred while \output is active [5]
<zorro-episode-interface.eps> <zorro-episode-demography.eps>
<zorro-test-production-size-ratio.eps> <zorro-tdd-coverage-2.eps> [6] [7]
Underfull \vbox (badness 10000) has occurred while \output is active [8]
Underfull \vbox (badness 10000) has occurred while \output is active [9]
(./00-00.bbl
Underfull \hbox (badness 2503) in paragraph at lines 34--38
[]\OT1/cmr/m/n/10 P. M. John-son, H. Kou, M. G. Pauld-ing,
) [10] (./00-00.aux) )
(see the transcript file for additional information)
Output written on 00-00.dvi (10 pages, 40240 bytes).
Transcript written on 00-00.log.


C:\csdl-techreports\00-00>dvipdfm 00-00
00-00.dvi -> 00-00.pdf
[1][2][3(./zorro-architecture.eps<PS>)][4][5(./episode-classification.eps<PS>)][6][7(./zorro-episode-interface.eps<PS>)(./zorro-episode-demography.eps<PS>)][8(./zorro-test-production-size-ratio.eps<PS>)][9(./zorro-tdd-coverage-2.eps<PS>)][10]
330996 bytes written

C:\csdl-techreports\00-00>
```

## 5.4 The pdf file ##

As a final check, you may want to view the 00-00.pdf file to see that it looks like the CSDL 06-13 technical report.

## 6.0 Generate your own tech report ##

Once you can generate the 00-00.pdf file locally, you are ready to start developing your own technical report.  To do so, you will generally do the following:

First, "claim" a tech report number by editing the [Directory](Directory.md) page.  If you are unsure of how to do this, contact Philip Johnson.

Next, create the directory inside the techreports/ directory to hold your files.  If your tech report number is 09-04, then the directory should be named "09-04".

Next, copy the 00-00.tex file and the make files from the 00-00/ directory to your new directory. Rename 00-00.tex to the number associated with your tech report. If you are using Unix and wish to use a more full-featured LaTeX build system, see this [tip about LaTeX makefiles.](http://code.google.com/p/csdl-techreports/wiki/LaTeXTips#Makefile_for_LaTeX)

Finally, edit your .tex file appropriately.  Run 'make' as desired to preview your document.

Be sure to SVN 'add' your new directory and files.  This provides a number of benefits:

  * Your files are backed up in case of a crash.
  * Committing regularly means if you accidentally mess things up, you can revert to a previous version.
  * You can work on your tech report from multiple computers.
  * Multiple people can collaborate on the same technical report.

## 7.0 FAQ ##

Check out the [tips page](http://code.google.com/p/csdl-techreports/wiki/LaTeXTips) for other useful information once you are up and running.