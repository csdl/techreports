

# Introduction #

LaTeX may be powerful, but it's not always easy or obvious. This page is intended to collect information about accomplishing particular tasks in LaTeX.

# Tutorials #

These [tutorials by Andrew Roberts](http://www.andy-roberts.net/misc/latex/index.html) are very good. They start at the beginning and work up through most common topics. Note that you should ignore specifics about the computing environment at the University of Leeds.

For specific tips on producing a dissertation/thesis, check out [Using LaTeX to Write a PhD Thesis](http://theoval.cmp.uea.ac.uk/~nlct/latex/thesis/thesis.html).

# IEEETran Format #

There is a format for IEEE Computer Society publications (journals and conferences) called IEEETran.  It is automatically provided
in the TexLive distribution, and replaces the latex8 style.  You can learn more from [How To Use IEEETran Document Class](http://www.computer.org/portal/cms_docs_ieeecs/ieeecs/publications/author/transguide/IEEEtran_HOWTO.pdf), and/or the [IEEEtran home page](http://www.michaelshell.org/tex/ieeetran/).

# Figures #

## Always use .eps (encapsulated postscript) for figure format ##

Journals often require the LaTeX sources, not just the final PDF.  Thus, you should always generate (or convert) your figures to .eps for inclusion in your tech reports, since they may be useful in future journal submissions on your research. See Appendix D of [How To Use IEEETran Document Class](http://www.computer.org/portal/cms_docs_ieeecs/ieeecs/publications/author/transguide/IEEEtran_HOWTO.pdf), or the [IEEE Publication submission guidelines](http://computer.org/author/transguide/electronicsub.htm), which explicitly disallow PDF files.

## Inserting figures in LaTeX ##

See the [page from the UHM thesis style project](http://code.google.com/p/latex-uhm-thesis/wiki/FiguresInLaTeX)

## Preventing a figure from migrating to the end of the document ##
http://dcwww.camd.dtu.dk/~schiotz/comp/LatexTips/LatexTips.html#figplacement explains why this happens and how to fix it.

# `\begin{center}` versus `\centering` #

Apparently, if you are centering a figure or table, you should use `\centering` inside the `\begin{figure}` environment rather than wrapping the whole thing in a `\begin{center}` environment. Using the latter will add unwanted vertical space around the figure or table. Here's [the source of the tip](http://texblog.net/latex-archive/layout/center-centering/).

# Makefile for LaTeX #

If you are doing your tech report work from the command line and use Mac OS X or another Unix-based OS, then the [latex-makefile](http://code.google.com/p/latex-makefile/) is highly recommended. Download the Makefile, copy it into your tech report directory, and create a Makefile.ini file that contains "onlysources.tex := report.tex" where 'report.tex' is the name of your master LaTeX file (the one that includes everything). Now when you type `make`, it will figure out all the dependencies (bibtex, etc) and run them for you until you end up with a PDF file. It also will only show you the log output that is actually useful, eliding all the useless cruft TeX outputs. There are many other cool features, you can read all about them via `make help`.

Note that the latex-makefile subscribes to the TeX->DVI->PS->PDF workflow, not the TeX->PDF workflow that pdflatex uses. If you don't understand what this means, see this [wiki page](http://code.google.com/p/latex-uhm-thesis/wiki/FiguresInLaTeX) or this [longer discussion](http://amath.colorado.edu/documentation/LaTeX/reference/figures.html)

# Tables #

## Footnotes in tables ##

There are a variety of ways, but the easiest is to wrap the tabular in a `minipage` environment. This [page has the details](http://www.hep.man.ac.uk/u/jenny/jcwdocs/latex/latex.html).

## `\label` in tables ##

The `\label` in a table environment (so you can refer to it elsewhere) must be **after** the `\caption`, otherwise it doesn't work. Arg!

## Making narrow columns in tables that don't look horrible ##

If you have text in a table cell and you want it to word wrap, the normal way to do this in LaTeX is the `p{size}` parameter in a tabular environment. However, if you have a few words in a cell in a narrow column in a table, it tends to look terrible due to the full justification normally used in LaTeX. The easiest solution to this problem requires three steps:
  1. `\usepackage{array}`
  1. prepend `>{\raggedright}` before each `p{size}` specifier
  1. change all the `\\` used for line breaks to `\tabularnewline`
If you don't want to do that last step, you can look into the `ragged2e` package. This [TUGboat article](http://www.tug.org/TUGboat/Articles/tb28-3/tb90hoeppner.pdf) on tables has a lot of good advice on tables in general, and is where I got this tip from.

# Cool packages #

Here are a selection of useful LaTeX packages. Some of these you will want to use in most documents, others are more custom. To use them, insert `\usepackage{foo}` in the preamble of your document (the part before `\begin{document}`). In some cases the order of the packages matters, check their documentation if you have problems.

  * `graphicx`: needed for inserting graphics into your document
  * `subfigure`: allows multiple labeled figures bundled together side-by-side
  * `url`: make URLs typeset sanely. Without this, you get URLs constantly running off the page
  * `xspace`: intelligently puts space after user-defined macros (space after macro unless followed by punctuation)
  * `fullpage`: makes LaTeX's margins less ridiculous
  * `setspace`: defines `\doublespacing`, useful if you need to produce a double-spaced document
  * `fixme`: allows the insertion of marginal fixme notes that prevent building in final mode
  * `hyperref`: makes URLs clickable in PDFs, turns references into intra-PDF links, provides `\autoref` which does intelligent expansion of references (`\autoref{tab:foo}` becomes "table 2.2"). hyperref rocks, and has many options.
  * `breakurl`: if you are using `dvips` (as the [latex-makefile](http://code.google.com/p/latex-makefile/) does), you need this to make clickable URLs linebreak properly

# Making typewriter text or emphasizing text #
`\texttt{foo}` makes foo print in `typewriter font`. Other examples [here](http://www.biostat.wisc.edu/bcg/categories/applications/unix_linux_solaris_metaframeforunix/latex.html) (look for More Font Manipulations). Note that for emphasized text (which is usually set in italics) you should use `\emph{foo}` because that will unitalicize the text if the surrounding text is already in an italics (like in a quotation).

# Verbatim text with tab expansion &  code listings #
`\usepackage{moreverb}` and then use the `\begin{verbatimtab}[4]` environment, where 4 is the number of spaces expanded per tab. Here's a page with [more explanation on verbatim environments.](http://www.tex.ac.uk/cgi-bin/texfaq2html?label=verbfile) If you want to include source code and want it pretty printed, check out [this FAQ page](http://www.tex.ac.uk/cgi-bin/texfaq2html?label=codelist)

# Putting index, etc into table of contents #
Index and bibliographies don't normally show up in the table of contents. To make them show up, use this:
```
\cleardoublepage
\addcontentsline{toc}{chapter}{Bibliography}
\bibliography{frooble}
```
If working in an article rather than a chapter-based document then use:
```
\cleardoublepage
\phantomsection
\addcontentsline{toc}{chapter}{Bibliography}
\bibliography{frooble}
```
There is also the `tocbibind` package (I haven't looked into it. More information at [this FAQ page](http://www.tex.ac.uk/cgi-bin/texfaq2html?label=tocbibind)

# Punctuation #

## Periods for abbreviations ##
When using periods in an abbreviation (like "et al."), you should precede the following space with a "\" to let TeX know not to put extra whitespace after the period like so: "Chetty et al.\ performed a qualitative study". [Web reference](http://john.regehr.org/latex/).

# Making a document fit into a certain page count #
A whole slew of suggestions [here](http://www-h.eng.cam.ac.uk/help/tpl/textprocessing/squeeze.html). Of course rewriting is the better option. :)

# Producing a single chapter for review #

If you want to create a PDF with only a single chapter of your multi-chapter document (like your thesis), there is an easy way to do this. First, make sure you have made a PDF of the whole document once, to produce all the intermediary files LaTeX needs. Then, in the preamble (somewhere before `\begin{document}`) add this:

```
\includeonly{chapterfilename}
```

This will make LaTeX ignore any `\include{}` statements other than the one with the given filename. However, the ToC, inter-chapter references, and bibliography will all still be produced, making for a much better looking document. Multiple files can be specified in the `\includeonly{}` statement, separated by commas. This tip is from [here](http://theoval.cmp.uea.ac.uk/~nlct/latex/thesis/node4.html).