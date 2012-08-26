# vim: ft=make
.PHONY: ict4s_proc._graphics
ict4s_proc.aux ict4s_proc.aux.make ict4s_proc.d ict4s_proc.dvi: /usr/local/texlive/2012/texmf-dist/tex/latex/amsfonts/amsfonts.sty
ict4s_proc.aux ict4s_proc.aux.make ict4s_proc.d ict4s_proc.dvi: /usr/local/texlive/2012/texmf-dist/tex/latex/amsfonts/amssymb.sty
ict4s_proc.aux ict4s_proc.aux.make ict4s_proc.d ict4s_proc.dvi: /usr/local/texlive/2012/texmf-dist/tex/latex/amsmath/amsbsy.sty
ict4s_proc.aux ict4s_proc.aux.make ict4s_proc.d ict4s_proc.dvi: /usr/local/texlive/2012/texmf-dist/tex/latex/amsmath/amsgen.sty
ict4s_proc.aux ict4s_proc.aux.make ict4s_proc.d ict4s_proc.dvi: /usr/local/texlive/2012/texmf-dist/tex/latex/amsmath/amsmath.sty
ict4s_proc.aux ict4s_proc.aux.make ict4s_proc.d ict4s_proc.dvi: /usr/local/texlive/2012/texmf-dist/tex/latex/amsmath/amsopn.sty
ict4s_proc.aux ict4s_proc.aux.make ict4s_proc.d ict4s_proc.dvi: /usr/local/texlive/2012/texmf-dist/tex/latex/amsmath/amstext.sty
ict4s_proc.aux ict4s_proc.aux.make ict4s_proc.d ict4s_proc.dvi: /usr/local/texlive/2012/texmf-dist/tex/latex/base/fontenc.sty
ict4s_proc.aux ict4s_proc.aux.make ict4s_proc.d ict4s_proc.dvi: /usr/local/texlive/2012/texmf-dist/tex/latex/base/latexsym.sty
ict4s_proc.aux ict4s_proc.aux.make ict4s_proc.d ict4s_proc.dvi: /usr/local/texlive/2012/texmf-dist/tex/latex/graphics/epsfig.sty
ict4s_proc.aux ict4s_proc.aux.make ict4s_proc.d ict4s_proc.dvi: /usr/local/texlive/2012/texmf-dist/tex/latex/graphics/graphics.sty
ict4s_proc.aux ict4s_proc.aux.make ict4s_proc.d ict4s_proc.dvi: /usr/local/texlive/2012/texmf-dist/tex/latex/graphics/graphicx.sty
ict4s_proc.aux ict4s_proc.aux.make ict4s_proc.d ict4s_proc.dvi: /usr/local/texlive/2012/texmf-dist/tex/latex/graphics/keyval.sty
ict4s_proc.aux ict4s_proc.aux.make ict4s_proc.d ict4s_proc.dvi: /usr/local/texlive/2012/texmf-dist/tex/latex/graphics/trig.sty
ict4s_proc.aux ict4s_proc.aux.make ict4s_proc.d ict4s_proc.dvi: ict4s_proc.tex
ict4s_proc.aux ict4s_proc.aux.make ict4s_proc.d ict4s_proc.dvi: ict4s_proc_article.cls
-include flies.gpi.d
-include fly.gpi.d
-include rosette.gpi.d
ict4s_proc.d ict4s_proc.dvi ict4s_proc._graphics: flies.eps
ict4s_proc.d ict4s_proc.dvi ict4s_proc._graphics: fly.eps
ict4s_proc.d ict4s_proc.dvi ict4s_proc._graphics: rosette.ps
ict4s_proc.bbl ict4s_proc.aux ict4s_proc.aux.make: ./examplebibliography.bib
