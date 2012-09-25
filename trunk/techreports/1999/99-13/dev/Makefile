
#default:	Makefile pubform


#####################################
## ICSE2000 pubform LaTEX example  ##
## (based on ICSE97 form)        ##
#####################################

icse-pubform:	pubform.ps

MACROS = icse2000.sty 
SECTIONS = pubform.tex

INGREDIENTS = $(MACROS) $(SECTIONS) 

paper.dvi: $(INGREDIENTS) 



####################################################################
## rules
########

%.dvi: %.tex  
	-@latex $*; while fgrep -s 'Rerun to get cross-references right.' $*.log; do latex $*; done

%.ps: %.dvi  
	dvips -f < $*.dvi > $@


####################################################################
## useful pseudotargets
#######################

bib::         ; bibtex paper; rm paper.dvi; 
clean::       ; /bin/rm -i *~ paper.aux paper.bbl paper.blg paper.log paper.toc 
preview::     ; xdvi paper
preview_ps::  ; gs paper.ps
print::       ; lpr paper.ps
print_sparc:: ; dvips -q -r -400 paper.dvi | lpr
spell::       ; tspell *.tex *.bbl
####################################################################
