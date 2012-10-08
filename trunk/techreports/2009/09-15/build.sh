# Builds the individual PDFs for the proposal.

# Build summary.pdf
cp nsf.summary.tex nsf.tex
make clean
make 
cp nsf.pdf upload/summary.pdf

# Build project.pdf and biblio.pdf
cp project-techreport.tex project.tex
emacs -batch project.tex -l convert-project.el -f save-buffer
cp nsf.project.tex nsf.tex
make clean
make
pdftk nsf.pdf cat 1-15 output upload/project.pdf
pdftk nsf.pdf cat 16-19 output upload/biblio.pdf

# Build bio.pdf
cp nsf.bio.tex nsf.tex
make clean
make
cp nsf.pdf upload/bio.pdf

# Build budget.pdf
cp nsf.budget.tex nsf.tex
make clean
make
cp nsf.pdf upload/budget.pdf

# Build supplemental.pdf
cp nsf.supplemental.tex nsf.tex
make clean
make
cp nsf.pdf upload/supplemental.pdf

# Build facilities.pdf
cp nsf.facilities.tex nsf.tex
make clean
make
cp nsf.pdf upload/facilities.pdf

# # Finish by building all
# cp nsf.all.tex nsf.tex
# make clean
# make
# mv nsf.pdf nsf.all.pdf




