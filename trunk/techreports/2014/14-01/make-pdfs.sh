#!/bin/bash

echo "Making bio"
( set -x ; cp ini/Makefile.bio.ini Makefile.ini ; make ; cp nsf.bio.pdf pdf ; make clean )

echo "Making budget"
( set -x ; cp ini/Makefile.budget.ini Makefile.ini ; make ; cp nsf.budget.pdf pdf ; make clean )

echo "Making data management plan"
( set -x ; cp ini/Makefile.data-management-plan.ini Makefile.ini ; make ; cp nsf.data-management-plan.pdf pdf ; make clean )

echo "Making facilities"
( set -x ; cp ini/Makefile.facilities.ini Makefile.ini ; make ; cp nsf.facilities.pdf pdf ; make clean )

echo "Making project"
( set -x ; cp ini/Makefile.project.ini Makefile.ini ; make ; cp nsf.project.pdf pdf ; make clean )

echo "Making summary"
( set -x ; cp ini/Makefile.summary.ini Makefile.ini ; make ; cp nsf.summary.pdf pdf ; make clean )

echo "Making supplemental"
( set -x ; cp ini/Makefile.supplemental.ini Makefile.ini ; make ; cp nsf.supplemental.pdf pdf ; make clean )

echo "Splitting project summary"
( set -x ; ./pdfsplit.sh pdf/nsf.project.pdf 1 10 pdf/nsf.project.nobib.pdf )
( set -x ; ./pdfsplit.sh pdf/nsf.project.pdf 11 14 pdf/nsf.project.justbib.pdf )


