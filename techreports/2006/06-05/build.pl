#!/usr/bin/perl

if (@ARGV != 1) {
  printUsage();
}
else {
  $target = $ARGV[0];
  
  if ('dvi' eq $target) {
    doDvi();
  }
  elsif ('pdf' eq $target) {
    doPdf();
  }
  elsif ('figures' eq $target) {
    doFigures();
  }
  elsif ('clean' eq $target) {
    doClean();
  }
  elsif ('bertha' eq $target) {
    doBertha();
  }
  else {
    printUsage();
  }
}

sub printUsage() {
  print("Build latex document.\n\n");
  print("build.pl target\n\n");
  print("  target   dvi     - [Windows] Generate dvi with MikTeX.\n");
  print("           pdf     - [Windows] Generate pdf with MikTeX.\n");
  print("           figures - [Windows] Covert graphic fileswith MikTeX and Cygwin.\n");
  print("           clean   - [Windows] Clean intermediate files.\n");
  print("           bertha  - [Bertha]  Generate pdf on Bertha.\n");
  print("\n");
}


# [Windows MikTeX/Bertha] 
sub doPdf() {
  print("[Generating pdf file...]\n");
  my $pdfFileName = 'CedricDissertation';
  my $tempMainTexFileName = $pdfFileName.'.tex';

  #MikTeX pdfLatex page size bug fix, force paper size to be US LETTER
  open(INFILE, "thesis.tex") or die "$!";
  open(OUTFILE, ">$tempMainTexFileName") or die "$!";
  while (<INFILE>) {
    if ($_ =~ /\\begin{document}/) {
      print(OUTFILE "\n\n");
      print(OUTFILE "\\setlength{\\pdfpagewidth}{8.5in}\n");
      print(OUTFILE "\\setlength{\\pdfpageheight}{11in}\n");
      print(OUTFILE "\n\n");
    }
    print(OUTFILE $_);
  }
  close(INFILE);
  close(OUTFILE);

  #Main command
  doClean();
  $texCmd = "pdflatex -interaction=batchmode $pdfFileName";
  $bibCmd = "bibtex $pdfFileName";
  print `$texCmd`;
  print `$bibCmd`;
  print `$texCmd`;
  print `$texCmd`;
  print `del $tempMainTexFileName`;
  doClean();

  print("[Output pdf is $pdfFileName.pdf]\n");
}

# [Windows MikTex Cygwin] Converts all png files in figures directory to eps format.
#                         If there is no pdf file, copy the png file.
sub doFigures() {
  print("[Converting png files to eps...]\n");
  my $sourceDir = 'figures/png';
  my $destDir = 'figures';
  
  my $handleSourceDir;
  opendir($handleSourceDir, $sourceDir)
    or die "Can't open directory: $sourceDir. Error message: $!";
  foreach(@file = readdir($handleSourceDir)) {        #exam each file in the directory
    if ($_ =~ /\.png$/) {                #if the file is a png file.
      $sourceFile = $sourceDir.'/'.$_;  #sourceFile, *.png
      $destPngFile = $destDir.'/'.$_;    
      $destEpsFile = $destPngFile; $destEpsFile =~ s/\.png$/.eps/;       #destEpsFile, replace 'png' with 'eps'
      $destPdfFile = $destPngFile; $destPdfFile =~ s/\.png$/.pdf/;  

      if (! -e $destEpsFile) {                #only convert when dest file does not exist
        print("converting $sourceFile to eps format ...\n");
        `bmeps -c $sourceFile $destEpsFile`;  #'bmeps.exe' comes with MikTeX
      }

      #if pdf file does not exist, copy the png file
      if (! -e $destPdfFile  && ! -e $destPngFile) {
         print("Copying $sourceFile ...\n");
         print `cp $sourceFile $destPngFile`;  #cygwin has cp. If you use copy, you need to replace / with \
      }
    }
  }
  closedir($handleSourceDir);
}

# [Windows MikTeX/Bertha] Generates dvi file, needs eps graphs.
sub doDvi() {
  print("[Generating dvi file...]\n");
  $texCmd = 'latex -interaction=batchmode thesis';
  $bibCmd = 'bibtex  thesis';
  print `$texCmd`;
  print `$bibCmd`;
  print `$texCmd`;
  print `$texCmd`;
}

sub doClean() {
  print("[Cleaning...]\n");
  print `del *.aux`;
  print `del *.toc`;
  print `del *.log`;
  print `del *.bbl`;
  print `del *.blg`;
  print `del *.out`;
  #print `del figures\\*.eps`;

  my $outputFileName = 'thesis';
  print `del $outputFileName.dvi`;
  print `del $outputFileName.ps`;
  print `del $outputFileName.pdf`;
}

# [Bertha] Generates pdf file from dvi file, needs eps graphs.
sub doBertha() {
  print("[Generating pdf file from dvi on Bertha...]\n");
  doDvi();
  print `dvips thesis.dvi -o thesis.ps`;
  print `distill -resolution 2400 -colordownsample off thesis.ps`;
  print `rm -f thesis.ps`;
  print `mv thesis.pdf 06-05.pdf`;
}

